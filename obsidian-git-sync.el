;;; obsidian-git-sync.el --- Bidirectional Git sync for Obsidian -*- lexical-binding: t; -*-

;;; Commentary:
;; Automatically commit and push saved notes, and periodically pull remote
;; changes for Git repositories inside the active Obsidian vault.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup obsidian-git-sync nil
  "Automatic Git synchronization for Obsidian notes."
  :group 'obsidian)

(defcustom obsidian-git-auto-sync-on-save t
  "When non-nil, commit and push Obsidian notes after saving."
  :type 'boolean
  :group 'obsidian-git-sync)

(defcustom obsidian-git-auto-pull-interval 60
  "Seconds between checks for remote Git changes.
Set this to nil or 0 to disable periodic pulls.  A manual pull is always
available through `obsidian-git-sync-pull'."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'obsidian-git-sync)

(defvar obsidian-git-sync--timer nil
  "Timer used to check Git repositories for remote changes.")

(defvar obsidian-git-sync--pull-processes (make-hash-table :test 'equal)
  "Active inbound synchronization processes, keyed by repository path.")

(defconst obsidian-git-sync--push-script
  "set -e
repo=$1
file=$2
git_dir=$(git -C \"$repo\" rev-parse --git-dir)
case $git_dir in
  /*) ;;
  *) git_dir=$repo/$git_dir ;;
esac
lock=$git_dir/obsidian-auto-sync.lock
attempt=0
while ! mkdir \"$lock\" 2>/dev/null; do
  attempt=$((attempt + 1))
  if [ $attempt -ge 150 ]; then
    echo 'Timed out waiting for another Obsidian Git sync.' >&2
    exit 1
  fi
  sleep 0.2
done
trap 'rmdir \"$lock\" 2>/dev/null || true' EXIT
git -C \"$repo\" add -- \"$file\"
if git -C \"$repo\" diff --cached --quiet -- \"$file\"; then
  exit 0
fi
git -C \"$repo\" commit -m \"Auto-save: ${file:t}\" -- \"$file\"
branch=$(git -C \"$repo\" symbolic-ref --quiet --short HEAD)
if ! git -C \"$repo\" pull --rebase --autostash origin \"$branch\"; then
  git -C \"$repo\" rebase --abort >/dev/null 2>&1 || true
  exit 1
fi
git -C \"$repo\" push origin \"$branch\"
"
  "Shell script used to publish a saved note without losing remote changes.")

(defconst obsidian-git-sync--pull-script
  "set -e
repo=$1
git_dir=$(git -C \"$repo\" rev-parse --git-dir)
case $git_dir in
  /*) ;;
  *) git_dir=$repo/$git_dir ;;
esac
lock=$git_dir/obsidian-auto-sync.lock
attempt=0
while ! mkdir \"$lock\" 2>/dev/null; do
  attempt=$((attempt + 1))
  if [ $attempt -ge 150 ]; then
    echo 'Timed out waiting for another Obsidian Git sync.' >&2
    exit 1
  fi
  sleep 0.2
done
trap 'rmdir \"$lock\" 2>/dev/null || true' EXIT
if [ -n \"$(git -C \"$repo\" status --porcelain)\" ]; then
  echo SKIPPED_DIRTY
  exit 0
fi
branch=$(git -C \"$repo\" symbolic-ref --quiet --short HEAD)
before=$(git -C \"$repo\" rev-parse HEAD)
git -C \"$repo\" pull --rebase origin \"$branch\"
git -C \"$repo\" push origin \"$branch\"
after=$(git -C \"$repo\" rev-parse HEAD)
if [ \"$before\" = \"$after\" ]; then
  echo UNCHANGED
else
  echo \"UPDATED $before $after\"
fi
"
  "Shell script used to fetch remote changes and publish queued commits.")

(defun obsidian-git-sync--vault-directory ()
  "Return the active Obsidian vault as a true directory name."
  (let ((vault (and (boundp 'obsidian--vault) obsidian--vault)))
    (when (and vault (file-directory-p vault))
      (file-name-as-directory (file-truename vault)))))

(defun obsidian-git-sync--repository (file)
  "Return FILE's Git repository when it is inside the active vault."
  (let ((vault (obsidian-git-sync--vault-directory)))
    (when (and vault
               (file-regular-p file)
               (file-in-directory-p (file-truename file) vault))
      (let ((repository (locate-dominating-file file ".git")))
        (when repository
          (file-name-as-directory (file-truename repository)))))))

(defun obsidian-git-sync--scan-directory-p (directory)
  "Return non-nil when DIRECTORY should be scanned for notes."
  (not (string= (file-name-nondirectory (directory-file-name directory))
                ".git")))

(defun obsidian-git-sync--repositories ()
  "Return Git repositories containing Markdown notes in the active vault."
  (when-let ((vault (obsidian-git-sync--vault-directory)))
    (let ((repositories nil)
          (root-repository (locate-dominating-file vault ".git")))
      (when (and root-repository
                 (file-in-directory-p (file-truename root-repository) vault))
        (push (file-name-as-directory (file-truename root-repository))
              repositories))
      (dolist (file (directory-files-recursively
                     vault "\\.md\\'" nil
                     #'obsidian-git-sync--scan-directory-p))
        (when-let ((repository (locate-dominating-file file ".git")))
          (push (file-name-as-directory (file-truename repository))
                repositories)))
      (delete-dups repositories))))

(defun obsidian-git-sync--modified-buffer-p (repository)
  "Return non-nil when REPOSITORY has an unsaved visiting buffer."
  (cl-some
   (lambda (buffer)
     (with-current-buffer buffer
       (and buffer-file-name
            (buffer-modified-p)
            (ignore-errors
              (file-in-directory-p (file-truename buffer-file-name)
                                   repository)))))
   (buffer-list)))

(defun obsidian-git-sync--refresh-workspace (repository)
  "Refresh clean buffers and Obsidian views after updating REPOSITORY."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and buffer-file-name
                 (not (buffer-modified-p))
                 (file-exists-p buffer-file-name)
                 (ignore-errors
                   (file-in-directory-p (file-truename buffer-file-name)
                                        repository))
                 (not (verify-visited-file-modtime buffer)))
        (revert-buffer t t t))))
  (when (and (boundp 'obsidian-tree-buffer-name)
             (get-buffer obsidian-tree-buffer-name)
             (fboundp 'obsidian--tree-refresh))
    (obsidian--tree-refresh))
  (when (fboundp 'obsidian-refresh-graph)
    (obsidian-refresh-graph)))

(defun obsidian-git-sync--push-sentinel (file buffer process event)
  "Report push PROCESS completion for FILE using BUFFER and EVENT."
  (when (memq (process-status process) '(exit signal))
    (if (and (eq (process-status process) 'exit)
             (zerop (process-exit-status process)))
        (progn
          (when (buffer-live-p buffer)
            (kill-buffer buffer))
          (message "GitHub sync complete: %s" (file-name-nondirectory file)))
      (message "GitHub sync failed for %s; see %s (%s)"
               (file-name-nondirectory file)
               (buffer-name buffer)
               (string-trim event)))))

(defun obsidian-git-sync--pull-sentinel (repository buffer process event)
  "Refresh REPOSITORY after inbound PROCESS, using BUFFER and EVENT."
  (when (memq (process-status process) '(exit signal))
    (remhash repository obsidian-git-sync--pull-processes)
    (let ((output (when (buffer-live-p buffer)
                    (with-current-buffer buffer (buffer-string)))))
      (if (and (eq (process-status process) 'exit)
               (zerop (process-exit-status process)))
          (progn
            (when (and output (string-match-p "^UPDATED " output))
              (obsidian-git-sync--refresh-workspace repository)
              (message "GitHub changes applied: %s"
                       (file-name-nondirectory
                        (directory-file-name repository))))
            (when (buffer-live-p buffer)
              (kill-buffer buffer)))
        (message "GitHub pull failed for %s; see %s (%s)"
                 (file-name-nondirectory (directory-file-name repository))
                 (buffer-name buffer)
                 (string-trim event))))))

(defun obsidian-git-sync-after-save ()
  "Commit, rebase, and push the current saved note when Git sync applies."
  (when (and obsidian-git-auto-sync-on-save buffer-file-name)
    (let* ((file (file-truename buffer-file-name))
           (repo (obsidian-git-sync--repository file)))
      (when repo
        (let* ((relative (file-relative-name file repo))
               (output (generate-new-buffer " *Obsidian Git Push*"))
               (process
                (make-process
                 :name (format "obsidian-git-push-%s" (file-name-base file))
                 :buffer output
                 :command (list "/bin/zsh" "-c" obsidian-git-sync--push-script
                                "obsidian-git-sync" repo relative)
                 :noquery t)))
          (set-process-sentinel
           process
           (apply-partially #'obsidian-git-sync--push-sentinel
                            file output)))))))

(defun obsidian-git-sync-pull ()
  "Synchronize every Git repository in the active vault in both directions."
  (interactive)
  (condition-case error-data
      (dolist (repository (obsidian-git-sync--repositories))
        (cond
         ((obsidian-git-sync--modified-buffer-p repository)
          (message "GitHub pull deferred; unsaved note in %s"
                   (file-name-nondirectory
                    (directory-file-name repository))))
         ((process-live-p (gethash repository
                                   obsidian-git-sync--pull-processes)))
         (t
          (let* ((output (generate-new-buffer " *Obsidian Git Pull*"))
                 (process
                  (make-process
                   :name (format
                          "obsidian-git-pull-%s"
                          (file-name-nondirectory
                           (directory-file-name repository)))
                   :buffer output
                   :command (list "/bin/zsh" "-c"
                                  obsidian-git-sync--pull-script
                                  "obsidian-git-sync" repository)
                   :noquery t)))
            (puthash repository process obsidian-git-sync--pull-processes)
            (set-process-sentinel
             process
             (apply-partially #'obsidian-git-sync--pull-sentinel
                              repository output))))))
    (error (message "GitHub sync could not start: %s"
                    (error-message-string error-data)))))

(defun obsidian-git-sync--stop-timer ()
  "Stop periodic inbound synchronization."
  (when (timerp obsidian-git-sync--timer)
    (cancel-timer obsidian-git-sync--timer))
  (setq obsidian-git-sync--timer nil))

(defun obsidian-git-sync--workspace-visible-p ()
  "Return non-nil while an Obsidian workspace is visible."
  (or (and (boundp 'obsidian-tree-buffer-name)
           (get-buffer-window obsidian-tree-buffer-name 'visible))
      (and (boundp 'obsidian-graph-buffer-name)
           (get-buffer-window obsidian-graph-buffer-name 'visible))))

(defun obsidian-git-sync--timer-tick ()
  "Pull remote changes only while an Obsidian workspace is visible."
  (when (obsidian-git-sync--workspace-visible-p)
    (obsidian-git-sync-pull)))

(defun obsidian-git-sync--start-timer ()
  "Start periodic inbound synchronization for the active vault."
  (obsidian-git-sync--stop-timer)
  (when (and (numberp obsidian-git-auto-pull-interval)
             (> obsidian-git-auto-pull-interval 0))
    (setq obsidian-git-sync--timer
          (run-at-time 0 obsidian-git-auto-pull-interval
                       #'obsidian-git-sync--timer-tick))))

;;;###autoload
(define-minor-mode obsidian-git-auto-sync-mode
  "Keep Git repositories in the active Obsidian vault synchronized."
  :global t
  :group 'obsidian-git-sync
  (if obsidian-git-auto-sync-mode
      (progn
        (add-hook 'after-save-hook #'obsidian-git-sync-after-save)
        (obsidian-git-sync--start-timer))
    (remove-hook 'after-save-hook #'obsidian-git-sync-after-save)
    (obsidian-git-sync--stop-timer)))

(provide 'obsidian-git-sync)
;;; obsidian-git-sync.el ends here
