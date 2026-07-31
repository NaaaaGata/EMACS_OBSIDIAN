;;; obsidian-git-sync.el --- Push saved Obsidian notes to Git -*- lexical-binding: t; -*-

;;; Commentary:
;; Automatically commit and push a saved file when it belongs to a Git
;; repository inside the active Obsidian vault.

;;; Code:

(require 'subr-x)

(defgroup obsidian-git-sync nil
  "Automatic Git synchronization for Obsidian notes."
  :group 'obsidian)

(defcustom obsidian-git-auto-sync-on-save t
  "When non-nil, commit and push Obsidian notes after saving."
  :type 'boolean
  :group 'obsidian-git-sync)

(defconst obsidian-git-sync--script
  "set -e
repo=$1
file=$2
lock=$repo/.git/obsidian-auto-sync.lock
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
git -C \"$repo\" push origin HEAD
"
  "Shell script used for serialized note synchronization.")

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

(defun obsidian-git-sync--sentinel (file buffer process event)
  "Report sync PROCESS completion for FILE using BUFFER and EVENT."
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

(defun obsidian-git-sync-after-save ()
  "Commit and push the current saved note when Git sync applies."
  (when (and obsidian-git-auto-sync-on-save buffer-file-name)
    (let* ((file (file-truename buffer-file-name))
           (repo (obsidian-git-sync--repository file)))
      (when repo
        (let* ((relative (file-relative-name file repo))
               (output (generate-new-buffer " *Obsidian Git Sync*"))
               (process
                (make-process
                 :name (format "obsidian-git-sync-%s" (file-name-base file))
                 :buffer output
                 :command (list "/bin/zsh" "-c" obsidian-git-sync--script
                                "obsidian-git-sync" repo relative)
                 :noquery t)))
          (set-process-sentinel
           process
           (apply-partially #'obsidian-git-sync--sentinel
                            file output)))))))

;;;###autoload
(define-minor-mode obsidian-git-auto-sync-mode
  "Automatically commit and push files saved in an Obsidian Git vault."
  :global t
  :group 'obsidian-git-sync
  (if obsidian-git-auto-sync-mode
      (add-hook 'after-save-hook #'obsidian-git-sync-after-save)
    (remove-hook 'after-save-hook #'obsidian-git-sync-after-save)))

(provide 'obsidian-git-sync)
;;; obsidian-git-sync.el ends here
