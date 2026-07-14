;;; obsidian-editor.el --- Editor mode, links, notes, auto-create -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides note opening, wiki-link interaction, history, and note creation.

;;; Code:

(require 'cl-lib)

(defvar obsidian--vault)
(defvar obsidian--current-file)
(defvar obsidian--current-scope)
(defvar obsidian--history)
(defvar obsidian-link-regexp)
(defvar obsidian-file-extension)
(defvar obsidian-tree-buffer-name)
(defvar obsidian-editor-buffer-name-prefix)

(declare-function obsidian--tree-refresh "obsidian-tree")
(declare-function obsidian--schedule-graph-update "obsidian-graph")
(declare-function obsidian--find-note "obsidian-graph")
(declare-function obsidian--all-note-names "obsidian-graph")
(declare-function obsidian--editor-window "obsidian-windows")
(declare-function obsidian--note-file-p "obsidian-tree")

(defvar-local obsidian--saved-link-names nil
  "Wiki-link targets recorded when this note was opened or last saved.")


;; Editor minor mode

(define-minor-mode obsidian-editor-mode
  "Minor mode for the Obsidian editor window."
  :lighter " Obs"
  :keymap obsidian-editor-mode-map
  (if obsidian-editor-mode
      (progn
        ;; Emacs normally truncates text in narrow side-by-side windows via
        ;; `truncate-partial-width-windows'.  The editor pane must instead
        ;; reflow visually whenever the frame or pane becomes narrower.
        (setq-local truncate-lines nil)
        (setq-local truncate-partial-width-windows nil)
        (visual-line-mode 1)
        ;; Buffer-local registration avoids running Obsidian work after every
        ;; save in unrelated Emacs buffers.
        (add-hook 'after-save-hook #'obsidian--after-save nil t)
        (obsidian--fontify-links)
        ;; Keep the pre-edit targets so a simple link edit can rename its note.
        (setq obsidian--saved-link-names (obsidian--link-names-in-buffer)))
    (remove-hook 'after-save-hook #'obsidian--after-save t)))


;; Note opening

(defun obsidian--open-note (file &optional no-record)
  "Open FILE in the editor window.
Unless NO-RECORD, push the previous file onto the history."
  (when (and obsidian--current-file (not no-record))
    (with-current-buffer (window-buffer (obsidian--editor-window))
      (push (cons obsidian--current-file (point)) obsidian--history)))
  (setq obsidian--current-file (expand-file-name file))
  (setq obsidian--current-scope (file-name-directory obsidian--current-file))
  (let ((buf (find-file-noselect obsidian--current-file)))
    (set-window-buffer (obsidian--editor-window) buf)
    (with-current-buffer buf
      (obsidian-editor-mode 1))
    (when (get-buffer obsidian-editor-buffer-name-prefix)
      (kill-buffer obsidian-editor-buffer-name-prefix))
    (select-window (obsidian--editor-window))
    (obsidian--tree-refresh)
    (obsidian--schedule-graph-update)))


;; Link rendering

(defvar obsidian-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'obsidian--mouse-follow-link)
    map)
  "Keymap placed only on rendered wiki links.")

(defun obsidian--fontify-links ()
  "Apply link face to wiki links in the current buffer."
  ;; Remove only ranges previously owned by this package.  Markdown mode's
  ;; unrelated font-lock properties must remain untouched.
  (let ((position (point-min)))
    (while (< position (point-max))
      (let ((next (next-single-property-change
                   position 'obsidian-link-property nil (point-max))))
        (when (get-text-property position 'obsidian-link-property)
          (remove-list-of-text-properties
           position next '(obsidian-link-property face mouse-face keymap)))
        (setq position next))))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward obsidian-link-regexp nil t)
      (add-text-properties
       (match-beginning 0) (match-end 0)
       `(obsidian-link-property t face obsidian-link mouse-face highlight
                                keymap ,obsidian-link-map)))))

(defun obsidian--setup-mouse ()
  "Refresh clickable wiki-link properties in the current buffer."
  (obsidian--fontify-links))

(defun obsidian--mouse-follow-link (event)
  "Follow the wiki link under the mouse click EVENT."
  (interactive "e")
  (posn-set-point (event-end event))
  (obsidian-follow-link-at-point))


;; Link operations

(defun obsidian-insert-link (target &optional alias)
  "Insert a wiki link to TARGET, with optional ALIAS."
  (interactive
   (list (completing-read "Link to: " (obsidian--all-note-names))
         (read-string "Alias (optional): ")))
  (insert (if (string-empty-p alias)
              (format "[[%s]]" target)
            (format "[[%s|%s]]" target alias)))
  (obsidian--fontify-links))

(defun obsidian-follow-link-at-point ()
  "Follow the wiki link at or near point."
  (interactive)
  (let ((target (obsidian--link-target-at-point)))
    (unless target
      (user-error "No link at point"))
    (let* ((name (obsidian--link-name target))
           (file (obsidian--find-note name)))
      (if file
          (obsidian--open-note file)
        (when (y-or-n-p (format "Note \"%s\" does not exist. Create it? " name))
          (obsidian--create-note-impl name))))))

(defun obsidian--link-target-at-point ()
  "Return the inner text of the wiki link at point, or nil."
  (save-excursion
    (catch 'found
      (when (re-search-backward "\\[\\[" (line-beginning-position 0) t)
        (when (looking-at obsidian-link-regexp)
          (throw 'found (match-string 1))))
      (when (re-search-forward obsidian-link-regexp (line-end-position) t)
        (throw 'found (match-string 1)))
      nil)))

(defun obsidian--link-name (target)
  "Return the note name portion of wiki-link TARGET."
  (string-trim (car (split-string target "[|#]"))))

(defun obsidian--link-names-in-buffer ()
  "Return the unique wiki-link note names in the current buffer."
  (let (names)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward obsidian-link-regexp nil t)
        (let ((name (obsidian--link-name (match-string-no-properties 1))))
          (unless (string-empty-p name)
            (push name names)))))
    (delete-dups (nreverse names))))


;; Note creation

(defcustom obsidian-auto-timestamp t
  "If non-nil, insert a timestamp when creating a new note."
  :type 'boolean
  :group 'obsidian)

(defcustom obsidian-timestamp-format "[%Y-%m-%d %a %H:%M]"
  "Format string for auto-inserted timestamp.
Uses `format-time-string' syntax."
  :type 'string
  :group 'obsidian)

(defun obsidian-create-note (name)
  "Create and open a new note NAME.
NAME can include a subdirectory path relative to the vault, e.g. \"music/song\"."
  (interactive
   (list (read-string "New note name (use / for subdirectory): "
                      (and (use-region-p)
                           (buffer-substring-no-properties
                            (region-beginning) (region-end))))))
  (obsidian--create-note-impl name))

(defun obsidian--create-note-impl (name)
  "Create and open a note named NAME."
  (let* (;; A simple name belongs beside the currently viewed notes.  An
         ;; explicit path such as "music/brass" remains vault-relative.
         (base (if (string-match-p "/" name)
                   obsidian--vault
                 (or obsidian--current-scope obsidian--vault)))
         (file (obsidian--safe-note-path name base))
         (existed (file-exists-p file)))
    (unless existed
      (obsidian--write-new-note file (file-name-base name)))
    (obsidian--open-note file)
    (obsidian--tree-refresh)
    (message (if existed "Opened existing note: %s" "Created note: %s") name)))


;; Back navigation

(defun obsidian-jump-back ()
  "Jump back to the previous note in history."
  (interactive)
  (if (null obsidian--history)
      (message "No history to go back to.")
    (let* ((entry (pop obsidian--history))
           (file (car entry))
           (pos  (cdr entry)))
      (obsidian--open-note file t)
      (when (buffer-live-p (current-buffer))
        (goto-char (min pos (point-max)))))))


;; Auto-create linked files on save

(defun obsidian--rename-edited-link-target (new-link-names)
  "Rename one unambiguous edited link target using NEW-LINK-NAMES.
The rename is performed only when exactly one old target disappeared and one
new target appeared.  This avoids guessing after a larger link edit."
  (let ((removed (cl-set-difference obsidian--saved-link-names new-link-names
                                    :test #'string=))
        (added (cl-set-difference new-link-names obsidian--saved-link-names
                                  :test #'string=)))
    (when (and (= (length removed) 1) (= (length added) 1))
      (let* ((old-name (car removed))
             (new-name (car added))
             (old-file (obsidian--find-note old-name)))
        (when old-file
          ;; A renamed note stays beside the old note, even when the editing
          ;; note and the link target live in different directories.
          (let ((new-file (obsidian--safe-note-path
                           new-name (file-name-directory old-file))))
            (unless (or (file-exists-p new-file)
                        (obsidian--find-note new-name))
              (let ((target-buffer (find-buffer-visiting old-file))
                    (renamed-current
                     (and obsidian--current-file
                          (file-equal-p obsidian--current-file old-file))))
                (make-directory (file-name-directory new-file) t)
                (rename-file old-file new-file)
                (when (buffer-live-p target-buffer)
                  (with-current-buffer target-buffer
                    (set-visited-file-name new-file t)))
                (when renamed-current
                  (setq obsidian--current-file new-file))
                (message "Renamed linked note: %s -> %s"
                         (file-name-nondirectory old-file)
                         (file-name-nondirectory new-file))))))))))

(defun obsidian--safe-note-path (name base-directory)
  "Return a safe note path for NAME below BASE-DIRECTORY and the vault."
  (when (string-empty-p (string-trim name))
    (user-error "Note name cannot be empty"))
  (let* ((vault (file-name-as-directory (expand-file-name obsidian--vault)))
         (file (expand-file-name (concat name "." obsidian-file-extension)
                                 base-directory)))
    (unless (string-prefix-p vault file)
      (user-error "Note path must stay inside the vault: %s" name))
    file))

(defun obsidian--write-new-note (file title)
  "Create FILE with TITLE and the configured timestamp."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert (format "# %s\n\n" title))
    (when obsidian-auto-timestamp
      (insert (format "%s\n\n"
                      (format-time-string obsidian-timestamp-format))))))

(defun obsidian--auto-create-linked-files ()
  "Create .md files for [[link]] targets that don't exist yet.
New files are created in the same directory as the current file."
  (when (and obsidian--current-file obsidian--vault)
    (let ((current-dir (file-name-directory obsidian--current-file)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward obsidian-link-regexp nil t)
          (let* ((target (match-string 1))
                 (name (obsidian--link-name target))
                 (file (obsidian--safe-note-path name current-dir)))
            (unless (or (string-empty-p name)
                        (file-exists-p file)
                        (obsidian--find-note name))
              (obsidian--write-new-note file (file-name-base name))
              (message "Auto-created: %s" file)
              (when (get-buffer obsidian-tree-buffer-name)
                (obsidian--tree-refresh)))))))))


;; After-save hook

(defun obsidian--after-save ()
  "Refresh graph, tree, and auto-create linked files after saving."
  (when (and obsidian--vault
             (buffer-file-name)
             (obsidian--note-file-p (buffer-file-name)))
    (let ((new-link-names (obsidian--link-names-in-buffer)))
      (obsidian--rename-edited-link-target new-link-names)
      (save-excursion
        (obsidian--auto-create-linked-files))
      (setq obsidian--saved-link-names new-link-names))
    (obsidian--fontify-links)
    (when (get-buffer obsidian-tree-buffer-name)
      (obsidian--tree-refresh))
    (obsidian--schedule-graph-update)))

(provide 'obsidian-editor)
;;; obsidian-editor.el ends here
