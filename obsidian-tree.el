;;; obsidian-tree.el --- File tree panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Builds the clickable folder tree and filters editor-generated temp files.

;;; Code:

(require 'cl-lib)

(defvar obsidian--vault)
(defvar obsidian--current-scope)
(defvar obsidian-tree-buffer-name)
(declare-function obsidian--open-note "obsidian-editor")
(declare-function obsidian--schedule-graph-update "obsidian-graph")

(defcustom obsidian-file-extension "md"
  "File extension for notes."
  :type 'string
  :group 'obsidian)

(defface obsidian-tree-file
  '((t :foreground "light blue"))
  "Face for files in the tree."
  :group 'obsidian)

(defface obsidian-tree-dir
  '((t :foreground "goldenrod" :weight bold))
  "Face for directories in the tree."
  :group 'obsidian)

(defface obsidian-link
  '((t :inherit link))
  "Face for wiki links."
  :group 'obsidian)

(defvar-local obsidian--tree-expanded nil
  "Hash table of expanded directories in the current tree buffer.")

(define-derived-mode obsidian-tree-mode fundamental-mode "Obsidian-Tree"
  "Major mode for the Obsidian file tree panel.
RET or mouse-click opens the file at cursor.
TAB or left/right arrows expand/collapse directories."
  :keymap obsidian-tree-mode-map
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (use-local-map obsidian-tree-mode-map))

(defun obsidian--note-file-p (file)
  "Return non-nil if FILE is a real, readable note file.
Emacs lock files such as `.#NAME', auto-save files such as `#NAME#', and
backup files such as `NAME~' are deliberately excluded."
  (let ((name (file-name-nondirectory file)))
    (and (string-suffix-p (concat "." obsidian-file-extension) name)
         (not (string-prefix-p ".#" name))
         (not (and (string-prefix-p "#" name)
                   (string-suffix-p "#" name)))
         (not (string-suffix-p "~" name))
         (file-regular-p file)
         (file-readable-p file))))

(defun obsidian-refresh-tree ()
  "Refresh the file tree."
  (interactive)
  (obsidian--tree-refresh))

(defun obsidian--tree-refresh ()
  "Rebuild the file tree buffer."
  (interactive)
  (with-current-buffer obsidian-tree-buffer-name
    (let ((inhibit-read-only t)
          (pt (point)))
      (erase-buffer)
      (unless obsidian--tree-expanded
        (setq-local obsidian--tree-expanded (make-hash-table :test 'equal)))
      (insert (propertize (format "Vault: %s\n" obsidian--vault)
                          'face 'obsidian-tree-dir))
      (obsidian--tree-insert obsidian--vault 0 obsidian--tree-expanded)
      (goto-char (min pt (point-max)))
      (set-buffer-modified-p nil))))

(defun obsidian--tree-insert (dir depth expanded)
  "Insert DIR at DEPTH using the EXPANDED directory table."
  ;; Let `directory-files' sort alphabetically.  A stable tree is easier to
  ;; scan and does not jump around between operating systems.
  (let* ((entries (ignore-errors
                    (directory-files dir t "^[^.]" nil)))
         (dirs  (cl-remove-if-not #'file-directory-p entries))
         (files (cl-remove-if (lambda (f)
                                (or (file-directory-p f)
                                    (not (obsidian--note-file-p f))))
                              entries)))
    (dolist (d dirs)
      (let* ((name (file-name-nondirectory d))
             (openp (gethash d expanded))
             (marker (if openp "v " "> ")))
        (insert (propertize (concat (make-string (* depth 2) ?\s) marker name "/\n")
                            'face 'obsidian-tree-dir
                            'obsidian-path d
                            'keymap obsidian-tree-mode-map
                            'mouse-face 'highlight
                            'help-echo (format "Directory: %s" d)))
        (when openp
          (obsidian--tree-insert d (1+ depth) expanded))))
    (dolist (f files)
      (let ((name (file-name-nondirectory f)))
        (insert (propertize (concat (make-string (* depth 2) ?\s) "  " name "\n")
                            'face 'obsidian-tree-file
                            'obsidian-path f
                            'keymap obsidian-tree-mode-map
                            'mouse-face 'highlight
                            'help-echo (format "Open: %s" f)))))))

(defun obsidian--tree-toggle ()
  "Expand/collapse directory at line point."
  (interactive)
  (let ((path (get-text-property (point) 'obsidian-path)))
    (when (and path (file-directory-p path))
      (unless obsidian--tree-expanded
        (setq-local obsidian--tree-expanded (make-hash-table :test 'equal)))
      (if (gethash path obsidian--tree-expanded)
          (remhash path obsidian--tree-expanded)
        (puthash path t obsidian--tree-expanded))
      (obsidian--tree-refresh))))

(defun obsidian--tree-collapse ()
  "Collapse directory at point (left arrow)."
  (interactive)
  (let ((path (get-text-property (point) 'obsidian-path)))
    (when (and path (file-directory-p path))
      (when obsidian--tree-expanded
        (remhash path obsidian--tree-expanded))
      (obsidian--tree-refresh))))

(defun obsidian--tree-expand ()
  "Expand directory at point (right arrow)."
  (interactive)
  (let ((path (get-text-property (point) 'obsidian-path)))
    (when (and path (file-directory-p path))
      (unless obsidian--tree-expanded
        (setq-local obsidian--tree-expanded (make-hash-table :test 'equal)))
      (puthash path t obsidian--tree-expanded)
      (obsidian--tree-refresh))))

(defun obsidian--tree-open ()
  "Open file or toggle directory at line point."
  (interactive)
  (let ((path (get-text-property (point) 'obsidian-path)))
    (cond
     ((and path (file-directory-p path))
      (setq obsidian--current-scope path)
      (unless obsidian--tree-expanded
        (setq-local obsidian--tree-expanded (make-hash-table :test 'equal)))
      (if (gethash path obsidian--tree-expanded)
          (remhash path obsidian--tree-expanded)
        (puthash path t obsidian--tree-expanded))
      (obsidian--tree-refresh)
      (obsidian--schedule-graph-update))
     ((and path (file-regular-p path))
      (obsidian--open-note path))
     (t
      (message "Nothing at cursor")))))

(defun obsidian--tree-mouse-open (event)
  "Open file or toggle directory at mouse click EVENT."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (position (posn-point (event-end event))))
    (when (and (windowp window) (integer-or-marker-p position))
      (with-selected-window window
        (goto-char position)
        ;; A click at the visual end can land on the newline.
        (unless (get-text-property (point) 'obsidian-path)
          (when (> (point) (line-beginning-position)) (backward-char 1)))
        (obsidian--tree-open)))))

(provide 'obsidian-tree)
;;; obsidian-tree.el ends here
