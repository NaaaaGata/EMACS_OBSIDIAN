;;; obsidian-tree.el --- File tree panel -*- lexical-binding: t; -*-

;;; Commentary:
;; Builds the clickable folder tree and filters editor-generated temp files.

;;; Code:

(require 'cl-lib)

(defvar obsidian--vault)
(defvar obsidian--current-scope)
(defvar obsidian--current-file)
(defvar obsidian-tree-buffer-name)
(defvar obsidian-delete-by-moving-to-trash)
(declare-function obsidian--open-note "obsidian-editor")
(declare-function obsidian--schedule-graph-update "obsidian-graph")
(declare-function obsidian--editor-window "obsidian-windows")
(declare-function obsidian--empty-editor-buffer "obsidian-windows")

(defcustom obsidian-file-extension "md"
  "File extension for notes."
  :type 'string
  :group 'obsidian)

(defcustom obsidian-tree-marquee-interval 0.40
  "Seconds between steps of an overflowing tree label.
Set this to nil to disable animated labels."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'obsidian)

(defcustom obsidian-tree-marquee-gap " "
  "Text separating the end and beginning of a scrolling tree label."
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

(defvar-local obsidian--tree-marquee-timer nil
  "Timer used to animate labels wider than the tree window.")

(defvar-local obsidian--tree-marquee-step 0
  "Current character offset of the tree marquee.")

(define-derived-mode obsidian-tree-mode fundamental-mode "Obsidian-Tree"
  "Major mode for the Obsidian file tree panel.
RET or mouse-click opens the file at cursor.
Up/down arrows move between entries.
TAB or left/right arrows expand/collapse directories."
  :keymap obsidian-tree-mode-map
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (use-local-map obsidian-tree-mode-map)
  (obsidian--tree-start-marquee)
  (add-hook 'kill-buffer-hook #'obsidian--tree-stop-marquee nil t))

(defun obsidian--tree-stop-marquee ()
  "Stop the marquee timer belonging to the current tree buffer."
  (when (timerp obsidian--tree-marquee-timer)
    (cancel-timer obsidian--tree-marquee-timer))
  (setq obsidian--tree-marquee-timer nil))

(defun obsidian--tree-start-marquee ()
  "Start the tree label marquee for the current buffer."
  (obsidian--tree-stop-marquee)
  (when (and obsidian-tree-marquee-interval
             (> obsidian-tree-marquee-interval 0))
    (setq obsidian--tree-marquee-timer
          (run-with-timer
           obsidian-tree-marquee-interval
           obsidian-tree-marquee-interval
           #'obsidian--tree-marquee-tick
           (current-buffer)))))

(defun obsidian--tree-marquee-text (name available step)
  "Return a marquee view of NAME at STEP using at most AVAILABLE columns.
Scrolling advances by characters rather than raw display columns, so a
double-width Japanese glyph is never cut in half."
  (let* ((cycle (concat name obsidian-tree-marquee-gap))
         (cycle-length (max 1 (length cycle)))
         (offset (% step cycle-length))
         ;; Repeating three times is enough because AVAILABLE is smaller than
         ;; NAME whenever this helper is called.
         (source (concat cycle cycle cycle))
         (visible (truncate-string-to-width
                   (substring source offset) available nil nil "")))
    visible))

(defun obsidian--tree-update-marquee ()
  "Update display properties for every tree label in the current buffer."
  (when-let ((window (get-buffer-window (current-buffer) t)))
    ;; Keep one safety column unused.  Exact-width terminal lines can trigger
    ;; an implicit continuation line and corrupt the neighboring pane.
    (let ((window-width (max 1 (1- (window-body-width window))))
          (inhibit-read-only t)
          (position (point-min)))
      ;; A timer must never move the user's tree cursor.
      (save-excursion
        (while (< position (point-max))
          (goto-char position)
          (let* ((line-end (line-end-position))
                 (name (get-text-property position 'obsidian-tree-name))
                 (name-start (and name
                                  (next-single-property-change
                                   position 'obsidian-tree-prefix nil line-end)))
                 (name-end (and name-start
                                (next-single-property-change
                                 name-start 'obsidian-tree-name nil line-end)))
                 (prefix-width (or (get-text-property
                                    position 'obsidian-tree-prefix-width)
                                   0))
                 (available (max 1 (- window-width prefix-width))))
            (when (and name-start name-end)
              (if (> (string-width name) available)
                  (put-text-property
                   name-start name-end 'display
                   (obsidian--tree-marquee-text
                    name available obsidian--tree-marquee-step))
                (remove-text-properties name-start name-end '(display nil))))
            (setq position (min (point-max) (1+ line-end))))))
      (set-buffer-modified-p nil))))

(defun obsidian--tree-marquee-tick (buffer)
  "Advance scrolling labels in tree BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (get-buffer-window buffer t)
        (cl-incf obsidian--tree-marquee-step)
        (obsidian--tree-update-marquee)))))

(defun obsidian--tree-insert-entry (prefix name suffix face path help)
  "Insert one tree entry using PREFIX, NAME, SUFFIX, FACE, PATH, and HELP."
  (let ((line-start (point))
        (prefix-width (string-width prefix)))
    (insert (propertize prefix
                        'face face
                        'obsidian-tree-prefix t))
    (insert (propertize name
                        'face face
                        'obsidian-tree-name name
                        'obsidian-path path
                        'keymap obsidian-tree-mode-map
                        'mouse-face 'highlight
                        'help-echo help))
    (insert (propertize (concat suffix "\n")
                        'face face
                        'obsidian-path path
                        'keymap obsidian-tree-mode-map
                        'mouse-face 'highlight
                        'help-echo help))
    (add-text-properties
     line-start (point)
     `(obsidian-path ,path
                     obsidian-tree-name ,name
                     obsidian-tree-prefix-width ,prefix-width))))

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
      (setq obsidian--tree-marquee-step 0)
      (obsidian--tree-update-marquee)
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
        (obsidian--tree-insert-entry
         (concat (make-string depth ?\s) marker)
         name "/" 'obsidian-tree-dir d (format "Directory: %s" d))
        (when openp
          (obsidian--tree-insert d (1+ depth) expanded))))
    (dolist (f files)
      (let ((name (file-name-base f)))
        (obsidian--tree-insert-entry
         (concat (make-string depth ?\s) "  ")
         name "" 'obsidian-tree-file f (format "Open: %s" f))))))

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

(defun obsidian--tree-move-entry (direction)
  "Move to the next tree entry in DIRECTION, skipping non-entry lines."
  (let ((origin (point))
        (step (if (< direction 0) -1 1))
        found)
    (beginning-of-line)
    (while (and (not found)
                (zerop (forward-line step)))
      (when (get-text-property (point) 'obsidian-path)
        (setq found t)))
    (if found
        (beginning-of-line)
      (goto-char origin))))

(defun obsidian--tree-previous-entry ()
  "Move the cursor to the previous selectable tree entry."
  (interactive)
  (obsidian--tree-move-entry -1))

(defun obsidian--tree-next-entry ()
  "Move the cursor to the next selectable tree entry."
  (interactive)
  (obsidian--tree-move-entry 1))

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

(defun obsidian--tree-delete-file ()
  "Confirm and delete the note at point, then refresh the workspace.
When `obsidian-delete-by-moving-to-trash' is non-nil, move the note to the
operating system trash instead of deleting it permanently."
  (interactive)
  (let ((path (get-text-property (point) 'obsidian-path)))
    (unless (and path (file-regular-p path))
      (user-error "Place the cursor on a note file before pressing Esc"))
    (let ((action (if obsidian-delete-by-moving-to-trash
                      "move to trash" "delete permanently")))
      (when (yes-or-no-p (format "%s: %s? "
                                 (capitalize action)
                                 (file-name-nondirectory path)))
        (let ((visiting-buffer (find-buffer-visiting path))
              (was-current (and obsidian--current-file
                                (file-equal-p path obsidian--current-file))))
          ;; The optional TRASH argument is supported by `delete-file' and
          ;; delegates to the platform-specific trash implementation.
          (delete-file path obsidian-delete-by-moving-to-trash)
          (when was-current
            (setq obsidian--current-file nil
                  obsidian--current-scope (file-name-directory path))
            (when (buffer-live-p visiting-buffer)
              (with-current-buffer visiting-buffer
                (set-buffer-modified-p nil))
              (kill-buffer visiting-buffer))
            (let ((editor-window (obsidian--editor-window)))
              (when (window-live-p editor-window)
                (set-window-buffer editor-window
                                   (obsidian--empty-editor-buffer)))))
          (obsidian--tree-refresh)
          (obsidian--schedule-graph-update)
          (message "%s: %s" (capitalize action)
                   (file-name-nondirectory path)))))))

(provide 'obsidian-tree)
;;; obsidian-tree.el ends here
