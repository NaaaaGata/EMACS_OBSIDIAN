;;; obsidian-tree.el --- Rich visual file tree panel with mouse support -*- lexical-binding: t; -*-

;;; Commentary:
;; Builds a standard-font-safe visual folder tree with box-drawing guides,
;; active-note indicator bar, connected-note badges, calm single-line marquee,
;; sorting, filtering, right-click context menu, and clickable header toolbar.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar obsidian--vault)
(defvar obsidian--current-scope)
(defvar obsidian--current-file)
(defvar obsidian-tree-buffer-name)
(defvar obsidian-delete-by-moving-to-trash)
(defvar obsidian-show-header-buttons)
(defvar obsidian-link-regexp)

(declare-function obsidian--open-note "obsidian-editor")
(declare-function obsidian-create-note "obsidian-editor")
(declare-function obsidian--safe-note-path "obsidian-editor")
(declare-function obsidian--update-backlinks "obsidian-editor")
(declare-function obsidian--schedule-graph-update "obsidian-graph")
(declare-function obsidian--all-note-files "obsidian-graph")
(declare-function obsidian--build-edges "obsidian-graph")
(declare-function obsidian--connected-nodes "obsidian-graph")
(declare-function obsidian--editor-window "obsidian-windows")
(declare-function obsidian--empty-editor-buffer "obsidian-windows")
(declare-function obsidian--make-header-button "obsidian-windows")
(declare-function obsidian-shrink-tree "obsidian-windows")
(declare-function obsidian-enlarge-tree "obsidian-windows")
(declare-function obsidian-toggle-tree "obsidian-windows")

(defcustom obsidian-file-extension "md"
  "File extension for notes."
  :type 'string
  :group 'obsidian)

(defcustom obsidian-tree-marquee-interval 0.40
  "Seconds between steps of an overflowing tree label.
Set this to nil to disable animated labels."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'obsidian)

(defcustom obsidian-tree-marquee-gap "   "
  "Text separating the end and beginning of a scrolling tree label."
  :type 'string
  :group 'obsidian)

(defcustom obsidian-tree-marquee-only-current-line t
  "When non-nil, only scroll the tree label under the cursor.
Other overflowing lines are quietly truncated with `…' so peripheral motion
does not distract from writing in the editor."
  :type 'boolean
  :group 'obsidian)

(defcustom obsidian-tree-show-counts t
  "When non-nil, show the note count next to each folder in the tree."
  :type 'boolean
  :group 'obsidian)

(defcustom obsidian-tree-auto-reveal t
  "When non-nil, opening a note expands its parent folders and highlights it."
  :type 'boolean
  :group 'obsidian)

(defcustom obsidian-tree-sort-method 'name
  "Default sort order for entries in the file tree."
  :type '(choice (const :tag "Alphabetical (A-Z)" name)
                 (const :tag "Recently modified first" mtime))
  :group 'obsidian)


;; Adaptive faces for dark & light terminals

(defface obsidian-tree-file
  '((((class color) (background dark)) :foreground "#abb2bf")
    (((class color) (background light)) :foreground "#24292f")
    (t :inherit default))
  "Face for regular note files in the tree."
  :group 'obsidian)

(defface obsidian-tree-current
  '((((class color) (background dark)) :foreground "#ff6b6b" :weight bold)
    (((class color) (background light)) :foreground "#cf222e" :weight bold)
    (t :weight bold :underline t))
  "Face for the currently open note in the tree."
  :group 'obsidian)

(defface obsidian-tree-connected
  '((((class color) (background dark)) :foreground "#61afef" :weight bold)
    (((class color) (background light)) :foreground "#0969da" :weight bold)
    (t :inherit link))
  "Face for notes directly linked with the current note."
  :group 'obsidian)

(defface obsidian-tree-dir
  '((((class color) (background dark)) :foreground "#e5c07b" :weight bold)
    (((class color) (background light)) :foreground "#9a6700" :weight bold)
    (t :weight bold))
  "Face for directories in the tree."
  :group 'obsidian)

(defface obsidian-tree-guide
  '((((class color) (background dark)) :foreground "#4b5263")
    (((class color) (background light)) :foreground "#afb8c1")
    (t :inherit shadow))
  "Face for tree indentation guides (`│', `├─', `└─')."
  :group 'obsidian)

(defface obsidian-tree-active-bar
  '((((class color) (background dark)) :foreground "#ff6b6b" :weight bold)
    (((class color) (background light)) :foreground "#cf222e" :weight bold)
    (t :weight bold))
  "Face for the left-edge `▎' indicator bar on the active note."
  :group 'obsidian)

(defface obsidian-tree-badge
  '((((class color) (background dark)) :foreground "#5c6370")
    (((class color) (background light)) :foreground "#6e7781")
    (t :inherit shadow))
  "Face for folder note-count badges in the tree."
  :group 'obsidian)

(defface obsidian-link
  '((((class color) (background dark)) :foreground "#61afef" :underline t)
    (((class color) (background light)) :foreground "#0969da" :underline t)
    (t :inherit link))
  "Face for existing wiki links."
  :group 'obsidian)


;; Buffer-local state

(defvar-local obsidian--tree-expanded nil
  "Hash table of expanded directories in the current tree buffer.")

(defvar-local obsidian--tree-marquee-timer nil
  "Timer used to animate labels wider than the tree window.")

(defvar-local obsidian--tree-marquee-step 0
  "Current character offset of the tree marquee.")

(defvar-local obsidian--tree-last-line nil
  "Last line start position observed in the tree buffer.")

(defvar-local obsidian--tree-filter-query nil
  "Current substring filter applied to note names, or nil.")


;; Major mode

(define-derived-mode obsidian-tree-mode fundamental-mode "Obsidian-Tree"
  "Major mode for the Obsidian file tree panel.
Click or RET opens files and toggles folders.  Right-click opens context menu.
Up/down arrows move between entries; TAB or left/right expand/collapse."
  :keymap obsidian-tree-mode-map
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (use-local-map obsidian-tree-mode-map)
  (when (fboundp 'hl-line-mode)
    (hl-line-mode 1))
  (setq-local header-line-format '(:eval (obsidian--tree-header-line)))
  (obsidian--tree-start-marquee)
  (add-hook 'post-command-hook #'obsidian--tree-on-cursor-move nil t)
  (add-hook 'kill-buffer-hook #'obsidian--tree-stop-marquee nil t))

(defun obsidian--tree-header-line ()
  "Build the clickable header toolbar for the tree window."
  (let* ((vault-name (if (and obsidian--vault (not (string-empty-p obsidian--vault)))
                         (file-name-nondirectory
                          (directory-file-name obsidian--vault))
                       "Vault"))
         (title (propertize (format " ▾ %s " vault-name)
                            'face 'obsidian-header-title
                            'help-echo (or obsidian--vault "")))
         (sort-label (if (eq obsidian-tree-sort-method 'mtime) "更新順" "名前順"))
         (filter-active (and obsidian--tree-filter-query
                             (not (string-empty-p obsidian--tree-filter-query))))
         (filter-label (if filter-active
                           (format "[/%s×]" obsidian--tree-filter-query)
                         "[/絞込]")))
    (if (not obsidian-show-header-buttons)
        title
      (concat
       title
       (obsidian--make-header-button "[+新規]" #'obsidian-create-note
                                     "新しいノートを作成 (n)")
       " "
       (obsidian--make-header-button (format "[%s]" sort-label)
                                     #'obsidian-tree-cycle-sort
                                     "クリックで並び順を切替: 名前順 / 更新順 (s)"
                                     (eq obsidian-tree-sort-method 'mtime))
       " "
       (obsidian--make-header-button filter-label #'obsidian-tree-filter
                                     "ノート名で絞り込み / 解除 (/)"
                                     filter-active)
       " "
       (obsidian--make-header-button "[◀]" #'obsidian-shrink-tree
                                     "ツリー幅を狭くする (<)")
       (obsidian--make-header-button "[▶]" #'obsidian-enlarge-tree
                                     "ツリー幅を広くする (>)")
       " "
       (obsidian--make-header-button "[×]" #'obsidian-toggle-tree
                                     "ツリーパネルを隠す (q)")))))


;; Marquee & truncation

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
         (source (concat cycle cycle cycle))
         (visible (truncate-string-to-width
                   (substring source offset) available nil nil "")))
    visible))

(defun obsidian--tree-on-cursor-move ()
  "Reset marquee step when the user moves onto a different tree line."
  (let ((line-pos (line-beginning-position)))
    (unless (eql line-pos obsidian--tree-last-line)
      (setq obsidian--tree-last-line line-pos
            obsidian--tree-marquee-step 0)
      (obsidian--tree-update-marquee))))

(defun obsidian--tree-update-marquee ()
  "Update display properties for overflowing tree labels in the current buffer."
  (when-let ((window (get-buffer-window (current-buffer) t)))
    ;; Keep one safety column unused so exact-width terminal lines never wrap.
    (let* ((window-width (max 1 (1- (window-body-width window))))
           (active-line (save-excursion
                          (goto-char (window-point window))
                          (line-beginning-position)))
           (inhibit-read-only t)
           (position (point-min)))
      ;; A timer must never move the user's tree cursor.
      (save-excursion
        (while (< position (point-max))
          (goto-char position)
          (let* ((line-start (line-beginning-position))
                 (line-end (line-end-position))
                (name (get-text-property position 'obsidian-tree-name))
                 (name-start (and name
                                  (next-single-property-change
                                   position 'obsidian-tree-label nil line-end)))
                 (name-end (and name-start
                                (next-single-property-change
                                 name-start 'obsidian-tree-label nil line-end)))
                 (prefix-width (or (get-text-property
                                    position 'obsidian-tree-prefix-width)
                                   0))
                 (suffix-width (or (get-text-property
                                    position 'obsidian-tree-suffix-width)
                                   0))
                 (available (max 1 (- window-width prefix-width suffix-width))))
            (when (and name-start name-end)
              (if (> (string-width name) available)
                  (let ((animate-p (or (not obsidian-tree-marquee-only-current-line)
                                       (= line-start active-line))))
                    (put-text-property
                     name-start name-end 'display
                     (if animate-p
                         (obsidian--tree-marquee-text
                          name available obsidian--tree-marquee-step)
                       (truncate-string-to-width
                        name available nil nil
                        (if (> available 1) "…" "")))))
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


;; Tree building & visual entries

(defun obsidian--tree-insert-entry (prefix name suffix face path help &optional badge)
  "Insert one propertized tree row with PREFIX, NAME, SUFFIX, FACE, PATH, HELP, and BADGE."
  (let* ((line-start (point))
         (badge-text (or badge ""))
         (prefix-width (string-width prefix))
         (suffix-width (+ (string-width suffix) (string-width badge-text))))
    (insert (propertize prefix
                        'obsidian-tree-prefix t))
    (insert (propertize name
                        'face face
                        'obsidian-tree-label t
                        'obsidian-tree-name name
                        'obsidian-path path
                        'keymap obsidian-tree-mode-map
                        'mouse-face 'highlight
                        'help-echo help))
    (when (> (length suffix) 0)
      (insert (propertize suffix
                          'face face
                          'obsidian-path path
                          'keymap obsidian-tree-mode-map
                          'mouse-face 'highlight
                          'help-echo help)))
    (when (> (length badge-text) 0)
      (insert (propertize badge-text
                          'face 'obsidian-tree-badge
                          'obsidian-path path
                          'keymap obsidian-tree-mode-map
                          'help-echo help)))
    (insert "\n")
    (add-text-properties
     line-start (point)
     `(obsidian-path ,path
                     obsidian-tree-name ,name
                     obsidian-tree-prefix-width ,prefix-width
                     obsidian-tree-suffix-width ,suffix-width))))

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

(defun obsidian--tree-file-matches-filter-p (file query)
  "Return non-nil if note FILE matches filter QUERY."
  (if (or (null query) (string-empty-p query))
      t
    (string-match-p (regexp-quote (downcase query))
                    (downcase (file-name-base file)))))

(defun obsidian--tree-count-notes (dir &optional query)
  "Return number of matching note files recursively inside DIR."
  (let ((entries (ignore-errors (directory-files dir t "^[^.]" nil)))
        (count 0))
    (dolist (entry entries)
      (cond
       ((file-directory-p entry)
        (unless (string= (file-name-nondirectory entry) ".git")
          (setq count (+ count (obsidian--tree-count-notes entry query)))))
       ((and (obsidian--note-file-p entry)
             (obsidian--tree-file-matches-filter-p entry query))
        (cl-incf count))))
    count))

(defun obsidian--tree-sort-files (files)
  "Sort note FILES according to `obsidian-tree-sort-method'."
  (if (eq obsidian-tree-sort-method 'mtime)
      (sort (copy-sequence files)
            (lambda (a b)
              (time-less-p
               (file-attribute-modification-time (file-attributes b))
               (file-attribute-modification-time (file-attributes a)))))
    files))

(defun obsidian--tree-connected-names ()
  "Return a list of note base names connected to `obsidian--current-file'."
  (when (and obsidian--current-file
             obsidian--vault
             (fboundp 'obsidian--all-note-files)
             (fboundp 'obsidian--build-edges)
             (fboundp 'obsidian--connected-nodes))
    (let* ((current (file-name-base obsidian--current-file))
           (all-files (obsidian--all-note-files)))
      (when (<= (length all-files) 300)
        (obsidian--connected-nodes current (obsidian--build-edges all-files))))))

(defun obsidian-refresh-tree ()
  "Refresh the file tree."
  (interactive)
  (obsidian--tree-refresh))

(defun obsidian--tree-refresh ()
  "Rebuild the file tree buffer while preserving selection by file path."
  (interactive)
  (when-let ((buf (get-buffer obsidian-tree-buffer-name)))
    (with-current-buffer buf
      (let* ((inhibit-read-only t)
             (saved-path (get-text-property (point) 'obsidian-path))
             (saved-line (line-number-at-pos))
             (connected (obsidian--tree-connected-names)))
        (erase-buffer)
        (unless obsidian--tree-expanded
          (setq-local obsidian--tree-expanded (make-hash-table :test 'equal)))
        (when (and obsidian--vault (file-directory-p obsidian--vault))
          (obsidian--tree-insert obsidian--vault 0 obsidian--tree-expanded
                                 "" connected))
        (when (= (point-min) (point-max))
          (insert (propertize
                   (if obsidian--tree-filter-query
                       (format "  一致するノートがありません: \"%s\"\n  (/ キーまたは上部ボタンで解除)\n"
                               obsidian--tree-filter-query)
                     "  ノートがまだありません。\n  上部 [+新規] または n キーで作成できます。\n")
                   'face 'obsidian-tree-badge)))
        (setq obsidian--tree-marquee-step 0)
        (obsidian--tree-restore-cursor saved-path saved-line)
        (obsidian--tree-update-marquee)
        (set-buffer-modified-p nil)))))

(defun obsidian--tree-restore-cursor (target-path fallback-line)
  "Move cursor to TARGET-PATH if present in the tree, else FALLBACK-LINE."
  (let (found)
    (when target-path
      (save-excursion
        (goto-char (point-min))
        (while (and (not found) (< (point) (point-max)))
          (let ((row-path (get-text-property (point) 'obsidian-path)))
            (if (and row-path (equal (expand-file-name row-path)
                                     (expand-file-name target-path)))
                (setq found (line-beginning-position))
              (forward-line 1))))))
    (if found
        (goto-char found)
      (goto-char (point-min))
      (forward-line (max 0 (1- (or fallback-line 1)))))))

(defun obsidian-tree-reveal-file (file)
  "Expand parent folders of FILE in the tree and highlight its row."
  (when (and obsidian-tree-auto-reveal
             file
             obsidian--vault
             (get-buffer obsidian-tree-buffer-name))
    (let ((vault (file-name-as-directory (file-truename obsidian--vault)))
          (true-file (file-truename file)))
      (when (string-prefix-p vault true-file)
        (with-current-buffer obsidian-tree-buffer-name
          (unless obsidian--tree-expanded
            (setq-local obsidian--tree-expanded (make-hash-table :test 'equal)))
          (let ((dir (file-name-directory (expand-file-name file))))
            (while (and dir
                        (> (length dir) (length (expand-file-name obsidian--vault)))
                        (string-prefix-p (file-name-as-directory
                                          (expand-file-name obsidian--vault))
                                         (file-name-as-directory dir)))
              (let ((clean-dir (directory-file-name dir)))
                (puthash clean-dir t obsidian--tree-expanded)
                (puthash (file-truename clean-dir) t obsidian--tree-expanded)
                (setq dir (file-name-directory clean-dir)))))
          (obsidian--tree-refresh)
          (obsidian--tree-restore-cursor file 1)
          (when-let ((win (get-buffer-window (current-buffer) t)))
            (set-window-point win (point))))))))

(defun obsidian--tree-dir-expanded-p (dir expanded)
  "Return non-nil if DIR is expanded in hash table EXPANDED or via active filter."
  (or (and obsidian--tree-filter-query
           (not (string-empty-p obsidian--tree-filter-query)))
      (and expanded
           (or (gethash dir expanded)
               (gethash (directory-file-name dir) expanded)
               (gethash (file-truename dir) expanded)))))

(defun obsidian--tree-insert (dir depth expanded &optional guide-prefix connected-names)
  "Insert DIR at DEPTH using EXPANDED, GUIDE-PREFIX, and CONNECTED-NAMES."
  (let* ((guide (or guide-prefix ""))
         (query obsidian--tree-filter-query)
         (entries (ignore-errors
                    (directory-files dir t "^[^.]" nil)))
         (raw-dirs (cl-remove-if-not
                    (lambda (d)
                      (and (file-directory-p d)
                           (not (string= (file-name-nondirectory d) ".git"))))
                    entries))
         (dirs (if (and query (not (string-empty-p query)))
                   (cl-remove-if (lambda (d)
                                   (zerop (obsidian--tree-count-notes d query)))
                                 raw-dirs)
                 raw-dirs))
         (files (obsidian--tree-sort-files
                 (cl-remove-if
                  (lambda (f)
                    (or (file-directory-p f)
                        (not (obsidian--note-file-p f))
                        (not (obsidian--tree-file-matches-filter-p f query))))
                  entries)))
         (total-items (+ (length dirs) (length files)))
         (index 0))
    (dolist (d dirs)
      (cl-incf index)
      (let* ((last-p (= index total-items))
             (name (file-name-nondirectory d))
             (openp (obsidian--tree-dir-expanded-p d expanded))
             (scope-p (and obsidian--current-scope
                           (file-equal-p d obsidian--current-scope)))
             (bar (propertize (if scope-p "▏ " "  ") 'face 'obsidian-tree-guide))
             (connector (if (> depth 0)
                            (propertize (if last-p "└─ " "├─ ")
                                        'face 'obsidian-tree-guide)
                          ""))
             (marker (propertize (if openp "▾ " "▸ ") 'face 'obsidian-tree-dir))
             (count (and obsidian-tree-show-counts
                         (obsidian--tree-count-notes d query)))
             (badge (if count (format "  %d" count) ""))
             (next-guide (if (> depth 0)
                             (concat guide
                                     (propertize (if last-p "   " "│  ")
                                                 'face 'obsidian-tree-guide))
                           guide)))
        (obsidian--tree-insert-entry
         (concat bar guide connector marker)
         name "/" 'obsidian-tree-dir d
         (format "フォルダ: %s (左クリック: 開閉 / 右クリック: メニュー)" d)
         badge)
        (when openp
          (obsidian--tree-insert d (1+ depth) expanded next-guide connected-names))))
    (dolist (f files)
      (cl-incf index)
      (let* ((last-p (= index total-items))
             (name (file-name-base f))
             (current-p (and obsidian--current-file
                             (file-equal-p f obsidian--current-file)))
             (connected-p (and (not current-p)
                               (member name connected-names)))
             (bar (if current-p
                      (propertize "▎ " 'face 'obsidian-tree-active-bar)
                    "  "))
             (connector (if (> depth 0)
                            (propertize (if last-p "└─ " "├─ ")
                                        'face 'obsidian-tree-guide)
                          ""))
             (face (cond (current-p 'obsidian-tree-current)
                         (connected-p 'obsidian-tree-connected)
                         (t 'obsidian-tree-file)))
             (marker (propertize (cond (current-p "◆ ")
                                       (connected-p "• ")
                                       (t "· "))
                                 'face face)))
        (obsidian--tree-insert-entry
         (concat bar guide connector marker)
         name "" face f
         (format "ノートを開く: %s (右クリック: メニュー)" (file-name-nondirectory f)))))))


;; Tree navigation & folder expansion

(defun obsidian--tree-set-expanded (path open-p)
  "Set directory PATH expansion state to OPEN-P in `obsidian--tree-expanded'."
  (unless obsidian--tree-expanded
    (setq-local obsidian--tree-expanded (make-hash-table :test 'equal)))
  (let ((clean (directory-file-name path))
        (true (file-truename (directory-file-name path))))
    (if open-p
        (progn
          (puthash path t obsidian--tree-expanded)
          (puthash clean t obsidian--tree-expanded)
          (puthash true t obsidian--tree-expanded))
      (remhash path obsidian--tree-expanded)
      (remhash clean obsidian--tree-expanded)
      (remhash true obsidian--tree-expanded))))

(defun obsidian--tree-toggle ()
  "Expand/collapse directory at line point."
  (interactive)
  (let ((path (get-text-property (point) 'obsidian-path)))
    (when (and path (file-directory-p path))
      (obsidian--tree-set-expanded
       path (not (obsidian--tree-dir-expanded-p path obsidian--tree-expanded)))
      (obsidian--tree-refresh))))

(defun obsidian--tree-collapse ()
  "Collapse directory at point, or jump to parent directory if on a file."
  (interactive)
  (let ((path (get-text-property (point) 'obsidian-path)))
    (cond
     ((and path (file-directory-p path)
           (obsidian--tree-dir-expanded-p path obsidian--tree-expanded))
      (obsidian--tree-set-expanded path nil)
      (obsidian--tree-refresh))
     (path
      (let ((parent (directory-file-name (file-name-directory path))))
        (when (and obsidian--vault
                   (not (file-equal-p parent obsidian--vault)))
          (obsidian--tree-restore-cursor parent 1)))))))

(defun obsidian--tree-expand ()
  "Expand directory at point, or open note if on a file."
  (interactive)
  (let ((path (get-text-property (point) 'obsidian-path)))
    (cond
     ((and path (file-directory-p path))
      (obsidian--tree-set-expanded path t)
      (obsidian--tree-refresh))
     ((and path (file-regular-p path))
      (obsidian--open-note path)))))

(defun obsidian-tree-toggle-all ()
  "Expand all directories if any are collapsed, otherwise collapse all."
  (interactive)
  (with-current-buffer (get-buffer-create obsidian-tree-buffer-name)
    (unless obsidian--tree-expanded
      (setq-local obsidian--tree-expanded (make-hash-table :test 'equal)))
    (if (> (hash-table-count obsidian--tree-expanded) 0)
        (clrhash obsidian--tree-expanded)
      (when (and obsidian--vault (file-directory-p obsidian--vault))
        (dolist (entry (directory-files-recursively obsidian--vault ".*" t))
          (when (and (file-directory-p entry)
                     (not (string-match-p "/\\.git\\(/\\|\\'\\)" entry)))
            (obsidian--tree-set-expanded entry t)))))
    (obsidian--tree-refresh)))

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

(defun obsidian--tree-scroll-up ()
  "Scroll tree upward smoothly with mouse wheel."
  (interactive)
  (ignore-errors (scroll-down 2))
  (obsidian--tree-previous-entry))

(defun obsidian--tree-scroll-down ()
  "Scroll tree downward smoothly with mouse wheel."
  (interactive)
  (ignore-errors (scroll-up 2))
  (obsidian--tree-next-entry))


;; Opening, Sorting, Filtering, Creating Folders, Renaming, Context Menu

(defun obsidian--tree-open ()
  "Open file or toggle directory at line point."
  (interactive)
  (let ((path (get-text-property (point) 'obsidian-path)))
    (cond
     ((and path (file-directory-p path))
      (setq obsidian--current-scope path)
      (obsidian--tree-set-expanded
       path (not (obsidian--tree-dir-expanded-p path obsidian--tree-expanded)))
      (obsidian--tree-refresh)
      (obsidian--schedule-graph-update))
     ((and path (file-regular-p path))
      (obsidian--open-note path))
     (t
      (message "カーソル位置にアイテムがありません")))))

(defun obsidian--tree-select-mouse-line (event)
  "Select the tree window and place point on the row clicked in EVENT."
  (let ((window (posn-window (event-end event)))
        (position (posn-point (event-end event))))
    (when (and (windowp window) (integer-or-marker-p position))
      (select-window window)
      (goto-char position)
      (unless (get-text-property (point) 'obsidian-path)
        (when (> (point) (line-beginning-position))
          (backward-char 1)))
      (beginning-of-line)
      t)))

(defun obsidian--tree-mouse-open (event)
  "Open file or toggle directory at mouse click EVENT."
  (interactive "e")
  (when (obsidian--tree-select-mouse-line event)
    (obsidian--tree-open)))

(defun obsidian-tree-cycle-sort ()
  "Toggle tree sorting between alphabetical (`name') and recent (`mtime')."
  (interactive)
  (setq obsidian-tree-sort-method
        (if (eq obsidian-tree-sort-method 'name) 'mtime 'name))
  (obsidian--tree-refresh)
  (force-mode-line-update t)
  (message "ツリーの並び順: %s"
           (if (eq obsidian-tree-sort-method 'mtime)
               "更新日時順（新しい順）"
             "名前順（A-Z）")))

(defun obsidian-tree-filter (&optional query)
  "Filter tree notes by substring QUERY, or clear filter if already active/empty."
  (interactive)
  (with-current-buffer (get-buffer-create obsidian-tree-buffer-name)
    (let ((next-query
           (or query
               (read-string
                (if obsidian--tree-filter-query
                    (format "ノート絞り込み (空欄で解除, 現在: %s): "
                            obsidian--tree-filter-query)
                  "ノート絞り込み (部分一致): ")))))
      (setq-local obsidian--tree-filter-query
                  (if (string-empty-p (string-trim next-query))
                      nil
                    (string-trim next-query)))
      (obsidian--tree-refresh)
      (force-mode-line-update t))))

(defun obsidian--tree-target-directory ()
  "Return the directory associated with point in the tree, or `obsidian--current-scope'."
  (let ((path (and (eq major-mode 'obsidian-tree-mode)
                   (get-text-property (point) 'obsidian-path))))
    (cond
     ((and path (file-directory-p path)) path)
     ((and path (file-regular-p path)) (file-name-directory path))
     (t (or obsidian--current-scope obsidian--vault)))))

(defun obsidian-tree-create-folder (name)
  "Create a new subdirectory NAME inside the selected tree folder."
  (interactive
   (list (read-string "新しいフォルダ名: ")))
  (let ((trimmed (string-trim name)))
    (when (string-empty-p trimmed)
      (user-error "フォルダ名を空にはできません"))
    (let* ((base (obsidian--tree-target-directory))
           (vault (file-name-as-directory (expand-file-name obsidian--vault)))
           (dir (expand-file-name trimmed base)))
      (unless (string-prefix-p vault (file-name-as-directory dir))
        (user-error "Vaultの外にはフォルダを作成できません: %s" trimmed))
      (make-directory dir t)
      (setq obsidian--current-scope dir)
      (with-current-buffer (get-buffer-create obsidian-tree-buffer-name)
        (obsidian--tree-set-expanded base t)
        (obsidian--tree-set-expanded dir t))
      (obsidian--tree-refresh)
      (obsidian--schedule-graph-update)
      (message "フォルダを作成しました: %s" trimmed))))

(defun obsidian-tree-rename-at-point ()
  "Rename the note or folder at point, updating all Vault backlinks for notes."
  (interactive)
  (let ((path (get-text-property (point) 'obsidian-path)))
    (unless path
      (user-error "変更したいノートまたはフォルダにカーソルを合わせてください"))
    (if (file-directory-p path)
        (let* ((old-name (file-name-nondirectory (directory-file-name path)))
               (new-name (string-trim
                          (read-string (format "フォルダ名を変更 (%s → ): " old-name)
                                       old-name)))
               (new-dir (expand-file-name new-name
                                          (file-name-directory
                                           (directory-file-name path)))))
          (when (or (string-empty-p new-name) (string= old-name new-name))
            (user-error "変更をキャンセルしました"))
          (when (file-exists-p new-dir)
            (user-error "同名のフォルダが既に存在します: %s" new-name))
          (rename-file (directory-file-name path) new-dir)
          (when (and obsidian--current-scope
                     (string-prefix-p (file-name-as-directory path)
                                      (file-name-as-directory obsidian--current-scope)))
            (setq obsidian--current-scope new-dir))
          (obsidian--tree-refresh)
          (obsidian--schedule-graph-update)
          (message "フォルダ名を変更しました: %s → %s" old-name new-name))
      ;; Note file rename + Vault-wide wiki-link update!
      (let* ((old-name (file-name-base path))
             (new-name (string-trim
                        (read-string (format "ノート名を変更 (%s → ): " old-name)
                                     old-name)))
             (new-file (obsidian--safe-note-path
                        new-name (file-name-directory path))))
        (when (or (string-empty-p new-name) (string= old-name new-name))
          (user-error "変更をキャンセルしました"))
        (when (file-exists-p new-file)
          (user-error "同名のノートが既に存在します: %s" new-name))
        (let ((visiting (find-buffer-visiting path))
              (was-current (and obsidian--current-file
                                (file-equal-p path obsidian--current-file)))
              (updated 0))
          (make-directory (file-name-directory new-file) t)
          (rename-file path new-file)
          (when (buffer-live-p visiting)
            (with-current-buffer visiting
              (set-visited-file-name new-file t)
              (save-buffer)))
          (when was-current
            (setq obsidian--current-file new-file))
          (when (fboundp 'obsidian--update-backlinks)
            (setq updated (obsidian--update-backlinks old-name new-name)))
          (obsidian--tree-refresh)
          (obsidian--tree-restore-cursor new-file 1)
          (obsidian--schedule-graph-update)
          (message "ノート名を変更しました: %s → %s (リンク %d 件を自動更新)"
                   old-name new-name updated))))))

(defun obsidian--tree-context-menu (event)
  "Open a mouse right-click context menu for the tree entry under EVENT."
  (interactive "e")
  (obsidian--tree-select-mouse-line event)
  (let* ((path (get-text-property (point) 'obsidian-path))
         (is-dir (and path (file-directory-p path)))
         (is-file (and path (file-regular-p path)))
         (menu (make-sparse-keymap "Obsidian Tree 操作")))
    (define-key menu [sort]
      `(menu-item "並び順を切替 (名前順 / 更新順)" obsidian-tree-cycle-sort))
    (define-key menu [filter]
      `(menu-item "ノートを絞り込み (/)" obsidian-tree-filter))
    (when is-file
      (define-key menu [delete]
        `(menu-item "ゴミ箱へ移動 (Esc)" obsidian--tree-delete-file)))
    (when path
      (define-key menu [rename]
        `(menu-item "名前を変更 (R)" obsidian-tree-rename-at-point)))
    (define-key menu [new-folder]
      `(menu-item "新しいフォルダを作成 (+)" obsidian-tree-create-folder))
    (define-key menu [new-note]
      `(menu-item "新しいノートを作成 (n)" obsidian-create-note))
    (when (or is-dir is-file)
      (define-key menu [open]
        `(menu-item ,(if is-dir "フォルダを開閉 (RET)" "ノートを開く (RET)")
                    obsidian--tree-open)))
    (condition-case nil
        (let ((choice (x-popup-menu event menu)))
          (when choice
            (let ((cmd (lookup-key menu (apply #'vector choice))))
              (when (commandp cmd)
                (call-interactively cmd)))))
      (error
       ;; Graceful fallback for minimal TTY builds without `x-popup-menu'.
       (let* ((actions `(("開く / 開閉" . obsidian--tree-open)
                         ("新しいノートを作成" . obsidian-create-note)
                         ("新しいフォルダを作成" . obsidian-tree-create-folder)
                         ("名前を変更" . obsidian-tree-rename-at-point)
                         ("並び順を切替" . obsidian-tree-cycle-sort)
                         ("絞り込み" . obsidian-tree-filter)
                         ,@(when is-file
                             '(("ゴミ箱へ移動" . obsidian--tree-delete-file)))))
              (picked (completing-read "ツリー操作: " (mapcar #'car actions) nil t))
              (cmd (cdr (assoc picked actions))))
         (when (commandp cmd)
           (call-interactively cmd)))))))

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
