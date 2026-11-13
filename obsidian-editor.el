;;; obsidian-editor.el --- Editor mode, Live Preview links, completion, backlinks -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides note opening, Obsidian-style Live Preview wiki-link concealing,
;; inline `[[' completion-at-point, clickable backlinks footer overlay,
;; history navigation, note creation, and automatic link target renaming.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar obsidian--vault)
(defvar obsidian--current-file)
(defvar obsidian--current-scope)
(defvar obsidian--history)
(defvar obsidian-link-regexp)
(defvar obsidian-file-extension)
(defvar obsidian-tree-buffer-name)
(defvar obsidian-graph-buffer-name)
(defvar obsidian-editor-buffer-name-prefix)
(defvar obsidian-show-header-buttons)
(defvar obsidian--latex-overlays)

(declare-function obsidian-quit "obsidian")
(declare-function obsidian-daily-note "obsidian")
(declare-function obsidian-find-note "obsidian")
(declare-function obsidian--tree-refresh "obsidian-tree")
(declare-function obsidian-tree-reveal-file "obsidian-tree")
(declare-function obsidian--note-file-p "obsidian-tree")
(declare-function obsidian--schedule-graph-update "obsidian-graph")
(declare-function obsidian--find-note "obsidian-graph")
(declare-function obsidian--all-note-names "obsidian-graph")
(declare-function obsidian--all-note-files "obsidian-graph")
(declare-function obsidian--editor-window "obsidian-windows")
(declare-function obsidian--make-header-button "obsidian-windows")
(declare-function obsidian-toggle-tree "obsidian-windows")
(declare-function obsidian-toggle-graph "obsidian-windows")
(declare-function obsidian-focus-editor "obsidian-windows")
(declare-function obsidian-toggle-latex-preview "obsidian-latex")

(defcustom obsidian-live-preview t
  "When non-nil, conceal `[[' and `]]' (and link targets with aliases) on inactive lines.
Moving the cursor onto a line immediately reveals the raw Markdown syntax."
  :type 'boolean
  :group 'obsidian)

(defcustom obsidian-show-backlinks t
  "When non-nil, display a clickable Backlinks section below the note body.
Uses a non-intrusive overlay so the Markdown file itself is never modified."
  :type 'boolean
  :group 'obsidian)

(defface obsidian-link-missing
  '((((class color) (background dark))
     :foreground "#d19a66" :underline (:style line))
    (((class color) (background light))
     :foreground "#9a6700" :underline (:style line))
    (t :inherit shadow :underline t))
  "Face for wiki links pointing to notes that do not exist yet."
  :group 'obsidian)

(defface obsidian-backlinks-heading
  '((((class color) (background dark))
     :foreground "#7f848e" :weight bold)
    (((class color) (background light))
     :foreground "#57606a" :weight bold)
    (t :inherit shadow :weight bold))
  "Face for the Backlinks footer separator and title."
  :group 'obsidian)

(defvar-local obsidian--saved-link-names nil
  "Wiki-link targets recorded when this note was opened or last saved.")

(defvar-local obsidian--live-link-overlays nil
  "Overlays used to conceal `[[' and `]]' brackets in Live Preview mode.")

(defvar-local obsidian--last-cursor-line nil
  "Line start position last used to reveal Live Preview brackets.")

(defvar-local obsidian--backlinks-overlay nil
  "Zero-length end-of-buffer overlay displaying incoming backlinks.")

(defvar obsidian--backlink-update-in-progress nil
  "Non-nil while a note rename is saving updated backlinks.")


;; Editor minor mode & header toolbar

(define-minor-mode obsidian-editor-mode
  "Minor mode for the Obsidian editor window."
  :lighter " Obs"
  :keymap obsidian-editor-mode-map
  (if obsidian-editor-mode
      (progn
        ;; Reflow visually whenever the frame or pane becomes narrower.
        (setq-local truncate-lines nil)
        (setq-local truncate-partial-width-windows nil)
        (visual-line-mode 1)
        (setq-local word-wrap t)
        (setq-local header-line-format '(:eval (obsidian--editor-header-line)))
        (add-hook 'after-save-hook #'obsidian--after-save nil t)
        (add-hook 'post-command-hook #'obsidian--sync-live-preview-line nil t)
        (add-hook 'after-change-functions #'obsidian--after-change-fontify nil t)
        (add-hook 'completion-at-point-functions
                  #'obsidian-completion-at-point nil t)
        (obsidian--fontify-links)
        (obsidian--refresh-backlinks-overlay)
        (setq obsidian--saved-link-names (obsidian--link-names-in-buffer)))
    (remove-hook 'after-save-hook #'obsidian--after-save t)
    (remove-hook 'post-command-hook #'obsidian--sync-live-preview-line t)
    (remove-hook 'after-change-functions #'obsidian--after-change-fontify t)
    (remove-hook 'completion-at-point-functions #'obsidian-completion-at-point t)
    (obsidian--clear-live-link-overlays)
    (obsidian--clear-backlinks-overlay)
    (kill-local-variable 'header-line-format)))

(defun obsidian--editor-header-line ()
  "Build the breadcrumb and clickable action bar for the center editor."
  (let* ((tree-hidden (null (get-buffer-window obsidian-tree-buffer-name)))
         (graph-hidden (null (get-buffer-window obsidian-graph-buffer-name)))
         (breadcrumb
          (if (and buffer-file-name obsidian--vault)
              (let* ((rel (file-relative-name buffer-file-name obsidian--vault))
                     (dir (file-name-directory rel))
                     (base (file-name-base rel)))
                (concat
                 (if dir
                     (propertize (format " ▸ %s" dir) 'face 'obsidian-header-muted)
                   " ▸ ")
                 (propertize (concat base " ") 'face 'obsidian-header-title)))
            (propertize " ◆ Welcome " 'face 'obsidian-header-title))))
    (if (not obsidian-show-header-buttons)
        breadcrumb
      (concat
       (if tree-hidden
           (concat (obsidian--make-header-button
                    "[▸ツリー]" #'obsidian-toggle-tree
                    "左ツリーパネルを表示 (C-c o 1)")
                   " ")
         "")
       breadcrumb
       (if obsidian--history
           (concat (obsidian--make-header-button
                    "[←戻]" #'obsidian-jump-back
                    "直前のノートへ戻る (C-c o b)")
                   " ")
         "")
       (obsidian--make-header-button "[+新規]" #'obsidian-create-note
                                     "新しいノートを作成 (C-c o n)")
       " "
       (obsidian--make-header-button "[今日]" #'obsidian-daily-note
                                     "今日のデイリーノートを開く (C-c o d)")
       " "
       (obsidian--make-header-button "[検索]" #'obsidian-find-note
                                     "Vault内のノートを検索して開く (C-c o p)")
       " "
       (obsidian--make-header-button "[リンク]" #'obsidian-insert-link
                                     "Wikiリンク [[...]] を挿入 (C-c o l)")
       " "
       (obsidian--make-header-button "[Live]" #'obsidian-toggle-live-preview
                                     "[[括弧]] の自動隠蔽 Live Preview を切替 (C-c o v)"
                                     obsidian-live-preview)
       " "
       (obsidian--make-header-button "[数式]" #'obsidian-toggle-latex-preview
                                     "LaTeX 数式プレビューを切替 (C-c o t)"
                                     (and (boundp 'obsidian--latex-overlays)
                                          obsidian--latex-overlays))
       " "
       (obsidian--make-header-button "[集中]" #'obsidian-focus-editor
                                     "集中執筆モード（左右パネルの一括開閉, C-c o z）"
                                     (and tree-hidden graph-hidden))
       " "
       (obsidian--make-header-button "[×終了]" #'obsidian-quit
                                     "Obsidianを閉じて元のウィンドウ構成へ戻る (C-c o q)")
       (if graph-hidden
           (concat " "
                   (obsidian--make-header-button
                    "[グラフ◂]" #'obsidian-toggle-graph
                    "右グラフパネルを表示 (C-c o 3)"))
         "")))))


;; Note opening

(defun obsidian--open-note (file &optional no-record)
  "Open FILE in the editor window.
Unless NO-RECORD, push the previous file onto the history."
  (when (and obsidian--current-file (not no-record))
    (let ((ed-win (obsidian--editor-window)))
      (when (window-live-p ed-win)
        (with-current-buffer (window-buffer ed-win)
          (push (cons obsidian--current-file (point)) obsidian--history)))))
  (setq obsidian--current-file (expand-file-name file))
  (setq obsidian--current-scope (file-name-directory obsidian--current-file))
  (let ((buf (find-file-noselect obsidian--current-file))
        (ed-win (obsidian--editor-window)))
    (set-window-buffer ed-win buf)
    (with-current-buffer buf
      ;; Activate `markdown-mode' automatically if installed and buffer is plain.
      (when (and (eq major-mode 'fundamental-mode)
                 (fboundp 'markdown-mode))
        (markdown-mode))
      (obsidian-editor-mode 1)
      (obsidian--fontify-links)
      (obsidian--refresh-backlinks-overlay))
    (when (get-buffer obsidian-editor-buffer-name-prefix)
      (kill-buffer obsidian-editor-buffer-name-prefix))
    (if (fboundp 'obsidian-tree-reveal-file)
        (obsidian-tree-reveal-file obsidian--current-file)
      (obsidian--tree-refresh))
    (select-window (obsidian--editor-window))
    (obsidian--schedule-graph-update)))


;; Link rendering & Obsidian Live Preview

(defvar obsidian-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'obsidian--mouse-follow-link)
    map)
  "Keymap placed only on rendered wiki links.")

(defun obsidian--clear-live-link-overlays ()
  "Delete all Live Preview bracket-concealing overlays in current buffer."
  (dolist (ov obsidian--live-link-overlays)
    (when (overlayp ov)
      (delete-overlay ov)))
  (setq obsidian--live-link-overlays nil
        obsidian--last-cursor-line nil))

(defun obsidian--make-conceal-overlay (beg end reveal-p)
  "Create an overlay hiding BEG..END unless REVEAL-P is non-nil."
  (when (< beg end)
    (let ((ov (make-overlay beg end nil t nil)))
      (overlay-put ov 'obsidian-live-link t)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'invisible (not reveal-p))
      (push ov obsidian--live-link-overlays))))

(defun obsidian--sync-live-preview-line ()
  "Reveal `[[...]]' syntax on the current cursor line and conceal other lines."
  (when obsidian-live-preview
    (let ((line-beg (line-beginning-position)))
      (unless (eql line-beg obsidian--last-cursor-line)
        (setq obsidian--last-cursor-line line-beg)
        (let ((line-end (line-end-position)))
          (dolist (ov obsidian--live-link-overlays)
            (when (and (overlayp ov) (overlay-buffer ov))
              (let ((ov-start (overlay-start ov)))
                (overlay-put ov 'invisible
                             (not (and (>= ov-start line-beg)
                                       (<= ov-start line-end))))))))))))

(defun obsidian-toggle-live-preview ()
  "Toggle Obsidian-style Live Preview concealing of `[[wiki-link]]' brackets."
  (interactive)
  (setq obsidian-live-preview (not obsidian-live-preview))
  (obsidian--fontify-links)
  (force-mode-line-update t)
  (message "Live Preview: %s" (if obsidian-live-preview "ON" "OFF")))

(defun obsidian--fontify-links ()
  "Apply link faces and optional Live Preview overlays to wiki links."
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t)
        (existing-table (make-hash-table :test #'equal)))
    (when (and obsidian--vault (fboundp 'obsidian--all-note-names))
      (dolist (n (obsidian--all-note-names))
        (puthash n t existing-table)))
    (obsidian--clear-live-link-overlays)
    ;; Remove only text properties previously owned by this package.
    (let ((position (point-min)))
      (while (< position (point-max))
        (let ((next (next-single-property-change
                     position 'obsidian-link-property nil (point-max))))
          (when (get-text-property position 'obsidian-link-property)
            (remove-list-of-text-properties
             position next
             '(obsidian-link-property face font-lock-face mouse-face keymap help-echo)))
          (setq position next))))
    (let ((cur-line-beg (line-beginning-position))
          (cur-line-end (line-end-position)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward obsidian-link-regexp nil t)
          (let* ((m-beg (match-beginning 0))
                 (m-end (match-end 0))
                 (inner-beg (match-beginning 1))
                 (inner (match-string-no-properties 1))
                 (name (obsidian--link-name inner))
                 (exists-p (or (zerop (hash-table-count existing-table))
                               (gethash name existing-table)
                               (and obsidian--vault (obsidian--find-note name))))
                 (face (if exists-p 'obsidian-link 'obsidian-link-missing))
                 (help (if exists-p
                           (format "クリックまたは M-RET で開く: %s" name)
                         (format "未作成ノート: %s (保存で自動作成 / クリックで今すぐ作成)" name)))
                 (on-cursor-line (and (>= m-beg cur-line-beg)
                                      (<= m-beg cur-line-end))))
            (add-text-properties
             m-beg m-end
             `(obsidian-link-property t face ,face font-lock-face ,face
                                      mouse-face highlight
                                      help-echo ,help keymap ,obsidian-link-map))
            (when obsidian-live-preview
              (let ((pipe-idx (string-match "|" inner)))
                (if pipe-idx
                    ;; `[[target|表示名]]' -> conceal `[[target|' and `]]'
                    (progn
                      (obsidian--make-conceal-overlay
                       m-beg (+ inner-beg pipe-idx 1) on-cursor-line)
                      (obsidian--make-conceal-overlay
                       (- m-end 2) m-end on-cursor-line))
                  ;; `[[target]]' -> conceal `[[' and `]]'
                  (obsidian--make-conceal-overlay
                   m-beg (+ m-beg 2) on-cursor-line)
                  (obsidian--make-conceal-overlay
                   (- m-end 2) m-end on-cursor-line)))))))
      (setq obsidian--last-cursor-line cur-line-beg))
    (set-buffer-modified-p modified)))

(defun obsidian--after-change-fontify (beg end _len)
  "Re-fontify wiki links when lines between BEG and END are edited."
  (when (and obsidian-editor-mode (not undo-in-progress))
    (save-excursion
      (goto-char beg)
      (let ((line-text (buffer-substring-no-properties
                        (line-beginning-position)
                        (save-excursion (goto-char end) (line-end-position)))))
        (when (string-match-p "\\[\\[\\|\\]\\]" line-text)
          (obsidian--fontify-links))))))

(defun obsidian--setup-mouse ()
  "Refresh clickable wiki-link properties in the current buffer."
  (obsidian--fontify-links))

(defun obsidian--mouse-follow-link (event)
  "Follow the wiki link under the mouse click EVENT."
  (interactive "e")
  (posn-set-point (event-end event))
  (obsidian-follow-link-at-point))


;; Inline `[[' completion-at-point

(defun obsidian-completion-at-point ()
  "Complete Vault note names right after typing `[['."
  (when obsidian--vault
    (let ((line-beg (line-beginning-position))
          (pt (point)))
      (save-excursion
        (when (re-search-backward "\\[\\[" line-beg t)
          (let ((start (+ (match-beginning 0) 2)))
            (when (and (<= start pt)
                       (not (string-match-p
                             "\\]\\]\\|[|#]"
                             (buffer-substring-no-properties start pt))))
              (list start pt
                    (obsidian--all-note-names)
                    :exclusive 'no
                    :exit-function
                    (lambda (_str status)
                      (when (eq status 'finished)
                        (unless (looking-at-p "\\]\\]")
                          (insert "]]"))
                        (obsidian--fontify-links)))))))))))


;; Non-intrusive Backlinks footer overlay

(defun obsidian--clear-backlinks-overlay ()
  "Remove the Backlinks footer overlay if present."
  (when (overlayp obsidian--backlinks-overlay)
    (delete-overlay obsidian--backlinks-overlay))
  (setq obsidian--backlinks-overlay nil))

(defun obsidian--collect-backlinks (target-file)
  "Return a list of `(SOURCE-FILE SOURCE-NAME SNIPPET)' linking to TARGET-FILE."
  (when (and target-file obsidian--vault (fboundp 'obsidian--all-note-files))
    (let ((target-name (file-name-base target-file))
          results)
      (dolist (file (obsidian--all-note-files))
        (unless (file-equal-p file target-file)
          (condition-case nil
              (with-temp-buffer
                (insert-file-contents file)
                (goto-char (point-min))
                (let (matched-line)
                  (while (and (not matched-line)
                              (re-search-forward obsidian-link-regexp nil t))
                    (when (string= (obsidian--link-name
                                    (match-string-no-properties 1))
                                   target-name)
                      (setq matched-line
                            (string-trim
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))))
                  (when matched-line
                    (push (list file
                                (file-name-base file)
                                (truncate-string-to-width matched-line 52 nil nil "…"))
                          results))))
            (file-error nil))))
      (nreverse results))))

(defun obsidian--make-backlink-line (source-file source-name snippet)
  "Build one clickable backlink entry for SOURCE-FILE."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
      (lambda (event)
        (interactive "e")
        (let ((win (posn-window (event-start event))))
          (when (window-live-p win) (select-window win)))
        (obsidian--open-note source-file)))
    (define-key map (kbd "RET")
      (lambda ()
        (interactive)
        (obsidian--open-note source-file)))
    (concat
     "  "
     (propertize (format "← %s" source-name)
                 'face 'obsidian-link
                 'mouse-face 'highlight
                 'help-echo (format "クリックで開く: %s" source-name)
                 'keymap map)
     (propertize (format "  %s\n" snippet)
                 'face 'obsidian-header-muted))))

(defun obsidian--refresh-backlinks-overlay ()
  "Update the end-of-buffer Backlinks overlay without modifying buffer text."
  (obsidian--clear-backlinks-overlay)
  (when (and obsidian-show-backlinks
             buffer-file-name
             obsidian--vault
             (obsidian--note-file-p buffer-file-name))
    (let ((backlinks (obsidian--collect-backlinks buffer-file-name)))
      (when backlinks
        (let* ((heading (propertize
                         (format "\n\n─── バックリンク (%d件・クリックでジャンプ) ───\n"
                                 (length backlinks))
                         'face 'obsidian-backlinks-heading))
               (body (mapconcat
                      (lambda (item)
                        (obsidian--make-backlink-line
                         (nth 0 item) (nth 1 item) (nth 2 item)))
                      backlinks
                      ""))
               (ov (make-overlay (point-max) (point-max) nil t t)))
          (overlay-put ov 'obsidian-backlinks t)
          (overlay-put ov 'after-string (concat heading body))
          (setq obsidian--backlinks-overlay ov))))))

(defun obsidian-toggle-backlinks ()
  "Toggle the Backlinks footer section in the editor."
  (interactive)
  (setq obsidian-show-backlinks (not obsidian-show-backlinks))
  (if obsidian-show-backlinks
      (obsidian--refresh-backlinks-overlay)
    (obsidian--clear-backlinks-overlay))
  (message "バックリンク表示: %s" (if obsidian-show-backlinks "ON" "OFF")))


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
      (let ((pt (point))
            (line-beg (line-beginning-position))
            (line-end (line-end-position)))
        (goto-char line-beg)
        (while (re-search-forward obsidian-link-regexp line-end t)
          (when (and (>= pt (match-beginning 0))
                     (<= pt (match-end 0)))
            (throw 'found (match-string-no-properties 1))))
        (goto-char pt)
        (when (re-search-forward obsidian-link-regexp line-end t)
          (throw 'found (match-string-no-properties 1)))
        nil))))

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
  (let* ((base (if (string-match-p "/" name)
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


;; Auto-create & rename linked files on save

(defun obsidian--replace-link-targets-in-buffer (old-name new-name)
  "Replace wiki-link target OLD-NAME with NEW-NAME in the current buffer.
Aliases and heading fragments are retained.  Return the number replaced."
  (let ((case-fold-search nil)
        replacements)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward obsidian-link-regexp nil t)
        (let* ((begin (match-beginning 0))
               (end (match-end 0))
               (target (match-string-no-properties 1))
               (separator (string-match "[|#]" target))
               (raw-name (if separator (substring target 0 separator) target)))
          (when (string= (string-trim raw-name) old-name)
            (push (list begin end
                        (concat "[[" new-name
                                (if separator
                                    (substring target separator) "")
                                "]]"))
                  replacements))))
      ;; Apply from the end so earlier buffer positions stay valid.
      (dolist (replacement replacements)
        (goto-char (nth 0 replacement))
        (delete-region (nth 0 replacement) (nth 1 replacement))
        (insert (nth 2 replacement))))
    (length replacements)))

(defun obsidian--update-backlinks (old-name new-name)
  "Change every Vault link from OLD-NAME to NEW-NAME.
Return the total number of updated links.  Visiting buffers are updated and
saved as well, preventing a later save from restoring an obsolete link."
  (let ((count 0)
        (obsidian--backlink-update-in-progress t))
    (dolist (file (obsidian--all-note-files))
      (let ((buffer (find-buffer-visiting file)))
        (if (buffer-live-p buffer)
            (with-current-buffer buffer
              (let ((changed (obsidian--replace-link-targets-in-buffer
                              old-name new-name)))
                (when (> changed 0)
                  (setq count (+ count changed))
                  (save-buffer))))
          (with-temp-buffer
            (insert-file-contents file)
            (let ((changed (obsidian--replace-link-targets-in-buffer
                            old-name new-name)))
              (when (> changed 0)
                (setq count (+ count changed))
                (write-region (point-min) (point-max) file nil 'silent)))))))
    count))

(defun obsidian--rename-edited-link-target (new-link-names)
  "Rename one unambiguous edited link target using NEW-LINK-NAMES."
  (let ((removed (cl-set-difference obsidian--saved-link-names new-link-names
                                    :test #'string=))
        (added (cl-set-difference new-link-names obsidian--saved-link-names
                                  :test #'string=)))
    (when (and (= (length removed) 1) (= (length added) 1))
      (let* ((old-name (car removed))
             (new-name (car added))
             (old-file (obsidian--find-note old-name)))
        (when old-file
          (let ((new-file (obsidian--safe-note-path
                           new-name (file-name-directory old-file))))
            (unless (or (file-exists-p new-file)
                        (obsidian--find-note new-name))
              (let ((target-buffer (find-buffer-visiting old-file))
                    (renamed-current
                     (and obsidian--current-file
                          (file-equal-p obsidian--current-file old-file)))
                    (backlink-count 0))
                (make-directory (file-name-directory new-file) t)
                (rename-file old-file new-file)
                (when (buffer-live-p target-buffer)
                  (with-current-buffer target-buffer
                    (set-visited-file-name new-file t)))
                (when renamed-current
                  (setq obsidian--current-file new-file))
                (setq backlink-count
                      (obsidian--update-backlinks old-name new-name))
                (message "Renamed linked note: %s -> %s (%d backlinks updated)"
                         (file-name-nondirectory old-file)
                         (file-name-nondirectory new-file)
                         backlink-count)))))))))

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
  "Refresh graph, tree, Live Preview, backlinks, and auto-create linked files."
  (when (and obsidian--vault
             (buffer-file-name)
             (obsidian--note-file-p (buffer-file-name)))
    (let ((new-link-names (obsidian--link-names-in-buffer)))
      (unless obsidian--backlink-update-in-progress
        (obsidian--rename-edited-link-target new-link-names)
        (save-excursion
          (obsidian--auto-create-linked-files)))
      (setq obsidian--saved-link-names new-link-names))
    (obsidian--fontify-links)
    (obsidian--refresh-backlinks-overlay)
    (when (get-buffer obsidian-tree-buffer-name)
      (obsidian--tree-refresh))
    (obsidian--schedule-graph-update)))

(provide 'obsidian-editor)
;;; obsidian-editor.el ends here
