;;; obsidian-windows.el --- Window layout, header toolbars, and mouse resizing -*- lexical-binding: t; -*-

;;; Commentary:
;; Owns the three-pane layout, clickable terminal/GUI header toolbars,
;; panel keymaps, mouse-drag border resizing, panel collapsing, and persisted widths.

;;; Code:

(require 'cl-lib)

;; Other modules are loaded after this foundational window module.  Explicit
;; declarations keep standalone byte compilation useful without circular
;; `require' calls.
(declare-function obsidian "obsidian")
(declare-function obsidian-quit "obsidian")
(declare-function obsidian-daily-note "obsidian")
(declare-function obsidian-find-note "obsidian")
(declare-function obsidian-insert-link "obsidian-editor")
(declare-function obsidian-follow-link-at-point "obsidian-editor")
(declare-function obsidian-create-note "obsidian-editor")
(declare-function obsidian-jump-back "obsidian-editor")
(declare-function obsidian-toggle-live-preview "obsidian-editor")
(declare-function obsidian-toggle-backlinks "obsidian-editor")
(declare-function obsidian-editor-mode "obsidian-editor")
(declare-function obsidian-toggle-latex-preview "obsidian-latex")
(declare-function obsidian-refresh-tree "obsidian-tree")
(declare-function obsidian--tree-refresh "obsidian-tree")
(declare-function obsidian--tree-open "obsidian-tree")
(declare-function obsidian--tree-toggle "obsidian-tree")
(declare-function obsidian--tree-collapse "obsidian-tree")
(declare-function obsidian--tree-expand "obsidian-tree")
(declare-function obsidian--tree-previous-entry "obsidian-tree")
(declare-function obsidian--tree-next-entry "obsidian-tree")
(declare-function obsidian--tree-mouse-open "obsidian-tree")
(declare-function obsidian--tree-context-menu "obsidian-tree")
(declare-function obsidian--tree-delete-file "obsidian-tree")
(declare-function obsidian-tree-rename-at-point "obsidian-tree")
(declare-function obsidian-tree-create-folder "obsidian-tree")
(declare-function obsidian-tree-cycle-sort "obsidian-tree")
(declare-function obsidian-tree-filter "obsidian-tree")
(declare-function obsidian-tree-toggle-all "obsidian-tree")
(declare-function obsidian--tree-scroll-up "obsidian-tree")
(declare-function obsidian--tree-scroll-down "obsidian-tree")
(declare-function obsidian--tree-update-marquee "obsidian-tree")
(declare-function obsidian-tree-mode "obsidian-tree")
(declare-function obsidian-refresh-graph "obsidian-graph")
(declare-function obsidian--graph-open-at-point "obsidian-graph")
(declare-function obsidian--graph-mouse-open "obsidian-graph")
(declare-function obsidian--graph-mouse-down "obsidian-graph")
(declare-function obsidian--graph-center-camera "obsidian-graph")
(declare-function obsidian--graph-move-up "obsidian-graph")
(declare-function obsidian--graph-move-down "obsidian-graph")
(declare-function obsidian--graph-move-left "obsidian-graph")
(declare-function obsidian--graph-move-right "obsidian-graph")
(declare-function obsidian-graph-next-node "obsidian-graph")
(declare-function obsidian-graph-previous-node "obsidian-graph")
(declare-function obsidian-graph-cycle-scope "obsidian-graph")
(declare-function obsidian-graph-toggle-style "obsidian-graph")
(declare-function obsidian-graph-zoom-in "obsidian-graph")
(declare-function obsidian-graph-zoom-out "obsidian-graph")
(declare-function obsidian--schedule-graph-update "obsidian-graph")
(declare-function obsidian-graph-mode "obsidian-graph")

(defgroup obsidian nil
  "Obsidian-like note-taking environment for Emacs."
  :group 'text
  :prefix "obsidian-")


;; Customization

(defcustom obsidian-graph-width 42
  "Width (in characters) of the graph view window."
  :type 'integer :group 'obsidian)

(defcustom obsidian-tree-width 30
  "Width (in characters) of the file tree window."
  :type 'integer :group 'obsidian)

(defcustom obsidian-editor-minimum-fraction 0.35
  "Minimum fraction of frame width reserved for the center editor."
  :type 'float :group 'obsidian)

(defcustom obsidian-tree-maximum-fraction 0.30
  "Maximum fraction of frame width used by the tree panel."
  :type 'float :group 'obsidian)

(defcustom obsidian-graph-maximum-fraction 0.45
  "Maximum fraction of frame width used by the graph panel."
  :type 'float :group 'obsidian)

(defcustom obsidian-save-window-sizes t
  "If non-nil, window sizes are saved and restored on next launch."
  :type 'boolean :group 'obsidian)

(defcustom obsidian-delete-by-moving-to-trash t
  "If non-nil, move deleted notes to the operating system trash."
  :type 'boolean :group 'obsidian)

(defcustom obsidian-show-header-buttons t
  "If non-nil, show clickable mouse toolbars in pane header lines."
  :type 'boolean :group 'obsidian)

(defcustom obsidian-panel-resize-step 4
  "Number of columns added or removed when clicking header resize buttons."
  :type 'integer :group 'obsidian)

(defcustom obsidian-window-sizes-file
  (expand-file-name "obsidian-window-sizes" user-emacs-directory)
  "File where window sizes are persisted."
  :type 'file :group 'obsidian)

(defcustom obsidian-tree-buffer-name "*Obsidian Tree*"
  "Name of the file tree buffer."
  :type 'string :group 'obsidian)

(defcustom obsidian-graph-buffer-name "*Obsidian Graph*"
  "Name of the graph view buffer."
  :type 'string :group 'obsidian)

(defcustom obsidian-editor-buffer-name-prefix "*Obsidian Editor*"
  "Prefix used for the editor window's dedicated buffer slot."
  :type 'string :group 'obsidian)


;; Shared UI faces (terminal & GUI adaptive)

(defface obsidian-header-title
  '((((class color) (background dark))
     :foreground "#e5e9f0" :weight bold)
    (((class color) (background light))
     :foreground "#24292f" :weight bold)
    (t :weight bold))
  "Face for pane titles in the Obsidian header line."
  :group 'obsidian)

(defface obsidian-header-muted
  '((((class color) (background dark))
     :foreground "#7f848e")
    (((class color) (background light))
     :foreground "#57606a")
    (t :inherit shadow))
  "Face for secondary labels and separators in the header line."
  :group 'obsidian)

(defface obsidian-header-button
  '((((class color) (background dark))
     :foreground "#88c0d0" :background "#2e3440" :weight normal)
    (((class color) (background light))
     :foreground "#0969da" :background "#eaeef2" :weight normal)
    (t :inherit button))
  "Face for clickable action buttons in pane header lines."
  :group 'obsidian)

(defface obsidian-header-button-active
  '((((class color) (background dark))
     :foreground "#2e3440" :background "#88c0d0" :weight bold)
    (((class color) (background light))
     :foreground "#ffffff" :background "#0969da" :weight bold)
    (t :inverse-video t :weight bold))
  "Face for active toggle buttons or hovered header buttons."
  :group 'obsidian)


;; Internal variables

(defvar obsidian--saved-tree-width nil
  "Tree width loaded from the persistence file.")
(defvar obsidian--saved-graph-width nil
  "Graph width loaded from the persistence file.")
(defvar obsidian--saved-tree-ratio nil
  "Tree width as a fraction of the complete workspace width.")
(defvar obsidian--saved-graph-ratio nil
  "Graph width as a fraction of the complete workspace width.")
(defvar obsidian--resizing-windows nil
  "Non-nil while Obsidian itself is resizing panes.")


;; Clickable header-line button helper

(defun obsidian--make-header-button (label command help &optional active-p)
  "Return a clickable header-line string LABEL running COMMAND.
HELP is shown as `help-echo'.  When ACTIVE-P is non-nil, highlight the button."
  (let ((map (make-sparse-keymap)))
    ;; Suppress `mouse-drag-header-line' on press so `mouse-1' always fires
    ;; cleanly in both GUI Emacs and `xterm-mouse-mode' terminal frames.
    (define-key map [header-line down-mouse-1] #'ignore)
    (define-key map [header-line mouse-1]
      (lambda (event)
        (interactive "e")
        (let ((win (posn-window (event-start event))))
          (if (window-live-p win)
              (with-selected-window win
                (call-interactively command))
            (call-interactively command)))))
    (define-key map [mouse-1] command)
    (propertize label
                'face (if active-p
                          'obsidian-header-button-active
                        'obsidian-header-button)
                'mouse-face 'obsidian-header-button-active
                'help-echo help
                'local-map map
                'keymap map)))


;; Keymaps

(defvar obsidian-editor-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c o l") #'obsidian-insert-link)
    (define-key m (kbd "C-c o f") #'obsidian-follow-link-at-point)
    (define-key m (kbd "C-c o g") #'obsidian-refresh-graph)
    (define-key m (kbd "C-c o t") #'obsidian-toggle-latex-preview)
    (define-key m (kbd "C-c o v") #'obsidian-toggle-live-preview)
    (define-key m (kbd "C-c o k") #'obsidian-toggle-backlinks)
    (define-key m (kbd "C-c o n") #'obsidian-create-note)
    (define-key m (kbd "C-c o d") #'obsidian-daily-note)
    (define-key m (kbd "C-c o p") #'obsidian-find-note)
    (define-key m (kbd "C-c o b") #'obsidian-jump-back)
    (define-key m (kbd "C-c o o") #'obsidian)
    (define-key m (kbd "C-c o q") #'obsidian-quit)
    (define-key m (kbd "C-c o z") #'obsidian-focus-editor)
    (define-key m (kbd "C-c o 1") #'obsidian-toggle-tree)
    (define-key m (kbd "C-c o 3") #'obsidian-toggle-graph)
    (define-key m (kbd "C-c o r") #'obsidian-refresh-tree)
    (define-key m (kbd "M-RET")   #'obsidian-follow-link-at-point)
    (define-key m (kbd "C-c o <left>")  #'obsidian-shrink-tree)
    (define-key m (kbd "C-c o <right>") #'obsidian-enlarge-tree)
    (define-key m (kbd "C-c o S-<left>")  #'obsidian-shrink-graph)
    (define-key m (kbd "C-c o S-<right>") #'obsidian-enlarge-graph)
    m))

(defvar obsidian-tree-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET")     #'obsidian--tree-open)
    (define-key m (kbd "TAB")     #'obsidian--tree-toggle)
    (define-key m (kbd "<up>")    #'obsidian--tree-previous-entry)
    (define-key m (kbd "<down>")  #'obsidian--tree-next-entry)
    (define-key m (kbd "<left>")  #'obsidian--tree-collapse)
    (define-key m (kbd "<right>") #'obsidian--tree-expand)
    (define-key m (kbd "k")       #'obsidian--tree-previous-entry)
    (define-key m (kbd "j")       #'obsidian--tree-next-entry)
    (define-key m (kbd "<")       #'obsidian-shrink-tree)
    (define-key m (kbd ">")       #'obsidian-enlarge-tree)
    (define-key m [mouse-1]       #'obsidian--tree-mouse-open)
    (define-key m [mouse-3]       #'obsidian--tree-context-menu)
    (define-key m [wheel-up]      #'obsidian--tree-scroll-up)
    (define-key m [wheel-down]    #'obsidian--tree-scroll-down)
    (define-key m [mouse-4]       #'obsidian--tree-scroll-up)
    (define-key m [mouse-5]       #'obsidian--tree-scroll-down)
    (define-key m (kbd "g")       #'obsidian--tree-refresh)
    (define-key m (kbd "n")       #'obsidian-create-note)
    (define-key m (kbd "+")       #'obsidian-tree-create-folder)
    (define-key m (kbd "d")       #'obsidian-daily-note)
    (define-key m (kbd "p")       #'obsidian-find-note)
    (define-key m (kbd "R")       #'obsidian-tree-rename-at-point)
    (define-key m (kbd "s")       #'obsidian-tree-cycle-sort)
    (define-key m (kbd "/")       #'obsidian-tree-filter)
    (define-key m (kbd "z")       #'obsidian-tree-toggle-all)
    (define-key m (kbd "<escape>") #'obsidian--tree-delete-file)
    (define-key m (kbd "r")       #'obsidian--tree-refresh)
    (define-key m (kbd "q")       #'obsidian-toggle-tree)
    m))

(defvar obsidian-graph-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g")       #'obsidian-refresh-graph)
    (define-key m (kbd "RET")     #'obsidian--graph-open-at-point)
    (define-key m (kbd "TAB")     #'obsidian-graph-next-node)
    (define-key m (kbd "<backtab>") #'obsidian-graph-previous-node)
    (define-key m (kbd "S-TAB")   #'obsidian-graph-previous-node)
    (define-key m (kbd "f")       #'obsidian-graph-cycle-scope)
    (define-key m (kbd "b")       #'obsidian-graph-toggle-style)
    (define-key m (kbd "+")       #'obsidian-graph-zoom-in)
    (define-key m (kbd "=")       #'obsidian-graph-zoom-in)
    (define-key m (kbd "-")       #'obsidian-graph-zoom-out)
    (define-key m (kbd "q")       #'obsidian-toggle-graph)
    (define-key m (kbd "<")       #'obsidian-shrink-graph)
    (define-key m (kbd ">")       #'obsidian-enlarge-graph)
    (define-key m (kbd "<up>")    #'obsidian--graph-move-up)
    (define-key m (kbd "<down>")  #'obsidian--graph-move-down)
    (define-key m (kbd "<left>")  #'obsidian--graph-move-left)
    (define-key m (kbd "<right>") #'obsidian--graph-move-right)
    (define-key m (kbd "0")       #'obsidian--graph-center-camera)
    (define-key m (kbd "h")       #'obsidian--graph-move-left)
    (define-key m (kbd "j")       #'obsidian--graph-move-down)
    (define-key m (kbd "k")       #'obsidian--graph-move-up)
    (define-key m (kbd "l")       #'obsidian--graph-move-right)
    (define-key m [down-mouse-1]  #'obsidian--graph-mouse-down)
    (define-key m [mouse-1]       #'obsidian--graph-mouse-open)
    (define-key m [wheel-up]      #'obsidian-graph-zoom-in)
    (define-key m [wheel-down]    #'obsidian-graph-zoom-out)
    (define-key m [mouse-4]       #'obsidian-graph-zoom-in)
    (define-key m [mouse-5]       #'obsidian-graph-zoom-out)
    m))


;; Window setup

(defun obsidian--empty-editor-buffer ()
  "Return a freshly initialized empty Obsidian editor buffer."
  (let ((buffer (get-buffer-create obsidian-editor-buffer-name-prefix)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (fundamental-mode)
        (erase-buffer)
        (insert
         (propertize "◆ EMACS_OBSIDIAN ワークスペース\n\n" 'face 'obsidian-header-title)
         "マウス操作ガイド:\n"
         "  ・左ツリーのノートをクリック        → 中央に開く\n"
         "  ・左ツリーで右クリック              → 操作メニュー（作成・改名・削除）\n"
         "  ・パネル間の縦線 │ を左右にドラッグ → 表示領域の幅を自由に変更\n"
         "  ・右グラフをドラッグ / ホイール回転 → マップ移動（パン）＆ 拡大縮小\n"
         "  ・各パネル上部の [ボタン] をクリック → キー操作なしで全機能を実行\n\n"
         "キーボードショートカット:\n"
         "  ・C-c o n : 新規ノート作成      ・C-c o d : 今日のデイリーノート\n"
         "  ・C-c o p : ノート高速検索      ・C-c o z : 集中モード（左右パネル開閉）\n"
         "  ・C-c o q : ワークスペースを閉じて元の画面へ戻る\n")
        (read-only-mode 1)
        (obsidian-editor-mode 1)))
    buffer))

(defconst obsidian--minimum-editor-width 20
  "Minimum width reserved for the center editor pane.")

(defconst obsidian--minimum-panel-width 10
  "Minimum usable width for either side panel.")

(defconst obsidian--window-divider-overhead 2
  "Columns consumed by the two boundaries between three side-by-side panes.")

(defun obsidian--fit-panel-widths (total tree-width graph-width)
  "Fit TREE-WIDTH and GRAPH-WIDTH inside TOTAL frame columns.
Saved widths are treated as preferences, subject to the configured responsive
limits.  If necessary, both panels shrink in roughly the same proportion."
  (let* ((editor-reserve
          (max obsidian--minimum-editor-width
               (ceiling (* total obsidian-editor-minimum-fraction))))
         (available (max (* 2 obsidian--minimum-panel-width)
                         (- total editor-reserve
                            obsidian--window-divider-overhead)))
         (tree-limit (max obsidian--minimum-panel-width
                          (floor (* total obsidian-tree-maximum-fraction))))
         (graph-limit (max obsidian--minimum-panel-width
                           (floor (* total obsidian-graph-maximum-fraction))))
         (tree (max obsidian--minimum-panel-width
                    (min tree-limit tree-width)))
         (graph (max obsidian--minimum-panel-width
                     (min graph-limit graph-width)))
         (requested (+ tree graph)))
    (if (<= requested available)
        (cons tree graph)
      (let* ((ratio (/ (float available) requested))
             (fitted-tree
              (max obsidian--minimum-panel-width (floor (* tree ratio))))
             (fitted-graph
              (max obsidian--minimum-panel-width (- available fitted-tree))))
        ;; If the graph minimum pushed the sum over AVAILABLE, take the excess
        ;; back from the tree while respecting its minimum.
        (when (> (+ fitted-tree fitted-graph) available)
          (setq fitted-tree
                (max obsidian--minimum-panel-width
                     (- available fitted-graph))))
        (cons fitted-tree fitted-graph)))))

(defun obsidian--restore-body-width (window desired-width)
  "Resize WINDOW until its body is DESIRED-WIDTH columns when possible."
  (when (window-live-p window)
    (let ((delta (- desired-width (window-body-width window))))
      (unless (zerop delta)
        ;; Decorations and fringes make split sizes differ from body sizes in
        ;; graphical frames.  `window-resize' corrects that final difference.
        (ignore-errors (window-resize window delta t))))))

(defun obsidian--requested-panel-widths (total)
  "Return fitted tree and graph widths for workspace TOTAL.
Saved ratios take precedence over legacy absolute widths."
  (let ((tree (if obsidian--saved-tree-ratio
                  (round (* total obsidian--saved-tree-ratio))
                (or obsidian--saved-tree-width obsidian-tree-width)))
        (graph (if obsidian--saved-graph-ratio
                   (round (* total obsidian--saved-graph-ratio))
                 (or obsidian--saved-graph-width obsidian-graph-width))))
    (obsidian--fit-panel-widths total tree graph)))

(defun obsidian--capture-window-ratios (tree-window graph-window total)
  "Capture TREE-WINDOW and GRAPH-WINDOW widths relative to TOTAL.
If only one side panel is visible, only update that panel's ratio."
  (when (and (numberp total) (> total 0))
    (when (window-live-p tree-window)
      (setq obsidian--saved-tree-ratio
            (/ (float (window-body-width tree-window)) total)))
    (when (window-live-p graph-window)
      (setq obsidian--saved-graph-ratio
            (/ (float (window-body-width graph-window)) total)))))

(defun obsidian--setup-windows ()
  "Create a three-window layout that fits the current frame."
  (let ((obsidian--resizing-windows t))
    (delete-other-windows)
    (let* ((frame (selected-frame))
           (total (window-total-width (frame-root-window frame)))
           (had-saved-ratios (and obsidian--saved-tree-ratio
                                  obsidian--saved-graph-ratio))
           (fitted (obsidian--requested-panel-widths total))
           (tree-width (car fitted))
           (graph-width (cdr fitted))
           left-win center-win right-win)
      (set-frame-parameter frame 'obsidian-last-total-width total)
      ;; Make vertical dividers easily draggable in GUI frames as well, while
      ;; preserving `mouse-drag-vertical-line' on `│' in terminal Emacs.
      (when (and (display-graphic-p) (fboundp 'window-divider-mode))
        (setq window-divider-default-right-width 4
              window-divider-default-places 'right-only)
        (window-divider-mode 1))
      (setq right-win (split-window-right (- graph-width)))
      (setq left-win (split-window nil tree-width 'left))
      (setq center-win (selected-window))
      (let ((buf (get-buffer-create obsidian-tree-buffer-name)))
        (with-current-buffer buf
          (unless (eq major-mode 'obsidian-tree-mode)
            (obsidian-tree-mode)))
        (set-window-buffer left-win buf))
      (let ((buf (get-buffer-create obsidian-graph-buffer-name)))
        (with-current-buffer buf
          (unless (eq major-mode 'obsidian-graph-mode)
            (obsidian-graph-mode)))
        (set-window-buffer right-win buf))
      (set-window-buffer center-win (obsidian--empty-editor-buffer))
      ;; Split sizes use total columns, while the persistence file stores body
      ;; columns.  Correct both panels after all three windows exist.
      (obsidian--restore-body-width right-win graph-width)
      (obsidian--restore-body-width left-win tree-width)
      ;; Convert a legacy absolute-width file into ratios after its first
      ;; successful layout.  A v2 ratio file remains stable across frame sizes.
      (unless had-saved-ratios
        (obsidian--capture-window-ratios left-win right-win total))
      (select-window center-win))))

(defun obsidian--editor-window ()
  "Return the window currently displaying the editor."
  (let ((tree-win (get-buffer-window obsidian-tree-buffer-name))
        (graph-win (get-buffer-window obsidian-graph-buffer-name)))
    (or (cl-find-if (lambda (w)
                      (not (or (eq w tree-win) (eq w graph-win))))
                    (window-list nil 0))
        (selected-window))))

(defun obsidian--tree-window ()
  "Return the window displaying the tree."
  (or (get-buffer-window obsidian-tree-buffer-name) (selected-window)))

(defun obsidian--graph-window ()
  "Return the window displaying the graph."
  (or (get-buffer-window obsidian-graph-buffer-name) (selected-window)))


;; Panel visibility toggles (1-click collapse/expand & Zen mode)

(defun obsidian-toggle-tree ()
  "Show or hide the left file-tree panel."
  (interactive)
  (let ((tree-win (get-buffer-window obsidian-tree-buffer-name))
        (total (window-total-width (frame-root-window (selected-frame)))))
    (if (and (window-live-p tree-win) (not (one-window-p t)))
        (let ((obsidian--resizing-windows t))
          (obsidian--capture-window-ratios tree-win nil total)
          (obsidian--save-window-sizes)
          (delete-window tree-win)
          (obsidian--schedule-graph-update))
      (let* ((obsidian--resizing-windows t)
             (widths (obsidian--requested-panel-widths total))
             (tree-width (car widths))
             (target-win (obsidian--editor-window))
             (left-win (split-window target-win tree-width 'left))
             (buf (get-buffer-create obsidian-tree-buffer-name)))
        (with-current-buffer buf
          (unless (eq major-mode 'obsidian-tree-mode)
            (obsidian-tree-mode)))
        (set-window-buffer left-win buf)
        (obsidian--restore-body-width left-win tree-width)
        (obsidian--tree-refresh)
        (obsidian--schedule-graph-update)))
    (force-mode-line-update t)))

(defun obsidian-toggle-graph ()
  "Show or hide the right graph panel."
  (interactive)
  (let ((graph-win (get-buffer-window obsidian-graph-buffer-name))
        (total (window-total-width (frame-root-window (selected-frame)))))
    (if (and (window-live-p graph-win) (not (one-window-p t)))
        (let ((obsidian--resizing-windows t))
          (obsidian--capture-window-ratios nil graph-win total)
          (obsidian--save-window-sizes)
          (delete-window graph-win))
      (let* ((obsidian--resizing-windows t)
             (widths (obsidian--requested-panel-widths total))
             (graph-width (cdr widths))
             (target-win (obsidian--editor-window))
             (right-win (split-window target-win (- graph-width) 'right))
             (buf (get-buffer-create obsidian-graph-buffer-name)))
        (with-current-buffer buf
          (unless (eq major-mode 'obsidian-graph-mode)
            (obsidian-graph-mode)))
        (set-window-buffer right-win buf)
        (obsidian--restore-body-width right-win graph-width)
        (obsidian-refresh-graph)))
    (force-mode-line-update t)))

(defun obsidian-focus-editor ()
  "Toggle Zen writing mode by hiding or restoring both side panels."
  (interactive)
  (let ((tree-win (get-buffer-window obsidian-tree-buffer-name))
        (graph-win (get-buffer-window obsidian-graph-buffer-name))
        (total (window-total-width (frame-root-window (selected-frame)))))
    (if (or (window-live-p tree-win) (window-live-p graph-win))
        (let ((obsidian--resizing-windows t))
          (obsidian--capture-window-ratios tree-win graph-win total)
          (obsidian--save-window-sizes)
          (when (and (window-live-p tree-win) (not (one-window-p t)))
            (delete-window tree-win))
          (when (and (window-live-p graph-win) (not (one-window-p t)))
            (delete-window graph-win))
          (message "集中モード ON（上部の [▸ツリー] [グラフ◂] または C-c o z で復帰）"))
      (obsidian-toggle-tree)
      (obsidian-toggle-graph)
      (select-window (obsidian--editor-window))
      (message "3ペイン表示に復帰しました"))
    (force-mode-line-update t)))


;; Window resizing & persistence

(defun obsidian--save-window-sizes ()
  "Save current tree and graph proportions to file."
  (when obsidian-save-window-sizes
    (let* ((frame (selected-frame))
           (tree-win (get-buffer-window obsidian-tree-buffer-name frame))
           (graph-win (get-buffer-window obsidian-graph-buffer-name frame))
           (total (window-total-width (frame-root-window frame))))
      (obsidian--capture-window-ratios tree-win graph-win total)
      (when (and obsidian--saved-tree-ratio obsidian--saved-graph-ratio)
        (ignore-errors
          (with-temp-file obsidian-window-sizes-file
            (insert (format "v2 %.8f %.8f\n"
                            obsidian--saved-tree-ratio
                            obsidian--saved-graph-ratio))))))))

(defun obsidian--load-window-sizes ()
  "Load saved window sizes into internal variables."
  (setq obsidian--saved-tree-width nil
        obsidian--saved-graph-width nil
        obsidian--saved-tree-ratio nil
        obsidian--saved-graph-ratio nil)
  (when (and obsidian-save-window-sizes
             (file-readable-p obsidian-window-sizes-file))
    (with-temp-buffer
      (insert-file-contents obsidian-window-sizes-file)
      (goto-char (point-min))
      (cond
       ;; Current format stores ratios and therefore survives frame resizing.
       ((looking-at "v2[ \\t]+\\([0-9.]+\\)[ \\t]+\\([0-9.]+\\)")
        (let ((tree-ratio (string-to-number (match-string 1)))
              (graph-ratio (string-to-number (match-string 2))))
          (when (and (> tree-ratio 0.0) (< tree-ratio 1.0)
                     (> graph-ratio 0.0) (< graph-ratio 1.0)
                     (< (+ tree-ratio graph-ratio) 1.0))
            (setq obsidian--saved-tree-ratio tree-ratio
                  obsidian--saved-graph-ratio graph-ratio))))
       ;; Legacy format: absolute columns.  Setup converts these to v2 ratios.
       ((looking-at "\\([0-9]+\\) \\([0-9]+\\)")
        (let ((tree-w (string-to-number (match-string 1)))
              (graph-w (string-to-number (match-string 2))))
          (when (> tree-w 5) (setq obsidian--saved-tree-width tree-w))
          (when (> graph-w 5) (setq obsidian--saved-graph-width graph-w))))))))

(defun obsidian--handle-frame-size-change (frame)
  "Handle both OS/terminal FRAME resizes and mouse divider drags cleanly.
When FRAME's total column width changed, preserve saved pane proportions.
When FRAME's total width stayed the same, the user dragged a `│' divider with
the mouse (or resized a window manually): adopt and save the new widths!"
  (unless obsidian--resizing-windows
    (let ((tree-win (get-buffer-window obsidian-tree-buffer-name frame))
          (graph-win (get-buffer-window obsidian-graph-buffer-name frame)))
      (when (or (window-live-p tree-win) (window-live-p graph-win))
        (let* ((total (window-total-width (frame-root-window frame)))
               (last-total (frame-parameter frame 'obsidian-last-total-width)))
          (cond
           ;; Case 1: The terminal/OS window itself changed width -> scale panes.
           ((and last-total (/= total last-total))
            (set-frame-parameter frame 'obsidian-last-total-width total)
            (let* ((obsidian--resizing-windows t)
                   (widths (obsidian--requested-panel-widths total)))
              (when (window-live-p graph-win)
                (obsidian--restore-body-width graph-win (cdr widths)))
              (when (window-live-p tree-win)
                (obsidian--restore-body-width tree-win (car widths)))))
           ;; Case 2: Frame width unchanged -> user dragged a vertical divider!
           ;; Persist the user's chosen pane widths instead of snapping back.
           (t
            (set-frame-parameter frame 'obsidian-last-total-width total)
            (obsidian--capture-window-ratios tree-win graph-win total)
            (obsidian--save-window-sizes)))
          ;; Always refresh overflowing tree labels and graph viewport after
          ;; any width change settles.
          (when (and (window-live-p tree-win)
                     (fboundp 'obsidian--tree-update-marquee))
            (with-current-buffer (window-buffer tree-win)
              (obsidian--tree-update-marquee)))
          (when (window-live-p graph-win)
            (obsidian--schedule-graph-update)))))))

(defun obsidian--resize-panel (window delta)
  "Resize WINDOW horizontally by DELTA and persist panel widths."
  (when (window-live-p window)
    (let ((obsidian--resizing-windows t))
      (with-selected-window window
        (ignore-errors (enlarge-window delta 'horizontal))))
    (obsidian--save-window-sizes)
    (when-let ((tree-buf (get-buffer obsidian-tree-buffer-name)))
      (when (fboundp 'obsidian--tree-update-marquee)
        (with-current-buffer tree-buf
          (obsidian--tree-update-marquee))))
    (obsidian--schedule-graph-update)))

(defun obsidian-enlarge-tree (&optional n)
  "Enlarge the tree window by N columns (defaults to `obsidian-panel-resize-step' on click)."
  (interactive "P")
  (obsidian--resize-panel (obsidian--tree-window)
                          (if n (prefix-numeric-value n) obsidian-panel-resize-step)))

(defun obsidian-shrink-tree (&optional n)
  "Shrink the tree window by N columns."
  (interactive "P")
  (obsidian--resize-panel (obsidian--tree-window)
                          (- (if n (prefix-numeric-value n) obsidian-panel-resize-step))))

(defun obsidian-enlarge-graph (&optional n)
  "Enlarge the graph window by N columns."
  (interactive "P")
  (obsidian--resize-panel (obsidian--graph-window)
                          (if n (prefix-numeric-value n) obsidian-panel-resize-step)))

(defun obsidian-shrink-graph (&optional n)
  "Shrink the graph window by N columns."
  (interactive "P")
  (obsidian--resize-panel (obsidian--graph-window)
                          (- (if n (prefix-numeric-value n) obsidian-panel-resize-step))))

;; Save sizes even when the user closes Emacs without explicitly resizing at
;; the end of the session.
(add-hook 'kill-emacs-hook #'obsidian--save-window-sizes)
(add-hook 'window-size-change-functions #'obsidian--handle-frame-size-change)

(provide 'obsidian-windows)
;;; obsidian-windows.el ends here
