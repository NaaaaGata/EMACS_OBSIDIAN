;;; obsidian-windows.el --- Window layout and resizing -*- lexical-binding: t; -*-

;;; Commentary:
;; Owns the three-pane layout, panel keymaps, resizing, and persisted widths.

;;; Code:

(require 'cl-lib)

;; Other modules are loaded after this foundational window module.  Explicit
;; declarations keep standalone byte compilation useful without circular
;; `require' calls.
(declare-function obsidian "obsidian")
(declare-function obsidian-insert-link "obsidian-editor")
(declare-function obsidian-follow-link-at-point "obsidian-editor")
(declare-function obsidian-create-note "obsidian-editor")
(declare-function obsidian-jump-back "obsidian-editor")
(declare-function obsidian-editor-mode "obsidian-editor")
(declare-function obsidian-toggle-latex-preview "obsidian-latex")
(declare-function obsidian-refresh-tree "obsidian-tree")
(declare-function obsidian--tree-refresh "obsidian-tree")
(declare-function obsidian--tree-open "obsidian-tree")
(declare-function obsidian--tree-toggle "obsidian-tree")
(declare-function obsidian--tree-collapse "obsidian-tree")
(declare-function obsidian--tree-expand "obsidian-tree")
(declare-function obsidian--tree-mouse-open "obsidian-tree")
(declare-function obsidian-tree-mode "obsidian-tree")
(declare-function obsidian-refresh-graph "obsidian-graph")
(declare-function obsidian--graph-open-at-point "obsidian-graph")
(declare-function obsidian--graph-mouse-open "obsidian-graph")
(declare-function obsidian--graph-center-camera "obsidian-graph")
(declare-function obsidian--graph-move-up "obsidian-graph")
(declare-function obsidian--graph-move-down "obsidian-graph")
(declare-function obsidian--graph-move-left "obsidian-graph")
(declare-function obsidian--graph-move-right "obsidian-graph")
(declare-function obsidian--schedule-graph-update "obsidian-graph")
(declare-function obsidian-graph-mode "obsidian-graph")

(defgroup obsidian nil
  "Obsidian-like note-taking environment for Emacs."
  :group 'text
  :prefix "obsidian-")


;; Customization

(defcustom obsidian-graph-width 40
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


;; Keymaps

(defvar obsidian-editor-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c o l") #'obsidian-insert-link)
    (define-key m (kbd "C-c o f") #'obsidian-follow-link-at-point)
    (define-key m (kbd "C-c o g") #'obsidian-refresh-graph)
    (define-key m (kbd "C-c o t") #'obsidian-toggle-latex-preview)
    (define-key m (kbd "C-c o n") #'obsidian-create-note)
    (define-key m (kbd "C-c o b") #'obsidian-jump-back)
    (define-key m (kbd "C-c o o") #'obsidian)
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
    (define-key m (kbd "<left>")  #'obsidian--tree-collapse)
    (define-key m (kbd "<right>") #'obsidian--tree-expand)
    (define-key m (kbd "<")       #'obsidian-shrink-tree)
    (define-key m (kbd ">")       #'obsidian-enlarge-tree)
    (define-key m [mouse-1]       #'obsidian--tree-mouse-open)
    (define-key m (kbd "g")       #'obsidian--tree-refresh)
    (define-key m (kbd "n")       #'obsidian-create-note)
    (define-key m (kbd "r")       #'obsidian--tree-refresh)
    (define-key m (kbd "q")       #'obsidian--tree-refresh)
    m))

(defvar obsidian-graph-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g")       #'obsidian-refresh-graph)
    (define-key m (kbd "RET")     #'obsidian--graph-open-at-point)
    (define-key m (kbd "q")       #'obsidian-refresh-graph)
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
    (define-key m [mouse-1]       #'obsidian--graph-mouse-open)
    m))


;; Window setup

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
  (let ((delta (- desired-width (window-body-width window))))
    (unless (zerop delta)
      ;; Decorations and fringes make split sizes differ from body sizes in
      ;; graphical frames.  `window-resize' corrects that final difference.
      (ignore-errors (window-resize window delta t)))))

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
  "Capture TREE-WINDOW and GRAPH-WINDOW widths relative to TOTAL."
  (setq obsidian--saved-tree-ratio
        (/ (float (window-body-width tree-window)) total)
        obsidian--saved-graph-ratio
        (/ (float (window-body-width graph-window)) total)))

(defun obsidian--setup-windows ()
  "Create a three-window layout that fits the current frame."
  (delete-other-windows)
  (let* ((total (window-total-width))
         (had-saved-ratios (and obsidian--saved-tree-ratio
                                obsidian--saved-graph-ratio))
         (fitted (obsidian--requested-panel-widths total))
         (tree-width (car fitted))
         (graph-width (cdr fitted))
         (obsidian--resizing-windows t)
         left-win center-win right-win)
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
    (let ((buf (get-buffer-create obsidian-editor-buffer-name-prefix)))
      (with-current-buffer buf
        (fundamental-mode)
        (erase-buffer)
        (insert "Obsidian workspace ready.\n\n"
                "Click a file on the left, or press C-c o n to create a note.\n"
                "Press C-c o l to insert a link, M-RET to follow a link.\n")
        (read-only-mode 1)
        (obsidian-editor-mode 1))
      (set-window-buffer center-win buf))
    ;; Split sizes use total columns, while the persistence file stores body
    ;; columns.  Correct both panels after all three windows exist.
    (obsidian--restore-body-width right-win graph-width)
    (obsidian--restore-body-width left-win tree-width)
    ;; Convert a legacy absolute-width file into ratios after its first
    ;; successful layout.  A v2 ratio file remains stable across frame sizes.
    (unless had-saved-ratios
      (obsidian--capture-window-ratios left-win right-win total))
    (select-window center-win)))

(defun obsidian--editor-window ()
  "Return the window currently displaying the editor."
  (let ((tree-win (get-buffer-window obsidian-tree-buffer-name))
        (graph-win (get-buffer-window obsidian-graph-buffer-name)))
    (cl-find-if (lambda (w)
                  (not (or (eq w tree-win) (eq w graph-win))))
                (window-list nil 0))))

(defun obsidian--tree-window ()
  "Return the window displaying the tree."
  (or (get-buffer-window obsidian-tree-buffer-name) (selected-window)))

(defun obsidian--graph-window ()
  "Return the window displaying the graph."
  (or (get-buffer-window obsidian-graph-buffer-name) (selected-window)))


;; Window resizing

(defun obsidian--save-window-sizes ()
  "Save current tree and graph proportions to file."
  (when obsidian-save-window-sizes
    (let ((tree-win (get-buffer-window obsidian-tree-buffer-name))
          (graph-win (get-buffer-window obsidian-graph-buffer-name)))
      (when (and (window-live-p tree-win) (window-live-p graph-win))
        (let ((total (window-total-width
                      (frame-root-window (window-frame tree-win)))))
          (obsidian--capture-window-ratios tree-win graph-win total))
        (with-temp-file obsidian-window-sizes-file
          (insert (format "v2 %.8f %.8f\n"
                          obsidian--saved-tree-ratio
                          obsidian--saved-graph-ratio)))))))

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
  "Maintain saved pane proportions after a size change to FRAME."
  (unless obsidian--resizing-windows
    (let ((tree-win (get-buffer-window obsidian-tree-buffer-name frame))
          (graph-win (get-buffer-window obsidian-graph-buffer-name frame)))
      (when (and obsidian--saved-tree-ratio obsidian--saved-graph-ratio
                 (window-live-p tree-win) (window-live-p graph-win))
        (let* ((obsidian--resizing-windows t)
               (total (window-total-width (frame-root-window frame)))
               (widths (obsidian--requested-panel-widths total)))
          (obsidian--restore-body-width graph-win (cdr widths))
          (obsidian--restore-body-width tree-win (car widths))
          ;; Rebuild the viewport at its new dimensions after resize events
          ;; settle; repeated events are coalesced by the graph scheduler.
          (obsidian--schedule-graph-update))))))

(defun obsidian--resize-panel (window delta)
  "Resize WINDOW horizontally by DELTA and persist panel widths."
  (when (window-live-p window)
    (with-selected-window window
      (enlarge-window delta 'horizontal))
    (obsidian--save-window-sizes)))

(defun obsidian-enlarge-tree (n)
  "Enlarge the tree window by N columns."
  (interactive "p")
  (obsidian--resize-panel (obsidian--tree-window) (or n 1)))

(defun obsidian-shrink-tree (n)
  "Shrink the tree window by N columns."
  (interactive "p")
  (obsidian--resize-panel (obsidian--tree-window) (- (or n 1))))

(defun obsidian-enlarge-graph (n)
  "Enlarge the graph window by N columns."
  (interactive "p")
  (obsidian--resize-panel (obsidian--graph-window) (or n 1)))

(defun obsidian-shrink-graph (n)
  "Shrink the graph window by N columns."
  (interactive "p")
  (obsidian--resize-panel (obsidian--graph-window) (- (or n 1))))

;; Save sizes even when the user closes Emacs without explicitly resizing at
;; the end of the session.
(add-hook 'kill-emacs-hook #'obsidian--save-window-sizes)
(add-hook 'window-size-change-functions #'obsidian--handle-frame-size-change)

(provide 'obsidian-windows)
;;; obsidian-windows.el ends here
