;;; obsidian-windows.el --- Window layout and resizing -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

(defgroup obsidian nil
  "Obsidian-like note-taking environment for Emacs."
  :group 'text
  :prefix "obsidian-")


;; Customization

(defcustom obsidian-graph-width 40
  "Width (in characters) of the graph view window."
  :type 'integer)

(defcustom obsidian-tree-width 30
  "Width (in characters) of the file tree window."
  :type 'integer)

(defcustom obsidian-save-window-sizes t
  "If non-nil, window sizes are saved and restored on next launch."
  :type 'boolean)

(defcustom obsidian-window-sizes-file
  (expand-file-name "obsidian-window-sizes" user-emacs-directory)
  "File where window sizes are persisted."
  :type 'file)

(defcustom obsidian-tree-buffer-name "*Obsidian Tree*"
  "Name of the file tree buffer."
  :type 'string)

(defcustom obsidian-graph-buffer-name "*Obsidian Graph*"
  "Name of the graph view buffer."
  :type 'string)

(defcustom obsidian-editor-buffer-name-prefix "*Obsidian Editor*"
  "Prefix used for the editor window's dedicated buffer slot."
  :type 'string)


;; Internal variables

(defvar obsidian--saved-tree-width nil)
(defvar obsidian--saved-graph-width nil)


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
    (define-key m [mouse-1]       #'obsidian--graph-mouse-open)
    m))


;; Window setup

(defun obsidian--setup-windows ()
  "Create the three-window Obsidian layout."
  (delete-other-windows)
  (let (left-win center-win right-win)
    (setq right-win (split-window-right (- (or obsidian--saved-graph-width
                                               obsidian-graph-width))))
    (setq left-win  (split-window nil (or obsidian--saved-tree-width
                                          obsidian-tree-width) 'left))
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
    (when obsidian--saved-tree-width
      (with-selected-window left-win
        (let ((delta (- obsidian--saved-tree-width (window-body-width))))
          (when (not (zerop delta))
            (enlarge-window delta 'horizontal)))))
    (when obsidian--saved-graph-width
      (with-selected-window right-win
        (let ((delta (- obsidian--saved-graph-width (window-body-width))))
          (when (not (zerop delta))
            (enlarge-window delta 'horizontal)))))
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
  "Save current tree and graph window widths to file."
  (when obsidian-save-window-sizes
    (let ((tree-win (obsidian--tree-window))
          (graph-win (obsidian--graph-window)))
      (when (and tree-win graph-win)
        (with-temp-file obsidian-window-sizes-file
          (insert (format "%d %d\n"
                          (window-body-width tree-win)
                          (window-body-width graph-win))))))))

(defun obsidian--load-window-sizes ()
  "Load saved window sizes into internal variables."
  (setq obsidian--saved-tree-width nil
        obsidian--saved-graph-width nil)
  (when (and obsidian-save-window-sizes
             (file-readable-p obsidian-window-sizes-file))
    (with-temp-buffer
      (insert-file-contents obsidian-window-sizes-file)
      (goto-char (point-min))
      (when (looking-at "\\([0-9]+\\) \\([0-9]+\\)")
        (let ((tree-w (string-to-number (match-string 1)))
              (graph-w (string-to-number (match-string 2))))
          (when (> tree-w 5)
            (setq obsidian--saved-tree-width tree-w))
          (when (> graph-w 5)
            (setq obsidian--saved-graph-width graph-w)))))))

(defun obsidian-enlarge-tree (n)
  "Enlarge the tree window by N columns."
  (interactive "p")
  (let ((win (obsidian--tree-window)))
    (when win
      (with-selected-window win
        (enlarge-window (- (or n 2)) 'horizontal))
      (obsidian--save-window-sizes))))

(defun obsidian-shrink-tree (n)
  "Shrink the tree window by N columns."
  (interactive "p")
  (let ((win (obsidian--tree-window)))
    (when win
      (with-selected-window win
        (enlarge-window (or n 2) 'horizontal))
      (obsidian--save-window-sizes))))

(defun obsidian-enlarge-graph (n)
  "Enlarge the graph window by N columns."
  (interactive "p")
  (let ((win (obsidian--graph-window)))
    (when win
      (with-selected-window win
        (enlarge-window (or n 2) 'horizontal))
      (obsidian--save-window-sizes))))

(defun obsidian-shrink-graph (n)
  "Shrink the graph window by N columns."
  (interactive "p")
  (let ((win (obsidian--graph-window)))
    (when win
      (with-selected-window win
        (enlarge-window (- (or n 2)) 'horizontal))
      (obsidian--save-window-sizes))))

(provide 'obsidian-windows)
;;; obsidian-windows.el ends here
