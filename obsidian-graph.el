;;; obsidian-graph.el --- Pannable text graph view -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function obsidian--open-note "obsidian-editor")
(declare-function obsidian--note-file-p "obsidian-tree")

(defcustom obsidian-graph-max-nodes 200
  "Maximum number of nodes drawn in the graph view."
  :type 'integer :group 'obsidian)

(defface obsidian-graph-current
  '((t :foreground "#ff5f5f" :weight bold))
  "Face for the current note." :group 'obsidian)
(defface obsidian-graph-connected
  '((t :foreground "#5fafff" :weight bold))
  "Face for notes connected to the current note." :group 'obsidian)
(defface obsidian-graph-unlinked
  '((t :foreground "gray55"))
  "Face for unlinked notes." :group 'obsidian)
(defface obsidian-graph-edge
  '((t :foreground "gray65"))
  "Face for graph edges and linked notes." :group 'obsidian)

(defvar obsidian--graph-points nil)
(defvar-local obsidian--graph-camera-x 0)
(defvar-local obsidian--graph-camera-y 0)
(defvar-local obsidian--graph-canvas nil)
(defvar-local obsidian--graph-canvas-width 0)
(defvar-local obsidian--graph-canvas-height 0)
(defvar-local obsidian--graph-current-position nil)

(define-derived-mode obsidian-graph-mode special-mode "Obsidian-Graph"
  "Major mode for a clickable, pannable text graph."
  (setq-local truncate-lines t)
  ;; The arrows control a camera, not a text cursor.
  (setq-local cursor-type nil))

;;; Notes and edges

(defun obsidian--all-note-files ()
  "Return real, readable Markdown notes in the vault."
  (when (and obsidian--vault (file-directory-p obsidian--vault))
    (cl-remove-if-not
     #'obsidian--note-file-p
     (directory-files-recursively
      obsidian--vault
      (concat "\\." (regexp-quote obsidian-file-extension) "\\'")))))

(defun obsidian--all-note-names ()
  "Return all note base names."
  (delete-dups (mapcar #'file-name-base (obsidian--all-note-files))))

(defun obsidian--notes-in-current-scope ()
  "Return notes in the selected directory scope."
  (let* ((scope (file-name-as-directory
                 (file-truename (or obsidian--current-scope obsidian--vault))))
         (vault (file-name-as-directory (file-truename obsidian--vault)))
         (files (obsidian--all-note-files)))
    (if (string= scope vault)
        (cl-remove-if-not
         (lambda (file)
           (string= (file-name-as-directory
                     (file-truename (file-name-directory file))) vault))
         files)
      (cl-remove-if-not
       (lambda (file) (string-prefix-p scope (file-truename file))) files))))

(defun obsidian--find-note (name)
  "Find note NAME, preferring the current scope."
  (let* ((clean (string-trim name))
         (relative (concat clean "." obsidian-file-extension))
         (scope-hit (and obsidian--current-scope
                         (expand-file-name relative obsidian--current-scope)))
         (vault-hit (expand-file-name relative obsidian--vault)))
    (cond ((and scope-hit (file-regular-p scope-hit)) scope-hit)
          ((file-regular-p vault-hit) vault-hit)
          (t (cl-find clean (obsidian--all-note-files)
                      :key #'file-name-base :test #'string=)))))

(defun obsidian--build-edges (files)
  "Read wiki links in FILES and return unique undirected edges."
  (let ((allowed (mapcar #'file-name-base files)) edges)
    (dolist (file files)
      (when (obsidian--note-file-p file)
        (condition-case nil
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (while (re-search-forward obsidian-link-regexp nil t)
                (let ((target (string-trim
                               (car (split-string
                                     (match-string-no-properties 1) "[|#]")))))
                  (when (member target allowed)
                    (let* ((source (file-name-base file))
                           (edge (if (string-lessp source target)
                                     (cons source target)
                                   (cons target source))))
                      (unless (string= source target) (push edge edges)))))))
          (file-error nil))))
    (delete-dups edges)))

(defun obsidian--connected-nodes (node edges)
  "Return nodes adjacent to NODE in EDGES."
  (delete-dups
   (delq nil
         (mapcar (lambda (edge)
                   (cond ((equal node (car edge)) (cdr edge))
                         ((equal node (cdr edge)) (car edge))))
                 edges))))

;;; Force-directed layout

(defun obsidian--force-layout (nodes edges width height)
  "Lay out NODES on a virtual WIDTH by HEIGHT canvas."
  (let* ((count (max 1 (length nodes)))
         (cx (/ width 2.0)) (cy (/ height 2.0))
         (natural (max 8.0 (sqrt (/ (* width height) count))))
         (positions (make-hash-table :test #'equal))
         (velocities (make-hash-table :test #'equal)))
    ;; A deterministic circle avoids the graph jumping on every refresh.
    (cl-loop for node in (sort (copy-sequence nodes) #'string-lessp)
             for i from 0
             for angle = (* 2.0 float-pi (/ i (float count)))
             do (puthash node
                         (cons (+ cx (* natural (cos angle)))
                               (+ cy (* 0.55 natural (sin angle)))) positions)
             do (puthash node (cons 0.0 0.0) velocities))
    (dotimes (_ 220)
      (let ((forces (make-hash-table :test #'equal)))
        (dolist (node nodes) (puthash node (cons 0.0 0.0) forces))
        ;; Coulomb repulsion.
        (cl-loop for tail on nodes do
                 (dolist (b (cdr tail))
                   (let* ((a (car tail)) (pa (gethash a positions))
                          (pb (gethash b positions))
                          (dx (- (car pa) (car pb))) (dy (- (cdr pa) (cdr pb)))
                          (d2 (max 1.0 (+ (* dx dx) (* dy dy))))
                          (d (sqrt d2)) (magnitude (/ (* natural natural) d2))
                          (fx (* magnitude (/ dx d)))
                          (fy (* magnitude (/ dy d)))
                          (fa (gethash a forces)) (fb (gethash b forces)))
                     (cl-incf (car fa) fx) (cl-incf (cdr fa) fy)
                     (cl-decf (car fb) fx) (cl-decf (cdr fb) fy))))
        ;; Hooke attraction.
        (dolist (edge edges)
          (let* ((a (car edge)) (b (cdr edge))
                 (pa (gethash a positions)) (pb (gethash b positions)))
            (when (and pa pb)
              (let* ((dx (- (car pb) (car pa))) (dy (- (cdr pb) (cdr pa)))
                     (d (max 1.0 (sqrt (+ (* dx dx) (* dy dy)))))
                     (magnitude (* 0.16 (- d natural)))
                     (fx (* magnitude (/ dx d))) (fy (* magnitude (/ dy d)))
                     (fa (gethash a forces)) (fb (gethash b forces)))
                (cl-incf (car fa) fx) (cl-incf (cdr fa) fy)
                (cl-decf (car fb) fx) (cl-decf (cdr fb) fy)))))
        ;; Gravity, velocity, friction.
        (dolist (node nodes)
          (let* ((p (gethash node positions)) (v (gethash node velocities))
                 (f (gethash node forces))
                 (fx (+ (car f) (* 0.02 (- cx (car p)))))
                 (fy (+ (cdr f) (* 0.02 (- cy (cdr p)))))
                 (vx (* 0.86 (+ (car v) (* fx 0.12))))
                 (vy (* 0.86 (+ (cdr v) (* fy 0.12)))))
            (puthash node (cons vx vy) velocities)
            (puthash node (cons (+ (car p) (* vx 0.12))
                                (+ (cdr p) (* vy 0.12))) positions)))))
    (obsidian--quantize-positions nodes positions width height)))

(defun obsidian--quantize-positions (nodes positions width height)
  "Scale POSITIONS and prevent labels from visually crowding one another."
  (let* ((values (mapcar (lambda (node) (gethash node positions)) nodes))
         (xmin (apply #'min (mapcar #'car values)))
         (xmax (apply #'max (mapcar #'car values)))
         (ymin (apply #'min (mapcar #'cdr values)))
         (ymax (apply #'max (mapcar #'cdr values)))
         (margin-x 2) (margin-y 2) occupied result)
    (dolist (node nodes)
      (let* ((p (gethash node positions))
             ;; Obsidian hides the Markdown extension in graph labels.
             (label-width (+ 2 (string-width node)))
             (usable-x (max 1 (- width label-width (* 2 margin-x))))
             (usable-y (max 1 (- height (* 2 margin-y))))
             (x (+ margin-x
                   (round (* (/ (- (car p) xmin) (max 0.01 (- xmax xmin)))
                             usable-x))))
             (y (+ margin-y
                   (round (* (/ (- (cdr p) ymin) (max 0.01 (- ymax ymin)))
                             usable-y))))
             (radius 0) found)
        (while (not found)
          (cl-loop for dy from (- radius) to radius until found do
                   (cl-loop for dx from (- radius) to radius until found do
                            (let ((nx (max margin-x
                                           (min (- width label-width margin-x)
                                                (+ x dx))))
                                  (ny (max margin-y
                                           (min (- height margin-y 1)
                                                (+ y dy)))))
                              (unless
                                  (cl-some
                                   (lambda (cell)
                                     (and (< (abs (- ny (cadr cell))) 3)
                                          (< (abs (- nx (car cell)))
                                             (+ 4 (max label-width
                                                       (caddr cell))))))
                                   occupied)
                                (push (list nx ny label-width) occupied)
                                (push (cons node (cons nx ny)) result)
                                (setq found t)))))
          (cl-incf radius))))
    result))

;;; Canvas rendering

(defun obsidian--line-cells (x0 y0 x1 y1)
  "Return Bresenham cells from X0,Y0 to X1,Y1."
  (let ((dx (abs (- x1 x0))) (sx (if (< x0 x1) 1 -1))
        (dy (- (abs (- y1 y0)))) (sy (if (< y0 y1) 1 -1))
        (err 0) cells done)
    (setq err (+ dx dy))
    (while (not done)
      (push (cons x0 y0) cells)
      (if (and (= x0 x1) (= y0 y1))
          (setq done t)
        (let ((twice (* 2 err)))
          (when (>= twice dy) (cl-incf err dy) (cl-incf x0 sx))
          (when (<= twice dx) (cl-incf err dx) (cl-incf y0 sy)))))
    (nreverse cells)))

(defun obsidian--edge-character (dx dy)
  "Choose a Unicode line character for vector DX,DY."
  (cond ((> (abs dx) (* 2 (abs dy))) ?─)
        ((> (abs dy) (* 2 (abs dx))) ?│)
        ((> (* dx dy) 0) ?╲)
        (t ?╱)))

(defun obsidian--render-canvas (nodes edges positions width height current)
  "Build a large, readable and clickable graph canvas."
  (let ((grid (make-vector height nil))
        (blocked (make-hash-table :test #'equal))
        (connected (obsidian--connected-nodes current edges)))
    (dotimes (row height) (aset grid row (make-vector width ?\s)))
    ;; Protect only label text.  The marker stays available as an edge anchor,
    ;; so connections visibly reach ●/◆ instead of stopping several cells away.
    (dolist (node nodes)
      (let* ((p (cdr (assoc node positions))) (x (car p)) (y (cdr p))
             (label (format "%s %s" (if (equal node current) "◆" "●") node)))
        (cl-loop for xx from (+ x 2)
                 to (min (1- width) (+ x (string-width label))) do
                 (puthash (cons xx y) t blocked))))
    ;; Edges are the lower layer.  Crossings become a single clear cross.
    (dolist (edge edges)
      (let* ((a (cdr (assoc (car edge) positions)))
             (b (cdr (assoc (cdr edge) positions))))
        (when (and a b)
          (let ((char (obsidian--edge-character (- (car b) (car a))
                                                (- (cdr b) (cdr a)))))
            (dolist (cell (cdr (butlast (obsidian--line-cells
                                         (car a) (cdr a) (car b) (cdr b)))))
              (unless (gethash cell blocked)
                (let* ((row (aref grid (cdr cell)))
                       (old (aref row (car cell))))
                  (aset row (car cell)
                        (if (or (= old ?\s) (= old char)) char ?┼)))))))))
    (let ((canvas (make-vector height nil)))
      (dotimes (row height)
        (aset canvas row (concat (append (aref grid row) nil))))
      ;; Labels are always the top layer.
      (dolist (node nodes)
        (let* ((p (cdr (assoc node positions))) (x (car p)) (y (cdr p))
               (label (format "%s %s" (if (equal node current) "◆" "●") node))
               (face (cond ((equal node current) 'obsidian-graph-current)
                           ((member node connected) 'obsidian-graph-connected)
                           ((cl-some (lambda (edge)
                                       (or (equal node (car edge))
                                           (equal node (cdr edge)))) edges)
                            'obsidian-graph-edge)
                           (t 'obsidian-graph-unlinked)))
               (line (aref canvas y)) (end (min width (+ x (length label))))
               (visible (substring label 0 (- end x))))
          (setq line (concat (substring line 0 x) visible (substring line end)))
          (add-text-properties
           x end `(face ,face mouse-face highlight help-echo "Open this note"
                        obsidian-node ,node keymap ,obsidian-graph-mode-map) line)
          (aset canvas y line)))
      canvas)))

(defun obsidian--canvas-as-string (canvas)
  "Convert CANVAS into a propertized multiline string."
  (mapconcat #'identity (append canvas nil) "\n"))

(defun obsidian--render-graph (nodes edges positions width height current)
  "Return a complete graph as text (also used by tests)."
  (obsidian--canvas-as-string
   (obsidian--render-canvas nodes edges positions width height current)))

;;; Camera and viewport

(defun obsidian--graph-viewport-size ()
  "Return usable graph viewport size."
  (let ((window (get-buffer-window (current-buffer))))
    (cons (max 20 (if window (window-body-width window) obsidian-graph-width))
          (max 6 (- (if window (window-body-height window) 24) 2)))))

(defun obsidian--graph-clamp-camera ()
  "Keep camera coordinates inside the virtual canvas."
  (let* ((view (obsidian--graph-viewport-size))
         (max-x (max 0 (- obsidian--graph-canvas-width (car view))))
         (max-y (max 0 (- obsidian--graph-canvas-height (cdr view)))))
    (setq obsidian--graph-camera-x
          (max 0 (min max-x obsidian--graph-camera-x))
          obsidian--graph-camera-y
          (max 0 (min max-y obsidian--graph-camera-y)))))

(defun obsidian--graph-draw-view ()
  "Render only the camera rectangle of the virtual canvas."
  (when obsidian--graph-canvas
    (obsidian--graph-clamp-camera)
    (let* ((inhibit-read-only t) (view (obsidian--graph-viewport-size))
           (scope (file-relative-name obsidian--current-scope obsidian--vault)))
      (erase-buffer)
      (insert (propertize
               (format "Graph: %s  view(%d,%d)  arrows: move  0: center\n"
                       (if (equal scope "./") "vault root" scope)
                       obsidian--graph-camera-x obsidian--graph-camera-y)
               'face 'bold))
      (dotimes (row (cdr view))
        (let ((source-row (+ obsidian--graph-camera-y row)))
          (if (>= source-row obsidian--graph-canvas-height)
              (insert "\n")
            (let* ((line (aref obsidian--graph-canvas source-row))
                   (start (min obsidian--graph-camera-x (length line)))
                   (end (min (length line) (+ start (car view)))))
              (insert (substring line start end) "\n")))))
      (goto-char (point-min))
      (set-buffer-modified-p nil))))

(defun obsidian--graph-center-camera ()
  "Center the camera on the active node or canvas center."
  (interactive)
  (let* ((view (obsidian--graph-viewport-size))
         (target (or obsidian--graph-current-position
                     (cons (/ obsidian--graph-canvas-width 2)
                           (/ obsidian--graph-canvas-height 2)))))
    (setq obsidian--graph-camera-x (- (car target) (/ (car view) 2))
          obsidian--graph-camera-y (- (cdr target) (/ (cdr view) 2)))
    (obsidian--graph-clamp-camera)
    (obsidian--graph-draw-view)))

(defun obsidian--graph-pan (dx dy)
  "Move the map camera by DX,DY, without moving a text cursor."
  (setq obsidian--graph-camera-x (+ obsidian--graph-camera-x dx)
        obsidian--graph-camera-y (+ obsidian--graph-camera-y dy))
  (obsidian--graph-clamp-camera)
  (obsidian--graph-draw-view))

(defun obsidian--graph-move-up () (interactive) (obsidian--graph-pan 0 -2))
(defun obsidian--graph-move-down () (interactive) (obsidian--graph-pan 0 2))
(defun obsidian--graph-move-left () (interactive) (obsidian--graph-pan -4 0))
(defun obsidian--graph-move-right () (interactive) (obsidian--graph-pan 4 0))

;;; Refresh and interaction

(defun obsidian-refresh-graph ()
  "Rebuild a spacious virtual canvas and center the camera."
  (interactive)
  (when-let ((buffer (get-buffer obsidian-graph-buffer-name)))
    (with-current-buffer buffer
      (let* ((files (seq-take (obsidian--notes-in-current-scope)
                              obsidian-graph-max-nodes))
             (nodes (delete-dups (mapcar #'file-name-base files)))
             (edges (obsidian--build-edges files))
             (view (obsidian--graph-viewport-size))
             ;; Roughly two viewports: enough whitespace to remove crowding,
             ;; while keeping a useful neighborhood visible on first open.
             (compact (<= (length nodes) 12))
             ;; Normal vault folders fit completely in the visible panel.
             ;; Only genuinely large graphs receive a pannable canvas.
             (width (if compact
                        (car view)
                      (max (car view) (* 7 (length nodes)))))
             (height (if compact
                         (cdr view)
                       (max (cdr view) (* 3 (length nodes)))))
             (current (and obsidian--current-file
                           (file-name-base obsidian--current-file))))
        (setq obsidian--graph-points nil)
        (if nodes
            (let ((positions (obsidian--force-layout nodes edges width height)))
              (setq obsidian--graph-canvas-width width
                    obsidian--graph-canvas-height height
                    obsidian--graph-current-position (cdr (assoc current positions))
                    obsidian--graph-canvas
                    (obsidian--render-canvas nodes edges positions
                                             width height current))
              (obsidian--graph-center-camera))
          (setq obsidian--graph-canvas-width (car view)
                obsidian--graph-canvas-height (cdr view)
                obsidian--graph-current-position nil
                obsidian--graph-camera-x 0 obsidian--graph-camera-y 0
                obsidian--graph-canvas
                (vconcat (list "  No notes in this scope. Select a folder on the left.")
                         (make-list (max 0 (1- (cdr view))) "")))
          (obsidian--graph-draw-view))))))

(defun obsidian--graph-node-at (position)
  "Return graph node at or immediately before POSITION."
  (or (get-text-property position 'obsidian-node)
      (and (> position (point-min))
           (get-text-property (1- position) 'obsidian-node))))

(defun obsidian--graph-open-node (node)
  "Open NODE in the central editor."
  (if-let ((file (and node (obsidian--find-note node))))
      (obsidian--open-note file)
    (user-error "No note at this position")))

(defun obsidian--graph-open-at-point ()
  "Open the graph node at point."
  (interactive)
  (obsidian--graph-open-node (obsidian--graph-node-at (point))))

(defun obsidian--graph-mouse-open (event)
  "Open the graph node clicked in EVENT."
  (interactive "e")
  (with-current-buffer obsidian-graph-buffer-name
    (let ((position (posn-point (event-end event))))
      (when (integer-or-marker-p position)
        (obsidian--graph-open-node (obsidian--graph-node-at position))))))

(defun obsidian--schedule-graph-update ()
  "Refresh the graph shortly, coalescing repeated requests."
  (when (timerp obsidian--graph-timer) (cancel-timer obsidian--graph-timer))
  (setq obsidian--graph-timer
        (run-with-idle-timer 0.15 nil #'obsidian-refresh-graph)))

(provide 'obsidian-graph)
;;; obsidian-graph.el ends here
