;;; obsidian-graph.el --- Force-directed text graph view -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function obsidian--open-note "obsidian-editor")
(declare-function obsidian--graph-window "obsidian-windows")

(defcustom obsidian-graph-max-nodes 200
  "Maximum number of nodes drawn in the graph view."
  :type 'integer
  :group 'obsidian)

(defface obsidian-graph-current
  '((t :foreground "red" :weight bold))
  "Face for the current note." :group 'obsidian)
(defface obsidian-graph-connected
  '((t :foreground "DeepSkyBlue" :weight bold))
  "Face for a note connected to the current note." :group 'obsidian)
(defface obsidian-graph-unlinked
  '((t :foreground "gray60"))
  "Face for an unlinked note." :group 'obsidian)
(defface obsidian-graph-edge
  '((t :foreground "gray45"))
  "Face for graph edges and linked notes." :group 'obsidian)

(defvar obsidian--graph-points nil
  "Alist mapping node names to graph buffer positions.")

(define-derived-mode obsidian-graph-mode special-mode "Obsidian-Graph"
  "Major mode for the clickable Obsidian graph panel."
  (setq-local truncate-lines t))

(defun obsidian--all-note-files ()
  "Return all Markdown note files in the vault."
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
  "Return notes below the current scope.
At vault root, only root-level notes are returned so unrelated folders do not
appear together before the user chooses a folder or note."
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
  "Read wiki links in FILES and return unique in-scope edges."
  (let ((allowed (mapcar #'file-name-base files)) edges)
    (dolist (file files)
      ;; A note can disappear between directory scanning and this timer-driven
      ;; read.  Skip it instead of letting an idle timer report an error.
      (when (obsidian--note-file-p file)
        (condition-case nil
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (while (re-search-forward obsidian-link-regexp nil t)
                (let ((target (string-trim
                               (car (split-string
                                     (match-string-no-properties 1)
                                     "[|#]")))))
                  (when (member target allowed)
                    (let* ((source (file-name-base file))
                           (edge (if (string-lessp source target)
                                     (cons source target)
                                   (cons target source))))
                      (unless (string= source target)
                        (push edge edges)))))))
          (file-error nil))))
    (delete-dups edges)))

(defun obsidian--connected-nodes (node edges)
  "Return nodes adjacent to NODE in EDGES."
  (delete-dups
   (delq nil (mapcar (lambda (edge)
                       (cond ((equal node (car edge)) (cdr edge))
                             ((equal node (cdr edge)) (car edge))))
                     edges))))

(defun obsidian--force-layout (nodes edges width height)
  "Lay out NODES inside WIDTH by HEIGHT using a damped force simulation."
  (let* ((count (max 1 (length nodes)))
         (cx (/ width 2.0)) (cy (/ height 2.0))
         (area (max 1.0 (* width height)))
         (natural (max 3.0 (sqrt (/ area count))))
         (positions (make-hash-table :test #'equal))
         (velocities (make-hash-table :test #'equal)))
    (cl-loop for node in (sort (copy-sequence nodes) #'string-lessp)
             for i from 0
             for angle = (* 2.0 float-pi (/ i (float count)))
             do (puthash node
                         (cons (+ cx (* natural (cos angle)))
                               (+ cy (* 0.55 natural (sin angle)))) positions)
             do (puthash node (cons 0.0 0.0) velocities))
    (dotimes (_ 200)
      (let ((forces (make-hash-table :test #'equal)))
        (dolist (node nodes) (puthash node (cons 0.0 0.0) forces))
        ;; Coulomb repulsion: magnitude k/d^2.
        (cl-loop for tail on nodes do
                 (dolist (b (cdr tail))
                   (let* ((a (car tail)) (pa (gethash a positions))
                          (pb (gethash b positions))
                          (dx (- (car pa) (car pb)))
                          (dy (- (cdr pa) (cdr pb)))
                          (d2 (max 0.25 (+ (* dx dx) (* dy dy))))
                          (d (sqrt d2)) (mag (/ (* natural natural) d2))
                          (fx (* mag (/ dx d))) (fy (* mag (/ dy d)))
                          (fa (gethash a forces)) (fb (gethash b forces)))
                     (setcar fa (+ (car fa) fx)) (setcdr fa (+ (cdr fa) fy))
                     (setcar fb (- (car fb) fx)) (setcdr fb (- (cdr fb) fy)))))
        ;; Hooke attraction on edges.
        (dolist (edge edges)
          (let* ((a (car edge)) (b (cdr edge))
                 (pa (gethash a positions)) (pb (gethash b positions)))
            (when (and pa pb)
              (let* ((dx (- (car pb) (car pa))) (dy (- (cdr pb) (cdr pa)))
                     (d (max 0.5 (sqrt (+ (* dx dx) (* dy dy)))))
                     (mag (* 0.18 (- d natural)))
                     (fx (* mag (/ dx d))) (fy (* mag (/ dy d)))
                     (fa (gethash a forces)) (fb (gethash b forces)))
                (setcar fa (+ (car fa) fx)) (setcdr fa (+ (cdr fa) fy))
                (setcar fb (- (car fb) fx)) (setcdr fb (- (cdr fb) fy))))))
        ;; Weak gravity, velocity integration, and friction.
        (dolist (node nodes)
          (let* ((p (gethash node positions)) (v (gethash node velocities))
                 (f (gethash node forces))
                 (fx (+ (car f) (* 0.025 (- cx (car p)))))
                 (fy (+ (cdr f) (* 0.025 (- cy (cdr p)))))
                 (vx (* 0.86 (+ (car v) (* fx 0.12))))
                 (vy (* 0.86 (+ (cdr v) (* fy 0.12)))))
            (puthash node (cons vx vy) velocities)
            (puthash node
                     (cons (max 1.0 (min (- width 3.0) (+ (car p) (* vx 0.12))))
                           (max 1.0 (min (- height 2.0) (+ (cdr p) (* vy 0.12)))))
                     positions)))))
    (obsidian--quantize-positions nodes positions width height)))

(defun obsidian--quantize-positions (nodes positions width height)
  "Scale POSITIONS to integer cells and keep node labels apart."
  (let* ((values (mapcar (lambda (n) (gethash n positions)) nodes))
         (xmin (apply #'min (mapcar #'car values)))
         (xmax (apply #'max (mapcar #'car values)))
         (ymin (apply #'min (mapcar #'cdr values)))
         (ymax (apply #'max (mapcar #'cdr values)))
         (usable-x (max 1 (- width 5))) (usable-y (max 1 (- height 3)))
         occupied result)
    (dolist (node nodes)
      (let* ((p (gethash node positions))
             (x (+ 1 (round (* (/ (- (car p) xmin) (max 0.01 (- xmax xmin))) usable-x))))
             (y (+ 1 (round (* (/ (- (cdr p) ymin) (max 0.01 (- ymax ymin))) usable-y))))
             ;; "o[NAME.md]" occupies NAME width plus six characters.
             (label-width (+ 6 (string-width node))) (radius 0) found)
        (while (not found)
          (cl-loop for dy from (- radius) to radius until found do
                   (cl-loop for dx from (- radius) to radius until found do
                            (let ((nx (max 0 (min (- width label-width) (+ x dx))))
                                  (ny (max 0 (min (1- height) (+ y dy)))))
                              (unless (cl-some
                                       (lambda (cell)
                                         (and (= ny (cadr cell))
                                              (< (abs (- nx (car cell)))
                                                 (1+ (max label-width (caddr cell))))))
                                       occupied)
                                (push (list nx ny label-width) occupied)
                                (push (cons node (cons nx ny)) result)
                                (setq found t)))))
          (cl-incf radius))))
    result))

(defun obsidian--line-cells (x0 y0 x1 y1)
  "Return Bresenham cells between two points, including endpoints."
  (let ((dx (abs (- x1 x0))) (sx (if (< x0 x1) 1 -1))
        (dy (- (abs (- y1 y0)))) (sy (if (< y0 y1) 1 -1))
        (err 0) cells done)
    (setq err (+ dx dy))
    (while (not done)
      (push (cons x0 y0) cells)
      (if (and (= x0 x1) (= y0 y1))
          (setq done t)
        (let ((e2 (* 2 err)))
          (when (>= e2 dy) (setq err (+ err dy) x0 (+ x0 sx)))
          (when (<= e2 dx) (setq err (+ err dx) y0 (+ y0 sy))))))
    (nreverse cells)))

(defun obsidian--render-graph (nodes edges positions width height current)
  "Return a propertized text graph and record clickable NODES."
  (let ((grid (make-vector height nil)) (blocked (make-hash-table :test #'equal))
        (connected (obsidian--connected-nodes current edges)))
    (dotimes (row height) (aset grid row (make-vector width ?\s)))
    ;; Reserve every complete label before drawing edges.
    (dolist (node nodes)
      (let* ((p (cdr (assoc node positions))) (x (car p)) (y (cdr p))
             (text (format "o[%s.md]" node)))
        (dotimes (i (min (length text) (- width x)))
          (puthash (cons (+ x i) y) t blocked))))
    (dolist (edge edges)
      (let* ((a (cdr (assoc (car edge) positions)))
             (b (cdr (assoc (cdr edge) positions))))
        (when (and a b)
          (let* ((dx (- (car b) (car a))) (dy (- (cdr b) (cdr a)))
                 (char (cond ((> (abs dx) (* 2 (abs dy))) ?-)
                             ((> (abs dy) (* 2 (abs dx))) ?|)
                             ((> (* dx dy) 0) ?\\) (t ?/))))
            (dolist (cell (cdr (butlast (obsidian--line-cells
                                         (car a) (cdr a) (car b) (cdr b)))))
              (unless (gethash cell blocked)
                (aset (aref grid (cdr cell)) (car cell) char)))))))
    ;; Labels are the final layer and therefore cannot be overwritten.
    (dolist (node nodes)
      (let* ((p (cdr (assoc node positions))) (x (car p)) (y (cdr p))
             (text (format "%s[%s.md]" (if (equal node current) "*" "o") node)))
        (dotimes (i (min (length text) (- width x)))
          (aset (aref grid y) (+ x i) (aref text i)))))
    (let ((start 1) (output ""))
      (dotimes (row height)
        (setq output (concat output (concat (append (aref grid row) nil)) "\n")))
      ;; Add properties by searching the completed text, not by grid columns.
      (dolist (node nodes)
        (let* ((needle (format "[%s.md]" node))
               (at (string-match (regexp-quote needle) output start))
               (face (cond ((equal node current) 'obsidian-graph-current)
                           ((member node connected) 'obsidian-graph-connected)
                           ((cl-some (lambda (edge)
                                       (or (equal node (car edge))
                                           (equal node (cdr edge))))
                                     edges)
                            'obsidian-graph-edge)
                           (t 'obsidian-graph-unlinked))))
          (when at
            (add-text-properties at (+ at (length needle))
                                 `(face ,face mouse-face highlight
                                        help-echo "Open this note"
                                        obsidian-node ,node
                                        keymap ,obsidian-graph-mode-map)
                                 output)
            (push (cons node (+ at 1)) obsidian--graph-points))))
      output)))

(defun obsidian-refresh-graph ()
  "Rebuild the graph for the current directory scope."
  (interactive)
  (when-let ((buffer (get-buffer obsidian-graph-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (files (seq-take (obsidian--notes-in-current-scope)
                             obsidian-graph-max-nodes)))
        (erase-buffer)
        (setq obsidian--graph-points nil)
        (let* ((scope (file-relative-name obsidian--current-scope obsidian--vault))
               (nodes (delete-dups (mapcar #'file-name-base files)))
               (edges (obsidian--build-edges files))
               (win (get-buffer-window buffer))
               (width (max 20 (if win (window-body-width win) obsidian-graph-width)))
               (height (max 8 (1- (if win (window-body-height win) 24))))
               (current (and obsidian--current-file
                             (file-name-base obsidian--current-file))))
          (insert (propertize (format "Graph: %s\n" (if (equal scope "./") "vault root" scope))
                              'face 'bold))
          (if nodes
              (insert (obsidian--render-graph
                       nodes edges (obsidian--force-layout nodes edges width height)
                       width height current))
            (insert "\n  No notes in this scope. Select a folder or file on the left.\n")))
        (goto-char (point-min))
        (set-buffer-modified-p nil)))))

(defun obsidian--graph-node-at (position)
  "Return the graph node at or immediately before POSITION."
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

(defun obsidian--graph-move-up () (interactive) (forward-line -1))
(defun obsidian--graph-move-down () (interactive) (forward-line 1))
(defun obsidian--graph-move-left () (interactive) (backward-char 1))
(defun obsidian--graph-move-right () (interactive) (forward-char 1))

(defun obsidian--schedule-graph-update ()
  "Refresh the graph shortly, coalescing repeated requests."
  (when (timerp obsidian--graph-timer) (cancel-timer obsidian--graph-timer))
  (setq obsidian--graph-timer
        (run-with-idle-timer 0.15 nil #'obsidian-refresh-graph)))

(provide 'obsidian-graph)
;;; obsidian-graph.el ends here
