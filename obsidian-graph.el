;;; obsidian-graph.el --- Interactive pannable & zoomable terminal graph view -*- lexical-binding: t; -*-

;;; Commentary:
;; Builds a deterministic force-directed layout, renders edges using either
;; rounded + heavy active Unicode box-drawing characters or high-resolution
;; 2x4 Braille sub-cell lines, and supports mouse grab-panning, wheel zoom,
;; clickable header controls, 2-hop local scope, and TAB node navigation.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar obsidian--vault)
(defvar obsidian--current-file)
(defvar obsidian--current-scope)
(defvar obsidian--graph-timer)
(defvar obsidian-file-extension)
(defvar obsidian-link-regexp)
(defvar obsidian-graph-width)
(defvar obsidian-graph-buffer-name)
(defvar obsidian-show-header-buttons)

(declare-function obsidian--open-note "obsidian-editor")
(declare-function obsidian--note-file-p "obsidian-tree")
(declare-function obsidian--make-header-button "obsidian-windows")
(declare-function obsidian-shrink-graph "obsidian-windows")
(declare-function obsidian-enlarge-graph "obsidian-windows")
(declare-function obsidian-toggle-graph "obsidian-windows")

(defcustom obsidian-graph-max-nodes 200
  "Maximum number of nodes drawn in the graph view."
  :type 'integer :group 'obsidian)

(defcustom obsidian-graph-cluster-column-spacing 10
  "Approximate horizontal spacing used by compact graph clusters."
  :type 'integer :group 'obsidian)

(defcustom obsidian-graph-cluster-row-spacing 4
  "Approximate vertical spacing used by compact graph clusters."
  :type 'integer :group 'obsidian)

(defcustom obsidian-graph-label-max-characters 8
  "Maximum number of note-name characters shown in a graph node."
  :type 'integer :group 'obsidian)

(defcustom obsidian-graph-label-marquee-interval 0.45
  "Seconds between scrolling steps for long graph node names.
Set this to nil to disable animated graph labels."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'obsidian)

(defcustom obsidian-graph-label-marquee-gap " "
  "Text separating the end and beginning of a scrolling graph label."
  :type 'string :group 'obsidian)

(defcustom obsidian-graph-edge-style 'box
  "Visual rendering style for graph edges in terminal and GUI frames.
`box' draws rounded orthogonal lines (`╭╮╰╯') with bold highlighted active
connections (`┏┓┗┛━┃').  `braille' draws smooth 2x4 sub-cell diagonal lines
using Unicode Braille patterns (`⠀'–`⣿')."
  :type '(choice (const :tag "Rounded + bold box-drawing" box)
                 (const :tag "High-resolution Braille patterns" braille))
  :group 'obsidian)

(defcustom obsidian-graph-scope-mode 'auto
  "Which notes to include in the graph view.
`auto' shows the active folder, falling back to the whole vault when at an
empty vault root.  `local' shows a 2-hop cross-folder neighborhood around the
current note.  `folder' restricts strictly to the current folder.  `global'
shows all notes in the vault."
  :type '(choice (const :tag "Auto (folder or vault fallback)" auto)
                 (const :tag "Local 2-hop neighborhood" local)
                 (const :tag "Current folder only" folder)
                 (const :tag "Entire vault" global))
  :group 'obsidian)


;; Adaptive graph faces

(defface obsidian-graph-current
  '((((class color) (background dark)) :foreground "#ff6b6b" :weight bold)
    (((class color) (background light)) :foreground "#cf222e" :weight bold)
    (t :foreground "#ff5f5f" :weight bold))
  "Face for the current note." :group 'obsidian)

(defface obsidian-graph-focused
  '((((class color) (background dark)) :foreground "#e5c07b" :weight bold :underline t)
    (((class color) (background light)) :foreground "#9a6700" :weight bold :underline t)
    (t :weight bold :underline t))
  "Face for the keyboard-focused node (`TAB' cycling)." :group 'obsidian)

(defface obsidian-graph-connected
  '((((class color) (background dark)) :foreground "#61afef" :weight bold)
    (((class color) (background light)) :foreground "#0969da" :weight bold)
    (t :foreground "#5fafff" :weight bold))
  "Face for notes connected to the current note." :group 'obsidian)

(defface obsidian-graph-node
  '((((class color) (background dark)) :foreground "#abb2bf")
    (((class color) (background light)) :foreground "#24292f")
    (t :inherit default))
  "Face for linked background notes." :group 'obsidian)

(defface obsidian-graph-unlinked
  '((((class color) (background dark)) :foreground "#5c6370")
    (((class color) (background light)) :foreground "#8c959f")
    (t :foreground "gray55"))
  "Face for unlinked notes." :group 'obsidian)

(defface obsidian-graph-edge
  '((((class color) (background dark)) :foreground "#4b5263")
    (((class color) (background light)) :foreground "#afb8c1")
    (t :foreground "gray65"))
  "Face for background graph edges." :group 'obsidian)

(defface obsidian-graph-edge-active
  '((((class color) (background dark)) :foreground "#61afef" :weight bold)
    (((class color) (background light)) :foreground "#0969da" :weight bold)
    (t :foreground "#5fafff" :weight bold))
  "Face for edges directly connected to the active or focused note."
  :group 'obsidian)


;; State variables

(defvar obsidian--graph-points nil)
(defvar-local obsidian--graph-camera-x 0)
(defvar-local obsidian--graph-camera-y 0)
(defvar-local obsidian--graph-zoom 1.0)
(defvar-local obsidian--graph-canvas nil)
(defvar-local obsidian--graph-canvas-width 0)
(defvar-local obsidian--graph-canvas-height 0)
(defvar-local obsidian--graph-current-position nil)
(defvar-local obsidian--graph-node-positions nil
  "Alist of `(NODE . (X . Y))' in the rendered canvas.")
(defvar-local obsidian--graph-ordered-nodes nil
  "Ordered list of nodes available for `TAB' focus cycling.")
(defvar-local obsidian--graph-focused-node nil
  "Node name currently highlighted via `TAB' keyboard navigation.")
(defvar-local obsidian--graph-marquee-timer nil
  "Timer used to animate long graph node names.")
(defvar-local obsidian--graph-marquee-step 0
  "Current character offset of graph label marquees.")

(define-derived-mode obsidian-graph-mode special-mode "Obsidian-Graph"
  "Major mode for a clickable, mouse-pannable, zoomable terminal graph."
  (setq-local truncate-lines t)
  ;; The arrows control a camera, not a text cursor.
  (setq-local cursor-type nil)
  (setq-local header-line-format '(:eval (obsidian--graph-header-line)))
  (add-hook 'kill-buffer-hook #'obsidian--graph-stop-marquee nil t))

(defun obsidian--graph-scope-label ()
  "Return a short Japanese label for `obsidian-graph-scope-mode'."
  (pcase obsidian-graph-scope-mode
    ('local "2-hop")
    ('folder "フォルダ")
    ('global "全体")
    (_ "自動")))

(defun obsidian--graph-header-line ()
  "Build the clickable header toolbar for the graph window."
  (let* ((scope-rel (if (and obsidian--current-scope obsidian--vault)
                        (file-relative-name obsidian--current-scope obsidian--vault)
                      "./"))
         (folder-name (if (equal scope-rel "./")
                          "root"
                        (directory-file-name scope-rel)))
         (title (propertize (format " ◆ %s " folder-name)
                            'face 'obsidian-header-title))
         (style-label (if (eq obsidian-graph-edge-style 'braille)
                          "[線:点字]"
                        "[線:罫線]")))
    (if (not obsidian-show-header-buttons)
        title
      (concat
       title
       (obsidian--make-header-button
        (format "[範囲:%s]" (obsidian--graph-scope-label))
        #'obsidian-graph-cycle-scope
        "表示範囲を切替: 自動 → 2-hop近傍 → フォルダ → Vault全体 (f)"
        (not (eq obsidian-graph-scope-mode 'auto)))
       " "
       (obsidian--make-header-button
        style-label
        #'obsidian-graph-toggle-style
        "線の描画方式を切替: 角丸＋太線罫線 / 点字高精細ライン (b)"
        (eq obsidian-graph-edge-style 'braille))
       " "
       (obsidian--make-header-button "[-]" #'obsidian-graph-zoom-out
                                     "グラフを縮小 (- / ホイール下)")
       (obsidian--make-header-button "[+]" #'obsidian-graph-zoom-in
                                     "グラフを拡大 (+ / ホイール上)")
       (obsidian--make-header-button "[中央]" #'obsidian--graph-center-camera
                                     "現在ノートを中央へ戻す (0)")
       " "
       (obsidian--make-header-button "[◀]" #'obsidian-shrink-graph
                                     "グラフ幅を狭くする (<)")
       (obsidian--make-header-button "[▶]" #'obsidian-enlarge-graph
                                     "グラフ幅を広くする (>)")
       " "
       (obsidian--make-header-button "[×]" #'obsidian-toggle-graph
                                     "グラフパネルを隠す (q)")))))


;;; Notes, scopes, and edges

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
  "Return notes strictly in the selected directory scope."
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

(defun obsidian--local-neighborhood-files (center-file max-hops)
  "Return files within MAX-HOPS wiki-link steps of CENTER-FILE across the vault."
  (let* ((all-files (obsidian--all-note-files))
         (edges (obsidian--build-edges all-files))
         (center (and center-file (file-name-base center-file)))
         (visited (make-hash-table :test #'equal))
         (frontier (and center (list center))))
    (when center
      (puthash center t visited)
      (dotimes (_ max-hops)
        (let (next-frontier)
          (dolist (node frontier)
            (dolist (nbr (obsidian--connected-nodes node edges))
              (unless (gethash nbr visited)
                (puthash nbr t visited)
                (push nbr next-frontier))))
          (setq frontier next-frontier))))
    (if (> (hash-table-count visited) 0)
        (cl-remove-if-not
         (lambda (f) (gethash (file-name-base f) visited))
         all-files)
      all-files)))

(defun obsidian--graph-effective-files ()
  "Return note files to display according to `obsidian-graph-scope-mode'."
  (pcase obsidian-graph-scope-mode
    ('global (obsidian--all-note-files))
    ('folder (obsidian--notes-in-current-scope))
    ('local (if obsidian--current-file
                (obsidian--local-neighborhood-files obsidian--current-file 2)
              (or (obsidian--notes-in-current-scope)
                  (obsidian--all-note-files))))
    (_ ;; `auto': prefer folder scope, but fall back to all vault notes if empty!
     (or (obsidian--notes-in-current-scope)
         (obsidian--all-note-files)))))

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
  "Lay out NODES and EDGES on a virtual WIDTH by HEIGHT canvas."
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
  "Place NODES from POSITIONS in a compact cluster inside WIDTH by HEIGHT."
  (let* ((values (mapcar (lambda (node) (gethash node positions)) nodes))
         (xmin (apply #'min (mapcar #'car values)))
         (xmax (apply #'max (mapcar #'car values)))
         (ymin (apply #'min (mapcar #'cdr values)))
         (ymax (apply #'max (mapcar #'cdr values)))
         (zoom (or obsidian--graph-zoom 1.0))
         (margin-x 2) (margin-y 2)
         (max-label-width
          (apply #'max (mapcar #'obsidian--graph-node-label-width nodes)))
         (available-x (max 1 (- width max-label-width (* 2 margin-x))))
         (available-y (max 1 (- height 1 (* 2 margin-y))))
         (root-count (sqrt (max 1.0 (float (length nodes)))))
         (target-x-span
          (min available-x
               (max 1 (round (* obsidian-graph-cluster-column-spacing
                                zoom
                                root-count)))))
         (target-y-span
          (min available-y
               (max 1 (round (* obsidian-graph-cluster-row-spacing
                                zoom
                                root-count)))))
         (origin-x (+ margin-x (/ (- available-x target-x-span) 2)))
         (origin-y (+ margin-y (/ (- available-y target-y-span) 2)))
         occupied result)
    (dolist (node nodes)
      (let* ((p (gethash node positions))
             (label-width (obsidian--graph-node-label-width node))
             (x (+ origin-x
                   (round (* (/ (- (car p) xmin) (max 0.01 (- xmax xmin)))
                             target-x-span))))
             (y (+ origin-y
                   (round (* (/ (- (cdr p) ymin) (max 0.01 (- ymax ymin)))
                             target-y-span))))
             (min-x margin-x)
             (max-x (max min-x (- width label-width margin-x)))
             (min-y margin-y)
             (max-y (max min-y (- height margin-y 1)))
             (center-x (max min-x (min max-x x)))
             (center-y (max min-y (min max-y y)))
             (search-limit (+ (- max-x min-x) (- max-y min-y)))
             found)
        (cl-loop for radius from 0 to search-limit until found do
                 (cl-loop for dy from (- radius) to radius until found do
                          (let ((dx-distance (- radius (abs dy))))
                            (dolist (dx (if (zerop dx-distance)
                                            '(0)
                                          (list (- dx-distance) dx-distance)))
                              (unless found
                                (let ((nx (+ center-x dx))
                                      (ny (+ center-y dy)))
                                  (when (and (<= min-x nx max-x)
                                             (<= min-y ny max-y)
                                             (not
                                              (cl-some
                                               (lambda (cell)
                                                 (and
                                                  (< (abs (- ny (cadr cell))) 3)
                                                  (< (abs (- nx (car cell)))
                                                     (+ 4
                                                        (max label-width
                                                             (caddr cell))))))
                                               occupied)))
                                    (push (list nx ny label-width) occupied)
                                    (push (cons node (cons nx ny)) result)
                                    (setq found t))))))))
        (unless found
          (push (list center-x center-y label-width) occupied)
          (push (cons node (cons center-x center-y)) result))))
    result))

(defun obsidian--center-positions (positions source-width source-height
                                             target-width target-height)
  "Center POSITIONS from SOURCE-WIDTH by SOURCE-HEIGHT in TARGET-WIDTH by TARGET-HEIGHT."
  (let ((dx (max 0 (/ (- target-width source-width) 2)))
        (dy (max 0 (/ (- target-height source-height) 2))))
    (mapcar (lambda (entry)
              (cons (car entry)
                    (cons (+ dx (cadr entry)) (+ dy (cddr entry)))))
            positions)))


;;; Canvas rendering (Rounded + Heavy Box-Drawing & 2x4 Braille Subpixel)

(defun obsidian--graph-name-window (node step)
  "Return the visible character window of NODE at marquee STEP."
  (let ((limit (max 1 obsidian-graph-label-max-characters)))
    (if (<= (length node) limit)
        node
      (let* ((cycle (concat node obsidian-graph-label-marquee-gap))
             (cycle-length (max 1 (length cycle)))
             (offset (% step cycle-length))
             (source (concat cycle cycle)))
        (substring source offset (+ offset limit))))))

(defun obsidian--graph-name-slot-width (node)
  "Return a stable display width for every marquee window of NODE."
  (if (<= (length node) (max 1 obsidian-graph-label-max-characters))
      (string-width node)
    (let* ((cycle-length
            (length (concat node obsidian-graph-label-marquee-gap)))
           (maximum 1)
           (offset 0))
      (while (< offset cycle-length)
        (setq maximum
              (max maximum
                   (string-width
                    (obsidian--graph-name-window node offset))))
        (cl-incf offset))
      maximum)))

(defun obsidian--graph-padded-name-window (node step)
  "Return NODE's marquee window at STEP padded to its stable slot width."
  (let* ((visible (obsidian--graph-name-window node step))
         (padding (- (obsidian--graph-name-slot-width node)
                     (string-width visible))))
    (concat visible (make-string (max 0 padding) ?\s))))

(defun obsidian--graph-node-label-width (node)
  "Return the fixed display width reserved for NODE and its marker."
  (+ 2 (obsidian--graph-name-slot-width node)))

(defun obsidian--node-label (node current &optional focused)
  "Return the visible label for NODE, marking CURRENT (`◆') and FOCUSED (`◈')."
  (let ((marker (cond ((equal node current) "◆")
                      ((and focused (equal node focused)) "◈")
                      (t "●"))))
    (if (<= (length node) (max 1 obsidian-graph-label-max-characters))
        (format "%s %s" marker node)
      (let* ((slot-width (obsidian--graph-name-slot-width node))
             (placeholder (make-string slot-width ?\s)))
        (add-text-properties
         0 (length placeholder)
         `(obsidian-graph-marquee-name ,node
           display ,(obsidian--graph-padded-name-window
                     node obsidian--graph-marquee-step))
         placeholder)
        (concat marker " " placeholder)))))

(defun obsidian--replace-display-columns (line column replacement)
  "Put REPLACEMENT into LINE at display COLUMN while preserving display width."
  (let* ((available (max 0 (- (string-width line) column)))
         (visible (truncate-string-to-width replacement available nil nil ""))
         (end-column (min (string-width line)
                          (+ column (string-width visible)))))
    (concat (truncate-string-to-width line column nil nil "")
            visible
            (truncate-string-to-width
             line (string-width line) end-column nil ""))))

(defun obsidian--display-column-slice (line start width)
  "Return WIDTH display columns of LINE beginning at display column START."
  (truncate-string-to-width line (+ start width) start nil ""))

;; Four-bit connection mask: north=1, east=2, south=4, west=8.
(defconst obsidian--graph-direction-north 1)
(defconst obsidian--graph-direction-east 2)
(defconst obsidian--graph-direction-south 4)
(defconst obsidian--graph-direction-west 8)

(defun obsidian--integer-range (start end)
  "Return all integers from START to END, in either direction."
  (let ((step (if (<= start end) 1 -1)) values)
    (while (/= start end)
      (push start values)
      (setq start (+ start step)))
    (nreverse (cons end values))))

(defun obsidian--orthogonal-path (from to horizontal-first)
  "Return an L-shaped path from FROM to TO.
When HORIZONTAL-FIRST is non-nil, use the horizontal leg first."
  (let ((x0 (car from)) (y0 (cdr from))
        (x1 (car to)) (y1 (cdr to)))
    (if horizontal-first
        (append (mapcar (lambda (x) (cons x y0))
                        (obsidian--integer-range x0 x1))
                (cdr (mapcar (lambda (y) (cons x1 y))
                             (obsidian--integer-range y0 y1))))
      (append (mapcar (lambda (y) (cons x0 y))
                      (obsidian--integer-range y0 y1))
              (cdr (mapcar (lambda (x) (cons x y1))
                           (obsidian--integer-range x0 x1)))))))

(defun obsidian--path-obstacle-score (path blocked)
  "Score PATH by how many label cells it crosses in BLOCKED."
  (cl-count-if (lambda (cell) (gethash cell blocked))
               (cdr (butlast path))))

(defun obsidian--choose-orthogonal-path (from to blocked)
  "Choose the less obstructed path FROM to TO using BLOCKED cells."
  (let ((horizontal (obsidian--orthogonal-path from to t))
        (vertical (obsidian--orthogonal-path from to nil)))
    (if (<= (obsidian--path-obstacle-score horizontal blocked)
            (obsidian--path-obstacle-score vertical blocked))
        horizontal
      vertical)))

(defun obsidian--segment-directions (from to)
  "Return direction bits for the adjacent cells FROM and TO."
  (cond ((< (car from) (car to))
         (cons obsidian--graph-direction-east obsidian--graph-direction-west))
        ((> (car from) (car to))
         (cons obsidian--graph-direction-west obsidian--graph-direction-east))
        ((< (cdr from) (cdr to))
         (cons obsidian--graph-direction-south obsidian--graph-direction-north))
        (t
         (cons obsidian--graph-direction-north obsidian--graph-direction-south))))

(defun obsidian--add-path-to-masks (path masks)
  "Merge every adjacent segment in PATH into the MASKS grid."
  (cl-loop for tail on path while (cdr tail) do
           (let* ((from (car tail)) (to (cadr tail))
                  (directions (obsidian--segment-directions from to))
                  (from-row (aref masks (cdr from)))
                  (to-row (aref masks (cdr to))))
             (aset from-row (car from)
                   (logior (aref from-row (car from)) (car directions)))
             (aset to-row (car to)
                   (logior (aref to-row (car to)) (cdr directions))))))

(defun obsidian--mask-character (mask &optional heavy-p)
  "Convert four-direction MASK to rounded light or HEAVY-P box-drawing glyph."
  (if heavy-p
      ;; Heavy box-drawing:  ┃ ━ ┗ ┃ ┃ ┏ ┣ ━ ┛ ━ ┻ ┓ ┫ ┳ ╋
      (aref [?\s ?┃ ?━ ?┗ ?┃ ?┃ ?┏ ?┣
             ?━ ?┛ ?━ ?┻ ?┓ ?┫ ?┳ ?╋]
            mask)
    ;; Rounded light box-drawing:  │ ─ ╰ │ │ ╭ ├ ─ ╯ ─ ┴ ╮ ┤ ┬ ┼
    (aref [?\s ?│ ?─ ?╰ ?│ ?│ ?╭ ?├
           ?─ ?╯ ?─ ?┴ ?╮ ?┤ ?┬ ?┼]
          mask)))

(defun obsidian--braille-bit (sub-x sub-y)
  "Return the Unicode Braille bit for SUB-X (0..1) and SUB-Y (0..3)."
  (if (zerop sub-x)
      (aref [#x01 #x02 #x04 #x40] sub-y)
    (aref [#x08 #x10 #x20 #x80] sub-y)))

(defun obsidian--add-braille-edge (from to masks width height blocked)
  "Rasterize a smooth 2x4 sub-cell Braille line FROM to TO into MASKS."
  (let* ((x0 (+ (* (car from) 2) 1))
         (y0 (+ (* (cdr from) 4) 2))
         (x1 (+ (* (car to) 2) 1))
         (y1 (+ (* (cdr to) 4) 2))
         (dx (abs (- x1 x0)))
         (sx (if (< x0 x1) 1 -1))
         (dy (- (abs (- y1 y0))))
         (sy (if (< y0 y1) 1 -1))
         (err (+ dx dy))
         (done nil))
    (while (not done)
      (let ((cx (/ x0 2))
            (cy (/ y0 4)))
        (when (and (<= 0 cx (1- width))
                   (<= 0 cy (1- height))
                   (not (gethash (cons cx cy) blocked)))
          (let ((row-vec (aref masks cy))
                (bit (obsidian--braille-bit (% x0 2) (% y0 4))))
            (aset row-vec cx (logior (aref row-vec cx) bit)))))
      (if (and (= x0 x1) (= y0 y1))
          (setq done t)
        (let ((e2 (* 2 err)))
          (when (>= e2 dy)
            (setq err (+ err dy)
                  x0 (+ x0 sx)))
          (when (<= e2 dx)
            (setq err (+ err dx)
                  y0 (+ y0 sy))))))))

(defun obsidian--render-edge-row (bg-row active-row width braille-p)
  "Build one propertized WIDTH-column string from BG-ROW and ACTIVE-ROW."
  (let ((chunks nil))
    (dotimes (col width)
      (let ((act (aref active-row col))
            (bg (aref bg-row col)))
        (cond
         (braille-p
          (let ((merged (logior act bg)))
            (if (zerop merged)
                (push " " chunks)
              (push (propertize (char-to-string (+ #x2800 merged))
                                'face (if (> act 0)
                                          'obsidian-graph-edge-active
                                        'obsidian-graph-edge))
                    chunks))))
         ((> act 0)
          (push (propertize (char-to-string (obsidian--mask-character act t))
                            'face 'obsidian-graph-edge-active)
                chunks))
         ((> bg 0)
          (push (propertize (char-to-string (obsidian--mask-character bg nil))
                            'face 'obsidian-graph-edge)
                chunks))
         (t
          (push " " chunks)))))
    (apply #'concat (nreverse chunks))))

(defun obsidian--render-canvas (nodes edges positions width height current)
  "Render NODES and EDGES at POSITIONS on a WIDTH by HEIGHT canvas.
CURRENT names the active node."
  (let* ((bg-masks (make-vector height nil))
         (active-masks (make-vector height nil))
         (blocked (make-hash-table :test #'equal))
         (focused obsidian--graph-focused-node)
         (highlight-node (or current focused))
         (connected (obsidian--connected-nodes highlight-node edges))
         (braille-p (eq obsidian-graph-edge-style 'braille)))
    (dotimes (row height)
      (aset bg-masks row (make-vector width 0))
      (aset active-masks row (make-vector width 0)))
    ;; Protect label text while leaving the marker cell open as an edge anchor.
    (dolist (node nodes)
      (let* ((p (cdr (assoc node positions))) (x (car p)) (y (cdr p))
             (label (obsidian--node-label node current focused))
             (text-offset (string-width
                           (substring label 0 (min 2 (length label))))))
        (cl-loop for xx from (+ x text-offset)
                 to (min (1- width) (+ x (string-width label))) do
                 (puthash (cons xx y) t blocked))))
    ;; Route background edges vs active incident edges into separate layers.
    (dolist (edge edges)
      (let* ((u (car edge)) (v (cdr edge))
             (a (cdr (assoc u positions)))
             (b (cdr (assoc v positions)))
             (hot-p (and highlight-node
                         (or (equal u highlight-node)
                             (equal v highlight-node))))
             (target-masks (if hot-p active-masks bg-masks)))
        (when (and a b)
          (if braille-p
              (obsidian--add-braille-edge a b target-masks width height blocked)
            (obsidian--add-path-to-masks
             (obsidian--choose-orthogonal-path a b blocked) target-masks)))))
    (let ((canvas (make-vector height nil)))
      (dotimes (row height)
        (aset canvas row
              (obsidian--render-edge-row (aref bg-masks row)
                                         (aref active-masks row)
                                         width braille-p)))
      ;; Labels are always placed on the top layer.
      (dolist (node nodes)
        (let* ((p (cdr (assoc node positions))) (x (car p)) (y (cdr p))
               (label (obsidian--node-label node current focused))
               (face (cond ((equal node current) 'obsidian-graph-current)
                           ((and focused (equal node focused))
                            'obsidian-graph-focused)
                           ((member node connected) 'obsidian-graph-connected)
                           ((cl-some (lambda (edge)
                                       (or (equal node (car edge))
                                           (equal node (cdr edge)))) edges)
                            'obsidian-graph-node)
                           (t 'obsidian-graph-unlinked)))
               (line (aref canvas y))
               (visible (truncate-string-to-width label (- width x) nil nil "")))
          (add-text-properties
           0 (length visible)
           `(face ,face mouse-face highlight
                  help-echo ,(format "クリックで開く: %s (ドラッグでマップ移動)" node)
                  obsidian-node ,node keymap ,obsidian-graph-mode-map)
           visible)
          (aset canvas y
                (obsidian--replace-display-columns line x visible))))
      canvas)))

(defun obsidian--canvas-as-string (canvas)
  "Convert CANVAS into a propertized multiline string."
  (mapconcat #'identity (append canvas nil) "\n"))

(defun obsidian--render-graph (nodes edges positions width height current)
  "Render NODES and EDGES at POSITIONS as WIDTH by HEIGHT text.
CURRENT names the active node."
  (obsidian--canvas-as-string
   (obsidian--render-canvas nodes edges positions width height current)))


;;; Marquee animation

(defun obsidian--graph-stop-marquee ()
  "Stop the graph label marquee timer in the current buffer."
  (when (timerp obsidian--graph-marquee-timer)
    (cancel-timer obsidian--graph-marquee-timer))
  (setq obsidian--graph-marquee-timer nil))

(defun obsidian--graph-apply-marquee-step ()
  "Apply the current marquee step to labels in the virtual canvas."
  (when obsidian--graph-canvas
    (dotimes (row (length obsidian--graph-canvas))
      (let* ((line (aref obsidian--graph-canvas row))
             (position 0)
             (limit (length line)))
        (while (< position limit)
          (let* ((node (get-text-property
                        position 'obsidian-graph-marquee-name line))
                 (next (next-single-property-change
                        position 'obsidian-graph-marquee-name line limit)))
            (when node
              (put-text-property
               position next 'display
               (obsidian--graph-padded-name-window
                node obsidian--graph-marquee-step)
               line))
            (setq position next)))))))

(defun obsidian--graph-marquee-tick (buffer)
  "Advance scrolling node names in graph BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (get-buffer-window buffer t)
        (cl-incf obsidian--graph-marquee-step)
        (obsidian--graph-apply-marquee-step)
        (obsidian--graph-draw-view)))))

(defun obsidian--graph-start-marquee (nodes)
  "Start label scrolling when one of NODES exceeds the visible limit."
  (obsidian--graph-stop-marquee)
  (when (and obsidian-graph-label-marquee-interval
             (> obsidian-graph-label-marquee-interval 0)
             (cl-some
              (lambda (node)
                (> (length node)
                   (max 1 obsidian-graph-label-max-characters)))
              nodes))
    (setq obsidian--graph-marquee-timer
          (run-with-timer
           obsidian-graph-label-marquee-interval
           obsidian-graph-label-marquee-interval
           #'obsidian--graph-marquee-tick
           (current-buffer)))))


;;; Camera, viewport, panning, and zoom

(defun obsidian--graph-viewport-size ()
  "Return usable graph viewport size."
  (let ((window (get-buffer-window (current-buffer))))
    ;; Reserve one physical column so terminal frames never wrap exact-width rows.
    (cons (max 1 (1- (if window
                         (window-body-width window)
                       obsidian-graph-width)))
          (max 1 (- (if window (window-body-height window) 24) 2)))))

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
    (let* ((inhibit-read-only t)
           (view (obsidian--graph-viewport-size))
           (focus-hint (if obsidian--graph-focused-node
                           (format "  選択: ◈ %s (RETで開く)"
                                   obsidian--graph-focused-node)
                         "  ドラッグ:移動  ホイール:拡縮  TAB:巡回")))
      (erase-buffer)
      (dotimes (row (cdr view))
        (let ((source-row (+ obsidian--graph-camera-y row)))
          (if (>= source-row obsidian--graph-canvas-height)
              (insert "\n")
            (let ((line (aref obsidian--graph-canvas source-row)))
              (insert (obsidian--display-column-slice
                       line obsidian--graph-camera-x (car view))
                      "\n")))))
      (insert (propertize
               (truncate-string-to-width focus-hint (car view) nil nil "")
               'face 'obsidian-header-muted))
      (goto-char (point-min))
      (set-buffer-modified-p nil))))

(defun obsidian--graph-center-on-position (pos)
  "Center the graph camera on canvas coordinate POS `(X . Y)'."
  (when pos
    (let ((view (obsidian--graph-viewport-size)))
      (setq obsidian--graph-camera-x (- (car pos) (/ (car view) 2))
            obsidian--graph-camera-y (- (cdr pos) (/ (cdr view) 2)))
      (obsidian--graph-clamp-camera)
      (obsidian--graph-draw-view))))

(defun obsidian--graph-center-camera ()
  "Center the camera on the active/focused node or canvas center."
  (interactive)
  (let ((target (or (and obsidian--graph-focused-node
                         (cdr (assoc obsidian--graph-focused-node
                                     obsidian--graph-node-positions)))
                    obsidian--graph-current-position
                    (cons (/ obsidian--graph-canvas-width 2)
                          (/ obsidian--graph-canvas-height 2)))))
    (obsidian--graph-center-on-position target)))

(defun obsidian--graph-pan (dx dy)
  "Move the map camera by DX,DY, without moving a text cursor."
  (setq obsidian--graph-camera-x (+ obsidian--graph-camera-x dx)
        obsidian--graph-camera-y (+ obsidian--graph-camera-y dy))
  (obsidian--graph-clamp-camera)
  (obsidian--graph-draw-view))

(defun obsidian--graph-move-up ()
  "Pan the graph camera upward."
  (interactive) (obsidian--graph-pan 0 -2))
(defun obsidian--graph-move-down ()
  "Pan the graph camera downward."
  (interactive) (obsidian--graph-pan 0 2))
(defun obsidian--graph-move-left ()
  "Pan the graph camera left."
  (interactive) (obsidian--graph-pan -4 0))
(defun obsidian--graph-move-right ()
  "Pan the graph camera right."
  (interactive) (obsidian--graph-pan 4 0))

(defun obsidian-graph-zoom-in ()
  "Zoom into the graph by widening node spacing."
  (interactive)
  (when-let ((buf (get-buffer obsidian-graph-buffer-name)))
    (with-current-buffer buf
      (setq obsidian--graph-zoom (min 2.2 (+ (or obsidian--graph-zoom 1.0) 0.2)))
      (obsidian-refresh-graph)
      (message "グラフ表示倍率: %d%%" (round (* 100 obsidian--graph-zoom))))))

(defun obsidian-graph-zoom-out ()
  "Zoom out of the graph by compacting node spacing."
  (interactive)
  (when-let ((buf (get-buffer obsidian-graph-buffer-name)))
    (with-current-buffer buf
      (setq obsidian--graph-zoom (max 0.6 (- (or obsidian--graph-zoom 1.0) 0.2)))
      (obsidian-refresh-graph)
      (message "グラフ表示倍率: %d%%" (round (* 100 obsidian--graph-zoom))))))

(defun obsidian-graph-toggle-style ()
  "Toggle graph edges between rounded+heavy box-drawing and Braille patterns."
  (interactive)
  (setq obsidian-graph-edge-style
        (if (eq obsidian-graph-edge-style 'braille) 'box 'braille))
  (obsidian-refresh-graph)
  (force-mode-line-update t)
  (message "グラフ線スタイル: %s"
           (if (eq obsidian-graph-edge-style 'braille)
               "点字パターン高精細モード (⠀〜⣿)"
             "角丸＋太線ハイライト罫線モード (╭╮ ━┃)")))

(defun obsidian-graph-cycle-scope ()
  "Cycle graph scope: `auto' -> `local' (2-hop) -> `folder' -> `global'."
  (interactive)
  (setq obsidian-graph-scope-mode
        (pcase obsidian-graph-scope-mode
          ('auto 'local)
          ('local 'folder)
          ('folder 'global)
          (_ 'auto)))
  (obsidian-refresh-graph)
  (force-mode-line-update t)
  (message "グラフ表示範囲: %s" (obsidian--graph-scope-label)))


;;; Keyboard node focus cycling (`TAB' / `S-TAB')

(defun obsidian--graph-cycle-focus (step)
  "Advance `obsidian--graph-focused-node' by STEP along ordered graph nodes."
  (when-let ((buf (get-buffer obsidian-graph-buffer-name)))
    (with-current-buffer buf
      (when obsidian--graph-ordered-nodes
        (let* ((nodes obsidian--graph-ordered-nodes)
               (len (length nodes))
               (cur-idx (and obsidian--graph-focused-node
                             (cl-position obsidian--graph-focused-node
                                          nodes :test #'equal)))
               (next-idx (if cur-idx
                             (mod (+ cur-idx step) len)
                           0))
               (next-node (nth next-idx nodes)))
          (setq obsidian--graph-focused-node next-node)
          (obsidian-refresh-graph t)
          (obsidian--graph-center-on-position
           (cdr (assoc next-node obsidian--graph-node-positions))))))))

(defun obsidian-graph-next-node ()
  "Highlight and center the next connected node in the graph."
  (interactive)
  (obsidian--graph-cycle-focus 1))

(defun obsidian-graph-previous-node ()
  "Highlight and center the previous node in the graph."
  (interactive)
  (obsidian--graph-cycle-focus -1))


;;; Refresh and mouse/keyboard interaction

(defun obsidian-refresh-graph (&optional keep-focus)
  "Rebuild the virtual graph canvas and center the camera.
When KEEP-FOCUS is nil, clear any temporary `TAB' focus if a note is open."
  (interactive)
  (when-let ((buffer (get-buffer obsidian-graph-buffer-name)))
    (with-current-buffer buffer
      (let* ((files (seq-take (obsidian--graph-effective-files)
                              obsidian-graph-max-nodes))
             (nodes (delete-dups (mapcar #'file-name-base files)))
             (edges (obsidian--build-edges files))
             (view (obsidian--graph-viewport-size))
             (zoom (or obsidian--graph-zoom 1.0))
             (compact (and (<= (length nodes) 12) (<= zoom 1.05)))
             (width (if compact
                        (car view)
                      (max (car view)
                           (round (* zoom 7 (max 4 (length nodes)))))))
             (height (if compact
                         (cdr view)
                       (max (cdr view)
                            (round (* zoom 3 (max 4 (length nodes)))))))
             (layout-width (if compact (min width 48) width))
             (layout-height (if compact (min height 22) height))
             (current (and obsidian--current-file
                           (file-name-base obsidian--current-file))))
        (obsidian--graph-stop-marquee)
        (unless (and keep-focus
                     (member obsidian--graph-focused-node nodes))
          (setq obsidian--graph-focused-node nil))
        ;; Order TAB focus so direct neighbors of CURRENT come first.
        (let ((neighbors (and current (obsidian--connected-nodes current edges))))
          (setq obsidian--graph-ordered-nodes
                (append neighbors
                        (cl-remove-if (lambda (n)
                                        (or (equal n current)
                                            (member n neighbors)))
                                      nodes)
                        (and (member current nodes) (list current)))))
        (setq obsidian--graph-points nil
              obsidian--graph-marquee-step 0)
        (if nodes
            (let* ((raw-positions
                    (obsidian--force-layout nodes edges
                                            layout-width layout-height))
                   (positions
                    (obsidian--center-positions
                     raw-positions layout-width layout-height width height)))
              (setq obsidian--graph-node-positions positions
                    obsidian--graph-canvas-width width
                    obsidian--graph-canvas-height height
                    obsidian--graph-current-position (cdr (assoc current positions))
                    obsidian--graph-canvas
                    (obsidian--render-canvas nodes edges positions
                                             width height current))
              (unless keep-focus
                (obsidian--graph-center-camera))
              (when keep-focus
                (obsidian--graph-draw-view))
              (obsidian--graph-start-marquee nodes))
          (setq obsidian--graph-node-positions nil
                obsidian--graph-ordered-nodes nil
                obsidian--graph-canvas-width (car view)
                obsidian--graph-canvas-height (cdr view)
                obsidian--graph-current-position nil
                obsidian--graph-camera-x 0 obsidian--graph-camera-y 0
                obsidian--graph-canvas
                (vconcat (list "  この範囲にノートがありません。上部の [範囲] または [+新規] をクリックしてください。")
                         (make-list (max 0 (1- (cdr view))) "")))
          (obsidian--graph-draw-view))))))

(defun obsidian--graph-node-at (position)
  "Return graph node at or immediately before POSITION."
  (when (and (integer-or-marker-p position)
             (>= position (point-min))
             (<= position (point-max)))
    (or (and (< position (point-max))
             (get-text-property position 'obsidian-node))
        (and (> position (point-min))
             (get-text-property (1- position) 'obsidian-node)))))

(defun obsidian--graph-open-node (node)
  "Open NODE in the central editor."
  (if-let ((file (and node (obsidian--find-note node))))
      (obsidian--open-note file)
    (user-error "ノートが選択されていません（TABキーまたはクリックで選択）")))

(defun obsidian--graph-open-at-point ()
  "Open the graph node at point, or the `TAB'-focused / active node."
  (interactive)
  (obsidian--graph-open-node
   (or (obsidian--graph-node-at (point))
       obsidian--graph-focused-node
       (car obsidian--graph-ordered-nodes))))

(defun obsidian--graph-mouse-down (event)
  "Grab-pan the graph viewport when dragging with mouse-1, or open clicked node."
  (interactive "e")
  (let* ((start-posn (event-start event))
         (win (posn-window start-posn))
         (start-pt (posn-point start-posn))
         (start-cr (posn-col-row start-posn))
         (last-col (car-safe start-cr))
         (last-row (cdr-safe start-cr))
         (dragged nil))
    (when (and (window-live-p win) (null (posn-area start-posn)))
      (select-window win)
      (track-mouse
        (let (ev)
          (while (progn
                   (setq ev (read-event))
                   (mouse-movement-p ev))
            (let* ((posn (event-end ev))
                   (cr (and posn
                            (eq (posn-window posn) win)
                            (posn-col-row posn))))
              (when (and cr (car cr) (cdr cr) last-col last-row)
                (let ((dx (- (car cr) last-col))
                      (dy (- (cdr cr) last-row)))
                  (when (or (/= dx 0) (/= dy 0))
                    (setq dragged t
                          last-col (car cr)
                          last-row (cdr cr))
                    (obsidian--graph-pan (- dx) (- dy)))))))))
      (when (and (not dragged) (integer-or-marker-p start-pt))
        (with-current-buffer obsidian-graph-buffer-name
          (when-let ((node (obsidian--graph-node-at start-pt)))
            (setq obsidian--graph-focused-node node)
            (obsidian--graph-open-node node)))))))

(defun obsidian--graph-mouse-open (event)
  "Open the graph node clicked in EVENT."
  (interactive "e")
  (with-current-buffer obsidian-graph-buffer-name
    (let ((position (posn-point (event-end event))))
      (when (integer-or-marker-p position)
        (when-let ((node (obsidian--graph-node-at position)))
          (obsidian--graph-open-node node))))))

(defun obsidian--schedule-graph-update ()
  "Refresh the graph shortly, coalescing repeated requests."
  (when (timerp obsidian--graph-timer) (cancel-timer obsidian--graph-timer))
  (setq obsidian--graph-timer
        (run-with-idle-timer 0.15 nil #'obsidian-refresh-graph)))

(provide 'obsidian-graph)
;;; obsidian-graph.el ends here
