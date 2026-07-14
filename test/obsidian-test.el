;;; obsidian-test.el --- Tests for EMACS_OBSIDIAN -*- lexical-binding: t; -*-

(require 'ert)
(require 'obsidian)

(defconst obsidian-test--vault
  (expand-file-name "../examples/demo-vault/"
                    (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest obsidian-loads ()
  (should (commandp 'obsidian)))

(ert-deftest obsidian-demo-graph-has-seven-edges ()
  (let* ((obsidian--vault obsidian-test--vault)
         (obsidian--current-scope (expand-file-name "music/" obsidian-test--vault))
         (files (obsidian--notes-in-current-scope))
         (edges (obsidian--build-edges files)))
    (should (= 8 (length files)))
    (should (= 7 (length edges)))
    (should (member '("guitar" . "music") edges))
    (should (member '("acoustic" . "guitar") edges))))

(ert-deftest obsidian-root-scope-does-not-mix-folders ()
  (let ((obsidian--vault obsidian-test--vault)
        (obsidian--current-scope obsidian-test--vault))
    (should-not (obsidian--notes-in-current-scope))))

(ert-deftest obsidian-ignores-emacs-lock-files ()
  (let* ((vault (make-temp-file "obsidian-lock-test-" t))
         (note (expand-file-name "fundamenta.md" vault))
         (lock (expand-file-name ".#fundamenta.md" vault))
         (obsidian--vault vault))
    (unwind-protect
        (progn
          (with-temp-file note (insert "[[technology]]\n"))
          ;; Emacs lock files are normally symlinks and can have no live target.
          (make-symbolic-link "missing-user@host.123" lock)
          (should (equal (list note) (obsidian--all-note-files)))
          (should-not (obsidian--note-file-p lock))
          (should-not (condition-case nil
                          (progn (obsidian--build-edges (list note lock)) nil)
                        (file-error t))))
      (delete-directory vault t))))

(ert-deftest obsidian-graph-labels-are-clickable ()
  (let* ((nodes '("music" "piano"))
         (edges '(("music" . "piano")))
         (positions '(("music"  . (1 . 1)) ("piano" . (20 . 5))))
         (obsidian--graph-points nil)
         (text (obsidian--render-graph nodes edges positions 40 8 "music"))
         (at (string-match "music" text)))
    (should at)
    (should (equal "music" (get-text-property at 'obsidian-node text)))))

(ert-deftest obsidian-force-layout-keeps-distinct-label-anchors ()
  (let* ((nodes '("music" "piano" "violin" "trumpet" "guitar"
                  "acoustic" "bass" "electric"))
         (edges '(("music" . "guitar") ("music" . "piano")
                  ("music" . "violin") ("music" . "trumpet")
                  ("guitar" . "acoustic") ("guitar" . "bass")
                  ("guitar" . "electric")))
         (positions (obsidian--force-layout nodes edges 80 22)))
    (should (= 8 (length positions)))
    (should (= 8 (length (delete-dups (mapcar #'cdr positions)))))))

(ert-deftest obsidian-demo-renders-every-label ()
  (let* ((obsidian--vault obsidian-test--vault)
         (obsidian--current-scope (expand-file-name "music/" obsidian-test--vault))
         (files (obsidian--notes-in-current-scope))
         (nodes (mapcar #'file-name-base files))
         (edges (obsidian--build-edges files))
         (positions (obsidian--force-layout nodes edges 80 22))
         (text (obsidian--render-graph nodes edges positions 80 22 "music")))
    (dolist (node nodes)
      (should (string-match-p (regexp-quote node) text)))))

(ert-deftest obsidian-graph-uses-box-drawing-not-diagonal-stairs ()
  (let* ((nodes '("source" "target"))
         (edges '(("source" . "target")))
         (positions '(("source" . (2 . 2)) ("target" . (25 . 8))))
         (text (obsidian--render-graph nodes edges positions 40 12 nil)))
    (should-not (string-match-p "[╱╲/\\\\]" text))
    (should (string-match-p "[┌┐└┘]" text))))

(ert-deftest obsidian-japanese-labels-preserve-canvas-column-width ()
  (let* ((nodes '("技術" "HumanDigitalTwin" "考察した要素"))
         (edges '(("技術" . "HumanDigitalTwin")
                  ("技術" . "考察した要素")))
         (positions '(("技術" . (3 . 4))
                      ("HumanDigitalTwin" . (25 . 4))
                      ("考察した要素" . (15 . 1))))
         (canvas (obsidian--render-canvas
                  nodes edges positions 50 8 "技術")))
    (dotimes (row (length canvas))
      (should (= 50 (string-width (aref canvas row)))))
    (should (string-match-p "◆ 技術" (aref canvas 4)))
    (should (string-match-p "● 考察した要素" (aref canvas 1)))))

(ert-deftest obsidian-camera-slices-by-display-columns ()
  (let ((line "1234● 技術────● target"))
    (should (= 12 (string-width
                   (obsidian--display-column-slice line 4 12))))))

(ert-deftest obsidian-tree-escape-deletes-confirmed-note ()
  (let* ((directory (make-temp-file "obsidian-delete-test-" t))
         (file (expand-file-name "unused.md" directory))
         (obsidian-delete-by-moving-to-trash nil)
         (obsidian--current-file nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert "unused\n"))
          (with-temp-buffer
            (insert (propertize "unused.md" 'obsidian-path file))
            (goto-char (point-min))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                      ((symbol-function 'obsidian--tree-refresh) #'ignore)
                      ((symbol-function 'obsidian--schedule-graph-update) #'ignore))
              (obsidian--tree-delete-file)))
          (should-not (file-exists-p file)))
      (delete-directory directory t))))

(ert-deftest obsidian-tree-escape-keeps-declined-note ()
  (let* ((directory (make-temp-file "obsidian-keep-test-" t))
         (file (expand-file-name "keep.md" directory))
         (obsidian-delete-by-moving-to-trash nil)
         (obsidian--current-file nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert "keep\n"))
          (with-temp-buffer
            (insert (propertize "keep.md" 'obsidian-path file))
            (goto-char (point-min))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
              (obsidian--tree-delete-file)))
          (should (file-exists-p file)))
      (delete-directory directory t))))

(ert-deftest obsidian-tree-escape-is-bound-to-delete ()
  (should (eq (lookup-key obsidian-tree-mode-map (kbd "<escape>"))
              #'obsidian--tree-delete-file)))

(ert-deftest obsidian-note-path-cannot-escape-vault ()
  (let ((obsidian--vault obsidian-test--vault))
    (should-error (obsidian--safe-note-path "../../outside" obsidian-test--vault)
                  :type 'user-error)))

(ert-deftest obsidian-simple-new-note-uses-current-scope ()
  (let* ((vault (make-temp-file "obsidian-create-test-" t))
         (scope (expand-file-name "music/" vault))
         (obsidian--vault vault)
         (obsidian--current-scope scope)
         (obsidian-auto-timestamp nil)
         opened)
    (make-directory scope t)
    (unwind-protect
        (cl-letf (((symbol-function 'obsidian--open-note)
                   (lambda (file &optional _no-record) (setq opened file)))
                  ((symbol-function 'obsidian--tree-refresh) #'ignore))
          (obsidian--create-note-impl "brass")
          (should (equal (expand-file-name "brass.md" scope) opened))
          (should (file-exists-p opened)))
      (delete-directory vault t))))

(ert-deftest obsidian-unicode-math-fallback-is-readable ()
  (should (equal "e⁽ⁱπ⁾+1=0"
                 (obsidian--latex-to-unicode "e^{i\\pi}+1=0"))))

(ert-deftest obsidian-inline-math-overlay-does-not-hide-prefix ()
  (with-temp-buffer
    (insert "公式は $e^{i\\pi}+1=0$ です。")
    (let ((obsidian-latex-command nil)
          (obsidian-dvipng-command nil))
      (obsidian--show-latex-preview)
      (should (= 1 (length obsidian--latex-overlays)))
      (let ((overlay (car obsidian--latex-overlays)))
        (should (equal "$e^{i\\pi}+1=0$"
                       (buffer-substring-no-properties
                        (overlay-start overlay) (overlay-end overlay))))
        (should (equal "e⁽ⁱπ⁾+1=0" (overlay-get overlay 'display)))))))

(ert-deftest obsidian-window-widths-remain-asymmetric-with-responsive-cap ()
  (should (equal '(25 . 72) (obsidian--fit-panel-widths 160 25 73))))

(ert-deftest obsidian-window-widths-shrink-proportionally-when-needed ()
  (let ((widths (obsidian--fit-panel-widths 100 25 73)))
    (should (<= (+ (car widths) (cdr widths)) 63))
    (should (> (cdr widths) (car widths)))
    (should (>= (car widths) obsidian--minimum-panel-width))))

(ert-deftest obsidian-editor-keeps-thirty-five-percent-on-user-frame ()
  (let ((widths (obsidian--fit-panel-widths 120 20 73)))
    (should (equal '(20 . 54) widths))
    ;; 120 - two dividers - side panels leaves 44 columns for editing.
    (should (>= (- 120 obsidian--window-divider-overhead
                   (car widths) (cdr widths))
                42))))

(ert-deftest obsidian-saved-ratios-scale-with-workspace ()
  (let ((obsidian--saved-tree-ratio 0.20)
        (obsidian--saved-graph-ratio 0.40)
        (obsidian--saved-tree-width nil)
        (obsidian--saved-graph-width nil))
    (should (equal '(24 . 48) (obsidian--requested-panel-widths 120)))
    (should (equal '(32 . 64) (obsidian--requested-panel-widths 160)))))

(ert-deftest obsidian-loads-v2-window-ratios ()
  (let ((file (make-temp-file "obsidian-window-ratios-"))
        (obsidian-save-window-sizes t))
    (unwind-protect
        (let ((obsidian-window-sizes-file file))
          (with-temp-file file (insert "v2 0.20000000 0.40000000\n"))
          (obsidian--load-window-sizes)
          (should (= 0.2 obsidian--saved-tree-ratio))
          (should (= 0.4 obsidian--saved-graph-ratio)))
      (delete-file file))))

(ert-deftest obsidian-arrow-pan-moves-camera-not-point ()
  (with-temp-buffer
    (obsidian-graph-mode)
    (setq obsidian--vault obsidian-test--vault
          obsidian--current-scope obsidian-test--vault
          obsidian--graph-canvas-width 100
          obsidian--graph-canvas-height 50
          obsidian--graph-camera-x 10
          obsidian--graph-camera-y 10
          obsidian--graph-canvas
          (vconcat (make-list 50 (make-string 100 ?\s))))
    (obsidian--graph-move-right)
    (should (= 14 obsidian--graph-camera-x))
    (should (= (point-min) (point)))))

(provide 'obsidian-test)
;;; obsidian-test.el ends here
