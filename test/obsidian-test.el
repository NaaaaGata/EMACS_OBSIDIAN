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

(ert-deftest obsidian-window-widths-remain-asymmetric-when-they-fit ()
  (should (equal '(25 . 73) (obsidian--fit-panel-widths 160 25 73))))

(ert-deftest obsidian-window-widths-shrink-proportionally-when-needed ()
  (let ((widths (obsidian--fit-panel-widths 100 25 73)))
    (should (= 78 (+ (car widths) (cdr widths))))
    (should (> (cdr widths) (car widths)))
    (should (>= (car widths) obsidian--minimum-panel-width))))

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
