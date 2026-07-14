;;; obsidian-latex.el --- LaTeX math preview -*- lexical-binding: t; -*-

;;; Commentary:
;; Renders inline/display math with latex+dvipng and falls back gracefully.

;;; Code:

(defcustom obsidian-latex-command
  (if (executable-find "latex") "latex" nil)
  "Command used to typeset LaTeX fragments.
When nil, a simple ASCII fallback is used."
  :type '(choice (const :tag "ASCII fallback" nil)
                 (string :tag "LaTeX executable"))
  :group 'obsidian)

(defcustom obsidian-dvipng-command
  (if (executable-find "dvipng") "dvipng" nil)
  "Dvipng command for converting DVI to PNG."
  :type '(choice (const :tag "No image" nil)
                 (string :tag "dvipng executable"))
  :group 'obsidian)

(defvar-local obsidian--latex-overlays nil
  "List of active LaTeX preview overlays.")

(defun obsidian-toggle-latex-preview ()
  "Toggle LaTeX math preview overlays in the editor buffer."
  (interactive)
  (if obsidian--latex-overlays
      (obsidian--remove-latex-overlays)
    (obsidian--show-latex-preview)))

(defun obsidian--remove-latex-overlays ()
  "Remove all LaTeX preview overlays."
  (dolist (ov obsidian--latex-overlays)
    (when (overlayp ov) (delete-overlay ov)))
  (setq obsidian--latex-overlays nil))

(defun obsidian--show-latex-preview ()
  "Create overlays for $...$ and $$...$$ fragments."
  (obsidian--remove-latex-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\$\\$\\(\\(?:.\\|\n\\)*?\\)\\$\\$" nil t)
      (obsidian--make-latex-overlay (match-beginning 0) (match-end 0) t
                                    (match-string-no-properties 1)))
    (goto-char (point-min))
    ;; Group 1 is exactly the $...$ fragment; the non-dollar prefix is only a
    ;; boundary assertion and must never disappear under the overlay.
    (while (re-search-forward
            "\\(?:^\\|[^$]\\)\\(\\$\\([^$\n]+\\)\\$\\)" nil t)
      (obsidian--make-latex-overlay (match-beginning 1) (match-end 1) nil
                                    (match-string-no-properties 2)))))

(defun obsidian--make-latex-overlay (start end display-p &optional math)
  "Make a LaTeX overlay from START to END.
DISPLAY-P means render a display equation rather than inline math.
MATH, when non-nil, is the already extracted expression."
  (let* ((text (buffer-substring-no-properties start end))
         (clean (or math
                    (string-trim (replace-regexp-in-string
                                  "\\`\\$+\\|\\$+\\'" "" text))))
         (image (obsidian--render-latex clean display-p))
         (ov (make-overlay start end)))
    (if image
        (overlay-put ov 'display image)
      ;; Text terminals and machines without TeX still get a single readable
      ;; mathematical rendering instead of yellow duplicated source text.
      (overlay-put ov 'display
                   (propertize (obsidian--latex-to-unicode clean)
                               'face 'font-lock-constant-face)))
    (overlay-put ov 'help-echo
                 (if image "LaTeX image preview" "Unicode math preview"))
    (overlay-put ov 'obsidian-latex t)
    (push ov obsidian--latex-overlays)))

(defconst obsidian--latex-unicode-replacements
  '(("\\\\alpha" . "α") ("\\\\beta" . "β") ("\\\\gamma" . "γ")
    ("\\\\delta" . "δ") ("\\\\theta" . "θ") ("\\\\lambda" . "λ")
    ("\\\\mu" . "μ") ("\\\\pi" . "π") ("\\\\sigma" . "σ")
    ("\\\\phi" . "φ") ("\\\\omega" . "ω") ("\\\\Gamma" . "Γ")
    ("\\\\Delta" . "Δ") ("\\\\Theta" . "Θ") ("\\\\Lambda" . "Λ")
    ("\\\\Pi" . "Π") ("\\\\Sigma" . "Σ") ("\\\\Phi" . "Φ")
    ("\\\\Omega" . "Ω") ("\\\\infty" . "∞") ("\\\\sum" . "∑")
    ("\\\\prod" . "∏") ("\\\\int" . "∫") ("\\\\sqrt" . "√")
    ("\\\\times" . "×") ("\\\\cdot" . "·") ("\\\\pm" . "±")
    ("\\\\leq" . "≤") ("\\\\geq" . "≥") ("\\\\neq" . "≠")
    ("\\\\to" . "→") ("\\\\rightarrow" . "→")
    ("\\\\leftarrow" . "←"))
  "Common LaTeX commands and their Unicode text equivalents.")

(defconst obsidian--superscript-characters
  '((?0 . ?⁰) (?1 . ?¹) (?2 . ?²) (?3 . ?³) (?4 . ?⁴)
    (?5 . ?⁵) (?6 . ?⁶) (?7 . ?⁷) (?8 . ?⁸) (?9 . ?⁹)
    (?+ . ?⁺) (?- . ?⁻) (?= . ?⁼) (?\( . ?⁽) (?\) . ?⁾)
    (?i . ?ⁱ) (?n . ?ⁿ))
  "Characters with standard Unicode superscript forms.")

(defconst obsidian--subscript-characters
  '((?0 . ?₀) (?1 . ?₁) (?2 . ?₂) (?3 . ?₃) (?4 . ?₄)
    (?5 . ?₅) (?6 . ?₆) (?7 . ?₇) (?8 . ?₈) (?9 . ?₉)
    (?+ . ?₊) (?- . ?₋) (?= . ?₌) (?\( . ?₍) (?\) . ?₎))
  "Characters with standard Unicode subscript forms.")

(defun obsidian--script-string (text table open close)
  "Convert TEXT using TABLE, surrounding it with OPEN and CLOSE."
  (concat open
          (mapconcat (lambda (character)
                       (char-to-string (or (cdr (assq character table))
                                           character)))
                     text "")
          close))

(defun obsidian--latex-to-unicode (math)
  "Return a readable Unicode approximation of LaTeX MATH."
  (let ((result math))
    (dolist (replacement obsidian--latex-unicode-replacements)
      (setq result (replace-regexp-in-string
                    (car replacement) (cdr replacement) result t t)))
    ;; Handle the most useful structural constructs without pretending to be
    ;; a complete TeX parser.
    (while (string-match "\\\\frac{\\([^{}]+\\)}{\\([^{}]+\\)}" result)
      (setq result (replace-match "(\\1)/(\\2)" t nil result)))
    (while (string-match "\\^{\\([^{}]+\\)}" result)
      (setq result
            (replace-match
             (obsidian--script-string (match-string 1 result)
                                      obsidian--superscript-characters "⁽" "⁾")
             t t result)))
    (while (string-match "_{\\([^{}]+\\)}" result)
      (setq result
            (replace-match
             (obsidian--script-string (match-string 1 result)
                                      obsidian--subscript-characters "₍" "₎")
             t t result)))
    (setq result (replace-regexp-in-string "[{}]" "" result))
    result))

(defun obsidian--render-latex (math display-p)
  "Render MATH into a PNG image, or nil if unavailable.
DISPLAY-P selects display equation syntax."
  (when (and obsidian-latex-command obsidian-dvipng-command)
    (let* ((tmp (make-temp-file "obs-latex-" nil ".tex"))
           (dir (file-name-directory tmp))
           (base (file-name-base tmp))
           (prefix (expand-file-name base dir))
           (png (concat prefix ".png"))
           (latex-body
            (format "\\documentclass{article}\\usepackage{amsmath,amssymb}\\usepackage[active,textmath,tightpage]{preview}\\begin{document}%s\\end{document}"
                    (if display-p (format "\\[%s\\]" math)
                      (format "$%s$" math))))
           image)
      (unwind-protect
          (condition-case nil
              (progn
                (with-temp-file tmp (insert latex-body))
                (let ((default-directory dir))
                  (when (and
                         (zerop (call-process
                                 obsidian-latex-command nil nil nil
                                 "-interaction=nonstopmode" base))
                         (zerop (call-process
                                 obsidian-dvipng-command nil nil nil
                                 "-q" "-D" "120" "-T" "tight"
                                 "-o" png (concat prefix ".dvi"))))
                    ;; Store PNG bytes in the image object.  This lets us delete
                    ;; every TeX artifact immediately without breaking display.
                    (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert-file-contents-literally png)
                      (setq image (create-image (buffer-string) 'png t))))))
            (file-error nil))
        (dolist (artifact (file-expand-wildcards (concat prefix ".*")))
          (ignore-errors (delete-file artifact))))
      image)))

(provide 'obsidian-latex)
;;; obsidian-latex.el ends here
