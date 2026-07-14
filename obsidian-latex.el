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
    (while (re-search-forward "\\$\\$\\(.\\|\n\\)*?\\$\\$" nil t)
      (obsidian--make-latex-overlay (match-beginning 0) (match-end 0) t))
    (goto-char (point-min))
    (while (re-search-forward "\\([^$]\\|^\\)\\$\\([^$\n]*?\\)\\$" nil t)
      (let ((b (1+ (match-beginning 2)))
            (e (match-end 2)))
        (when (> e b)
          (obsidian--make-latex-overlay (match-beginning 0) (match-end 0) nil))))))

(defun obsidian--make-latex-overlay (start end display-p)
  "Make a LaTeX overlay from START to END.
DISPLAY-P means render a display equation rather than inline math."
  (let* ((text (buffer-substring-no-properties start end))
         (clean (string-trim (replace-regexp-in-string
                              "\\`\\$+\\|\\$+\\'" "" text)))
         (image (obsidian--render-latex clean display-p))
         (ov (make-overlay start end)))
    (if image
        (overlay-put ov 'display image)
      (overlay-put ov 'face '(:background "gray20" :foreground "yellow"))
      (overlay-put ov 'after-string
                   (propertize (format " [%s] " clean)
                               'face '(:foreground "yellow"))))
    (overlay-put ov 'obsidian-latex t)
    (push ov obsidian--latex-overlays)))

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
