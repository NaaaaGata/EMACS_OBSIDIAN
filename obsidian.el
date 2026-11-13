;;; obsidian.el --- Obsidian-like note-taking environment for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  osadayuushi

;; Author: osadayuushi
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: notes, obsidian, markdown, links, graph
;; URL: https://github.com/NaaaaGata/EMACS_OBSIDIAN

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  Obsidian-like note-taking workspace for Terminal & GUI Emacs.
;;  Features: mouse-draggable 3-window layout, clickable header toolbars,
;;  visual folder tree with guides & context menu, pannable/zoomable text &
;;  Braille force-directed graph, Obsidian Live Preview wiki links, inline `[['
;;  completion, clickable backlinks, Daily Notes, LaTeX preview, and Git sync.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'subr-x)

(require 'obsidian-windows)

(defcustom obsidian-vault-directory nil
  "Default vault directory.  When nil, `obsidian' asks for one."
  :type '(choice (const :tag "Ask when starting" nil) directory)
  :group 'obsidian)

(defcustom obsidian-link-regexp
  (rx "[[" (group-n 1 (+? anything)) "]]")
  "Regexp matching wiki links.  Group 1 contains target and optional alias."
  :type 'regexp
  :group 'obsidian)

(defcustom obsidian-enable-terminal-mouse t
  "When non-nil, automatically enable `xterm-mouse-mode' in terminal Emacs.
Allows clicking notes/buttons, dragging `│' panel dividers, grab-panning the
graph, and scrolling/zooming with the mouse wheel inside `emacs -nw'."
  :type 'boolean
  :group 'obsidian)

(defcustom obsidian-daily-folder "daily"
  "Subdirectory inside the vault used by `obsidian-daily-note'.
Set to nil or an empty string to place daily notes in the vault root."
  :type '(choice (const :tag "Vault root" nil) string)
  :group 'obsidian)

(defcustom obsidian-daily-note-format "%Y-%m-%d"
  "Filename format string (via `format-time-string') for `obsidian-daily-note'."
  :type 'string
  :group 'obsidian)

(require 'obsidian-tree)
(require 'obsidian-editor)
(require 'obsidian-graph)
(require 'obsidian-latex)
(require 'obsidian-git-sync)


;; Internal variables

(defvar obsidian--vault nil
  "Current vault directory (absolute, expanded).")

(defvar obsidian--history nil
  "History of opened notes for back navigation.
List of (file . position) pairs, most recent first.")

(defvar obsidian--current-file nil
  "Currently open note file (absolute path).")

(defvar obsidian--current-scope nil
  "Current graph scope directory.
When nil, defaults to vault root.
Set to a subdirectory when a file in that directory is opened.")

(defvar obsidian--graph-timer nil
  "Idle timer for refreshing the graph view.")

(defvar obsidian--saved-window-config nil
  "Window configuration captured before entering the Obsidian workspace.")


;; Helpers & Commands

(defun obsidian--ensure-terminal-mouse ()
  "Enable mouse support in terminal Emacs frames when configured."
  (when (and obsidian-enable-terminal-mouse
             (not (display-graphic-p)))
    (when (fboundp 'xterm-mouse-mode)
      (xterm-mouse-mode 1))
    (when (fboundp 'mouse-wheel-mode)
      (mouse-wheel-mode 1))))

(defun obsidian--ensure-vault ()
  "Ensure `obsidian--vault' is initialized, prompting if needed."
  (unless (and obsidian--vault (file-directory-p obsidian--vault))
    (let ((dir (or obsidian-vault-directory
                   (read-directory-name "Obsidian vault: "))))
      (setq dir (file-truename (expand-file-name dir)))
      (unless (file-directory-p dir)
        (user-error "Not a directory: %s" dir))
      (setq obsidian--vault dir
            obsidian--current-scope dir))))

;;;###autoload
(defun obsidian ()
  "Open an Obsidian-like workspace in the current frame.
Prompts for a vault directory unless `obsidian-vault-directory' is set.
Sets up three windows: file tree (left), editor (center), graph (right)."
  (interactive)
  (let ((dir (or obsidian-vault-directory
                 (read-directory-name "Obsidian vault: "))))
    (setq dir (file-truename (expand-file-name dir)))
    (unless (file-directory-p dir)
      (user-error "Not a directory: %s" dir))
    ;; Save the pre-Obsidian layout so `C-c o q' / `[×終了]' restores it cleanly.
    (unless (or (get-buffer-window obsidian-tree-buffer-name)
                (get-buffer-window obsidian-graph-buffer-name))
      (setq obsidian--saved-window-config (current-window-configuration)))
    (obsidian--ensure-terminal-mouse)
    (setq obsidian--vault dir)
    (setq obsidian--current-file nil)
    (setq obsidian--current-scope dir)
    (obsidian-git-auto-sync-mode 1)
    (obsidian--load-window-sizes)
    (obsidian--setup-windows)
    (obsidian--tree-refresh)
    (obsidian-refresh-graph)
    (select-window (obsidian--tree-window))
    (goto-char (point-min))
    (message "Obsidian vault: %s (マウス操作有効・C-c o q で元の画面へ復帰)" dir)))

;;;###autoload
(defun obsidian-daily-note ()
  "Create or open today's Daily Note in the Obsidian workspace."
  (interactive)
  (obsidian--ensure-vault)
  (unless (or (get-buffer-window obsidian-tree-buffer-name)
              (get-buffer-window obsidian-graph-buffer-name))
    (setq obsidian--saved-window-config (current-window-configuration))
    (obsidian--ensure-terminal-mouse)
    (obsidian--load-window-sizes)
    (obsidian--setup-windows)
    (obsidian--tree-refresh))
  (let* ((date-name (format-time-string obsidian-daily-note-format))
         (rel-name (if (and obsidian-daily-folder
                            (not (string-empty-p obsidian-daily-folder)))
                       (concat (file-name-as-directory obsidian-daily-folder)
                               date-name)
                     date-name)))
    (obsidian--create-note-impl rel-name)))

;;;###autoload
(defun obsidian-find-note ()
  "Quickly search and open (or create) any note in the active vault."
  (interactive)
  (obsidian--ensure-vault)
  (let* ((files (obsidian--all-note-files))
         (candidates
          (mapcar (lambda (f)
                    (let ((rel (file-relative-name f obsidian--vault)))
                      (cons (file-name-sans-extension rel) f)))
                  files))
         (choice (completing-read "ノート検索 / 新規作成: "
                                  (mapcar #'car candidates)
                                  nil nil)))
    (unless (string-empty-p (string-trim choice))
      (if-let ((existing (cdr (assoc choice candidates))))
          (obsidian--open-note existing)
        (obsidian--create-note-impl (string-trim choice))))))

(defun obsidian-quit ()
  "Leave the Obsidian workspace and restore the previous window configuration."
  (interactive)
  (obsidian--save-window-sizes)
  (dolist (buf-name (list obsidian-tree-buffer-name obsidian-graph-buffer-name))
    (when-let ((buf (get-buffer buf-name)))
      (with-current-buffer buf
        (when (fboundp 'obsidian--tree-stop-marquee)
          (obsidian--tree-stop-marquee))
        (when (fboundp 'obsidian--graph-stop-marquee)
          (obsidian--graph-stop-marquee)))))
  (if (window-configuration-p obsidian--saved-window-config)
      (let ((cfg obsidian--saved-window-config))
        (setq obsidian--saved-window-config nil)
        (set-window-configuration cfg)
        (message "元のウィンドウ構成に戻りました (M-x obsidian で再開)"))
    (let ((tree-win (get-buffer-window obsidian-tree-buffer-name))
          (graph-win (get-buffer-window obsidian-graph-buffer-name)))
      (when (and (window-live-p tree-win) (not (one-window-p t)))
        (delete-window tree-win))
      (when (and (window-live-p graph-win) (not (one-window-p t)))
        (delete-window graph-win))
      (message "Obsidian のサイドパネルを閉じました"))))

;;;###autoload
(defalias 'obsidian-open #'obsidian)

(provide 'obsidian)
;;; obsidian.el ends here
