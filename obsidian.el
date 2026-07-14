;;; obsidian.el --- Obsidian-like note-taking environment for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  osadayuushi

;; Author: osadayuushi
;; Version: 0.3.0
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
;;  Obsidian-like note-taking workspace for Emacs.
;;  Features: 3-window layout, wiki links, force-directed text graph,
;;  LaTeX preview, auto file creation, timestamp insertion.

;;; Code:

(require 'cl-lib)
(require 'rx)

(require 'obsidian-windows)
(defcustom obsidian-vault-directory nil
  "Default vault directory.  When nil, `obsidian' asks for one."
  :type '(choice (const :tag "Ask when starting" nil) directory)
  :group 'obsidian)

(defcustom obsidian-link-regexp
  (rx "[[" (group-n 1 (+? anything)) "]]" )
  "Regexp matching wiki links.  Group 1 contains target and optional alias."
  :type 'regexp
  :group 'obsidian)

(require 'obsidian-tree)
(require 'obsidian-editor)
(require 'obsidian-graph)
(require 'obsidian-latex)


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


;; Entry point

;;;###autoload
(defun obsidian ()
  "Open an Obsidian-like workspace in the current frame.
Prompts for a vault directory unless `obsidian-vault-directory' is set.
Sets up three windows: file tree (left), editor (center), graph (right).
Initial graph scope is set to the vault root."
  (interactive)
  (let ((dir (or obsidian-vault-directory
                 (read-directory-name "Obsidian vault: "))))
    (setq dir (file-truename (expand-file-name dir)))
    (unless (file-directory-p dir)
      (user-error "Not a directory: %s" dir))
    (setq obsidian--vault dir)
    (setq obsidian--current-file nil)
    (setq obsidian--current-scope dir)
    (obsidian--load-window-sizes)
    (obsidian--setup-windows)
    (obsidian--tree-refresh)
    (obsidian-refresh-graph)
    (select-window (obsidian--tree-window))
    (goto-char (point-min))
    (forward-line 1)
    (message "Obsidian vault: %s (scope: vault root)" dir)))

;;;###autoload
(defalias 'obsidian-open #'obsidian)

(provide 'obsidian)
;;; obsidian.el ends here
