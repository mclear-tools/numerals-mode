;;; numerals-parser.el --- Parsing functions for numerals-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: calc, convenience
;; Version: 0.1.0

;; This file is part of numerals-mode.

;;; Commentary:

;; This module provides parsing functionality for numerals-mode.
;; It recognizes variable assignments and calculations in plain text.

;;; Code:

(defconst numerals-parser-variable-name-regexp
  "\\(?:[A-Za-z][A-Za-z0-9_ ]*[A-Za-z0-9_]\\|[A-Za-z]\\)"
  "Regexp matching valid variable names.
Variable names can contain letters, numbers, underscores and spaces, but must
start with a letter and end with a letter, number, or underscore.")

(defconst numerals-parser-assignment-regexp
  (concat "^\\s-*\\(" numerals-parser-variable-name-regexp "\\)\\s-*=\\s-*\\(.+\\)$")
  "Regexp matching variable assignment lines.
Group 1: variable name
Group 2: expression")

(defconst numerals-parser-calculation-regexp
  "^\\s-*=\\s-*\\(.+\\)$"
  "Regexp matching standalone calculation lines.
Group 1: expression")

(defun numerals-parser-is-commented-line-p (line)
  "Check if LINE is a comment line based on current buffer's major mode.
Returns t if the line should be treated as a comment and ignored."
  (let ((trimmed (string-trim line)))
    (cond
     ;; Org-mode comments (# at start)
     ((derived-mode-p 'org-mode)
      (string-match-p "^#" trimmed))
     ;; Markdown comments (HTML-style comments)
     ((or (derived-mode-p 'markdown-mode)
          (derived-mode-p 'gfm-mode))
      (string-match-p "^<!--" trimmed))
     ;; General text files - support common comment styles
     (t
      (or (string-match-p "^#" trimmed)      ; Shell/Python style
          (string-match-p "^//" trimmed)     ; C++/JavaScript style
          (string-match-p "^<!--" trimmed)   ; HTML style
          (string-match-p "^%" trimmed))))))  ; LaTeX style

(defun numerals-parser-parse-line (line)
  "Parse a single LINE and return its type and components.
Returns a plist with the following properties:
  :type - 'assignment, 'calculation, or 'text
  :variable - variable name (for assignments)
  :expression - the expression to evaluate
  :line - the original line"
  (let ((trimmed (string-trim line)))
    (cond
     ;; Skip commented lines based on major mode
     ((numerals-parser-is-commented-line-p line)
      (list :type 'text
            :line line))
     ;; Variable assignment
     ((string-match numerals-parser-assignment-regexp trimmed)
      (list :type 'assignment
            :variable (string-trim (match-string 1 trimmed))
            :expression (string-trim (match-string 2 trimmed))
            :line line))
     ;; Standalone calculation
     ((string-match numerals-parser-calculation-regexp trimmed)
      (list :type 'calculation
            :expression (string-trim (match-string 1 trimmed))
            :line line))
     ;; Regular text
     (t
      (list :type 'text
            :line line)))))

(defun numerals-parser-extract-variables (expression)
  "Extract all variable references from EXPRESSION.
Returns a list of variable names found in the expression."
  (let ((variables '())
        (pos 0))
    ;; Find all potential variable names in the expression
    (while (string-match numerals-parser-variable-name-regexp expression pos)
      (let ((var (match-string 0 expression))
            (start (match-beginning 0))
            (end (match-end 0)))
        ;; Check if this is actually a variable reference (not part of a number)
        (when (and (or (= start 0)
                       (not (string-match-p "[0-9.]" (substring expression (1- start) start))))
                   (or (= end (length expression))
                       (not (string-match-p "[0-9]" (substring expression end (min (1+ end) (length expression)))))))
          (push var variables))
        ;; Always advance pos to avoid infinite loop
        (setq pos (max (1+ pos) end))))
    (delete-dups (nreverse variables))))


(provide 'numerals-parser)
;;; numerals-parser.el ends here