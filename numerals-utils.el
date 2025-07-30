;;; numerals-utils.el --- Utility functions for numerals-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: calc, convenience
;; Version: 0.1.0

;; This file is part of numerals-mode.

;;; Commentary:

;; This module provides common utility functions used across numerals-mode.
;; It consolidates shared functionality to reduce code duplication and
;; improve maintainability.

;;; Code:

(require 'cl-lib)

;;; Cell Reference Utilities

(defun numerals-utils-column-letter-to-number (letters)
  "Convert column LETTERS (e.g., \"A\", \"AB\") to column number.
Column numbers are 1-indexed, so A=1, B=2, etc."
  (let ((result 0))
    (dolist (char (string-to-list (upcase letters)))
      (setq result (+ (* result 26) (- char ?A) 1)))
    result))

(defun numerals-utils-parse-cell-reference (ref)
  "Parse a cell reference REF like \"A1\", \"B2\", etc.
Returns a plist with :row and :col (1-indexed), or nil if invalid."
  (when (string-match "^\\([A-Z]+\\)\\([0-9]+\\)$" ref)
    (let ((col (numerals-utils-column-letter-to-number (match-string 1 ref)))
          (row (string-to-number (match-string 2 ref))))
      (list :row row :col col))))

;;; String Utilities

(defun numerals-utils-extract-numbers (values)
  "Extract numeric values from a list of string VALUES.
Returns a list of numbers, skipping non-numeric values."
  (delq nil
        (mapcar (lambda (v)
                  (when (and (stringp v)
                             (string-match "^[ \t]*\\(-?[0-9]+\\.?[0-9]*\\)" v))
                    (string-to-number (match-string 1 v))))
                values)))

(defun numerals-utils-is-numeric-string-p (str)
  "Return non-nil if STR contains only digits, dots, and minus signs."
  (and (stringp str)
       (string-match-p "^[ \t]*-?[0-9]+\\.?[0-9]*[ \t]*$" str)))

;;; Error Handling Utilities

(defconst numerals-utils-error-zero "0"
  "Standard error return value for numeric calculations.")

(defconst numerals-utils-error-text "Error"
  "Standard error return value for text operations.")

(defun numerals-utils-safe-calc (func &rest args)
  "Safely call FUNC with ARGS, returning standard error value on failure."
  (condition-case err
      (apply func args)
    (error
     (message "Calculation error: %s" (error-message-string err))
     numerals-utils-error-zero)))

;;; Buffer Position Utilities

(defun numerals-utils-line-bounds ()
  "Return cons cell of (line-beginning-position . line-end-position)."
  (cons (line-beginning-position) (line-end-position)))

(defun numerals-utils-safe-buffer-substring (start end)
  "Safely extract buffer substring from START to END.
Returns empty string if positions are invalid."
  (condition-case nil
      (when (and (<= start (point-max))
                 (<= end (point-max))
                 (>= start (point-min))
                 (>= end (point-min))
                 (<= start end))
        (buffer-substring start end))
    (error "")))

;;; Regex Compilation Cache

(defvar numerals-utils--regex-cache (make-hash-table :test 'equal)
  "Cache for compiled regular expressions.")

(defun numerals-utils-cached-regex (pattern)
  "Return cached compiled regex for PATTERN, compiling if necessary."
  (or (gethash pattern numerals-utils--regex-cache)
      (puthash pattern pattern numerals-utils--regex-cache)))

;;; Collection Utilities

(defun numerals-utils-flatten (list)
  "Flatten nested LIST structure into a single-level list."
  (cond
   ((null list) nil)
   ((atom list) (list list))
   (t (append (numerals-utils-flatten (car list))
              (numerals-utils-flatten (cdr list))))))

(defun numerals-utils-partition (predicate list)
  "Partition LIST into two lists based on PREDICATE.
Returns (MATCHING . NON-MATCHING)."
  (let ((matching '())
        (non-matching '()))
    (dolist (item list)
      (if (funcall predicate item)
          (push item matching)
        (push item non-matching)))
    (cons (nreverse matching) (nreverse non-matching))))

(provide 'numerals-utils)
;;; numerals-utils.el ends here