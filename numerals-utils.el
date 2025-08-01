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
Returns a list of numbers, skipping non-numeric values.
Handles comma-formatted numbers like \"1,234\" or \"1,234.56\"."
  (delq nil
        (mapcar (lambda (v)
                  (when (stringp v)
                    (let ((stripped (numerals-utils-strip-commas v)))
                      (when (string-match "^[ \t]*\\(-?[0-9]+\\.?[0-9]*\\)" stripped)
                        (string-to-number (match-string 1 stripped))))))
                values)))

(defun numerals-utils-is-numeric-string-p (str)
  "Return non-nil if STR is a valid numeric string.
Accepts formats like \"123\", \"1,234\", \"1,234.56\", \"-1,000\"."
  (and (stringp str)
       (or
        ;; Original pattern for numbers without commas
        (string-match-p "^[ \t]*-?[0-9]+\\.?[0-9]*[ \t]*$" str)
        ;; New pattern for numbers with commas (properly escaped)
        ;; Must have exactly 3 digits after each comma, and decimal part (if any) must be valid
        (string-match-p "^[ \t]*-?[0-9]\\{1,3\\}\\(,[0-9]\\{3\\}\\)*\\(\\.[0-9]+\\)?[ \t]*$" str))))

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

;;; Number Formatting Utilities

(defun numerals-utils-strip-commas (str)
  "Remove commas from numeric string STR.
Handles cases like \"1,000\" -> \"1000\" and \"1,234.56\" -> \"1234.56\"."
  (if (stringp str)
      (replace-regexp-in-string "," "" str)
    str))

(defun numerals-utils-format-number-with-commas (number)
  "Format NUMBER with commas for thousands separators.
NUMBER can be a string or numeric value.
Examples: 1000 -> \"1,000\", 1234567.89 -> \"1,234,567.89\"."
  (let* ((str (if (numberp number)
                  (number-to-string number)
                number))
         ;; Split into integer and decimal parts
         (parts (split-string str "\\."))
         (integer-part (car parts))
         (decimal-part (cadr parts))
         ;; Handle negative numbers
         (negative-p (string-match-p "^-" integer-part))
         (abs-integer (if negative-p
                          (substring integer-part 1)
                        integer-part))
         ;; Add commas to integer part
         (formatted-integer
          (let ((len (length abs-integer)))
            (if (<= len 3)
                abs-integer
              (let ((result "")
                    (idx 0))
                (while (< idx len)
                  (let ((remaining (- len idx)))
                    (if (and (> idx 0) (= (mod remaining 3) 0))
                        (setq result (concat result ",")))
                    (setq result (concat result (substring abs-integer idx (1+ idx))))
                    (setq idx (1+ idx))))
                result)))))
    ;; Reconstruct the number
    (concat (if negative-p "-" "")
            formatted-integer
            (if decimal-part (concat "." decimal-part) ""))))

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