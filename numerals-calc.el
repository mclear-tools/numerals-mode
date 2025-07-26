;;; numerals-calc.el --- Calculator integration for numerals-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: calc, convenience
;; Version: 0.1.0

;; This file is part of numerals-mode.

;;; Commentary:

;; This module provides calculator functionality for numerals-mode.
;; It integrates with Emacs calc to evaluate mathematical expressions.

;;; Code:

(require 'calc)
(require 'calc-ext)

(defun numerals-calc-substitute-variables (expression variables)
  "Substitute VARIABLES in EXPRESSION with their values.
VARIABLES is an alist of (name . value) pairs.
Returns the expression with variables replaced by their values."
  (let ((result expression))
    ;; Sort variables by length (longest first) to avoid partial replacements
    (dolist (var (sort variables
                       (lambda (a b)
                         (> (length (car a)) (length (car b))))))
      (let ((name (car var))
            (value (cdr var)))
        ;; Use word boundaries to ensure we don't replace partial matches
        (setq result (replace-regexp-in-string
                      (concat "\\<" (regexp-quote name) "\\>")
                      (format "%s" value)
                      result))))
    result))

(defun numerals-calc-evaluate (expression &optional variables)
  "Evaluate mathematical EXPRESSION using calc.
Optional VARIABLES is an alist of (name . value) pairs.
Returns the calculated result as a string, or nil on error."
  (condition-case err
      (let* ((substituted (if variables
                              (numerals-calc-substitute-variables expression variables)
                            expression))
             (calc-eval-result (calc-eval substituted)))
        ;; Check if calc-eval returned an error
        (if (string-match-p "^\\(Error\\|\\*\\)" calc-eval-result)
            nil
          calc-eval-result))
    (error
     (message "Calculation error: %s" (error-message-string err))
     nil)))

(defun numerals-calc-format-result (result)
  "Format the calculation RESULT for display.
Handles number formatting and error display."
  (cond
   ((null result) "Error")
   ((string-match-p "\\." result)
    ;; Format decimal numbers nicely
    (let ((num (string-to-number result)))
      (if (= (floor num) num)
          (format "%.0f" num)
        (format "%.2f" num))))
   (t result)))

(defun numerals-calc-validate-number (value)
  "Validate that VALUE represents a valid number.
Returns the normalized number string or nil if invalid."
  (when (stringp value)
    (let ((trimmed (string-trim value)))
      (when (string-match-p "^-?[0-9]+\\(\\.[0-9]+\\)?$" trimmed)
        trimmed))))

(provide 'numerals-calc)
;;; numerals-calc.el ends here