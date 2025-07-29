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

;; Ensure calc is properly initialized
(unless (featurep 'calc-aent)
  (require 'calc-aent))

;; Forward declaration to avoid circular dependency
(declare-function numerals-table-refs-substitute "numerals-table-refs-simple")

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
        ;; Skip variables with nil values to avoid substituting "nil" as text
        (if value
            (progn
              (setq result (replace-regexp-in-string
                            (concat "\\<" (regexp-quote name) "\\>")
                            (format "%s" value)
                            result))))))
    result))


(defun numerals-calc-evaluate (expression &optional variables)
  "Evaluate mathematical EXPRESSION using calc.
Optional VARIABLES is an alist of (name . value) pairs.
Returns a plist with :value (the result) and :error (error message if any)."
  (condition-case err
      (let* (;; First substitute table references
             (table-substituted (condition-case table-err
                                    (if (fboundp 'numerals-table-refs-substitute)
                                        (numerals-table-refs-substitute expression variables)
                                      expression)
                                  (error
                                   (message "Table substitution error: %s" (error-message-string table-err))
                                   expression)))
             ;; Then substitute variables
             (substituted (if variables
                              (numerals-calc-substitute-variables table-substituted variables)
                            table-substituted))
             ;; Ensure calc is in a clean state
             (calc-language nil)
             (calc-algebraic-mode t)
             (calc-eval-result (with-timeout (1.0 "*Timeout*")
                                 (calc-eval substituted))))
        ;; Ensure calc-eval-result is a string
        (unless (stringp calc-eval-result)
          (setq calc-eval-result (format "%s" calc-eval-result)))
        ;; Check if calc-eval timed out
        (cond
         ((equal calc-eval-result "*Timeout*")
          (list :value nil :error "Calculation timeout"))
         ;; Check if calc-eval returned an error
         ((string-match "^\\*Error: \\(.+\\)" calc-eval-result)
          (list :value nil :error (match-string 1 calc-eval-result)))
         ((string-match-p "^\\(Error\\|\\*\\)" calc-eval-result)
          (list :value nil :error calc-eval-result))
         ;; Check for division by zero and other calc errors
         ((string-match-p "\\(/[ ]*0\\|inf\\|nan\\)" calc-eval-result)
          (list :value nil :error "Division by zero"))
         ;; Check if result contains undefined variables  
         ((string-match-p "[A-Za-z]" calc-eval-result)
          (list :value nil :error "Undefined variable"))
         (t
          (list :value calc-eval-result :error nil))))
    (error
     (message "Calculation error: %s" (error-message-string err))
     (list :value nil :error (error-message-string err)))))

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