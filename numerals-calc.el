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
(require 'cl-lib)

;; Add current directory to load-path for utils loading
(when load-file-name
  (add-to-list 'load-path (file-name-directory load-file-name)))

;; Try to require numerals-utils, but don't fail if not found
(condition-case nil
    (require 'numerals-utils)
  (error nil))

;; Ensure calc is properly initialized
(unless (featurep 'calc-aent)
  (require 'calc-aent))

;; Forward declaration to avoid circular dependency
(declare-function numerals-table-refs-substitute "numerals-table-refs")

(defun numerals-calc-substitute-variables (expression variables)
  "Substitute VARIABLES in EXPRESSION with their values.
VARIABLES is an alist of (name . value) pairs.
Returns the expression with variables replaced by their values."
  ;; Ensure numerals-utils is loaded
  (unless (fboundp 'numerals-utils-strip-commas)
    (require 'numerals-utils))
  (if (null variables)
      expression
    (let ((result expression)
          ;; Sort a copy to avoid modifying the original list
          (sorted-vars (cl-sort (copy-sequence variables)
                               (lambda (a b)
                                 (> (length (car a)) (length (car b)))))))
      ;; Process variables from longest name to shortest to avoid partial matches
      (dolist (var sorted-vars)
        (let ((name (car var))
              (value (cdr var)))
          ;; Skip variables with nil values to avoid substituting "nil" as text
          (when value
            ;; Strip commas from the value before substitution
            (let ((clean-value (if (stringp value)
                                   (numerals-utils-strip-commas value)
                                 (format "%s" value))))
              (setq result (replace-regexp-in-string
                           (concat "\\<" (regexp-quote name) "\\>")
                           clean-value
                           result))))))
      ;; Also strip commas from any numeric literals in the expression
      (setq result (replace-regexp-in-string
                    "\\b\\([0-9]\\{1,3\\}\\(?:,[0-9]\\{3\\}\\)*\\(?:\\.[0-9]+\\)?\\)\\b"
                    (lambda (match)
                      (numerals-utils-strip-commas match))
                    result))
      result)))


(defun numerals-calc-evaluate-local (expression &optional variables)
  "Evaluate mathematical EXPRESSION using calc without table-refs substitution.
Optional VARIABLES is an alist of (name . value) pairs.
Returns a plist with :value (the result) and :error (error message if any)."
  ;; Ensure numerals-utils is loaded
  (unless (fboundp 'numerals-utils-strip-commas)
    (require 'numerals-utils))
  (condition-case err
      (let* (;; Skip table reference substitution for local table processing
             ;; Strip commas from numeric literals first
             (comma-stripped (replace-regexp-in-string
                              "\\b\\([0-9]\\{1,3\\}\\(?:,[0-9]\\{3\\}\\)*\\(?:\\.[0-9]+\\)?\\)\\b"
                              (lambda (match)
                                (numerals-utils-strip-commas match))
                              expression))
             ;; Then substitute variables
             (substituted (if (and variables (listp variables))
                              (numerals-calc-substitute-variables comma-stripped variables)
                            comma-stripped))
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

(defun numerals-calc-evaluate (expression &optional variables)
  "Evaluate mathematical EXPRESSION using calc.
Optional VARIABLES is an alist of (name . value) pairs.
Returns a plist with :value (the result) and :error (error message if any)."
  ;; Ensure numerals-utils is loaded
  (unless (fboundp 'numerals-utils-strip-commas)
    (require 'numerals-utils))
  (condition-case err
      (let* (;; First strip commas from numeric literals
             (comma-stripped (replace-regexp-in-string
                              "\\b\\([0-9]\\{1,3\\}\\(?:,[0-9]\\{3\\}\\)*\\(?:\\.[0-9]+\\)?\\)\\b"
                              (lambda (match)
                                (numerals-utils-strip-commas match))
                              expression))
             ;; Then substitute table references
             (table-substituted (condition-case table-err
                                    (if (fboundp 'numerals-table-refs-substitute)
                                        (numerals-table-refs-substitute comma-stripped variables)
                                      comma-stripped)
                                  (error
                                   (message "Table substitution error: %s" (error-message-string table-err))
                                   comma-stripped)))
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
  ;; Ensure numerals-utils is loaded
  (unless (fboundp 'numerals-utils-format-number-with-commas)
    (require 'numerals-utils))
  (cond
   ((null result) "Error")
   ((string-match-p "\\." result)
    ;; Format decimal numbers with up to 4 decimal places, removing trailing zeros
    (let* ((num (string-to-number result))
           (formatted (format "%.4f" num)))
      ;; Remove trailing zeros after decimal point
      (let ((cleaned
             (cond
              ;; If it ends with .0000, just remove the decimal point
              ((string-match "\\.0+$" formatted)
               (replace-match "" nil nil formatted))
              ;; If it has trailing zeros after other digits, remove them
              ((string-match "\\(\\.[0-9]*?[1-9]\\)0+$" formatted)
               (replace-match "\\1" nil nil formatted))
              (t formatted))))
        ;; Add comma formatting
        (numerals-utils-format-number-with-commas cleaned))))
   ;; Integer result - add comma formatting
   (t (numerals-utils-format-number-with-commas result))))


(provide 'numerals-calc)
;;; numerals-calc.el ends here