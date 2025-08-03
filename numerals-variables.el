;;; numerals-variables.el --- Variable management for numerals-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: calc, convenience
;; Version: 0.1.0

;; This file is part of numerals-mode.

;;; Commentary:

;; This module manages variables and their dependencies for numerals-mode.
;; It tracks variable values and handles update propagation.

;;; Code:

(defvar-local numerals-variables-table nil
  "Hash table storing variable names and their values.")


(defun numerals-variables-init ()
  "Initialize the variable storage for the current buffer."
  (setq numerals-variables-table (make-hash-table :test 'equal)))

(defun numerals-variables-set (name value expression dependencies)
  "Set variable NAME to VALUE.
NAME is a string identifying the variable.
VALUE is the computed numeric value to store.
EXPRESSION is the original expression string (stored for reference).
DEPENDENCIES is a list of variable names this variable depends on."
  ;; Store the value
  (puthash name value numerals-variables-table))

(defun numerals-variables-get-all ()
  "Return an alist of all variables and their values."
  (let ((result nil))
    (maphash (lambda (k v)
               (push (cons k v) result))
             numerals-variables-table)
    result))

(defun numerals-variables-clear ()
  "Clear all variables and dependencies."
  (clrhash numerals-variables-table))

(provide 'numerals-variables)
;;; numerals-variables.el ends here