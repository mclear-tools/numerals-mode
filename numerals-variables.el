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

(defvar-local numerals-variables-dependencies nil
  "Alist tracking which variables depend on others.
Each element is (VAR . DEPENDENTS) where DEPENDENTS is a list
of variables that use VAR in their expressions.")

(defvar-local numerals-variables-expressions nil
  "Alist storing the original expressions for each variable.
Each element is (VAR . EXPRESSION).")

(defun numerals-variables-init ()
  "Initialize the variable storage for the current buffer."
  (setq numerals-variables-table (make-hash-table :test 'equal))
  (setq numerals-variables-dependencies nil)
  (setq numerals-variables-expressions nil))

(defun numerals-variables-set (name value expression dependencies)
  "Set variable NAME to VALUE with EXPRESSION and DEPENDENCIES.
DEPENDENCIES is a list of variable names that this variable depends on."
  ;; Store the value
  (puthash name value numerals-variables-table)
  
  ;; Store the expression
  (setf (alist-get name numerals-variables-expressions nil nil 'equal)
        expression)
  
  ;; Update dependency tracking
  (numerals-variables-update-dependencies name dependencies))

(defun numerals-variables-get (name)
  "Get the value of variable NAME.
Returns nil if the variable doesn't exist."
  (gethash name numerals-variables-table))

(defun numerals-variables-get-all ()
  "Return an alist of all variables and their values."
  (let ((result nil))
    (maphash (lambda (k v)
               (push (cons k v) result))
             numerals-variables-table)
    result))

(defun numerals-variables-update-dependencies (var new-deps)
  "Update dependency tracking for VAR with NEW-DEPS.
Removes old dependencies and adds new ones."
  ;; First, remove VAR from all existing dependency lists
  (dolist (dep-entry numerals-variables-dependencies)
    (let ((dep-var (car dep-entry))
          (dependents (cdr dep-entry)))
      (when (member var dependents)
        (setf (alist-get dep-var numerals-variables-dependencies nil nil 'equal)
              (delete var dependents)))))
  
  ;; Add VAR to the dependency lists of its new dependencies
  (dolist (dep new-deps)
    (let ((dependents (alist-get dep numerals-variables-dependencies nil nil 'equal)))
      (unless (member var dependents)
        (setf (alist-get dep numerals-variables-dependencies nil nil 'equal)
              (cons var dependents))))))

(defun numerals-variables-get-dependents (var)
  "Get all variables that depend on VAR."
  (alist-get var numerals-variables-dependencies nil nil 'equal))

(defun numerals-variables-get-expression (var)
  "Get the original expression for VAR."
  (alist-get var numerals-variables-expressions nil nil 'equal))

(defun numerals-variables-delete (name)
  "Delete variable NAME and clean up its dependencies."
  ;; Remove from hash table
  (remhash name numerals-variables-table)
  
  ;; Remove from expressions
  (setq numerals-variables-expressions
        (assoc-delete-all name numerals-variables-expressions))
  
  ;; Clean up dependencies
  (numerals-variables-update-dependencies name nil)
  
  ;; Remove as a dependency source
  (setq numerals-variables-dependencies
        (assoc-delete-all name numerals-variables-dependencies)))

(defun numerals-variables-get-all-dependents (var)
  "Get all variables that transitively depend on VAR.
Returns a list of variables in dependency order (deepest first)."
  (let ((visited '())
        (result '()))
    (numerals-variables--collect-dependents var visited result)
    result))

(defun numerals-variables--collect-dependents (var visited result)
  "Helper function to collect transitive dependents of VAR.
VISITED tracks already processed variables to avoid cycles.
RESULT accumulates the dependency-ordered list."
  (unless (member var visited)
    (push var visited)
    (let ((direct-deps (numerals-variables-get-dependents var)))
      (dolist (dep direct-deps)
        (numerals-variables--collect-dependents dep visited result))
      (dolist (dep direct-deps)
        (unless (member dep result)
          (push dep result))))
    result))

(defun numerals-variables-clear ()
  "Clear all variables and dependencies."
  (clrhash numerals-variables-table)
  (setq numerals-variables-dependencies nil)
  (setq numerals-variables-expressions nil))

(provide 'numerals-variables)
;;; numerals-variables.el ends here