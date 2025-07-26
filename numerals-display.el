;;; numerals-display.el --- Display functionality for numerals-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: calc, convenience
;; Version: 0.1.0

;; This file is part of numerals-mode.

;;; Commentary:

;; This module handles the visual display of calculation results
;; using overlays in numerals-mode.

;;; Code:

(defface numerals-result-face
  '((t :foreground "gray60"))
  "Face for displaying calculation results."
  :group 'numerals)

(defface numerals-calculated-face
  '((t :foreground "dark green" :weight bold))
  "Face for displaying calculated results (non-literal values)."
  :group 'numerals)

(defface numerals-error-face
  '((t :foreground "red" :weight bold))
  "Face for displaying calculation errors."
  :group 'numerals)

(defvar-local numerals-display-overlays nil
  "List of overlays created by numerals-mode.")

(defun numerals-display-result (pos result &optional error-p face-override)
  "Display RESULT at position POS using an overlay.
If ERROR-P is non-nil, display as an error.
FACE-OVERRIDE can specify a specific face to use."
  (let* ((line-end (save-excursion
                     (goto-char pos)
                     (line-end-position)))
         (overlay (make-overlay line-end line-end))
         (face (cond (error-p 'numerals-error-face)
                     (face-override face-override)
                     (t 'numerals-result-face)))
         (text (concat " => " result)))
    ;; Configure the overlay
    (overlay-put overlay 'after-string
                 (propertize text 'face face))
    (overlay-put overlay 'numerals-overlay t)
    ;; Add to our list for cleanup
    (push overlay numerals-display-overlays)))

(defun numerals-display-clear-line (pos)
  "Clear any numerals overlays on the line containing POS."
  (let ((overlays-to-remove nil))
    (dolist (overlay numerals-display-overlays)
      (when (and (overlay-buffer overlay)
                 (>= (overlay-start overlay) (save-excursion
                                               (goto-char pos)
                                               (line-beginning-position)))
                 (<= (overlay-start overlay) (save-excursion
                                               (goto-char pos)
                                               (line-end-position))))
        (push overlay overlays-to-remove)))
    ;; Remove the overlays
    (dolist (overlay overlays-to-remove)
      (delete-overlay overlay)
      (setq numerals-display-overlays
            (delq overlay numerals-display-overlays)))))

(defun numerals-display-clear-all ()
  "Clear all numerals overlays in the current buffer."
  (dolist (overlay numerals-display-overlays)
    (when (overlay-buffer overlay)
      (delete-overlay overlay)))
  (setq numerals-display-overlays nil))

(defun numerals-display-update-line (pos parse-result variables)
  "Update the display for a line at POS with PARSE-RESULT.
VARIABLES is the current variable table."
  (numerals-display-clear-line pos)
  (let ((type (plist-get parse-result :type)))
    (cond
     ((eq type 'assignment)
      (let* ((var-name (plist-get parse-result :variable))
             (value (gethash var-name variables)))
        (when value
          (numerals-display-result pos value))))
     ((eq type 'calculation)
      (let* ((expression (plist-get parse-result :expression))
             (calc-module (require 'numerals-calc nil t))
             (result (when calc-module
                       (numerals-calc-evaluate expression 
                                               (numerals-variables-get-all)))))
        (if result
            (numerals-display-result pos 
                                     (numerals-calc-format-result result))
          (numerals-display-result pos "Error" t)))))))

(provide 'numerals-display)
;;; numerals-display.el ends here