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

(defface numerals-variable-face
  '((t :weight bold))
  "Face for displaying variable names (bold)."
  :group 'numerals)

(defvar-local numerals-display-overlays nil
  "List of overlays created by numerals-mode.")


(defun numerals-display-result (pos result &optional error-p face-override)
  "Display RESULT at position POS using an overlay.
If ERROR-P is non-nil, display as an error.
FACE-OVERRIDE can specify a specific face to use."
  (condition-case err
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
        (push overlay numerals-display-overlays))
    (error
     (message "Display result error at pos %d: %s" pos (error-message-string err)))))

(defun numerals-display-bold-variable-name (pos)
  "Add bold formatting to the variable name at the beginning of the line containing POS."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    ;; Look for variable assignment pattern: "VarName = ..."
    (when (looking-at "^\\s-*\\([A-Za-z_][A-Za-z0-9_ ]*\\)\\s-*=")
      (let* ((var-start (match-beginning 1))
             (var-end (match-end 1))
             (var-name (match-string 1))
             (overlay (make-overlay var-start var-end)))
        ;; Apply bold face to the variable name
        (overlay-put overlay 'face 'numerals-variable-face)
        (overlay-put overlay 'numerals-overlay t)
        ;; Add to our list for cleanup
        (push overlay numerals-display-overlays)))))

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


(provide 'numerals-display)
;;; numerals-display.el ends here