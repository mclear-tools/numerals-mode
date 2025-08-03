;;; numerals-export.el --- Export integration for numerals-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: calc, convenience, export
;; Version: 0.1.0

;; This file is part of numerals-mode.

;;; Commentary:

;; This module provides export integration for numerals-mode.
;; It hooks into org-mode's export process to substitute overlay
;; calculations with actual values in the exported content.

;;; Code:

(require 'numerals-utils)
(require 'numerals-parser)

;;; Export Integration

(defvar numerals-export-substitute-overlays t
  "Whether to substitute numerals overlays during export.
When non-nil, overlays showing calculated results will be substituted
into the exported content. Set to nil to export raw expressions only.")

(defun numerals-export-should-substitute-overlay-p (overlay)
  "Return non-nil if OVERLAY should be substituted during export.
This function determines the substitution logic:
- Table formula overlays: always substitute (replace formula with result)
- Variable assignment overlays: substitute only if it's a calculation, not a literal
- Standalone calculation overlays: always substitute"
  (when (and numerals-export-substitute-overlays
             (overlay-get overlay 'numerals-overlay))
    (let* ((start (overlay-start overlay))
           (end (overlay-end overlay))
           (display-text (overlay-get overlay 'display))
           (after-string (overlay-get overlay 'after-string)))
      (cond
       ;; Table formula overlay (has 'display property)
       (display-text t)
       ;; Variable/calculation overlay (has 'after-string property)
       (after-string
        (save-excursion
          (goto-char start)
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
                 (parse-result (numerals-parser-parse-line line))
                 (type (plist-get parse-result :type))
                 (expression (plist-get parse-result :expression)))
            (cond
             ;; For assignments, only substitute if it's not a literal
             ((eq type 'assignment)
              (let* ((dependencies (when expression 
                                     (numerals-parser-extract-variables expression)))
                     (is-literal (and expression
                                      (numerals-utils-is-numeric-string-p (string-trim expression))
                                      (null dependencies))))
                (not is-literal)))
             ;; For standalone calculations, always substitute
             ((eq type 'calculation) t)
             ;; Default: don't substitute
             (t nil)))))
       ;; Default: don't substitute
       (t nil)))))

(defun numerals-export-substitute-overlays (backend)
  "Replace numerals overlays with calculated values for export.
BACKEND is the export backend being used, as a symbol.
This function operates on a temporary copy of the buffer during export."
  (when (and numerals-mode numerals-export-substitute-overlays)
    (save-excursion
      (save-restriction
        (widen)
        ;; Process overlays from end to beginning to avoid position shifts
        (let ((overlays-to-process '()))
          ;; Collect overlays that should be substituted
          (dolist (overlay numerals-display-overlays)
            (when (and (overlay-buffer overlay)
                       (eq (overlay-buffer overlay) (current-buffer))
                       (numerals-export-should-substitute-overlay-p overlay))
              (push overlay overlays-to-process)))
          
          ;; Sort by position (end to beginning) to avoid position shifts
          (setq overlays-to-process
                (sort overlays-to-process
                      (lambda (a b) (> (overlay-start a) (overlay-start b)))))
          
          ;; Substitute each overlay
          (dolist (overlay overlays-to-process)
            (numerals-export-substitute-single-overlay overlay)))))))

(defun numerals-export-substitute-single-overlay (overlay)
  "Substitute a single OVERLAY with its calculated content.
This function modifies the buffer content directly."
  (let* ((start (overlay-start overlay))
         (end (overlay-end overlay))
         (display-text (overlay-get overlay 'display))
         (after-string (overlay-get overlay 'after-string)))
    (cond
     ;; Table formula overlay - replace entire cell content with result
     (display-text
      (goto-char start)
      (delete-region start end)
      (insert (substring-no-properties display-text)))
     
     ;; Variable/calculation overlay - append result to line
     (after-string
      (goto-char end)
      (insert (substring-no-properties after-string))))))

;;; Hook Integration

(defun numerals-export-enable ()
  "Enable numerals export integration.
Adds hooks to org-mode's export process."
  (add-hook 'org-export-before-processing-functions 
            #'numerals-export-substitute-overlays))

(defun numerals-export-disable ()
  "Disable numerals export integration.
Removes hooks from org-mode's export process."
  (remove-hook 'org-export-before-processing-functions 
               #'numerals-export-substitute-overlays))

;;; User Commands

(defun numerals-export-toggle-substitution ()
  "Toggle overlay substitution during export."
  (interactive)
  (setq numerals-export-substitute-overlays 
        (not numerals-export-substitute-overlays))
  (message "Numerals export substitution: %s" 
           (if numerals-export-substitute-overlays "enabled" "disabled")))

(defun numerals-export-preview-substitutions ()
  "Preview what substitutions would be made during export.
Shows a list of overlays that would be substituted."
  (interactive)
  (if (not numerals-mode)
      (message "Numerals-mode is not active in this buffer")
    (let ((substitutions '())
          (total-overlays 0))
      (dolist (overlay numerals-display-overlays)
        (setq total-overlays (1+ total-overlays))
        (when (numerals-export-should-substitute-overlay-p overlay)
          (let* ((start (overlay-start overlay))
                 (end (overlay-end overlay))
                 (line-num (line-number-at-pos start))
                 (display-text (overlay-get overlay 'display))
                 (after-string (overlay-get overlay 'after-string))
                 (substitution-text (or display-text after-string)))
            (push (format "Line %d: %s" line-num 
                          (substring-no-properties substitution-text))
                  substitutions))))
      
      (if substitutions
          (let ((buf (get-buffer-create "*Numerals Export Preview*")))
            (with-current-buffer buf
              (erase-buffer)
              (insert (format "Export substitutions for %s:\n\n" (buffer-name)))
              (insert (format "Total overlays: %d\n" total-overlays))
              (insert (format "Will substitute: %d\n\n" (length substitutions)))
              (dolist (sub (nreverse substitutions))
                (insert sub "\n"))
              (goto-char (point-min)))
            (display-buffer buf))
        (message "No overlays would be substituted during export")))))

(provide 'numerals-export)
;;; numerals-export.el ends here