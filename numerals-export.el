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

(defvar numerals-export-overlay-cache (make-hash-table :test 'equal)
  "Global cache of overlay data for export substitution, keyed by buffer name.")

(defvar-local numerals-export-overlay-data nil
  "Cache of overlay data for export substitution.")

(defun numerals-export-cache-overlay-data ()
  "Cache overlay data for export substitution.
This must be called from the original buffer before export begins."
  (when (and (boundp 'numerals-mode) numerals-mode)
    (let ((buffer-key (buffer-name))
          (overlay-data '()))
      (dolist (overlay numerals-display-overlays)
        (when (and (overlay-buffer overlay)
                   (numerals-export-should-substitute-overlay-p overlay))
          (let* ((start (overlay-start overlay))
                 (end (overlay-end overlay))
                 (display-text (overlay-get overlay 'display))
                 (after-string (overlay-get overlay 'after-string))
                 (line-text (save-excursion
                              (goto-char start)
                              (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))))
            (push (list :start start
                        :end end  
                        :display-text display-text
                        :after-string after-string
                        :line-text line-text)
                  overlay-data))))
      (setq overlay-data (nreverse overlay-data))
      ;; Store in both local and global cache
      (setq numerals-export-overlay-data overlay-data)
      (puthash buffer-key overlay-data numerals-export-overlay-cache))))

(defun numerals-export-substitute-overlays (backend)
  "Replace numerals overlays with calculated values for export.
BACKEND is the export backend being used, as a symbol.
This function operates on a temporary copy of the buffer during export."
  (when numerals-export-substitute-overlays
    (let* ((current-buffer-name (buffer-name))
           ;; Try to find original buffer name by removing copy suffix
           (original-buffer-name (if (string-match "\\(.*\\)<[0-9]+>$" current-buffer-name)
                                     (match-string 1 current-buffer-name)
                                   current-buffer-name))
           (cached-overlays (or numerals-export-overlay-data
                                (gethash original-buffer-name numerals-export-overlay-cache))))
      (when cached-overlays
        (save-excursion
          (save-restriction
            (widen)
            ;; Process cached overlay data from end to beginning to avoid position shifts
            (let ((sorted-overlays (sort (copy-sequence cached-overlays)
                                         (lambda (a b) 
                                           (> (plist-get a :start) (plist-get b :start))))))
              (dolist (overlay-data sorted-overlays)
                (numerals-export-substitute-from-data overlay-data)))))))))

(defun numerals-export-substitute-from-data (overlay-data)
  "Substitute overlay content using cached OVERLAY-DATA.
This function searches for the line content and substitutes accordingly."
  (let* ((line-text (plist-get overlay-data :line-text))
         (display-text (plist-get overlay-data :display-text))
         (after-string (plist-get overlay-data :after-string)))
    (save-excursion
      (goto-char (point-min))
      ;; Search for the exact line content
      (when (search-forward line-text nil t)
        (cond
         ;; Table formula overlay - find and replace the formula in the line
         (display-text
          (let ((line-start (line-beginning-position))
                (line-end (line-end-position)))
            ;; Look for formulas in this line and replace them
            (goto-char line-start)
            (while (re-search-forward "=\\([^|]+\\)" line-end t)
              (let* ((formula-start (match-beginning 0))
                     (formula-end (match-end 0))
                     (formula-content (match-string 0)))
                ;; Replace the formula with the calculated result
                (goto-char formula-start)
                (delete-region formula-start formula-end)
                (insert (substring-no-properties display-text))
                ;; Update line-end position after replacement
                (setq line-end (+ line-end (- (length display-text) 
                                              (length formula-content))))))))
         
         ;; Variable/calculation overlay - append result to end of line
         (after-string
          (end-of-line)
          (insert (substring-no-properties after-string))))))))

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

(defun numerals-export-prepare-for-export (backend)
  "Prepare overlay data for export by caching it.
This hook runs very early in the export process while overlays are still available."
  (when (and (boundp 'numerals-mode) numerals-mode)
    (numerals-export-cache-overlay-data)))

(defun numerals-export-pre-export-advice (orig-fun &rest args)
  "Advice to cache overlay data before export begins.
This runs in the original buffer context before any copying occurs."
  (when (and (boundp 'numerals-mode) numerals-mode)
    (numerals-export-cache-overlay-data))
  (apply orig-fun args))

(defun numerals-export-enable ()
  "Enable numerals export integration.
Adds hooks to org-mode's export process and advice to export functions."
  ;; Add advice to export functions to cache data in original buffer
  (advice-add 'org-export-as :around #'numerals-export-pre-export-advice)
  (advice-add 'org-export-to-file :around #'numerals-export-pre-export-advice)
  ;; Substitute overlays during export processing  
  (add-hook 'org-export-before-processing-functions 
            #'numerals-export-substitute-overlays))

(defun numerals-export-disable ()
  "Disable numerals export integration.
Removes hooks from org-mode's export process and advice from export functions."
  ;; Remove advice from export functions
  (advice-remove 'org-export-as #'numerals-export-pre-export-advice)
  (advice-remove 'org-export-to-file #'numerals-export-pre-export-advice)
  ;; Remove export hook
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