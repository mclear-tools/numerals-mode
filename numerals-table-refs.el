;;; numerals-table-refs.el --- Cross-table references for numerals-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: calc, convenience, tables
;; Version: 0.1.0

;; This file is part of numerals-mode.

;;; Commentary:

;; This module provides cross-table reference support using org-mode's
;; native table lookup mechanisms. Supports TableName.CellRef syntax that
;; maps directly to org-mode table cells.

;;; Code:

(require 'cl-lib)

;;; Table Reference System

(defun numerals-simple-table-refs-substitute (expression)
  "Replace table references in EXPRESSION with their values.
Handles TableName.CellRef format (e.g., Budget.E24, Budget.TOTALS[0])."
  (let ((result expression))
    ;; Handle TableName.TOTALS[N] format - convert to actual cell reference
    (setq result (replace-regexp-in-string
                  "\\b\\([A-Za-z_][A-Za-z0-9_]*\\)\\.TOTALS\\[\\([0-9]+\\)\\]"
                  (lambda (match)
                    (condition-case err
                      (when (string-match "\\b\\([A-Za-z_][A-Za-z0-9_]*\\)\\.TOTALS\\[\\([0-9]+\\)\\]" match)
                        (let* ((table-name (match-string 1 match))
                               (col-num (string-to-number (match-string 2 match)))
                               (value (numerals-simple-get-totals-value table-name col-num)))
                          (if (and value (stringp value) (string-match-p "^[0-9.-]+$" value))
                              value
                            match)))
                      (error
                       (message "Debug TOTALS substitution ERROR: %s in %s" (error-message-string err) match)
                       match)))
                  result))
    
    ;; Handle TableName.CellRef format (e.g., Budget.E24)
    (setq result (replace-regexp-in-string
                  "\\b\\([A-Za-z_][A-Za-z0-9_]*\\)\\.\\([A-Z]+[0-9]+\\)\\b"
                  (lambda (match)
                    ;; Use save-match-data to prevent match data corruption from nested functions
                    (save-match-data
                      (when (string-match "\\b\\([A-Za-z_][A-Za-z0-9_]*\\)\\.\\([A-Z]+[0-9]+\\)\\b" match)
                        (let* ((table-name (match-string 1 match))
                               (cell-ref (match-string 2 match))
                               (value (numerals-simple-get-cell-value table-name cell-ref)))
                          (if (and value (stringp value) (string-match-p "^[0-9.-]+$" value))
                              value
                            match)))))
                  result))
    result))

(defun numerals-simple-get-cell-value (table-name cell-ref)
  "Get the value from CELL-REF in TABLE-NAME using org-mode table lookup."
  (condition-case err
      (save-excursion
        (goto-char (point-min))
        ;; Find the named table
        (when (re-search-forward (format "^#\\+NAME:[ \t]+%s" (regexp-quote table-name)) nil t)
          (forward-line 1)
          ;; Make sure we're at a table
          (when (looking-at "^[ \t]*|")
            ;; Parse the cell reference (e.g., "E24" -> row 24, col E)
            (when (string-match "^\\([A-Z]+\\)\\([0-9]+\\)$" cell-ref)
              (let* ((col-letter (match-string 1 cell-ref))
                     (row-num (string-to-number (match-string 2 cell-ref)))
                     (col-num (numerals-simple-column-to-number col-letter))
                     (table-start (point)))
                ;; Find the specific cell and get its overlay value
                (numerals-simple-find-cell-value table-start row-num col-num))))))
    (error
     (message "Cell lookup error for %s.%s: %s" table-name cell-ref (error-message-string err))
     nil)))

(defun numerals-simple-get-totals-value (table-name col-num)
  "Get the COL-NUM'th calculated value from the TOTALS row of TABLE-NAME."
  (condition-case err
      (save-excursion
        (goto-char (point-min))
        ;; Find the named table
        (when (re-search-forward (format "^#\\+NAME:[ \t]+%s" (regexp-quote table-name)) nil t)
          (forward-line 1)
          ;; Find TOTALS row
          (when (re-search-forward "^[ \t]*|.*TOTALS.*|" nil t)
            (beginning-of-line)
            ;; Get all numeric overlays in this row, sorted by position
            (let ((overlays-found '())
                  (line-start (line-beginning-position))
                  (line-end (line-end-position)))
              (dolist (overlay (overlays-in line-start line-end))
                (when (overlay-get overlay 'numerals-overlay)
                  (let ((display-text (overlay-get overlay 'display)))
                    (when (and display-text 
                               (stringp display-text)
                               (string-match-p "^[ \t]*[0-9.-]+[ \t]*$" display-text))
                      (push (cons (overlay-start overlay) (string-trim display-text)) 
                            overlays-found)))))
              ;; Sort by position and return the requested column
              (setq overlays-found (sort overlays-found (lambda (a b) (< (car a) (car b)))))
              (if (< col-num (length overlays-found))
                  (cdr (nth col-num overlays-found))
                (progn
                  (message "TOTALS column %d not found (only %d available)" col-num (length overlays-found))
                  nil))))))
    (error
     (message "TOTALS lookup error: %s" (error-message-string err))
     nil)))

(defun numerals-simple-find-cell-value (table-start row-num col-num)
  "Find the value at ROW-NUM, COL-NUM starting from TABLE-START."
  (condition-case err
      (save-excursion
        (goto-char table-start)
        (let ((current-row 0)
              (found-target nil))
          ;; Skip to the target row
          (while (and (< current-row row-num) 
                      (not (eobp))
                      (looking-at "^[ \t]*|"))
            ;; Skip separator rows
            (let ((line-content (buffer-substring (line-beginning-position) (line-end-position))))
              (unless (string-match-p "^[ \t]*|[ \t:+-]+|" line-content)
                (setq current-row (1+ current-row))
                ;; Check if we've reached the target row
                (when (= current-row row-num)
                  (setq found-target t))))
            ;; Only advance if we haven't found the target and there are more table rows
            (unless found-target
              (forward-line 1)))
          
          ;; Return result only if we found the target row and we're still in a table row
          (if (and found-target (= current-row row-num) (looking-at "^[ \t]*|"))
              (numerals-simple-get-cell-overlay-at-column col-num)
            (progn
              (message "Target row %d not found in table (only found %d rows)" row-num current-row)
              nil))))
    (error
     (message "Cell lookup error: %s" (error-message-string err))
     nil)))

(defun numerals-simple-get-cell-overlay-at-column (col-num)
  "Get the overlay value at COL-NUM in the current table row."
  (condition-case err
      (save-excursion
        (beginning-of-line)
        (let ((current-col 0)
              (line-end (line-end-position)))
          ;; Skip the first |
          (when (re-search-forward "|" line-end t)
            (setq current-col 1)
            ;; Move to the target column
            (while (and (< current-col col-num)
                        (re-search-forward "|" line-end t))
              (setq current-col (1+ current-col)))
            
            ;; Now we're at the start of the target column
            (when (= current-col col-num)
              (let* ((cell-start (point))
                     (cell-end (if (re-search-forward "|" line-end t)
                                   (1- (point))
                                 line-end))
                     (cell-text (buffer-substring cell-start cell-end)))
                ;; Check bounds before calling overlays-in
                (if (and (<= cell-start (buffer-size)) 
                         (<= cell-end (buffer-size))
                         (>= cell-start 1)
                         (>= cell-end 1)
                         (<= cell-start cell-end))
                    ;; Look for numerals overlay in this cell
                    (let ((found-value nil)
                          (overlays-found '()))
                      (dolist (overlay (overlays-in cell-start cell-end))
                        (when (overlay-get overlay 'numerals-overlay)
                          (let ((display-text (overlay-get overlay 'display)))
                            (push (cons (overlay-start overlay) display-text) overlays-found)
                            (when (stringp display-text)
                              (setq found-value (string-trim display-text))))))
                      found-value)
                  (progn
                    (message "Invalid cell bounds - start=%d end=%d buffer-size=%d" 
                             cell-start cell-end (buffer-size))
                    nil)))))))
    (error
     (message "Cell overlay error: %s" (error-message-string err))
     nil)))

(defun numerals-simple-column-to-number (col-letters)
  "Convert column letters (A, B, AA, etc.) to column number."
  (let ((result 0))
    (dolist (char (string-to-list (upcase col-letters)))
      (setq result (+ (* result 26) (- char ?A) 1)))
    result))

;; Integration function
(defun numerals-table-refs-substitute (expression variables)
  "Substitute table references in EXPRESSION (simple version).
VARIABLES parameter ignored in this simple implementation."
  (numerals-simple-table-refs-substitute expression))

(provide 'numerals-table-refs)
;;; numerals-table-refs.el ends here