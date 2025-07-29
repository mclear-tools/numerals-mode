;;; numerals-mode.el --- Minor mode for literate calculations -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: calc, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; numerals-mode provides literate calculation functionality similar to
;; Obsidian's Numerals plugin.  It allows you to write calculations in
;; plain text with automatic variable references and live updates.

;; Example usage:
;;   Monthly Rent = 2350.00              => Monthly Rent: 2350
;;   Taxes = 3560.22                     => Taxes: 3560.22
;;   Total = Monthly Rent + Taxes        => Total: 5910.22

;;; Code:

;; Add the directory containing this file to load-path
(when load-file-name
  (add-to-list 'load-path (file-name-directory load-file-name)))

(require 'numerals-parser)
(require 'numerals-calc)
(require 'numerals-table-refs)
(require 'numerals-variables)
(require 'numerals-display)
(require 'numerals-tables)

(defgroup numerals nil
  "Literate calculation mode."
  :group 'convenience
  :prefix "numerals-")

(defvar numerals-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'numerals-recalculate)
    (define-key map (kbd "C-c C-k") 'numerals-clear)
    (define-key map (kbd "C-c C-t") 'numerals-toggle-overlays)
    (define-key map (kbd "C-c C-l") 'numerals-toggle-overlay-at-point)
    map)
  "Keymap for numerals-mode.")



;;;###autoload
(define-minor-mode numerals-mode
  "Toggle Numerals mode for literate calculations.
When Numerals mode is enabled, lines containing calculations
are automatically evaluated and results are displayed as overlays.

Key bindings:
\\{numerals-mode-map}"
  :lighter " Num"
  :keymap numerals-mode-map
  :group 'numerals
  (if numerals-mode
      (numerals-mode-enable)
    (numerals-mode-disable)))

(defun numerals-mode-enable ()
  "Enable numerals-mode in the current buffer."
  ;; Initialize variables
  (numerals-variables-init)
  ;; Disable org-mode table calculations if in org-mode
  (when (derived-mode-p 'org-mode)
    (numerals-disable-org-table-calculations))
  ;; Process the buffer
  (numerals-update-buffer)
  ;; Set up hook to update on save
  (add-hook 'after-save-hook #'numerals-update-buffer nil t))

(defun numerals-mode-disable ()
  "Disable numerals-mode in the current buffer."
  ;; Clear display
  (numerals-display-clear-all)
  ;; Clear variables
  (numerals-variables-clear)
  ;; Re-enable org-mode table calculations if in org-mode
  (when (derived-mode-p 'org-mode)
    (numerals-enable-org-table-calculations))
  ;; Remove hook
  (remove-hook 'after-save-hook #'numerals-update-buffer t))

;;; Org-mode Integration

(defvar-local numerals-org-table-auto-blank-field nil
  "Saved value of org-table-auto-blank-field.")

(defvar-local numerals-org-table-formula-evaluate-inline nil
  "Saved value of org-table-formula-evaluate-inline.")

(defun numerals-disable-org-table-calculations ()
  "Disable org-mode's built-in table calculations."
  (when (featurep 'org-table)
    ;; Save current values
    (setq numerals-org-table-auto-blank-field org-table-auto-blank-field)
    (setq numerals-org-table-formula-evaluate-inline org-table-formula-evaluate-inline)
    ;; Disable org table features that interfere
    (setq-local org-table-auto-blank-field nil)
    (setq-local org-table-formula-evaluate-inline nil)
    ;; Remove org table calculation hooks
    (remove-hook 'org-ctrl-c-ctrl-c-hook 'org-table-maybe-eval-formula t)
    (remove-hook 'org-ctrl-c-ctrl-c-hook 'org-table-maybe-recalculate-line t)
    ;; Disable automatic table recalculation
    (when (fboundp 'org-table-recalculate)
      (advice-add 'org-table-recalculate :around #'numerals-suppress-org-table-recalc))
    (when (fboundp 'org-table-calc-current-TBLFM)
      (advice-add 'org-table-calc-current-TBLFM :around #'numerals-suppress-org-table-recalc))))

(defun numerals-enable-org-table-calculations ()
  "Re-enable org-mode's built-in table calculations."
  (when (featurep 'org-table)
    ;; Restore saved values
    (setq-local org-table-auto-blank-field numerals-org-table-auto-blank-field)
    (setq-local org-table-formula-evaluate-inline numerals-org-table-formula-evaluate-inline)
    ;; Re-add org table calculation hooks
    (add-hook 'org-ctrl-c-ctrl-c-hook 'org-table-maybe-eval-formula nil t)
    (add-hook 'org-ctrl-c-ctrl-c-hook 'org-table-maybe-recalculate-line nil t)
    ;; Remove advice
    (when (fboundp 'org-table-recalculate)
      (advice-remove 'org-table-recalculate #'numerals-suppress-org-table-recalc))
    (when (fboundp 'org-table-calc-current-TBLFM)
      (advice-remove 'org-table-calc-current-TBLFM #'numerals-suppress-org-table-recalc))))

(defun numerals-suppress-org-table-recalc (orig-fun &rest args)
  "Suppress org-table recalculation when numerals-mode is active."
  (unless numerals-mode
    (apply orig-fun args)))

(defun numerals-display-table-result (start end result)
  "Display RESULT as an overlay replacing text from START to END.
Pads to exact original length for perfect table alignment."
  (let* ((original-text (buffer-substring start end))
         (original-length (length original-text))
         (result-length (length result))
         ;; Always pad to exact original length for perfect alignment
         (padded-result (if (< result-length original-length)
                           (let ((padding-needed (- original-length result-length)))
                             ;; Right-align numbers, left-align everything else
                             (if (string-match-p "^[0-9.-]+$" result)
                                 (concat (make-string padding-needed ?\s) result)
                               (concat result (make-string padding-needed ?\s))))
                         ;; If result is longer, truncate to fit
                         (substring result 0 original-length)))
         (overlay (make-overlay start end))
         (text (propertize padded-result 'face 'numerals-calculated-face)))
    ;; Configure the overlay to replace the text
    (overlay-put overlay 'display text)
    (overlay-put overlay 'numerals-overlay t)
    ;; Add to our list for cleanup
    (push overlay numerals-display-overlays)))




(defun numerals-update-buffer ()
  "Update all calculations in the current buffer using two-pass evaluation."
  (when numerals-mode
    (save-excursion
      ;; Clear existing display
      (numerals-display-clear-all)
      ;; Clear and rebuild variables
      (numerals-variables-clear)
      
      ;; Pass 1: Process all tables first
      (goto-char (point-min))
      (while (not (eobp))
        (when (numerals-table-at-point-p)
          (numerals-process-table))
        (forward-line 1))
      
      ;; Pass 2: Process all non-table lines (variables and calculations)
      (goto-char (point-min))
      (while (not (eobp))
        (unless (numerals-table-at-point-p)
          (numerals-process-line))
        (forward-line 1)))))

(defun numerals-process-line ()
  "Process the current line for calculations.
Returns the parse result for the line."
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
         (parse-result (numerals-parser-parse-line line))
         (type (plist-get parse-result :type)))
        (cond
         ;; Handle variable assignment
         ((eq type 'assignment)
          (let* ((var-name (plist-get parse-result :variable))
                 (expression (plist-get parse-result :expression))
                 (dependencies (numerals-parser-extract-variables expression))
                 (calc-result (numerals-calc-evaluate expression
                                                     (numerals-variables-get-all)))
                 (result (plist-get calc-result :value))
                 (error-msg (plist-get calc-result :error))
                 ;; Check if this is a simple literal or a calculation
                 (is-literal (and result
                                  (string-match-p "^[0-9.-]+$" (string-trim expression))
                                  (null dependencies))))
            (if result
                (progn
                  (numerals-variables-set var-name result expression dependencies)
                  (numerals-display-result (point) 
                                           (numerals-calc-format-result result)
                                           nil
                                           (if is-literal nil 'numerals-calculated-face))
                  ;; Add bold formatting to the variable name
                  (numerals-display-bold-variable-name (point)))
              (progn
                ;; Even if calculation failed, set the variable to nil so it's defined
                (numerals-variables-set var-name nil expression dependencies)
                (numerals-display-result (point) 
                                         (or error-msg "Error")
                                         t)
                ;; Add bold formatting to the variable name even if calculation failed
                (numerals-display-bold-variable-name (point))))))
         ;; Handle standalone calculation
         ((eq type 'calculation)
          (let* ((expression (plist-get parse-result :expression))
                 (calc-result (numerals-calc-evaluate expression
                                                     (numerals-variables-get-all)))
                 (result (plist-get calc-result :value))
                 (error-msg (plist-get calc-result :error)))
            (if result
                (numerals-display-result (point)
                                         (numerals-calc-format-result result)
                                         nil
                                         'numerals-calculated-face)
              (numerals-display-result (point) 
                                       (or error-msg "Error")
                                       t)))))
    parse-result))

(defun numerals-process-table ()
  "Process formulas in the table at point.
Advances point past the table."
  (when-let ((table (numerals-table-parse)))
    (let ((bounds (plist-get table :bounds))
          (data (plist-get table :data))
          (headers (plist-get table :headers))
          (current-row 0)
          (column-widths (numerals-calculate-table-column-widths table)))
      ;; Store column widths in table for use by display function
      (plist-put table :column-widths column-widths)
      ;; Process headers if they exist
      (when headers
        (setq current-row 1)
        (numerals-process-table-row headers table current-row))
      ;; Process data rows
      (let ((data-row-index 0))
        (dolist (row data)
          (setq current-row (if headers (+ 2 data-row-index) (+ 1 data-row-index)))
          (numerals-process-table-row row table current-row)
          (setq data-row-index (1+ data-row-index))))
      ;; Move past the table
      (goto-char (cdr bounds)))))

(defun numerals-calculate-table-column-widths (table)
  "Calculate the maximum formula width for each column in TABLE.
Returns a vector of column widths."
  (let* ((data (plist-get table :data))
         (headers (plist-get table :headers))
         (all-rows (if headers (cons headers data) data))
         (num-cols (if all-rows (length (car all-rows)) 0))
         (column-widths (make-vector num-cols 0)))
    ;; Find maximum formula length in each column
    (dolist (row all-rows)
      (let ((col-index 0))
        (dolist (cell row)
          (when (< col-index num-cols)
            (when (string-match "^\\s-*=\\s-*\\(.+\\)" cell)
              (let ((formula-length (length cell)))
                (when (> formula-length (aref column-widths col-index))
                  (aset column-widths col-index formula-length))))
            (setq col-index (1+ col-index))))))
    column-widths))

(defun numerals-process-table-row (row table row-num)
  "Process formulas in a table ROW.
TABLE is the parsed table structure, ROW-NUM is the 1-indexed row number."
  (let ((col-num 0))
    (dolist (cell row)
      (setq col-num (1+ col-num))
      (when (string-match "^\\s-*=\\s-*\\(.+\\)" cell)
        (let* ((formula (match-string 1 cell))
               (result (numerals-table-process-formula formula table row-num col-num))
               (formula-text (concat "=" formula)))
          ;; Search for the formula text directly in the table
          (save-excursion
            (goto-char (car (plist-get table :bounds)))
            (when (search-forward formula-text (cdr (plist-get table :bounds)) t)
              (let ((formula-start (- (point) (length formula-text)))
                    (formula-end (point)))
                (numerals-display-table-result formula-start formula-end result)))))))))

(defun numerals-recalculate ()
  "Manually trigger recalculation of all expressions."
  (interactive)
  (numerals-update-buffer))

(defun numerals-clear ()
  "Clear all variables and calculations."
  (interactive)
  (numerals-variables-clear)
  (numerals-display-clear-all)
  (message "Numerals: Cleared all variables and calculations"))

(defun numerals-toggle-overlays ()
  "Toggle visibility of all numerals overlays in the current buffer."
  (interactive)
  (if numerals-display-overlays
      (progn
        (numerals-display-clear-all)
        (message "Numerals: Overlays hidden"))
    (progn
      (numerals-update-buffer)
      (message "Numerals: Overlays shown"))))

(defun numerals-toggle-overlay-at-point ()
  "Toggle the numerals overlay on the current line."
  (interactive)
  (let ((had-overlay nil))
    ;; Check if there's an overlay on this line
    (dolist (overlay numerals-display-overlays)
      (when (and (overlay-buffer overlay)
                 (>= (overlay-start overlay) (line-beginning-position))
                 (<= (overlay-start overlay) (line-end-position)))
        (setq had-overlay t)))
    
    (if had-overlay
        (progn
          (numerals-display-clear-line (point))
          (message "Numerals: Overlay hidden at point"))
      (progn
        ;; Re-process just this line
        (let ((parse-result (numerals-process-line)))
          (when parse-result
            (message "Numerals: Overlay shown at point")))))))

(defun numerals-reload ()
  "Reload numerals-mode for development."
  (interactive)
  (let ((enabled-buffers '()))
    ;; Find all buffers with numerals-mode enabled and disable it
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (boundp 'numerals-mode) numerals-mode)
          (push buffer enabled-buffers)
          (numerals-mode -1))))
    ;; Reload all modules using dynamic path resolution
    (let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
      (load (expand-file-name "numerals-parser.el" base-dir))
      (load (expand-file-name "numerals-calc.el" base-dir))
      (load (expand-file-name "numerals-table-refs.el" base-dir))
      (load (expand-file-name "numerals-variables.el" base-dir))
      (load (expand-file-name "numerals-display.el" base-dir))
      (load (expand-file-name "numerals-tables.el" base-dir))
      (load (expand-file-name "numerals-mode.el" base-dir)))
    ;; Re-enable in all previously enabled buffers
    (dolist (buffer enabled-buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (numerals-mode 1))))
    (message "Numerals-mode reloaded in %d buffer(s)" (length enabled-buffers))))

(provide 'numerals-mode)
;;; numerals-mode.el ends here