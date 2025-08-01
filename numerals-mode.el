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

(require 'numerals-utils)
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

(defvar-local numerals-org-pretty-entities nil
  "Saved value of org-pretty-entities.")

(defun numerals-disable-org-table-calculations ()
  "Disable org-mode's built-in table calculations and org-pretty-entities."
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
      (advice-add 'org-table-calc-current-TBLFM :around #'numerals-suppress-org-table-recalc)))
  ;; Save and disable org-pretty-entities
  (when (boundp 'org-pretty-entities)
    (setq numerals-org-pretty-entities org-pretty-entities)
    (setq-local org-pretty-entities nil)))

(defun numerals-enable-org-table-calculations ()
  "Re-enable org-mode's built-in table calculations and restore org-pretty-entities."
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
      (advice-remove 'org-table-calc-current-TBLFM #'numerals-suppress-org-table-recalc)))
  ;; Restore org-pretty-entities
  (when (boundp 'org-pretty-entities)
    (setq-local org-pretty-entities numerals-org-pretty-entities)))

(defun numerals-suppress-org-table-recalc (orig-fun &rest args)
  "Suppress org-table recalculation when numerals-mode is active."
  (unless numerals-mode
    (apply orig-fun args)))

(defun numerals-display-table-result (start end result)
  "Display RESULT as an overlay replacing entire cell content from START to END.
CRITICAL: Maintains exact character count for perfect pipe alignment."
  ;; Ensure numerals-utils is loaded
  (unless (fboundp 'numerals-utils-is-numeric-string-p)
    (require 'numerals-utils))
  (let* ((original-cell-content (buffer-substring start end))
         (original-length (length original-cell-content))
         (result-length (length result))
         (is-numeric (numerals-utils-is-numeric-string-p result))
         ;; Create replacement with EXACTLY the same character count as original cell
         (formatted-result (cond
                            ((< result-length original-length)
                             (let ((padding-needed (- original-length result-length)))
                               (if is-numeric
                                   ;; Right-align numbers with small right margin like non-overlay numbers
                                   (if (>= padding-needed 1)
                                       (concat (make-string (1- padding-needed) ?\s) result " ")
                                     (concat (make-string padding-needed ?\s) result))
                                 ;; Left-align text with all padding on right
                                 (concat result (make-string padding-needed ?\s)))))
                            ((> result-length original-length)
                             ;; If result is longer than cell, truncate
                             (substring result 0 original-length))
                            (t
                             ;; Exact same length
                             result)))
         (overlay (numerals-display--get-overlay start end))
         (text (propertize formatted-result 'face 'numerals-calculated-face)))
    ;; Configure the overlay to replace the text
    (overlay-put overlay 'display text)
    (overlay-put overlay 'numerals-overlay t)
    ;; Add to our list for cleanup
    (push overlay numerals-display-overlays)))




(defvar-local numerals-processed-positions nil
  "Set of buffer positions that have already been processed in this update cycle.")

(defun numerals-update-buffer ()
  "Update all calculations in the current buffer with reliable dependency resolution."
  (when numerals-mode
    (save-excursion
      ;; Clear existing display and state
      (numerals-display-clear-all)
      (numerals-variables-clear)
      (setq numerals-processed-positions nil)
      (setq numerals-table-cell-cache nil)
      
      ;; Pass 1: Process simple variables first (literals and basic math)
      (goto-char (point-min))
      (while (not (eobp))
        (unless (numerals-table-at-point-p)
          (numerals-process-line-if-no-dependencies))
        (forward-line 1))
      
      ;; Pass 2: Process all tables (can now use simple variables)
      (goto-char (point-min))
      (while (not (eobp))
        (when (numerals-table-at-point-p)
          (numerals-process-table-once))
        (forward-line 1))
      
      ;; Pass 3: Process remaining variables (can reference tables and other variables)
      (goto-char (point-min))
      (while (not (eobp))
        (unless (numerals-table-at-point-p)
          (let ((pos (point)))
            (unless (member pos numerals-processed-positions)
              (numerals-process-line)
              (push pos numerals-processed-positions))))
        (forward-line 1))
      
      ;; Pass 4: Re-process only tables that have formulas referencing variables
      (goto-char (point-min))
      (while (not (eobp))
        (when (numerals-table-at-point-p)
          (numerals-reprocess-table-if-needed))
        (forward-line 1)))))

(defun numerals-process-line-if-no-dependencies ()
  "Process the current line only if it has no variable or table dependencies."
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
         (parse-result (numerals-parser-parse-line line))
         (type (plist-get parse-result :type))
         (expression (plist-get parse-result :expression)))
    (when (and (eq type 'assignment) expression)
      ;; Check if expression has no variables or table references
      (let ((variables (numerals-parser-extract-variables expression))
            (has-table-refs (string-match-p "\\b[A-Za-z_][A-Za-z0-9_]*\\.[A-Z]+[0-9]+\\b\\|\\b[A-Za-z_][A-Za-z0-9_]*\\.TOTALS\\[" expression)))
        (when (and (null variables) (not has-table-refs))
          (numerals-process-line)
          (push (line-beginning-position) numerals-processed-positions))))
    parse-result))

(defun numerals-process-table-once ()
  "Process table only if it hasn't been processed yet."
  (let ((table-start (point)))
    (unless (member table-start numerals-processed-positions)
      (numerals-process-table)
      (push table-start numerals-processed-positions))))

(defun numerals-reprocess-table-if-needed ()
  "Re-process table only if it contains formulas that reference variables."
  (let ((table-start (point)))
    (when-let ((table (numerals-table-parse)))
      (let ((data (plist-get table :data))
            (needs-reprocessing nil))
        ;; Check if any formula references variables or other cells
        (dolist (row data)
          (dolist (cell row)
            (when (string-match "^\\s-*=\\s-*\\(.+\\)" cell)
              (let* ((formula (match-string 1 cell))
                     (variables (numerals-parser-extract-variables formula))
                     (has-cell-refs (string-match-p "\\b[A-Z]+[0-9]+\\b" formula)))
                (when (or variables has-cell-refs)
                  (setq needs-reprocessing t))))))
        ;; Only reprocess if needed and hasn't been reprocessed yet
        (when (and needs-reprocessing
                   (not (member (cons table-start 'reprocessed) numerals-processed-positions)))
          (numerals-clear-table-overlays table)
          (numerals-process-table)
          (push (cons table-start 'reprocessed) numerals-processed-positions))))))

(defun numerals-clear-table-overlays (table)
  "Clear overlays within the TABLE bounds."
  (let ((start (car (plist-get table :bounds)))
        (end (cdr (plist-get table :bounds))))
    (dolist (overlay (overlays-in start end))
      (when (overlay-get overlay 'numerals-overlay)
        (numerals-display--return-overlay overlay)
        (setq numerals-display-overlays
              (delq overlay numerals-display-overlays))))))

(defun numerals-process-line ()
  "Process the current line for calculations.
Returns the parse result for the line."
  ;; Ensure numerals-utils is loaded
  (unless (fboundp 'numerals-utils-is-numeric-string-p)
    (require 'numerals-utils))
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
                                  (numerals-utils-is-numeric-string-p (string-trim expression))
                                  (null dependencies))))
            (if result
                (progn
                  (numerals-variables-set var-name result expression dependencies)
                  (numerals-display-result (point) 
                                           (if is-literal expression (numerals-calc-format-result result))
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
          ;; Cache the calculated result for cross-references
          (when result
            (numerals-table-cache-cell-value table row-num col-num result))
          ;; Search for the formula text at the specific row/column position
          (save-excursion
            (let ((found-position (numerals-find-formula-at-position table row-num col-num formula-text)))
              (when found-position
                (numerals-display-table-result (car found-position) (cdr found-position) result))
)))))))

(defun numerals-find-formula-at-position (table row-num col-num formula-text)
  "Find the exact buffer position of FORMULA-TEXT at ROW-NUM, COL-NUM in TABLE.
Returns a cons (START . END) of the formula position, or nil if not found."
  (let* ((bounds (plist-get table :bounds))
         (start-pos (car bounds))
         (end-pos (cdr bounds)))
    (save-excursion
      (goto-char start-pos)
      (let ((current-row 0))
        ;; Find the target row (skip separator lines like table parser does)
        (while (and (< (point) end-pos) (/= current-row row-num))
          (when (looking-at "^[ \t]*|\\(.*\\)|[ \t]*$")
            (let ((content (match-string 1)))
              ;; Only count non-separator lines
              (unless (string-match-p "^[ \t:+|-]+$" content)
                (setq current-row (1+ current-row)))))
          (when (/= current-row row-num)
            (forward-line 1)))
        
        ;; Now we're at the target row - search for formula-text in this line only
        (when (= current-row row-num)
          (let ((line-start (point))  
                (line-end (line-end-position))
                (col-count 0)
                (found-pos nil))
            ;; Parse cells in this row to find the target column
            (when (looking-at "^[ \t]*|\\(.*\\)|[ \t]*$")
              (let* ((content (match-string 1))
                     (remaining-content content)
                     (current-pos (1+ line-start))) ; Start after first |
                ;; Use direct parsing for accurate cell boundary detection
                (while (and remaining-content 
                           (> (length remaining-content) 0)
                           (string-match "^\\([^|]*\\)" remaining-content))
                  (setq col-count (1+ col-count))
                  (let* ((cell (match-string 1 remaining-content))
                         (trimmed-cell (string-trim cell))
                         (cell-start current-pos)
                         (cell-end (+ current-pos (length cell))))
                    (when (and (= col-count col-num) (string= trimmed-cell formula-text))
                      ;; Found our target cell - replace the entire cell content
                      (setq found-pos (cons cell-start cell-end)))
                    ;; Move to next cell: past this cell + "|" separator  
                    (setq current-pos (+ cell-end 1))
                    (setq remaining-content 
                          (if (>= (+ (length cell) 1) (length remaining-content))
                              nil
                            (substring remaining-content (+ (length cell) 1)))))))
            found-pos)))))))


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
      (load (expand-file-name "numerals-utils.el" base-dir))
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

;;; Org-mode auto-activation support

(defun numerals-check-org-keywords ()
  "Check for numerals-mode keywords in org buffer.
Returns t if numerals-mode should be activated."
  (save-excursion
    (goto-char (point-min))
    (or (re-search-forward "^#\\+STARTUP:.*\\bnumerals\\b" nil t)
        (re-search-forward "^#\\+PROPERTY: numerals-mode t" nil t))))

(defun numerals-setup-org-mode ()
  "Setup numerals-mode for org files if keywords present.
This function is intended to be added to `org-mode-hook'."
  (when (and (derived-mode-p 'org-mode)
             (numerals-check-org-keywords))
    (numerals-mode 1)))

;; Add to org-mode-hook for auto-activation
(add-hook 'org-mode-hook #'numerals-setup-org-mode)

(provide 'numerals-mode)
;;; numerals-mode.el ends here
