;;; numerals-tables.el --- Table support for numerals-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: calc, convenience, tables
;; Version: 0.1.0

;; This file is part of numerals-mode.

;;; Commentary:

;; This module provides table calculation support for numerals-mode.
;; It supports both org-mode and markdown table formats, allowing
;; calculations within table cells using Excel-like references.

;;; Code:

(require 'cl-lib)
(require 'numerals-calc)
(require 'numerals-utils)

;;; Table Detection

(defun numerals-table-at-point-p ()
  "Return non-nil if point is inside a table.
Returns 'org for org-mode tables, 'markdown for markdown tables,
or nil if not in a table."
  (or (numerals-table-org-at-point-p)
      (numerals-table-markdown-at-point-p)))

(defun numerals-table-org-at-point-p ()
  "Return 'org if point is inside an org-mode table."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^[ \t]*|")
      'org)))

(defun numerals-table-markdown-at-point-p ()
  "Return 'markdown if point is inside a markdown table."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^[ \t]*|.*|[ \t]*$")
      ;; Check if there's a separator line nearby
      (save-excursion
        (or (progn (forward-line -1)
                   (looking-at "^[ \t]*|[ \t:-]+|[ \t]*$"))
            (progn (forward-line 2)
                   (looking-at "^[ \t]*|[ \t:-]+|[ \t]*$")))))))

;;; Table Parsing

(defun numerals-table-bounds ()
  "Return the bounds of the table at point as (BEG . END).
Returns nil if not in a table."
  (when (numerals-table-at-point-p)
    (save-excursion
      (let ((beg (progn
                   (while (and (not (bobp))
                               (progn (forward-line -1)
                                      (looking-at "^[ \t]*|")))
                     nil)
                   (unless (looking-at "^[ \t]*|")
                     (forward-line 1))
                   (point)))
            (end (progn
                   (while (and (not (eobp))
                               (progn (forward-line 1)
                                      (looking-at "^[ \t]*|")))
                     nil)
                   (point))))
        (cons beg end)))))

(defun numerals-table-parse ()
  "Parse the table at point.
Returns a plist with :type, :headers, :data, :separator-line, and :bounds."
  (when-let ((bounds (numerals-table-bounds))
             (type (numerals-table-at-point-p)))
    (save-excursion
      (goto-char (car bounds))
      (let ((lines '())
            (separator-line nil))
        ;; Collect all table lines
        (while (< (point) (cdr bounds))
          (when (looking-at "^[ \t]*|\\(.*\\)|[ \t]*$")
            (let ((content (match-string 1)))
              (if (string-match-p "^[ \t:+|-]+$" content)
                  (progn
                    (setq separator-line (length lines)))
                (progn
                  (push (numerals-table-parse-row content) lines)))))
          (forward-line 1))
        (setq lines (nreverse lines))
        ;; Build result
        (list :type type
              :headers (when separator-line (nth 0 lines))
              :data (if separator-line
                       (nthcdr 1 lines)
                     lines)
              :separator-line separator-line
              :bounds bounds)))))

(defun numerals-table-parse-row (row-content)
  "Parse ROW-CONTENT string into a list of cell values.
ROW-CONTENT should be the content between the outer pipes."
  (mapcar #'string-trim
          (split-string row-content "|" t)))

;;; Cell References

(defun numerals-table-parse-reference (ref)
  "Parse a cell reference REF.
Handles Excel-style (A1, B2) and org-style (@1$1) references.
Returns a plist with :row and :col (1-indexed), or nil if invalid."
  (cond
   ;; Excel-style reference (A1, B2, etc.)
   ((string-match "^\\([A-Z]+\\)\\([0-9]+\\)$" ref)
    (let ((col (numerals-table-column-letter-to-number (match-string 1 ref)))
          (row (string-to-number (match-string 2 ref))))
      (list :row row :col col)))
   ;; Org-style reference (@1$1)
   ((string-match "^@\\([0-9]+\\)\\$\\([0-9]+\\)$" ref)
    (list :row (string-to-number (match-string 1 ref))
          :col (string-to-number (match-string 2 ref))))
   (t nil)))

;; Use centralized utility function
(defalias 'numerals-table-column-letter-to-number
  'numerals-utils-column-letter-to-number
  "Convert column LETTERS (e.g., \"A\", \"AB\") to column number.")

(defun numerals-table-parse-range (range)
  "Parse a range reference like A1:B3 or @1$1..@2$3.
Returns a plist with :start-row :start-col :end-row :end-col."
  (cond
   ;; Excel-style range (A1:B3)
   ((string-match "^\\([A-Z]+[0-9]+\\):\\([A-Z]+[0-9]+\\)$" range)
    (let ((start-str (match-string 1 range))
          (end-str (match-string 2 range)))
      (let ((start (numerals-table-parse-reference start-str))
            (end (numerals-table-parse-reference end-str)))
        (when (and start end)
          (let ((result (list :start-row (plist-get start :row)
                              :start-col (plist-get start :col)
                              :end-row (plist-get end :row)
                              :end-col (plist-get end :col))))
            result)))))
   ;; Org-style range (@1$1..@2$3)
   ((string-match "^\\(@[0-9]+\\$[0-9]+\\)\\.\\.\\(@[0-9]+\\$[0-9]+\\)$" range)
    (let ((start (numerals-table-parse-reference (match-string 1 range)))
          (end (numerals-table-parse-reference (match-string 2 range))))
      (when (and start end)
        (list :start-row (plist-get start :row)
              :start-col (plist-get start :col)
              :end-row (plist-get end :row)
              :end-col (plist-get end :col)))))
   (t 
    nil)))

;;; Cell Access

(defun numerals-table-get-cell (table row col &optional evaluate-formulas)
  "Get the value of cell at ROW and COL in TABLE.
ROW and COL are 1-indexed. TABLE is a parsed table structure.
If EVALUATE-FORMULAS is non-nil, evaluate formulas before returning."
  (let* ((data (plist-get table :data))
         (headers (plist-get table :headers))
         (row-data (cond
                    ((and headers (= row 1)) headers)
                    (headers (nth (- row 2) data))
                    (t (nth (- row 1) data))))
         (raw-value (when (and row-data (> col 0) (<= col (length row-data)))
                      (nth (- col 1) row-data))))
      (if (and evaluate-formulas raw-value 
             (string-match "^[ \t]*=[ \t]*\\(.+\\)" raw-value))
        ;; This is a formula - evaluate it
        (let ((formula (condition-case err
                           (match-string 1 raw-value)
                         (error nil)))
              (cell-id (format "%s-%s" row col)))
          (if formula
              (if (member cell-id numerals-table-evaluation-stack)
                  ;; Circular reference detected
                  "0"
                (condition-case nil
                    (let ((numerals-table-evaluation-stack 
                           (cons cell-id numerals-table-evaluation-stack)))
                      (numerals-table-process-formula formula table row col))
                  (error "Error")))
            "Error"))
      ;; Return raw value
      raw-value)))

(defun numerals-table-get-range-values (table range-spec &optional evaluate-formulas)
  "Get all values in RANGE-SPEC from TABLE.
RANGE-SPEC is a plist with :start-row :start-col :end-row :end-col.
If EVALUATE-FORMULAS is non-nil, evaluate formulas before returning.
Returns a flat list of values."
  (let ((values '()))
    (cl-loop for row from (plist-get range-spec :start-row)
             to (plist-get range-spec :end-row)
             do (cl-loop for col from (plist-get range-spec :start-col)
                         to (plist-get range-spec :end-col)
                         do (when-let ((value (numerals-table-get-cell table row col evaluate-formulas)))
                              (push value values))))
    (nreverse values)))

(defun numerals-table-get-range-values-for-sum (table range-spec)
  "Get values in RANGE-SPEC from TABLE, evaluating simple formulas.
Handles formulas like =B2*0.03 but avoids nested SUM calls to prevent recursion."
  (let ((values '()))
    (cl-loop for row from (plist-get range-spec :start-row)
             to (plist-get range-spec :end-row)
             do (cl-loop for col from (plist-get range-spec :start-col)
                         to (plist-get range-spec :end-col)
                         do (let ((raw-value (numerals-table-get-cell table row col nil))) ; Don't evaluate formulas here
                              (when raw-value
                                (let ((processed-value
                                       (if (string-match "^[ \t]*=[ \t]*\\(.+\\)" raw-value)
                                           ;; This is a formula - evaluate it carefully
                                           (let ((formula (match-string 1 raw-value)))
                                             (condition-case nil
                                                 ;; Only evaluate simple formulas that don't contain functions
                                                 (if (string-match-p "\\(SUM\\|AVERAGE\\|COUNT\\|MAX\\|MIN\\)" formula)
                                                     ;; Skip nested function calls to avoid recursion, but try basic evaluation
                                                     (condition-case nil
                                                       (let ((simple-result (numerals-table-process-formula formula table row col)))
                                                         (if (numerals-utils-is-numeric-string-p simple-result)
                                                             simple-result
                                                           "0"))
                                                       (error "0"))
                                                   ;; Evaluate simple arithmetic formulas
                                                   (let ((expanded (numerals-table-expand-simple-references formula table)))
                                                     (let ((result (numerals-calc-evaluate expanded (numerals-variables-get-all))))
                                                       (if (plist-get result :error)
                                                           "0"
                                                         (plist-get result :value)))))
                                               (error "0")))
                                         ;; Not a formula, use as-is
                                         raw-value)))
                                  (push processed-value values))))))
    (let ((final-values (nreverse values)))
      final-values)))

(defun numerals-table-expand-simple-references (formula table)
  "Expand simple cell references in FORMULA without recursive evaluation.
Only expands references to literal values, not other formulas."
  (let ((result formula)
        (loop-count 0))
    ;; Replace simple cell references
    (while (and (string-match "\\b\\([A-Z]+[0-9]+\\)\\b" result)
                (< loop-count 50)) ; Safety limit
      (let* ((ref (match-string 1 result))
             (cell (numerals-table-parse-reference ref))
             (raw-value (when cell (numerals-table-get-cell table
                                                           (plist-get cell :row)
                                                           (plist-get cell :col)
                                                           nil)))) ; Don't evaluate formulas
        (let ((replacement (if (and raw-value 
                                   (not (string-match "^[ \t]*=" raw-value))) ; Not a formula
                              raw-value
                            "0"))) ; Use 0 for formulas to avoid recursion
          (setq result (replace-match replacement t t result))
          (setq loop-count (1+ loop-count)))))
    result))

;;; Table Functions

(defun numerals-table-sum (values)
  "Calculate the sum of VALUES.
VALUES is a list of strings that may contain numbers."
  (let ((numbers (numerals-table-extract-numbers values)))
    (if numbers
        (let ((result (apply #'+ numbers)))
          result)
      (progn
        0))))


(defvar-local numerals-table-evaluation-stack nil
  "Stack to track cells currently being evaluated to prevent recursion.")

(defvar-local numerals-table-expansion-stack nil
  "Stack to track cells currently being expanded to prevent recursion.")

;; Use centralized utility function
(defalias 'numerals-table-extract-numbers
  'numerals-utils-extract-numbers
  "Extract numeric values from a list of string VALUES.")

;;; Formula Processing Helper Functions

(defun numerals-table--expand-sum-functions (formula table)
  "Expand SUM function calls in FORMULA using TABLE data.
Returns formula with SUM calls replaced by calculated values."
  (let ((result formula))
    (when (string-match "SUM\\s-*(\\s-*\\([^)]+\\)\\s-*)" result)
      (let* ((range-str (match-string 1 result))
             (start (match-beginning 0))
             (end (match-end 0)))
        (condition-case nil
            (let* ((range (numerals-table-parse-range range-str))
                   (values (when range 
                             (numerals-table-get-range-values-for-sum table range)))
                   (sum-result (if values
                                   (number-to-string (numerals-table-sum values))
                                 numerals-utils-error-zero)))
              (setq result (concat (substring result 0 start)
                                   sum-result
                                   (substring result end))))
          (error 
           (setq result (concat (substring result 0 start)
                                numerals-utils-error-zero
                                (substring result end)))))))
    result))

(defun numerals-table--expand-cell-reference (ref table)
  "Expand single cell reference REF using TABLE data.
Returns the cell value or error value if not found."
  (condition-case nil
      (let ((cell (numerals-table-parse-reference ref)))
        (if cell
            (let* ((row (plist-get cell :row))
                   (col (plist-get cell :col))
                   (value (numerals-table-get-cell table row col nil)))
              (if (and value (not (string-match "^[ \t]*=" value)))
                  ;; Literal value - use directly
                  (string-trim value)
                ;; Formula - evaluate with recursion protection
                (let ((ref-key (format "%d-%d" row col)))
                  (if (member ref-key numerals-table-expansion-stack)
                      numerals-utils-error-zero
                    (condition-case nil
                        (let ((numerals-table-expansion-stack 
                               (cons ref-key numerals-table-expansion-stack))
                              (eval-value (numerals-table-get-cell table row col t)))
                          (if eval-value 
                              (string-trim eval-value) 
                            numerals-utils-error-zero))
                      (error numerals-utils-error-zero))))))
          numerals-utils-error-zero))
    (error numerals-utils-error-zero)))

(defun numerals-table--expand-cell-references (formula table)
  "Expand all cell references in FORMULA using TABLE data.
Returns formula with cell references replaced by their values."
  (replace-regexp-in-string
   "\\b\\([A-Z]+[0-9]+\\)\\b"
   (lambda (match)
     (numerals-table--expand-cell-reference (match-string 1 match) table))
   formula t))

(defun numerals-table--expand-variable-references (formula)
  "Expand variable references in FORMULA.
Returns formula with variables replaced by their values."
  (let ((result formula)
        (variables (condition-case nil 
                       (numerals-variables-get-all) 
                     (error '()))))
    (dolist (var variables)
      (let ((var-name (car var))
            (var-value (cdr var)))
        ;; Only replace if it's not a cell reference pattern
        (unless (string-match-p "^[A-Z]+[0-9]+$" var-name)
          (setq result (replace-regexp-in-string
                        (concat "\\b" (regexp-quote var-name) "\\b")
                        (if (numberp var-value)
                            (number-to-string var-value)
                          (format "%s" var-value))
                        result t t)))))
    result))

;;; Formula Processing

(defun numerals-table-process-formula (formula table current-row current-col)
  "Process FORMULA in the context of TABLE at CURRENT-ROW and CURRENT-COL.
Returns the calculated result as a string."
  (condition-case err
      (let ((expanded (condition-case expand-err
                          (numerals-table-expand-references formula table current-row current-col)
                        (error formula))))
        ;; Use calc directly since we've already expanded all references
        (let ((calc-result (condition-case nil (calc-eval expanded) (error "Error"))))
          (let ((result (list :value (if (stringp calc-result) calc-result (format "%s" calc-result)) :error nil)))
            (if (plist-get result :error)
                (format "Error: %s" (plist-get result :error))
              (numerals-calc-format-result (plist-get result :value))))))
    (error 
     (format "Error: %s" (error-message-string err)))))

(defun numerals-table-expand-references (formula table &optional current-row current-col)
  "Expand all table references in FORMULA using TABLE data.
Replaces cell references with their values and function calls with results."
  (let ((result formula))
    ;; Step 1: Expand SUM function calls
    (setq result (numerals-table--expand-sum-functions result table))
    
    ;; Step 2: Expand cell references
    (setq result (numerals-table--expand-cell-references result table))
    
    ;; Step 3: Expand variable references
    (setq result (numerals-table--expand-variable-references result))
    
    result))



(provide 'numerals-tables)
;;; numerals-tables.el ends here