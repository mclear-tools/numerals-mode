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
(require 'numerals-variables)
(require 'numerals-display)

(defgroup numerals nil
  "Literate calculation mode."
  :group 'convenience
  :prefix "numerals-")



;;;###autoload
(define-minor-mode numerals-mode
  "Toggle Numerals mode for literate calculations.
When Numerals mode is enabled, lines containing calculations
are automatically evaluated and results are displayed as overlays."
  :lighter " Num"
  :group 'numerals
  (if numerals-mode
      (numerals-mode-enable)
    (numerals-mode-disable)))

(defun numerals-mode-enable ()
  "Enable numerals-mode in the current buffer."
  ;; Initialize variables
  (numerals-variables-init)
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
  ;; Remove hook
  (remove-hook 'after-save-hook #'numerals-update-buffer t))





(defun numerals-update-buffer ()
  "Update all calculations in the current buffer."
  (when numerals-mode
    (save-excursion
      ;; Clear existing display
      (numerals-display-clear-all)
      ;; Clear and rebuild variables
      (numerals-variables-clear)
      ;; Process each line
      (goto-char (point-min))
      (while (not (eobp))
        (numerals-process-line)
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
                                       (if is-literal nil 'numerals-calculated-face)))
          (numerals-display-result (point) 
                                   (or error-msg "Error")
                                   t))))
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

(provide 'numerals-mode)
;;; numerals-mode.el ends here