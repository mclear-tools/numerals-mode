;;; test-minimal.el --- Minimal test

(load "/Users/roambot/bin/lisp-projects/numerals-mode/numerals-mode.el")

;; Test just the calculation functions directly
(message "=== DIRECT CALCULATION TESTS ===")

;; Test basic comma calculation
(let ((result (numerals-calc-evaluate "1,000 + 2,000")))
  (message "1,000 + 2,000 = %s" (plist-get result :value))
  (when (plist-get result :value)
    (message "Formatted: %s" (numerals-calc-format-result (plist-get result :value)))))

;; Test variable with comma
(let ((result (numerals-calc-evaluate "Revenue - Expenses" '(("Revenue" . "1,000,000") ("Expenses" . "250,000")))))
  (message "Revenue - Expenses = %s" (plist-get result :value))
  (when (plist-get result :value)
    (message "Formatted: %s" (numerals-calc-format-result (plist-get result :value)))))

;; Test large number formatting
(message "Formatting tests:")
(message "1000000 → %s" (numerals-utils-format-number-with-commas 1000000))
(message "1234567.89 → %s" (numerals-utils-format-number-with-commas 1234567.89))