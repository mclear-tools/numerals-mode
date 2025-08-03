;;; test-edge-cases.el --- Test edge cases for comma support

(load (expand-file-name "../numerals-mode.el" (file-name-directory load-file-name)))

(message "=== EDGE CASE TESTS ===")

;; Test negative numbers with commas
(let ((result (numerals-calc-evaluate "-1,000 + 500")))
  (message "Negative: -1,000 + 500 = %s" (plist-get result :value))
  (when (plist-get result :value)
    (message "Formatted: %s" (numerals-calc-format-result (plist-get result :value)))))

;; Test decimal numbers with commas
(let ((result (numerals-calc-evaluate "1,234.56 + 2,345.67")))
  (message "Decimals: 1,234.56 + 2,345.67 = %s" (plist-get result :value))
  (when (plist-get result :value)
    (message "Formatted: %s" (numerals-calc-format-result (plist-get result :value)))))

;; Test very large numbers
(let ((result (numerals-calc-evaluate "1,000,000 * 1,000")))
  (message "Large: 1,000,000 * 1,000 = %s" (plist-get result :value))
  (when (plist-get result :value)
    (message "Formatted: %s" (numerals-calc-format-result (plist-get result :value)))))

;; Test mixed formatted and unformatted numbers
(let ((result (numerals-calc-evaluate "1,000 + 500 + 2000")))
  (message "Mixed: 1,000 + 500 + 2000 = %s" (plist-get result :value))
  (when (plist-get result :value)
    (message "Formatted: %s" (numerals-calc-format-result (plist-get result :value)))))

;; Test validation function with edge cases
(message "\n=== VALIDATION TESTS ===")
(message "Valid comma numbers:")
(dolist (test '("1,000" "12,345" "1,234,567" "1,234.56" "-1,000" "-12,345.67"))
  (message "  '%s' → %s" test (numerals-utils-is-numeric-string-p test)))

(message "Invalid comma numbers:")
(dolist (test '("1,00" "12,34" "1234,567" "12,3456" "1,0000" "abc,123"))
  (message "  '%s' → %s" test (numerals-utils-is-numeric-string-p test)))

;; Test formatting edge cases
(message "\n=== FORMATTING TESTS ===")
(dolist (test '(0 100 999 1000 1234 12345 123456 1234567 -1000 -1234567))
  (message "  %s → %s" test (numerals-utils-format-number-with-commas test)))

;; Test decimal formatting
(dolist (test '(1000.5 1234.56 1234567.89 -1234.56))
  (message "  %s → %s" test (numerals-utils-format-number-with-commas test)))