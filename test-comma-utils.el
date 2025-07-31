;;; test-comma-utils.el --- Test comma formatting utilities

(load "/Users/roambot/bin/lisp-projects/numerals-mode/numerals-utils.el")

;; Test strip-commas
(message "Testing strip-commas:")
(message "  '1,000' -> '%s'" (numerals-utils-strip-commas "1,000"))
(message "  '1,234,567.89' -> '%s'" (numerals-utils-strip-commas "1,234,567.89"))
(message "  '-1,000' -> '%s'" (numerals-utils-strip-commas "-1,000"))
(message "  'no commas' -> '%s'" (numerals-utils-strip-commas "no commas"))

;; Test format-number-with-commas  
(message "\nTesting format-number-with-commas:")
(message "  1000 -> '%s'" (numerals-utils-format-number-with-commas 1000))
(message "  1234567 -> '%s'" (numerals-utils-format-number-with-commas 1234567))
(message "  1234567.89 -> '%s'" (numerals-utils-format-number-with-commas 1234567.89))
(message "  -1000 -> '%s'" (numerals-utils-format-number-with-commas -1000))
(message "  '1000' -> '%s'" (numerals-utils-format-number-with-commas "1000"))
(message "  100 -> '%s'" (numerals-utils-format-number-with-commas 100))

;; Test is-numeric-string-p
(message "\nTesting is-numeric-string-p:")
(message "  '1,000' -> %s" (numerals-utils-is-numeric-string-p "1,000"))
(message "  '1,234.56' -> %s" (numerals-utils-is-numeric-string-p "1,234.56"))
(message "  '-1,000' -> %s" (numerals-utils-is-numeric-string-p "-1,000"))
(message "  '1000' -> %s" (numerals-utils-is-numeric-string-p "1000"))
(message "  'abc' -> %s" (numerals-utils-is-numeric-string-p "abc"))

;; Test extract-numbers
(message "\nTesting extract-numbers:")
(message "  ('1,000' '2,000' 'abc') -> %s" 
         (numerals-utils-extract-numbers '("1,000" "2,000" "abc")))
(message "  ('100' '-1,234.56' 'text') -> %s"
         (numerals-utils-extract-numbers '("100" "-1,234.56" "text")))