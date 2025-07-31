;;; test-comma-simple.el --- Simple test for comma support

(load "/Users/roambot/bin/lisp-projects/numerals-mode/numerals-mode.el")

;; Test calculation with commas
(let ((expr "1,000 + 2,000")
      (variables nil))
  (message "Testing expression: %s" expr)
  (let ((calc-result (numerals-calc-evaluate expr variables)))
    (message "Raw result: %s" calc-result)
    (when (plist-get calc-result :value)
      (let ((formatted (numerals-calc-format-result (plist-get calc-result :value))))
        (message "Formatted result: %s" formatted)))))

;; Test variable substitution with commas
(let ((expr "Revenue - Expenses")
      (variables '(("Revenue" . "1,000,000") ("Expenses" . "250,000"))))
  (message "\nTesting variable substitution:")
  (message "Expression: %s" expr)
  (message "Variables: %s" variables)
  (let ((calc-result (numerals-calc-evaluate expr variables)))
    (message "Raw result: %s" calc-result)
    (when (plist-get calc-result :value)
      (let ((formatted (numerals-calc-format-result (plist-get calc-result :value))))
        (message "Formatted result: %s" formatted)))))

;; Test table function
(let ((values '("1,000" "2,000" "3,000" "non-numeric")))
  (message "\nTesting table SUM function:")
  (message "Input values: %s" values)
  (let ((numbers (numerals-utils-extract-numbers values)))
    (message "Extracted numbers: %s" numbers)
    (let ((sum (apply #'+ numbers)))
      (message "Sum: %s" sum)
      (message "Formatted sum: %s" (numerals-calc-format-result (number-to-string sum))))))