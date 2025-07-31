;;; debug-regex.el --- Debug the comma regex

(load "/Users/roambot/bin/lisp-projects/numerals-mode/numerals-utils.el")

(let ((test-expr "1,000 + 2,000")
      (pattern "\\b\\([0-9]\\{1,3\\}\\(?:,[0-9]\\{3\\}\\)*\\(?:\\.[0-9]+\\)?\\)\\b"))
  (message "Testing expression: %s" test-expr)
  (message "Pattern: %s" pattern)
  
  ;; Test if pattern matches
  (let ((start 0))
    (while (string-match pattern test-expr start)
      (message "Match found: '%s' at position %d-%d" 
               (match-string 0 test-expr)
               (match-beginning 0)
               (match-end 0))
      (setq start (match-end 0))))
  
  ;; Test replacement
  (let ((result (replace-regexp-in-string
                 pattern
                 (lambda (match)
                   (message "Replacing '%s' with '%s'" match (numerals-utils-strip-commas match))
                   (numerals-utils-strip-commas match))
                 test-expr)))
    (message "Final result: '%s'" result)))