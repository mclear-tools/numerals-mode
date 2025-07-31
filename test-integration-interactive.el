;;; test-integration-interactive.el --- Interactive test for comma support

(load "/Users/roambot/bin/lisp-projects/numerals-mode/numerals-mode.el")

;; Open the test file and process it
(find-file "/Users/roambot/bin/lisp-projects/numerals-mode/test-comma-integration.org")
(numerals-mode)
(numerals-recalculate)

;; Print results
(message "=== COMMA INTEGRATION TEST RESULTS ===")
(save-excursion
  (goto-char (point-min))
  (while (not (eobp))
    (let ((after-str (get-char-property (point) 'after-string))
          (overlay-display (get-char-property (point) 'display)))
      (when (or after-str overlay-display)
        (message "Line %d: %s" 
                 (line-number-at-pos) 
                 (or after-str overlay-display))))
    (forward-char 1)))