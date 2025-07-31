;;; test-simple-interactive.el --- Simple interactive test

(load "/Users/roambot/bin/lisp-projects/numerals-mode/numerals-mode.el")

;; Open and process the simple test file
(find-file "/Users/roambot/bin/lisp-projects/numerals-mode/test-simple-comma.org")
(numerals-mode)
(numerals-recalculate)

;; Display the buffer content with overlays
(message "=== FILE CONTENT WITH OVERLAYS ===")
(save-excursion
  (goto-char (point-min))
  (let ((line-num 1))
    (while (not (eobp))
      (let ((line-start (line-beginning-position))
            (line-end (line-end-position)))
        (let ((line-text (buffer-substring line-start line-end))
              (overlay-found nil))
          ;; Check for overlays on this line
          (save-excursion
            (goto-char line-start)
            (while (< (point) line-end)
              (let ((after-str (get-char-property (point) 'after-string)))
                (when after-str
                  (setq overlay-found after-str)
                  (message "Line %d: %s → %s" line-num line-text after-str)))
              (forward-char 1)))
          (unless overlay-found
            (when (string-match-p "=" line-text)
              (message "Line %d: %s (no overlay)" line-num line-text))))
        (setq line-num (1+ line-num)))
      (forward-line 1))))