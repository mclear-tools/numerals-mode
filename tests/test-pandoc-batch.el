;;; test-pandoc-batch.el --- Test pandoc export in batch mode -*- lexical-binding: t; -*-

;; This test verifies that pandoc export works correctly in batch mode

(require 'ert)

;; Add the numerals-mode directory to load-path
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

(require 'numerals-mode)
(require 'numerals-pandoc)

(ert-deftest test-pandoc-temp-file-creation ()
  "Test that temporary file creation works correctly."
  (with-temp-buffer
    (insert "Base = 100\n")
    (insert "Tax = 0.1\n")
    (insert "Total = Base * (1 + Tax)\n")
    (org-mode)
    (numerals-mode 1)
    (numerals-update-buffer)
    
    ;; Create temp file
    (let ((temp-file (numerals-pandoc-create-temp-file (current-buffer))))
      (should (file-exists-p temp-file))
      
      ;; Check content
      (let ((content (with-temp-buffer
                       (insert-file-contents temp-file)
                       (buffer-string))))
        (should (string-match "Base = 100" content))
        (should (string-match "Total = Base \\* (1 \\+ Tax) => 110" content)))
      
      ;; Clean up
      (delete-file temp-file))))

(ert-deftest test-pandoc-command-building ()
  "Test pandoc command construction."
  (let ((numerals-pandoc-executable "pandoc")
        (command (numerals-pandoc-build-command 
                  "input.org" "output.html" "html" '("--toc"))))
    (should (equal (car command) "pandoc"))
    (should (member "-f" command))
    (should (member "org" command))
    (should (member "-t" command))
    (should (member "html" command))
    (should (member "--toc" command))
    (should (member "input.org" command))
    (should (member "output.html" command))))

(ert-deftest test-pandoc-table-export ()
  "Test that table formulas are correctly exported."
  (with-temp-buffer
    (insert "#+NAME: test-table\n")
    (insert "| A | B | C |\n")
    (insert "|---+---+---|\n")
    (insert "| 1 | 2 | =A2+B2 |\n")
    (insert "| 3 | 4 | =A3+B3 |\n")
    (insert "| =SUM(A2:A3) | =SUM(B2:B3) | =SUM(C2:C3) |\n")
    (org-mode)
    (numerals-mode 1)
    (numerals-update-buffer)
    
    ;; Create temp file
    (let ((temp-file (numerals-pandoc-create-temp-file (current-buffer))))
      (let ((content (with-temp-buffer
                       (insert-file-contents temp-file)
                       (buffer-string))))
        ;; Check that formulas are replaced with results
        (should-not (string-match "=A2\\+B2" content))
        (should-not (string-match "=SUM" content))
        ;; The actual calculated values should be present
        (should (string-match "3" content))  ; Result of 1+2
        (should (string-match "7" content))) ; Result of 3+4
      
      ;; Clean up
      (delete-file temp-file))))

;; Run all tests
(ert-run-tests-batch-and-exit)