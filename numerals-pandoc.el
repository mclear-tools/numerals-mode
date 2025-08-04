;;; numerals-pandoc.el --- Pandoc export integration for numerals-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Keywords: calc, convenience, export, pandoc
;; Version: 0.1.0

;; This file is part of numerals-mode.

;;; Commentary:

;; This module provides pandoc export integration for numerals-mode.
;; It creates a preprocessed temporary file with numerals overlays
;; substituted as actual text, then runs pandoc on that file.

;;; Code:

(require 'numerals-export)
(require 'numerals-utils)
(require 'numerals-parser)

(defgroup numerals-pandoc nil
  "Pandoc export settings for numerals-mode."
  :group 'numerals
  :prefix "numerals-pandoc-")

(defcustom numerals-pandoc-executable "pandoc"
  "Path to the pandoc executable."
  :type 'string
  :group 'numerals-pandoc)

(defcustom numerals-pandoc-output-format "html"
  "Default output format for pandoc export.
Common formats: html, pdf, docx, markdown, latex"
  :type 'string
  :group 'numerals-pandoc)

(defcustom numerals-pandoc-extra-args nil
  "Extra arguments to pass to pandoc.
Example: '(\"--toc\" \"--standalone\" \"--number-sections\")"
  :type '(repeat string)
  :group 'numerals-pandoc)

(defcustom numerals-pandoc-delete-temp-file t
  "Whether to delete the temporary preprocessed file after export."
  :type 'boolean
  :group 'numerals-pandoc)

(defvar numerals-pandoc-temp-file-prefix "numerals-pandoc-"
  "Prefix for temporary files created during pandoc export.")

(defun numerals-pandoc-validate-extra-args (args)
  "Validate that ARGS is a proper list of strings or nil."
  (when args
    (unless (listp args)
      (error "numerals-pandoc-extra-args must be a list, got: %S" args))
    (dolist (arg args)
      (unless (stringp arg)
        (error "All pandoc extra args must be strings, got: %S in list: %S" arg args))))
  args)

(defun numerals-pandoc-create-temp-file (source-buffer)
  "Create a temporary file with overlays substituted from SOURCE-BUFFER.
Returns the path to the temporary file."
  (let* ((source-name (buffer-name source-buffer))
         (file-extension (if (buffer-file-name source-buffer)
                             (file-name-extension (buffer-file-name source-buffer) t)
                           ".org"))
         (temp-file (make-temp-file numerals-pandoc-temp-file-prefix
                                    nil
                                    file-extension))
         (content nil))
    
    ;; Get the preprocessed content
    (with-current-buffer source-buffer
      ;; Ensure overlay data is cached
      (numerals-export-cache-overlay-data)
      
      ;; Get cached data before creating temp buffer
      (let* ((cached-overlays numerals-export-overlay-data)
             (cached-variables (when (boundp 'numerals-variables-storage)
                                 numerals-variables-storage))
             (cached-tables (numerals-export-extract-table-calculated-values))
             (temp-buffer (generate-new-buffer " *numerals-pandoc-temp*")))
        (unwind-protect
            (progn
              ;; Copy content to temp buffer
              (copy-to-buffer temp-buffer (point-min) (point-max))
              
              ;; Apply substitutions in temp buffer
              (with-current-buffer temp-buffer
                ;; Set cached data as buffer-local variables
                (setq-local numerals-export-overlay-data cached-overlays)
                (setq-local numerals-variables-storage cached-variables)
                (setq-local numerals-tables-storage cached-tables)
                
                ;; Apply overlay substitutions
                (numerals-export-substitute-overlays 'pandoc)
                
                ;; Get the final content
                (setq content (buffer-string))))
          
          ;; Clean up temp buffer
          (kill-buffer temp-buffer))))
    
    ;; Write content to temp file
    (with-temp-file temp-file
      (insert content))
    
    temp-file))

(defun numerals-pandoc-build-command (input-file output-file format extra-args)
  "Build the pandoc command with given parameters.
INPUT-FILE is the preprocessed input file.
OUTPUT-FILE is the desired output file.
FORMAT is the output format.
EXTRA-ARGS is a list of additional arguments."
  ;; Validate all arguments are strings
  (unless (stringp input-file)
    (error "INPUT-FILE must be a string, got: %S" input-file))
  (unless (stringp output-file)
    (error "OUTPUT-FILE must be a string, got: %S" output-file))
  (unless (stringp format)
    (error "FORMAT must be a string, got: %S" format))
  (unless (stringp numerals-pandoc-executable)
    (error "numerals-pandoc-executable must be a string, got: %S" numerals-pandoc-executable))
  
  (let ((command (append (list numerals-pandoc-executable
                               "-f" (if (string-match "\\.org$" input-file) "org" "markdown")
                               "-t" format
                               "-o" output-file)
                         extra-args
                         (list input-file))))
    ;; Validate final command has only strings
    (dolist (arg command)
      (unless (stringp arg)
        (error "Command argument must be string, got: %S in command: %S" arg command)))
    command))

(defun numerals-pandoc-run-process (command)
  "Run pandoc with COMMAND arguments.
Returns a plist with :success, :output, and :error."
  ;; Validate command is a list of strings
  (unless (listp command)
    (error "Command must be a list, got: %S" command))
  (dolist (arg command)
    (unless (stringp arg)
      (error "All command arguments must be strings, got: %S in command: %S" arg command)))
  
  ;; Use temporary files instead of buffers to avoid buffer killing issues
  (let* ((output-file (make-temp-file "numerals-pandoc-output"))
         (error-file (make-temp-file "numerals-pandoc-error"))
         (exit-code nil))
    (unwind-protect
        (progn
          ;; Validate command arguments
          (dolist (arg command)
            (unless (stringp arg)
              (error "Non-string argument found: %S (type: %S)" arg (type-of arg))))
          
          ;; Use apply with temp files instead of buffers
          (let ((full-args (append (list (car command) nil (list output-file error-file) nil) 
                                   (cdr command))))
            (setq exit-code (apply #'call-process full-args)))
          
          (list :success (= exit-code 0)
                :output (when (file-exists-p output-file)
                          (with-temp-buffer
                            (insert-file-contents output-file)
                            (buffer-string)))
                :error (when (file-exists-p error-file)
                         (with-temp-buffer
                           (insert-file-contents error-file)
                           (buffer-string)))))
      
      ;; Clean up temp files safely
      (when (and output-file (file-exists-p output-file))
        (delete-file output-file))
      (when (and error-file (file-exists-p error-file))
        (delete-file error-file)))))

(defun numerals-pandoc-export (output-file &optional format extra-args)
  "Export the current buffer to OUTPUT-FILE using pandoc.
Optional FORMAT specifies the output format (default from customization).
Optional EXTRA-ARGS provides additional pandoc arguments."
  (interactive
   (list (read-file-name "Export to file: "
                         nil
                         (let ((buffer-file (buffer-file-name)))
                           (concat (if buffer-file
                                       (file-name-base buffer-file)
                                     (buffer-name))
                                   "."
                                   (pcase numerals-pandoc-output-format
                                     ("html" "html")
                                     ("pdf" "pdf")
                                     ("docx" "docx")
                                     ("latex" "tex")
                                     (_ numerals-pandoc-output-format)))))))
  
  (unless (executable-find numerals-pandoc-executable)
    (error "Pandoc executable not found: %s" numerals-pandoc-executable))
  
  (unless numerals-mode
    (error "Numerals-mode must be active to export with overlays"))
  
  (let* ((format (or format numerals-pandoc-output-format))
         (extra-args (numerals-pandoc-validate-extra-args
                      (or extra-args numerals-pandoc-extra-args)))
         (source-buffer (current-buffer))
         (temp-file nil))
    
    
    ;; Validate and expand parameters
    (unless (stringp output-file)
      (error "OUTPUT-FILE must be a string, got: %S" output-file))
    (unless (stringp format)
      (error "FORMAT must be a string, got: %S" format))
    (unless (buffer-live-p source-buffer)
      (error "SOURCE-BUFFER is not live: %S" source-buffer))
    
    ;; Expand the output file path to handle ~ and ensure directory exists
    (setq output-file (expand-file-name output-file))
    (let ((output-dir (file-name-directory output-file)))
      (unless (file-directory-p output-dir)
        (make-directory output-dir t)))
    
    
    (message "Creating preprocessed file for pandoc...")
    
    ;; Create temporary file with substitutions
    (setq temp-file (numerals-pandoc-create-temp-file source-buffer))
    
    (unwind-protect
        (progn
          (message "Running pandoc conversion to %s..." format)
          
          ;; Build and run pandoc command
          (let* ((command (numerals-pandoc-build-command temp-file output-file 
                                                         format extra-args)))
            (let ((result (numerals-pandoc-run-process command)))
              
              (if (plist-get result :success)
                  (progn
                    (message "Pandoc export successful: %s" output-file)
                    ;; Optionally open the output file
                    (when (y-or-n-p "Open exported file? ")
                      (find-file output-file)))
                (error "Pandoc export failed: %s" (plist-get result :error))))))
      
      ;; Clean up temp file
      (when (and temp-file
                 (file-exists-p temp-file)
                 numerals-pandoc-delete-temp-file)
        (delete-file temp-file)))))

(defun numerals-pandoc-export-to-html ()
  "Export the current buffer to HTML using pandoc."
  (interactive)
  (let ((output-file (read-file-name "Export to HTML file: "
                                     nil
                                     (let ((buffer-file (buffer-file-name)))
                                       (concat (if buffer-file
                                                   (file-name-base buffer-file)
                                                 (buffer-name))
                                               ".html")))))
    (numerals-pandoc-export output-file "html")))

(defun numerals-pandoc-export-to-pdf (&optional output-file)
  "Export the current buffer to PDF using pandoc.
If OUTPUT-FILE is not provided, prompt for it interactively."
  (interactive)
  (let ((file (or output-file
                  (read-file-name "Export to PDF file: "
                                  nil
                                  (let ((buffer-file (buffer-file-name)))
                                    (concat (if buffer-file
                                                (file-name-base buffer-file)
                                              (buffer-name))
                                            ".pdf"))))))
    (numerals-pandoc-export file "pdf")))

(defun numerals-pandoc-export-to-docx ()
  "Export the current buffer to DOCX using pandoc."
  (interactive)
  (let ((output-file (read-file-name "Export to DOCX file: "
                                     nil
                                     (let ((buffer-file (buffer-file-name)))
                                       (concat (if buffer-file
                                                   (file-name-base buffer-file)
                                                 (buffer-name))
                                               ".docx")))))
    (numerals-pandoc-export output-file "docx")))

(defun numerals-pandoc-preview-preprocessed ()
  "Preview the preprocessed file that would be sent to pandoc.
This is useful for debugging export issues."
  (interactive)
  (unless numerals-mode
    (error "Numerals-mode must be active"))
  
  (let* ((source-buffer (current-buffer))
         (temp-file (numerals-pandoc-create-temp-file source-buffer))
         (preview-buffer (get-buffer-create "*Numerals Pandoc Preview*")))
    
    (with-current-buffer preview-buffer
      (erase-buffer)
      (insert-file-contents temp-file)
      ;; Set appropriate major mode
      (let ((source-file (buffer-file-name source-buffer)))
        (if (and source-file (string-match "\\.org$" source-file))
            (org-mode)
          (markdown-mode)))
      (goto-char (point-min)))
    
    (display-buffer preview-buffer)
    
    ;; Clean up temp file
    (when (and temp-file
               (file-exists-p temp-file)
               numerals-pandoc-delete-temp-file)
      (delete-file temp-file))))

(defun numerals-pandoc-export-region (start end output-file &optional format extra-args)
  "Export region from START to END to OUTPUT-FILE using pandoc.
This creates a temporary buffer with just the region content."
  (interactive
   (list (region-beginning)
         (region-end)
         (read-file-name "Export region to file: ")))
  
  (unless numerals-mode
    (error "Numerals-mode must be active to export with overlays"))
  
  (let* ((format (or format numerals-pandoc-output-format))
         (extra-args (or extra-args numerals-pandoc-extra-args))
         (source-buffer (current-buffer))
         (region-buffer (generate-new-buffer " *numerals-pandoc-region*"))
         (temp-file nil))
    
    (unwind-protect
        (progn
          ;; Copy region to temp buffer
          (with-current-buffer source-buffer
            (copy-to-buffer region-buffer start end))
          
          ;; Process the region buffer
          (with-current-buffer region-buffer
            ;; Enable numerals-mode to process calculations
            (numerals-mode 1)
            ;; Force update
            (numerals-update-buffer))
          
          ;; Create temp file from region buffer
          (setq temp-file (numerals-pandoc-create-temp-file region-buffer))
          
          ;; Run pandoc
          (message "Running pandoc on region...")
          (let* ((command (numerals-pandoc-build-command temp-file output-file 
                                                         format extra-args))
                 (result (numerals-pandoc-run-process command)))
            
            (if (plist-get result :success)
                (message "Pandoc export successful: %s" output-file)
              (error "Pandoc export failed: %s" (plist-get result :error)))))
      
      ;; Clean up
      (when (buffer-live-p region-buffer)
        (kill-buffer region-buffer))
      (when (and temp-file
                 (file-exists-p temp-file)
                 numerals-pandoc-delete-temp-file)
        (delete-file temp-file)))))

;; Shell command wrapper for use outside Emacs
(defun numerals-pandoc-create-shell-wrapper ()
  "Create a shell script that can preprocess and run pandoc on numerals files."
  (interactive)
  (let ((script-path (read-file-name "Save shell wrapper to: "
                                     nil
                                     "numerals-pandoc.sh")))
    (with-temp-file script-path
      (insert "#!/bin/bash\n")
      (insert "# Numerals-mode pandoc wrapper\n")
      (insert "# Usage: numerals-pandoc.sh input.org output.html [pandoc-args...]\n\n")
      (insert "if [ $# -lt 2 ]; then\n")
      (insert "    echo \"Usage: $0 input-file output-file [pandoc-args...]\"\n")
      (insert "    exit 1\n")
      (insert "fi\n\n")
      (insert "INPUT=\"$1\"\n")
      (insert "OUTPUT=\"$2\"\n")
      (insert "shift 2\n\n")
      (insert "# Run Emacs in batch mode to preprocess the file\n")
      (insert "emacs --batch -l ")
      (insert (expand-file-name "numerals-mode.el" 
                                (file-name-directory (locate-library "numerals-mode"))))
      (insert " \\\n")
      (insert "      --eval \"(require 'numerals-pandoc)\" \\\n")
      (insert "      --eval \"(find-file \\\"$INPUT\\\")\" \\\n")
      (insert "      --eval \"(numerals-mode 1)\" \\\n")
      (insert "      --eval \"(numerals-pandoc-export \\\"$OUTPUT\\\" nil '($*))\" \\\n")
      (insert "      2>/dev/null\n"))
    
    (chmod script-path #o755)
    (message "Shell wrapper created: %s" script-path)))

(defun numerals-pandoc-export-pdf-with-default-name ()
  "Export current buffer to PDF with a sensible default filename.
This is a non-interactive version suitable for calling from other functions."
  (let* ((buffer-file (buffer-file-name))
         (base-name (if buffer-file
                        (file-name-base buffer-file)
                      (buffer-name)))
         ;; Remove any problematic characters from buffer name
         (safe-name (replace-regexp-in-string "[<>:\"/\\|?*]" "_" base-name))
         (output-file (concat safe-name ".pdf")))
    ;; Explicitly pass empty list for extra-args to avoid issues with defaults
    (numerals-pandoc-export output-file "pdf" '())))

(provide 'numerals-pandoc)
;;; numerals-pandoc.el ends here