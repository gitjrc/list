(autoload 'compile-internal "compile" "")
(require 'compile)

(defun jrc-convert-directory (directory)
  "Check out and run ansi conversion on all c,h files in directory"
  (interactive "Ddirectory to convert: ")
  (let ((file-list (directory-files directory t "[.][ch]$"))
        (file-string nil))
    (mapcar '(lambda (s) (setq file-string (concat file-string " " s))) 
            file-list)
;    (message (concat "files: " file-string))
    ;; check of file from ade
    (message "Checking out files from ade...")
    (shell-command (concat "ade co -c 'ANSI prototypes; miscellaneous cleanup' "
                           file-string))
    ;; load each file, and convert
    (mapcar '(lambda (s)
               (find-file s)
               (ansi-convert-buffer)) file-list)
    (show-message-log)))

(defun jrc-recompile-directory (directory)
  "Run recomp on all .c files in directory"
  (interactive "Ddirectory to recomp: ")
  (let ((recomp (concat (getenv "ADE_VIEW_ROOT") "/utl/recomp -k"))
        (file-list (directory-files directory t "[.][c]$"))
        (file-string nil))
    (mapcar '(lambda (s) (setq file-string (concat file-string " " s))) 
            file-list)
    (compile-internal (concat recomp " " file-string)
                      "No more errors" "Recomp")))

