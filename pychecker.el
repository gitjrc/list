(defun my-hook()
;  (save-excursion
;    (goto-char  compilation-filter-start)
    (message "%d %d"  compilation-filter-start  (point))
    (message "%s" my_root)

    (toggle-read-only)
    (goto-char compilation-filter-start)
    ( insert (number-to-string  (point)))
    (toggle-read-only)
)

;;;###autoload
(defun pylint ()
  "Run pylint against the file behind the current buffer after
  checking if unsaved buffers should be saved."

  (interactive)
  (let* ((file (buffer-file-name (current-buffer)))
         (config (if (file-exists-p "./pylintrc-tcompile")
                     "./pylintrc-tcompile"))
         (options (concat "--msg-template='{path}:{line}: [{msg_id}({symbol}),"
                          "{obj}] {msg}' "
                          (concat "--rcfile=" config)))
         (command (concat "pylint " options " " file)))
;    (setenv "PYTHONPATH" path)
    (setq my_root "mind")
    (add-hook 'compilation-filter-hook 'my-hook)
    (save-some-buffers 
     (not compilation-ask-about-save) nil) ; save  files.
    (compile  command)
    (remove-hook 'compilation-filter-hook 'my-hook)))
                      
