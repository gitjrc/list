
;;;###autoload
(defun pylint ()
  "Run pylint against the file behind the current buffer after
  checking if unsaved buffers should be saved."

  (interactive)
  (let* ((file (buffer-file-name (current-buffer)))
         (pylint "/ade_autofs/ade_infra/DBDEVENV_MAIN_GENERIC.rdd/LATEST/dbdevenv/bin/pylint ")
         (view (getenv "ADE_VIEW_NAME"))
         (view_root (getenv "ADE_VIEW_ROOT"))
         (root (if view_root view_root "/ade_autofs/ade_infra/ORAREVIEW_MAIN_GENERIC.rdd/LATEST"))
         (config (if (file-exists-p "./pylintrc-tcompile")
                     "./pylintrc-tcompile"
                   "~/exadata-git/python-utilities/pylintrc"))
         (options (concat "--output-format=parseable "
                          (concat "--rcfile=" config)))
         ;; (path (concat root "/orareview:" 
         ;;               root "/orareview/oracle:" 
         ;;               root "/orareview/codereview:"
         ;;               root "/orareview/generic_src/py"))
         (command (concat "pylint " options " " file)))
;    (setenv "PYTHONPATH" path)
    (setq my_root "mind")
    (add-hook 'compilation-filter-hook 'my-hook)
    (save-some-buffers 
     (not compilation-ask-about-save) nil) ; save  files.
    (compile  command)
    (remove-hook 'compilation-filter-hook 'my-hook)))
                      
