(defun javascript-buffer-functions()
  (make-local-variable 'buffer-find-functions)
  (setq buffer-find-functions 
        '(lambda (unused)
           (if (re-search-backward "function[ \t]+\\([a-zA-Z_0-9]+\\)(" () t)
               (match-string 1)
             nil))))
