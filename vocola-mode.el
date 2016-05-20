(defvar vocola-font-lock-keywords (purecopy
      (list
       ;; Comment 
       '("\\(^#.*$\\)" 1 font-lock-comment-face))))


(defun vocola-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'vocola-mode)
  (setq mode-name "Vocola")
  (set (make-local-variable 'font-lock-defaults) '(vocola-font-lock-keywords))
  (run-hooks 'vocola-mode-hook))

(provide 'vocola-mode)
