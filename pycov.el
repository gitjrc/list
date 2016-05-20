
(make-face 'pycov-not-hit)
(custom-set-face-bold 'pycov-not-hit t)
(set-face-foreground 'pycov-not-hit "red")

(defconst pycov-font-lock-keywords (purecopy
      (list
       '("\\(^!.*$\\)" 1 pycov-not-hit nil)
       ))
  "Expressions to font-lock in pycov-mode")

(defun pycov-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'pycov-mode)
  (setq mode-name "Pycov")
  (run-hooks 'pycov-mode-hook))

(provide 'pycov-mode)
