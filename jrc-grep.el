(require 'prompt-for-symbol)

(defun jrc-grep--wrapper (symbol)
  (grep (concat "grep -nHi -e " 
                "'" symbol "'"
                " "
                (file-name-nondirectory (buffer-file-name)))))

(defun jrc-grep-symbol ()
  (interactive)
  (let ((symbol (prompt-for-symbol "grep")))
    (jrc-grep--wrapper symbol)))

(defun jrc-grep-region (start finish)
  (interactive "r")
  (jrc-grep--wrapper (buffer-substring start finish)))
