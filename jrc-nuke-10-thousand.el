(defun jrc-nuke-10-thousand (number-of-lines)
  "Delete first or last 10,000 lines from current buffer."
  (interactive "p")
  (when (> number-of-lines 0)
    (beginning-of-buffer))
  (let ((start  (point)))
    (forward-line number-of-lines)
    (delete-region start (point))
    (end-of-buffer)))
