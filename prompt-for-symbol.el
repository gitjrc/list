(defun prompt--get-current-word ()
  (save-excursion
    (let ((search-string "[^a-zA-Z_0-9.]"))
      (re-search-backward search-string)
      (forward-word 1) 
      (backward-word 1)
      ;(message (number-to-string (point)))(sleep-for 1)
      (let ((start (point)))
        ; If point is at eob re-search-forward will fail.
        ; This conditional is probably not necessary.
        (if (re-search-forward search-string nil t)
            (backward-word 1))
        (forward-word 1)
        ;(message (number-to-string (point)))(sleep-for 1)
        (buffer-substring start (point))))))

(defun prompt-for-symbol (prompt)
  "Find symbol at point, using that as default for mini buffer read."
  (let  ((symbol (prompt--get-current-word)))
    (read-string (concat prompt " ("  symbol "): ") nil nil  symbol)))

(provide 'prompt-for-symbol)
