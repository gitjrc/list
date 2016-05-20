(defun find-python-functions()
  (interactive)
  (let ((result)
        (enclosing-class-or-function))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward
              "^\\([ ]*\\)\\(def\\|class\\)[ ]+\\([^ (]+\\)[ (]"
                                nil t)
        (let ((spaces (match-string  1))
              (class-or-function-name (match-string  3)))
          (if (> (length spaces) 0) ; if !0, this is an embedded function/class
              (setq class-or-function-name 
                    (concat enclosing-class-or-function  "."
                            class-or-function-name))
            (setq enclosing-class-or-function class-or-function-name)); save
          (setq result (cons (cons class-or-function-name (point))
                             result)))))
    ;;;  Now sort result.
    (setq  result (sort result
                        '(lambda (x y) (let ((nx (car x)) (px (cdr x))
                                             (ny (car y)) (py (cdr y)))
                                         (if (string= nx ny) (< px py)
                                           (string< nx ny))))))
    ;;;  Replace "something." with "  "
    (let ((temp  result))
      (while temp
        (let* ((first-cell (car temp))
               (full-symbol (car  first-cell)))
          (if (string-match  ".*\\."  full-symbol)
              (setcar first-cell (replace-match  "  " nil nil  full-symbol))))
        (setq temp (cdr  temp))))
    ( message result)))

    
  