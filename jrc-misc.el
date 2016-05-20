(defun jrc-function-template ()
  "Insert function template at point"
  (interactive)
  (insert"
/*
  NAME:
  PARAMETERS:
  REQUIRES:
  DESCRIPTION:
  RETURNS:
  NOTES:
*/"))


(defun show-backslash-keymap ()
  (interactive)
  (let ((bufname "*backslash map*"))
    (get-buffer-create bufname)
    (pop-to-buffer bufname)
    (insert "Key bindings for backslash keymap:")(newline)
    (describe-bindings-internal my-backslash-map)))

(defun show-controldot-keymap ()
  (interactive)
  (let ((bufname "*controldot map*"))
    (get-buffer-create bufname)
    (pop-to-buffer bufname)
    (insert "Key bindings for controldot keymap:")(newline)
    (describe-bindings-internal my-controldot-map)))

(defun move-comment () 
  "move a C comment to right margin"
  (interactive) 
  (save-excursion
     (end-of-line)
     (search-backward "/*")
     (delete-horizontal-space)
     (end-of-line)
     (setq addn (- 79 (current-column)))                    
     (search-backward "/*")
     (while (> addn 0)
       (insert ? )
       (setq addn (1- addn))
     )))

(defun jrc-move-assign (beg end)
  "move all assignments in region so right hand sides line up"
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (let ((endmark (copy-marker end))
	  (col 0))
      (while (< (point) endmark)
	(beginning-of-line)
	(search-forward "=")
	(if (> (current-column) col)
	    (setq col (current-column)))
	(forward-line 1))
      (jrc-move-line beg end col "="))))
;;
;; private func to move part of a line
(defun jrc-move-line (beg end targ-column search-string)
  (save-excursion
    (goto-char beg)
    (let ((endmark (copy-marker end)))
      (while (< (point) endmark)
	(beginning-of-line)
	(search-forward search-string)
	(backward-char (length search-string))
	(delete-horizontal-space)
	(setq addn (- targ-column (current-column)))
	(backward-char) 
	(if (not (looking-at ":\\|<\\|>\\|!")) ;; keep :=, <=, etc intact
	  (forward-char 1))
	(insert (make-string addn ? ))
	(forward-line 1)))))

(defun jrc-toggle-frames ()
  "toggle exposure of all frames"
  (interactive)
  (mapcar '(lambda (f) (if (frame-totally-visible-p f)
                           (lower-frame f)
                         (raise-frame f))) 
          (frame-list)))
