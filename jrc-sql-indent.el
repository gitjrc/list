(defconst jrc-sql-keywords "from \\|FROM \\|where \\|WHERE \\|and \\|AND \\|or \\|OR \\|group \\|GROUP \\|order \\|ORDER \\|into \\|INTO")
(defconst jrc-sql-select "select \\|SELECT")
(defconst jrc-sql-start (concat "[^(][ ]*\\(" jrc-sql-select "\\)"))

(defun jrc-sql-indent-line ()
  (interactive)
  (jrc-sql-move-line (save-excursion (re-search-backward jrc-sql-select)
                                (+ (current-column) 7))))
(defun jrc-beginning ()
  (save-excursion
    (beginning-of-line)
    (point)))

(defun jrc-end ()
  (save-excursion
    (end-of-line)
    (point)))

(defun jrc-sql-indent-back ()
  (interactive)
  (let ((finish (jrc-end))
        (start (save-excursion (re-search-backward jrc-sql-start)
                               (beginning-of-line) (point))))
    (jrc-sql-indent-region start finish)))

;; assume first line in region has "select"
(defun jrc-sql-indent-region (start finish)
  (interactive "r")
  (goto-char start)
  (let ((finish-marker (copy-marker finish))
        (anchor (save-excursion (re-search-forward jrc-sql-select finish)
                                (current-column) )))
    (forward-line 1)                    ; skip line with "select"
    (while (< (point) finish-marker)
      (jrc-sql-move-line anchor)
      (forward-line 1))))
      

(defun jrc-sql-move-line (move)
  (beginning-of-line)
  (delete-horizontal-space)
  (when (looking-at jrc-sql-keywords)
    (setq move  (- move (- (match-end 0)(match-beginning 0)))))
  (insert-string (make-string move ? ))
  (jrc-sql-view))

(defun jrc-sql-view ()
  (when (re-search-forward (concat "([ \n]*\\(" jrc-sql-select "\\)")
                           (jrc-end)
                           t)
    (jrc-sql-indent-region (jrc-beginning) (jrc-sql-find-close finish))
    ;; backup a line because caller immediately does a forward-line
    (previous-line 1)))

;; assume point is after (
(defun jrc-sql-find-close (stop)
  (save-excursion
    (let ((count 1))
      (while (and (> count 0)
                  (re-search-forward "[()]" stop t))
        (if (eq (match-string 0) "(")
            (setq count (+ count 1))
          (setq count (- count 1))))
      (point))))
