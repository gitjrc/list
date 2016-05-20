;;
;; Print using enscript.
;; 
(defvar enscript-printer-command "/usr/bin/enscript" 
  "*Command to spool to printer")

(defvar enscript-q "-Pjrc")

(defun enscript-pp ()
  (let ((bn (buffer-name)))
    (delete nil (list "-G" (cond ((string-match "\\.c$" bn) "-Ec")
				 ((string-match "\\.java$" bn) "-Ejava")
				 ((string-match "\\akefile$" bn) "-Emakefile")
				 ((string-match "\\.sql$" bn) "-Esql")
				 ((string-match "\\.sh$" bn) "-Esh")
				 ((string-match "\\.py$" bn) "-Epython")
				 ((or
                                   (string-match mode-name "CPerl")
                                   (string-match "\\.pl$" bn)) "-Eperl")
				 ((string-match "\\.el$" bn) "-Eelisp")
				 ((string-match "\\.html$" bn) "-Ehtml")
				 ((string-match "\\.h$" bn) "-Ec"))))))

(defun enscript-buffer ()
   "enscript buffer contents as with Unix command `enscript'."
   (interactive)
   (enscript-region-1 (point-min) (point-max) (enscript-pp)))

(defun a2ps-buffer ()
   "enscript buffer contents as with Unix command `a2ps'."
   (interactive)
   (let ((enscript-printer-command "/usr/local/bin/a2ps"))
     (enscript-region-1 (point-min) (point-max) (enscript-pp))))


(defun enscript-buffer-linenumbers ()
   "enscript buffer contents as with Unix command `enscript', lines numbered"
   (interactive)
   (enscript-region-1 (point-min) (point-max) (append (enscript-pp)
						       (list "-C"))))

(defun enscript-buffer-widepage ()
   "enscript buffer contents as with Unix command `enscript', landscape mode."
   (interactive)
   (enscript-region-1 (point-min) (point-max) (append (enscript-pp) 
						       (list "-r"))))
(defun 2enscript-buffer ()
   "enscript buffer contents as with Unix command `enscript -2r'."
   (interactive)
   (enscript-region-1 (point-min) (point-max) (list "-2r")))

(defun enscript-region (start end)
   "Print region contents as with Unix command `enscript'."
    (interactive "r")
    (enscript-region-1 start end (enscript-pp)))

(defun a2ps-region (start end)
   "Print region contents as with Unix command `a2ps'."
    (interactive "r")
    (let ((enscript-printer-command "/usr/local/bin/a2ps"))
      (enscript-region-1 start end (enscript-pp))))

(defun enscript-region-widepage (start end)
   "Print region contents as with Unix command `enscript'."
    (interactive "r")
    (enscript-region-1 start end (append (enscript-pp) 
					  (list "-r"))))

(defun 2enscript-region (start end)
   "Print region contents as with Unix command `enscript'."
    (interactive "r")
    (enscript-region-1 start end (list "-2r")))

(defun enscript-region-1 (start end switches)
  (let ((name (concat (buffer-name) " Emacs buffer"))
        (width tab-width))
    (save-excursion
     (message "Spooling...")
     (if (/= tab-width 8)
         (let ((oldbuf (current-buffer)))
          (set-buffer (get-buffer-create " *spool temp*"))
          (widen) (erase-buffer)
          (insert-buffer-substring oldbuf start end)
          (setq tab-width width)
          (untabify (point-min) (point-max))
          (setq start (point-min) end (point-max))))
     (apply 'call-process-region
            (nconc (list start end enscript-printer-command
                         nil nil nil)
                   (nconc (list "-t" name); enscript-q)
                          switches)))
     (message "Spooling...done"))))

