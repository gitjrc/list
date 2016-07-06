
(defconst *jrc-speed-frame* nil)
(defconst *jrc-speed-buffer-name* nil)
(defconst *jrc-speed-alist* nil)

(defun jrc-speed-initialize ()
  "Initialize speed list for this buffer; create new frame if necessary"
  (interactive)
  (unless (or (string= mode-name "Python")
              (string= mode-name "JPython")
              (string= mode-name "Shell-script")
              (string= mode-name "C/l")
              (string= mode-name "Perl")
              (string= mode-name "Java/l")
              (string= mode-name "Vocola")
              (string= mode-name "GNUmakefile")
              (boundp 'buffer-find-functions))
    (error "Cannot find function names for this buffer"))
  ;; may be create the frame
  (unless (and *jrc-speed-frame* (frame-live-p *jrc-speed-frame*))
    (setq *jrc-speed-frame* (make-frame)))
  (let* ((original-frame (selected-frame))
         (here (point))
         (page-length 60)
         (is-python (string= mode-name "Python"))
         (is-crap (string= mode-name "Perl"))
         (is-shell (string=  mode-name "Shell-script"))
         (is-c (string=  mode-name "C/l"))
         (is-makefile (string= mode-name "GNUmakefile"))
         (is-vocola (string=  mode-name "Vocola"))
         (is-java (string=  mode-name "Java/l"))
         (speed-buffer (get-buffer-create "*speed*"))
         (function-alist 
          (cond  (is-python (jrc-speed-find-python-functions))
                 (is-shell (jrc-speed-find-shell-functions))
                 (is-c (jrc-speed-find-c-functions))
                 (is-crap (jrc-speed-find-perl-functions))
                 (is-makefile (jrc-speed-find-makefile-functions))
                 (is-vocola (jrc-speed-find-vocola-functions))
                 (is-java (jrc-speed-find-java-functions))
                 (t (sort 
                     (jrc-speed-create-function-alist) 
                     '(lambda (x y) (let ((nx (car x)) (px (cdr x))
                                          (ny (car y)) (py (cdr y)))
                                      (if (string= nx ny) (< px py)
                                        (string< nx ny))))))))
         (counter 0)
         (normal-font "Inconsolata-14")
         (small-font "Inconsolata-12")
         (number-columns (1+ (/ (length function-alist) page-length)))
         (multiple-column-p (> number-columns 1))
         (truncation-length (if multiple-column-p 32 50))
         (frame-width (if multiple-column-p (* number-columns 37) 55))
         (font (format "Inconsolata-%d" (max 8(- 20 number-columns))))
         temp variable-alist )
    (if (string= mode-name "JDE")
        (setq variable-alist (sort (java-find-variables) 
                                   '(lambda (x y) (let ((nx (car x)) 
                                                        (px (cdr x))
                                                        (ny (car y)) 
                                                        (py (cdr y)))
                                                    (if (string= nx ny)
                                                        (< px py)
                                                      (string< nx ny)))))))
    (set-frame-position *jrc-speed-frame* 5 100)
    (set-frame-size *jrc-speed-frame* frame-width 62)
    (save-window-excursion
      (select-frame *jrc-speed-frame*)
      (set-frame-font font)
      (switch-to-buffer speed-buffer) 
      (erase-buffer)
      (setq temp function-alist)
      (while temp
        (let* ((temp-string (caar temp)) ; may need to truncate this
               (temp-string-length (length temp-string))
               (display-length (min truncation-length temp-string-length))
               (display-string (substring temp-string 0 display-length))
               (first-column-format 
                (if multiple-column-p
                    (format "%%2d %%-%ds\n" truncation-length)
                  "%3d %s\n"))
               (remainder-column-format
                (format " %%3d %%-%ds" truncation-length))
               (first-column-p (= 0 (/ counter page-length))))
          (if first-column-p 
              (insert (format first-column-format counter display-string))
            (goto-line (1+ (% counter page-length)))
            (end-of-line)
            (insert (format remainder-column-format counter display-string))))
        (setq counter (1+ counter))
        (setq temp (cdr temp)))
      (setq *jrc-speed-alist* function-alist)
      (unless nil ;; Don't execute this stuff for now
        (setq *jrc-speed-alist* (append *jrc-speed-alist* variable-alist))
        (setq temp variable-alist)
        (while temp
          (insert (format "%2d %s\n" counter (caar temp)))
          (setq counter (1+ counter))
          (setq temp (cdr temp))))
;      (jrc-numbers-mode -1)             ;  this turns off numbers mode
      (select-frame original-frame)))
    (setq *jrc-speed-buffer-name* (buffer-name)))

(defun jrc-speed-find-c-functions ()
  (interactive)
  (let ((buffer (get-buffer-create " *SCOPE OUTPUT* "))
        (scope "/ade_autofs/ade_infra/ORAREVIEW_MAIN_GENERIC.rdd/LATEST/orareview/generic_src/py/ora_scope.py")
        (file-name (buffer-file-name)))
    (save-excursion
      (set-buffer buffer)
      (delete-region (point-min) (point-max))
      (call-process scope nil buffer nil "-t" file-name)
      (read (buffer-string)))))

(defun jrc-speed-expand (num)
  "expand nth outline element"
  (interactive "p")
  (beginning-of-buffer)
  (search-forward (concat " " (number-to-string num) " "))
  (show-subtree))

(defun jrc-speed-collapse (num)
  "collapse nth outline element"
  (interactive "p")
  (beginning-of-buffer)
  (search-forward (concat " " (number-to-string num) " "))
  (hide-subtree))

(defun jrc-speed-relative-line-to-absolute (relative-line)
  (let* ((original-frame (selected-frame)))
    (save-window-excursion
      (select-frame *jrc-speed-frame*)
;    (switch-to-buffer speed-buffer) 
      (let* ((base 100)
             (window-start-raw (count-lines 1 (window-start)))
             (window-start-tens (% window-start-raw base))
             (window-start-hundreds (- window-start-raw window-start-tens)))
        (select-frame original-frame)
        (if (< relative-line window-start-tens)
            (+ window-start-hundreds relative-line base)
          (+ window-start-hundreds relative-line))))))

(defun jrc-speed-jump (index)
  "jump to nth function name"
  (interactive "p")
  (unless (eq *jrc-speed-buffer-name* (buffer-name))
    (error "no speed list for this buffer"))
  (goto-char (cdr (nth (jrc-speed-relative-line-to-absolute index)
                       *jrc-speed-alist*))))

(defun jrc-speed-find-perl-functions()
 (interactive)
 (let ((result))
   (save-excursion
     (beginning-of-buffer)
     (while (re-search-forward "^sub[ ]+\\([a-zA-Z0-9_]+\\)" nil t)
       (let ((function-name (match-string 1)))
         (setq  result (cons (cons function-name (point))  result)))))
   (jrc-speed-sort-result result)))

(defun jrc-speed-find-vocola-functions ()
  (interactive)
  (let ((result))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward "^#-[ ]+\\([a-zA-Z0-9_ ]+$\\)" nil t)
        (let ((function-name (match-string 1)))
          (setq  result (cons (cons function-name (point))  result)))))
    (jrc-speed-sort-result result)))  

(defun jrc-speed-find-shell-functions ()
  (interactive)
  (let ((result))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward "^[ ]*\\([a-zA-Z0-9_]+\\)[ ]*()" nil t)
        (let ((function-name (match-string 1)))
          (setq  result (cons (cons function-name (point))  result))))
      (when (not result)
        (beginning-of-buffer)
        (while (re-search-forward 
                "^[ ]*function[ ]+\\([a-zA-Z0-9_]+\\)[ ]*[{(]" nil t)
          (let ((function-name (match-string 1)))
            (setq  result (cons (cons function-name (point))  result))))))
    (jrc-speed-sort-result result)))

(defun jrc-speed-find-makefile-functions ()
  (interactive)
  (let ((result))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward "^\\([a-zA-Z0-9_]+\\)[ ]*[=:]" nil t)
        (let ((function-name (match-string 1)))
          (setq  result (cons (cons function-name (point))  result)))))
    (jrc-speed-sort-result result)))  

(defun jrc-speed-find-python-functions ()
  (interactive)
  (let ((result)
        (enclosing-class-or-function))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward
              "^\\([ ]*\\)\\(def\\|class\\)[ ]+\\([^ (:]+\\)[ (:]"
                                nil t)
        (let ((spaces (match-string  1))
              (class-or-function-name (match-string  3)))
          (if (> (length spaces) 0) ; if !0, this is an embedded function/class
              (setq class-or-function-name ;  Make name outer.inner
                    (concat enclosing-class-or-function  "."
                            class-or-function-name))
            (setq enclosing-class-or-function class-or-function-name)); save
          (setq result (cons (cons class-or-function-name (point))
                             result)))))
    ;;;  Now sort result.  We need outer.inner to sort properly.
    (setq  result  (jrc-speed-sort-result result))
    ;;;  Replace "outer." with "  "
    (let ((temp  result))
      (while temp
        (let* ((first-cell (car temp))
               (full-symbol (car  first-cell)))
          (if (string-match  ".*\\."  full-symbol)
              (setcar first-cell (replace-match  "  " nil nil  full-symbol))))
        (setq temp (cdr  temp))))
    result))

(defun jrc-speed-sort-result (result)
  (sort result
        '(lambda (x y) (let ((nx (car x)) (px (cdr x))
                             (ny (car y)) (py (cdr y)))
                         (if (string= nx ny) (< px py)
                           (string< nx ny))))))

(defun jrc-speed-create-function-alist ()
  (interactive)
    (save-excursion
      (let ((result ))
        (end-of-buffer)
        (while 
            (let ((answer (funcall buffer-find-functions t)))
              (when answer (setq result (cons (cons answer (point)) result)))
              answer))
        result)))

(defun jrc-speed-grep (index)
  "run grep using index's string"
  (interactive "p")
  (grep (concat "grep -nHi -e " 
                (car (nth index *jrc-speed-alist*))
                " "
                (file-name-nondirectory (buffer-file-name)))))

(defun jrc-speed-insert (index)
  "Insert index's string at point"
  (interactive "p")
  (insert (car (nth index *jrc-speed-alist*))))


(defun jrc-speed-find-java-functions ()
  (interactive)
  (let ((result)
        (kw "protected\\|private\\|public"))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward
              (concat "^[ ]*\\(" kw "\\)[ ]+[a-zA-Z0-9_]+[ ]*[a-zA-Z0-9_]*"
                      "[ ]+\\([a-zA-Z0-9_]+\\)[ ]*(") nil t)
        (let ((method-name (match-string 2)))
          (setq result (cons (cons method-name (point))
                             result)))))
    (jrc-speed-sort-result result)))
