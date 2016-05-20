;(defun jrc-convert (the-list &optional prefix)
;  (interactive)
;  (when the-list
;    (cons (car the-list)
;          (jrc-convert ( cdr the-list)))))

(defun jrc-python-speed-create (the-list &optional prefix)
  (interactive)
  (when the-list
    (let* ((class-list (when (listp (cdar the-list)) (cdar the-list)))
           (class-name (when class-list (substring (caar class-list) 6)))
           (prefix-connector (when (and prefix class-name) "."))
           (new-prefix (concat prefix prefix-connector class-name))
           (new-list (or class-list the-list))
           (element (car new-list))
           (the-rest (cdr new-list)))
      (setq new-prefix (when (> (length new-prefix) 0) new-prefix))
                                        ; change "class thing" to "thing"
      (when (string-match "class " (car element))
        (setq element (cons (substring (car element) 6) (cdr element))))
                                        ; maybe add prefix
      (when (and (not class-list) 
                 prefix
                 (not (string= prefix (car element))))
        (setq element (cons (concat prefix "." (car element))
                            (cdr element))))
      (if class-name
          (append (jrc-python-speed-create class-list new-prefix);class-name)
                  (jrc-python-speed-create (cdr the-list)))
        (cons element (jrc-python-speed-create (cdr the-list) new-prefix))))))

(defvar jrc-python-current-function () 
  "Current function name in Python buffer")

(defun jrc-python-find-function-name ()
  "Return the name of the function containing the point."
  (interactive)
  (let ((result "") ;  store enclosing symbols separated by ":"
        (column 80) ;  column of current class/def
        match-column)
    (save-excursion
      (catch 'search-failed
        (while (> column 0)
          (unless (re-search-backward 
                   "^\\([ ]*\\)\\(def\\|class\\)[ ]+\\([a-zA-Z_0-9]+\\)" () t)
            (throw 'search-failed  result))
          (goto-char (match-beginning 2))
          (setq match-column (current-column)) ; Get column of current match
          (when (< match-column column) 
            (when (< 0 (length result)) ; Add ":" if we found something earlier
              (setq result (concat result ":")))
            (setq column match-column) ; This match closer to left 
            ;;          (message "%d" (match-beginning 1))
            (setq result (concat result (match-string 3)))))))
    result))

(defun jrc-python-show-function-update ()
  (setq jrc-python-current-function (jrc-python-find-function-name)))

(defun jrc-python-show-function-name()
  "Add to  python-mode-hook to maintain function name in mode line."
  (make-local-variable 'post-command-hook)
  (make-local-variable 'jrc-python-current-function)
  (add-hook 'post-command-hook 'jrc-python-show-function-update)
  (setq mode-line-buffer-identification
        (append mode-line-buffer-identification
                '("(" jrc-python-current-function ") "))))
  

(provide 'jrc-python)
