
(defun python-fix-line ()
  "Perform various syntactic fix ups for current line of Python code."
  (interactive)
  (save-excursion
    ; Narrow to make life a little easier
    (beginning-of-line)
    (let ((beginning (point)))
      (end-of-line)
      (narrow-to-region beginning (point))
      ;; Nuke extraneous whitespace
      (delete-horizontal-space)
      ;; Convert Capitalized words to lowercase, but only if no more caps      
      (let ((current-case-fold case-fold-search))
        (setq case-fold-search nil)
        (while (> (point) beginning)
          (backward-word)
          (when (looking-at "[A-Z][a-z_]+\\b")
            ( message (match-string 0))
            (downcase-region-or-word 1)
            (backward-word)))
        (setq case-fold-search current-case-fold)) ; Restore to original 
      (mapcar '(lambda (item)
                 (beginning-of-buffer)
                 (while (re-search-forward (car item) nil t)
                   (condition-case  nil
                       (replace-match (cadr item) nil nil)
                     (error nil))))
              '(("( " "(")                      ; no space after (
                 ("\\[ " "[")                   ; no space after [
                 (" \\[" "[")                   ; no space before [
                 (" \\]" "]")                   ; no space before ]
                 (" (" "(")                     ; no space before (
                 (",\\([^ ]\\)" ", \\1")        ; space after ,
                 (" ," ",")                     ; no space before ,
                 ("none" "None")                ; None, not none
                 ("true" "True")                ; True, not true
                 ("false" "False")              ; False, not false
                 ("exception" "Exception")      ; Exception, not exception
                 ("\\([^ =]\\)=" "\\1 =")       ; space before =
                 ("=\\([^ =]\\)" "= \\1")       ; space after =
                                                ; "something, else"
                 ("\\([^ ]\\)[ ]*,[ ]*\\([^ ]\\)" "\\1, \\2")
                                                ; single spaces around =
                 ("\\([^ =]\\)[ ]*=[ ]*\\([^ =]\\)" "\\1 = \\2")
                                                ; single spaces around <>
                 ("\\([^ ]\\)[ ]*\\([<>]\\)[ ]*\\([^ ]\\)" "\\1 \\2 \\3")
                                                ; single spaces around ==
                 ("\\([^ ]\\)[ ]*==[ ]*\\([^ ]\\)" "\\1 == \\2")
                 (" \\+ = " " += ")             ; fix  += 
                 (" \\! = " " != ")             ; fix  != 
                 ("%(" "% (")                   ; fix  % (
                 ("popen" "Popen")              ; fix Popen
                 ("\\b\\(\\w+\\)\\b = \\b\\(\\w+\\)\\b\\([,)]\\)"
                  "\\1=\\2\\3")                 ; no spaces on param assignment
                 ("\\( if\\| while\\| return\\)(" "\\1 (") ))
                                                ; fix 1 space before (
      ; This must be last.
      (widen))))

(defun python-helpers-dump-function-template ()
  "Insert the Python function template. The cursor must be on the first line
of the function definition. Argument names are automatically filled in."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let* ((start-paren (save-excursion (re-search-forward "(")))
           (end-paren (save-excursion (re-search-forward ")" )))
           (the-colon (save-excursion (re-search-forward ":")))
           (formals ()))
      (goto-char start-paren)
      (while (re-search-forward "\\([^,)]+\\)[,)]" end-paren t)
        (let* ((entire-match  (match-string 1))
               (formal-with-space (car 
                                   (split-string (match-string 1)  "[ ,=]" t)))
               (formal  (car (split-string formal-with-space))))
          (setq formals (if (string= formal "self") formals
                          (append  formals (list formal) )))))
      (goto-char the-colon)
      (insert "\n")
      (indent-for-tab-command)
      (insert "\"\"\"\n\n")
      (when formals
        (indent-for-tab-command)
        (insert "Args:\n")
        (indent-for-tab-command)
        (dolist (formal formals)
          (indent-for-tab-command)
          (insert (concat "    " formal ": \n")))
        (insert "\n"))
      (indent-for-tab-command)
      (insert "Returns:\n")
      (indent-for-tab-command)
      (insert "    \n")
      (indent-for-tab-command)
      (insert "\"\"\"\n"))))

