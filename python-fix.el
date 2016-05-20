
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
;                 (perform-replace (car item) (cadr item) nil t nil nil)
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
                 (" \\+ = " " += ")           ; fix  += 
                 (" \\! = " " != ")           ; fix  != 
                 ("%(" "% (")                 ; fix  % (
                 ("popen" "Popen")      ; fix Popen
                 ("\\b\\(\\w+\\)\\b = \\b\\(\\w+\\)\\b\\([,)]\\)"
                  "\\1=\\2\\3")             ; no spaces on param assignment
              ))


      ; This must be last.
      (widen))
    ))

