(defconst jrc-vocola-assignment-column 25)
(defvar jrc-vocola-time 0)

(defun jrc-vocola-equals ()
  "insert = at column 25"
  (interactive)
  (jrc-vocola-insert-string "="))

(defun jrc-vocola-assign ()
  "insert := at column 25"
  (interactive)
  (jrc-vocola-insert-string ":="))

(defun jrc-vocola-indent ()
 "insert spaces up to column 25"
 (interactive)
 (jrc-vocola-insert-string " "))

(defun jrc-vocola-insert-string (the-string)
  (delete-horizontal-space)
  (insert (make-string (- jrc-vocola-assignment-column (current-column)) ? ))
  (insert (concat the-string " ")))


(defun jrc-vocola-generate (file)
  (interactive "sEnter file name ")
  (let* ((error-file "/home/john/vocola/commands/vcl2py_log.txt")
        (perl "/usr/local/bin/perl ")
        (input-file (concat file ".vcl "))
        (natlink "jrc@jrc.dyndns.ws:/cygdrive/c/natlink/natlink/macrosystem/")
        (vocola "/home/john/vocola/2.6/vocola/exec/vcl2py.pl -f ")
        (output "/home/john/vocola/2.6/vocola/generated")
        (output-file (concat output "/" file "_vcl.py ")))
    (shell-command(concat perl vocola input-file output))
    (if (file-exists-p error-file)
        (find-file error-file)
      (shell-command (concat "scp -v jrc.dyndns.ws "
                             output-file
                             (shell-quote-argument natlink))))))
    
        
(defun jrc-vocola-timing-and-fill ()
  (interactive)
  (let ((seconds-count (-  (cadr (current-time)) jrc-vocola-time ))
        (word-count (count-words-region (region-beginning) (region-end))))
    (fill-region (region-beginning) (region-end))
    (message (concat (number-to-string seconds-count) " seconds,"  
                     (number-to-string word-count) " words, "
                     (number-to-string 
                      (truncate (/ (float (* word-count 60))
                                   (float seconds-count)))) " WPM"))))
                                        
             
    
