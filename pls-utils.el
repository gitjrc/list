;;
;; add the following in .emacs to display the plsql function name in the 
;; pls-mode mode line:
;;
;; (add-hook 'pls-mode-hook 'pls-show-function-name)
;;
;; add the following to bind pls-insert-trace to control-backslash t:
;; (global-set-key "\C-\\t" 'pls-insert-trace)
;;
;; These require pls-mode to be available.
;;
(require 'pls-mode)

(defconst pls-function-search-string 
  "^[ \t]*\\(PROCEDURE\\|procedure\\|function\\|FUNCTION\\|CREATE or replace VIEW\\)")
; dbms_prvt_trace
(defvar pls-parameter-dump-function "showdiffs_log.trace_expression"
  "name of function dumped by pls-parameter-dump")

(defvar pls-procedure-enter-function "showdiffs_log.trace_enter_procedure"
  "name of procedure dumped by pls-enter-procedure")

(defvar pls-prepend-package-name nil
  "*If non-nil, prepends package name to names generated by pls-new-errorname 
and pls-insert trace")

(defvar pls-error-numbers-only nil
  "*If non-nil, pls-new-errorname outputs only a unique three digit number")

(defun pls-find-package-name ()
  "Return the name of the current package"
  (interactive)
  (save-excursion
    (re-search-backward 
     "PACKAGE[ \t]*BODY[ \t]*\\([a-zA-Z_0-9]+\\)[ \t\n]*"))
    (match-string 1))

(defun pls-find-function-name (&optional move-point)
  "Return the name of the function or procedure containing the point."
  (interactive)
  (let (location result)
    (save-excursion
      (let* ((name-string "[ \t]+\\([$a-zA-Z_0-9]+\\)\\((\\| \\|\t\\|\n\\)")
             (master-string (concat pls-function-search-string name-string)))
        (unless move-point (end-of-line))
	(if (re-search-backward master-string () t)
            (setq result (match-string 2)))
        (setq location (point))))
    (when move-point
      (goto-char location))
    (if (interactive-p)
        (message result)
      result)))

(defun pls-parameter-dump ()
  "insert, at point, function calls to dump values of all IN parameters"
  (interactive)
  (let (parameter-list
        parameter-start
        parameter-end
        parameter-name)
    (save-excursion
      (if (re-search-backward pls-function-search-string () t)
          (progn
            (re-search-forward "(")
            (setq parameter-start (point))
            (re-search-forward ")")
            (setq parameter-end (point))
            (goto-char parameter-start)
            (while (re-search-forward
                    "[ \t]*\\([a-zA-Z_0-9]+\\)[ \t]*IN[ \t].*[,)]"
                    parameter-end
                    t)
              (setq parameter-name (match-string 1))
              (setq parameter-list (append parameter-list
                                           (list parameter-name)))))))
    (mapcar '(lambda (s)
               (pls-tab) 
               (insert (concat pls-parameter-dump-function "('" s "' , " s ");
")))
            parameter-list)))
      

(defun pls-new-errorname ()
  "Insert a new named error at the point.
The named errors are of the form \"<funcname>ddd\".
"
  (interactive)
  (let ((func-name (pls-find-function-name)) 
	(errnm ()))
    (if pls-prepend-package-name
	(setq func-name 
;	      (concat (car (split-string 
;			    (file-name-sans-extension 
;			     (file-name-nondirectory (buffer-file-name)))
;			    "_body"))
              (concat (pls-find-package-name)
		      "." func-name)))
    (if pls-error-numbers-only
        (setq func-name nil))
    (while (null errnm)
      (setq errnm
            (concat
             func-name
             (format "%03d" (% (abs (random t)) 1000))
             ))
      (save-excursion
        (goto-char (point-min))
        (if (search-forward errnm () t 1)
            (setq errnm ()))))
    (insert errnm)))

	
(defun pls-insert-trace ()
  (interactive)
  (progn
    (insert "put_line(\n")
    (pls-tab)
    (insert "trace_file,\n")
    (pls-tab)
    (insert "\'")
    (pls-new-errorname)
    (insert "\'")
    (insert ",\n")
    (pls-tab)))

(defun pls-enter-procedure ()
  "insert procedure entered boilerplate"
  (interactive)
  (let ((name (pls-find-function-name)))
    (if pls-prepend-package-name
        (setq name (concat (pls-find-package-name) "." name)))
    (pls-tab)
    (insert (concat pls-procedure-enter-function " ('"
                    name "');\n"))
    (pls-tab)))

(defun pls-insert-location-trace ()
  "insert trace_location := <pls err name>; in buffer"
  (interactive)
  (progn
    (insert "trace_location := \'")
    (pls-new-errorname)
    (insert "\';\n")
    (pls-tab)))


(defvar pls-current-function () "Current function name in a pls-mode buffer")

(defun pls-show-function-update ()
  (setq pls-current-function (pls-find-function-name)))

(defun pls-show-function-name()
  "Add to pls-mode-hook to maintain function name in mode line"
  (make-local-variable 'post-command-hook)
  (make-local-variable 'pls-current-function)
  (add-hook 'post-command-hook 'pls-show-function-update)
  (setq mode-line-buffer-identification
        (append mode-line-buffer-identification
                '("(" pls-current-function ") "))))


(defun pls-move-comment (beg end targ-column)
  "move all plsql comments in the region to a given column"
  (interactive "r\nnColumn: ")
  (pls-move-line beg end targ-column "--"))

(defun pls-move-declarations (beg end)
  "move all declarations in region so types line up"
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (let ((endmark (copy-marker end))
	  (col 0))
      (while (< (point) endmark)
	(beginning-of-line)
        (forward-word)(backward-word)
	(search-forward " ")
	(if (> (current-column) col)
	    (setq col (current-column)))
	(forward-line 1))
      (pls-move-line beg end col " "))))

(defun pls-move-function-call (start finish)
  "function_call (a,
     b);
   becomes
   function_call (a,
                  b);"
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let* ((finish-mark (copy-marker finish))
           (target-column (if (re-search-forward "(" finish-mark)
                              (current-column))))
      (forward-line 1)
      (while (< (point) finish-mark )
        (beginning-of-line)
        (delete-horizontal-space)
        (insert (make-string target-column ? ))
        (forward-line 1)))))

(defun pls-move-assign (beg end)
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
      (pls-move-line beg end col "="))))
;;
;; private func to move part of a line
(defun pls-move-line (beg end targ-column search-string)
  (save-excursion
    (goto-char beg)
    (let ((endmark (copy-marker end)))
      (while (< (point) endmark)
	(beginning-of-line)
        (when (equal search-string " ")
          (forward-word) (backward-word))
	(search-forward search-string)
	(backward-char (length search-string))
	(delete-horizontal-space)
	(setq addn (- targ-column (current-column)))
	(backward-char) 
	(if (not (looking-at ":\\|<\\|>\\|!")) ;; keep :=, <=, etc intact
	  (forward-char 1))
	(insert (make-string addn ? ))
	(forward-line 1)))))
  
(defun pls-package (name)
  (interactive "sPackage name: ")
  (insert (concat "CREATE or replace PACKAGE " name " is
   ----------------------------------------------------------------------------
   -- TYPES
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   -- CONSTANTS
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   -- FUNCTIONS AND PROCEDURES
   ----------------------------------------------------------------------------
   
END " name ";
/
show errors;

CREATE or replace PACKAGE BODY " name " is
   ----------------------------------------------------------------------------
   -- TYPES
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   -- CONSTANTS
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   -- VARIABLES
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   -- PRIVATE FUNCTIONS AND PROCEDURES
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   -- PUBLIC FUNCTIONS AND PROCEDURES
   ----------------------------------------------------------------------------   
   ----------------------------------------------------------------------------
   -- PACKAGE INITIALIZATION
   ----------------------------------------------------------------------------
   BEGIN
     null;
   END " name ";
/
show errors;
")))

(defun pls-function (name)
  (interactive "sfunction name: ")
  (insert (concat "   FUNCTION " name  "
   RETURN 
   IS
   BEGIN
   END " name ";"))
  (search-backward name nil nil 2)
  (forward-word))

(defun pls-procedure (name)
  (interactive "sprocedure name: ")
  (insert (concat "   PROCEDURE " name  "  
   IS
   BEGIN
   END " name ";"))
  (search-backward name nil nil 2)
  (forward-word))

(defun pls-buffer-functions ()
  (make-local-variable 'buffer-find-functions)
  (setq buffer-find-functions 'pls-find-function-name))

(defconst pls-rename-prefixes (list "PACKAGE " "PACKAGE BODY " "END "
                                    "CREATE or replace FUNCTION "
                                    "CREATE or replace PROCEDURE " ))

(defun pls-rename-package-temporary ()
  "Rename current package to a temporary name."
  (interactive)
  (let* ((current-package-name (file-name-sans-extension (buffer-name)))
         (temporary-name (concat current-package-name "_temp")))
    (mapcar '(lambda (s)
               (beginning-of-buffer)
               (perform-replace (concat s current-package-name)
                                (concat s temporary-name)
                                nil nil nil))
            pls-rename-prefixes)))
    
  

(defun pls-rename-package-original ()
  (interactive)
  (let* ((current-package-name (file-name-sans-extension (buffer-name)))
         (temporary-name (concat current-package-name "_temp")))
    (mapcar '(lambda (s)
               (beginning-of-buffer)
               (perform-replace (concat s temporary-name)
                                (concat s current-package-name)
                                nil nil nil))
            pls-rename-prefixes)))

