;;
;; This file contains functions that generate file and function prototypes,
;; and that generate comments.
;;

(defun jrc-proto-fullcommentline (str)
  "Insert a comment /*------ str ------*/"
  (let ((half-width 36)
	(half-str (/ (length str) 2)))
    (insert-string "/*")
    (insert-char ?- (- half-width half-str))
    (insert-string " ")
    (insert-string str)
    (insert-string " ")
    (insert-char ?- (- half-width half-str))
    (insert-string "*/")))

(defun jrc-proto-function-comment ()
  "When point is on first line of a function decl, generate /*-- FUNC --*/"
  (interactive)
  (beginning-of-line)
  (re-search-forward 
   "[ \t]\\**[ \t]*\\([a-zA-Z_0-9]+\\)[ \t]*(")
  (let ((func-name (message (match-string 1))))
    (beginning-of-line)
    (open-line 1)
    (jrc-proto-fullcommentline func-name)))
  
  
(defun jrc-proto-h-function ()
  "When point is on first line of .h function decl, generates boilerplate."
  (interactive)
  (let (param-start-point
	param-end-point
	param-name
	(longest-param 0)
	func-name
	(decl-string "()    - ")
	param-list)
    ;;
    ;; find func name
    ;;
    (beginning-of-line)
    (re-search-forward 
     "[ \t]\\**[ \t]*\\([a-zA-Z_0-9]+\\)[ \t]*(")
    (setq func-name (match-string 1))
    ;;
    ;; Insert the function name string
    ;;
    (beginning-of-line)
    (open-line 1)
    (jrc-proto-fullcommentline func-name)
    ;;
    ;; find bounds of parameters
    ;;
    (re-search-forward (concat func-name "[ \t]*("))
    (setq param-start-point (point))
    (re-search-forward ")[ \t]*;")
    (setq param-end-point (point))
    ;;
    ;; Find each parameter.  A parameter name is the string immediately 
    ;; preceeding a comma or _*/ , even if the parameter is within a 
    ;; callback function.  A parameter name is also within (*NAME) when
    ;; the parameter is a function.  Save the length of the longest parameter
    ;; as we go.
    ;;
    (goto-char param-start-point)
    (while (re-search-forward 
;	    "[ \t]*\\**[ \t]*\\([a-zA-Z_0-9]+\\)[ \t]*[,)]"
	    "[ \t]*\\**[ \t]*\\([a-zA-Z_0-9]+\\)[][ \t]*[,)]"
	    param-end-point
	    t)
      (setq param-name (match-string 1))
      (if (> (length param-name) longest-param) 
	  (setq longest-param (length param-name)))
      (setq param-list (append param-list 
			       (list (match-string 1) ))))
    (re-search-forward ";")
    (end-of-line)
    ;;
    ;; Now insert the boilerplate.
    ;;
    (insert "
/*
  NAME: 
    " func-name " - 
  PARAMETERS:
")
    (mapcar '(lambda (s)
	       (insert "    " s 
		       (make-string (+ 1 (- longest-param (length s))) ? )
		       decl-string "
"))
	       param-list)
    (insert 
"  REQUIRES:
    
  DESCRIPTION:
    
  RETURNS:
    
  NOTES:
    
*/")))
