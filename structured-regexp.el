;; structured-regexps.el --- building complex regexps `hassle-free'.

;;; Copyright (C) 1996 by Oracle Corp.
;;; Author: Dmitry Nizhegorodov.

;;;  This program is free software; you can redistribute it and/or
;;;  modify it under the terms of the GNU General Public License as
;;;  published by the Free Software Foundation; either version 1, or
;;;  (at your option) any later version.
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;  General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program; if not, write to the Free Software
;;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; This package relieves (somewhat) the burden of writing and
;;; understanding `regexps' in emacs. Not only regexps are dense and
;;; hard to comprehend, they become worse when you need to squeeze
;;; every bit of performance out of your pattern-matching emacs code.
;;; Nested constructs built with lots of \(, |), \<, |>, \| seem to
;;; improve performance, but they are very difficult to write and
;;; understand.  But the nightmare only starts here; in emacs,
;;; backslashes replicate like rabbits. This small shell-regexp
;;; "\<\(a\|b\|c\)\>", already cumbersome enough, in emacs becomes
;;; quite incomprehensible "\\<\\(a\\|b\\|c\\)\\>". Now throw in some
;;; [],\t,?, etc. - and you will reach a point of complete
;;; disaster. Read any font-lock package to get an example.
;;
;;; With this package, all you need to generate the fast regexp above
;;; is to call (srx-word (srx-build-tree "a" "b" "c")).
;;
;;; For all srx-xxx APIs, arguments can in turn contain arbitrary
;;; regexps, except for `srx-build-tree', which allows only trailing
;;; regexps.  (e.g. (srx-build-tree "a?" "b[ \t;]+" "c[ ]*") will
;;; work).

;;; Dependencies.

;;; Now this package is included in this file. 
;;; 
;;(require 'proto-dictionary)

(defun srx-word (pattern)
  "convert regexp PATTERN into a word-delimiting regexp"
  (concat "\\<" pattern "\\>"))

(defun srx-or (&rest list)
  "Assemble a ONE-OF regexp from LIST of strings.
Example: (srx-or \"a\" \"b\" \"c\") ==> \"\\\\(a\\\\|b\\\\|c\\\\)\""
  (concat "\\("
	  (mapconcat 'identity 
		     list
		     "\\|")
	  "\\)"))
	   
;;; (srx-or "a" "x")

(defun srx-build-tree  (&rest list)
  "build a decision-tree-like structured `regexp' from LIST of regexps.
This function is a great little helper if you want to compress a long
list of regexp-variants into a decision-tree-like structure.

`srx-build-tree' performs the following steps:
  (1) builds a dictionary where the variants are ordered `lexicographically'
  (2) generates a complex regexp with nested \\( and \\) by walking 
     the dictionary.

Example: 
  (srx-build-tree \"cba\" \"aba\" \"cab\") 
==>
  \"\\\\(aba\\\\|c\\\\(ab\\\\|ba\\\\)\\\\)\".

The arguments itself can be regexps. Be sure, though, that 
structured regexps that are part of arguments do NOT appear on the
discrimination path. For instance,  (srx-build-tree \"cb[xy]\" \"cbx\")
is OK: ==> \"\\\\(c\\\\(b\\\\(a[xy]\\\\|x\\\\)\\\\)\\\\)\", but
  (srx-build-tree \"c[xy]b\" \"c[xy]d\") will not produce correct regexp.

The rule of thumb, therefore, is in using only TRAILING regexps in the
arguments.
"
  (let ((dictionary (srx-pd-make-dictionary)))
    (while list
      (srx-pd-lookup (car list) nil dictionary)
      (setq list (cdr list)))
    (srx-pd-print-as-regexp dictionary)))

(defvar srx-build-regex-default-level-max 6)

(defun srx-build-regex  (list &optional max-level)
  "build a decision-tree-like structured `regexp' from LIST of regexps.
Just like `srx-build-tree', but limits the number of levels to 
MAX-LEVEL. if MAX-LEVEL is not supplied, limit it to 
`srx-build-regex-default-level-max'."
  (let ((dictionary (srx-pd-make-dictionary)))
    (while list
      (srx-pd-lookup (car list) 
		     (or max-level
			 srx-build-regex-default-level-max)
		     dictionary)
      (setq list (cdr list)))
    (srx-pd-print-as-regexp dictionary)))

;;; (srx-build-regex '("varcharacter2") 3)
;;; (srx-build-regex '("varcharacter" "varcharacter2" ) 3)
;;; (srx-build-regex '("varcharacter" "varcharacter2" "var"  ) 7)
;;; (srx-build-regex '("varcha" "varchb"  "var" ))
;;; (srx-build-regex '("varcharacter" "v" "varc" ) 7)
;;; (srx-build-tree "int" "in")
;;;  (srx-build-tree "varchar[2]?")
;;; (looking-at (srx-build-tree "varchar[2]"))
;;; (insert (srx-build-tree "cba" "aba" "cab"))
;;; (insert (srx-build-tree "cba[xy]" "cbx"))
;;; (insert (srx-build-tree "c[xy]b" "c[xy]d"))

;;------------------------------------------------------
;;; 
;;; Structured Regexps use functionality of Proto Dictionary.
;;; For simplicity, the required functionality is pasted here.
;;
;;; Proto Dictionary : a dictionary where
;;;   entities are organized in "natural" or "naive"
;;;   format. See example below.
;;
;;------------------------------------------------------
;;
;;;    Input: "adba" "abba" "adda"  "baba"
;;; ==>
;;;    ((a (b . "ba") (d (b . "a") (d . "a"))) (b . "aba"))
;;
;;------------------------------------------------------


(defun srx-pd-make-dictionary (&optional name)
  (list (or name "structured dictionary")))


(defvar srx-pd-path-teminator-char 222
  "a non-ascii char used to indicate end of path")

(defvar srx-pd-path-teminator-string 
  (char-to-string srx-pd-path-teminator-char)
  "a non-ascii 1-char string used to indicate end of path")

(defun srx-pd-insert 
  (back-bone word-to-insert word-to-insert-pos &optional max-level)
  "Insert a new word into a dictionary starting from BACK-BONE.
The first element is a dictionary header, skipped over.
A word to insert is a (substring word-to-insert word-to-insert-pos).

Words are placed in lexicographical order; collisions, when detected,
resolved on the level of FIRST character; a corresponding entry spawns
out a new SUB-DICTIONARY. This process is recursive. Words that
may collide on the last character terminate with srx-pd-path-teminator-char.

When MAX-LEVEL supplied, however, it will limit the number of spanned
levels.

Example:

 insert the following in arbitrary order \"adba\" \"abba\" \"adda\"  \"baba\"
  ==>
    ((a (b . \"ba\") (d (b . \"a\") (d . \"a\"))) (b . \"aba\"))

"
  ;;(insert (format "%s" max-level))
  (let* ((char (if (< word-to-insert-pos (length word-to-insert))
		  (elt word-to-insert word-to-insert-pos)
		srx-pd-path-teminator-char))
	 (entry (car (cdr back-bone)))
	 (key (car entry)))

    (while (and key (> char key))
      (setq back-bone (cdr back-bone)
	    entry     (car (cdr back-bone))
	    key       (car entry)))

    (if (eq char key)
	;; match found
	(cond
	 ((consp (cdr entry))
	  (srx-pd-insert
	   entry word-to-insert (1+ word-to-insert-pos) max-level))
	 ((eq char srx-pd-path-teminator-char) 
	  ;; found duplicate words, do nothing 
	  nil) 
	 ((string-equal (cdr entry) 
			(substring word-to-insert 
				   (1+ word-to-insert-pos)))
	  ;; found duplicate entry, do nothing 
	  nil)
	 (t;; collision on CHAR, may need to create a sublevel
	  (if (or (not max-level) (< word-to-insert-pos max-level))
	      (srx-pd-spawn-level entry word-to-insert 
				  (1+ word-to-insert-pos) max-level)
	    (srx-insert-new-entry back-bone word-to-insert 
				  word-to-insert-pos char))))
      (srx-insert-new-entry 
       back-bone word-to-insert word-to-insert-pos char))))


(defun srx-insert-new-entry 
  (back-bone word-to-insert word-to-insert-pos char)
  (setcdr back-bone
	  (cons (if (eq char srx-pd-path-teminator-char)
		    (cons char "")
		  (cons char
		      (substring word-to-insert
				 (1+ word-to-insert-pos))))
		(cdr back-bone))))

(defun srx-pd-spawn-level (entry word-to-insert word-to-insert-pos max-level)
  (let* ((prev-word (cdr entry)))
    (if (eq (length prev-word) 0) 
	(setq prev-word srx-pd-path-teminator-string))
    (if (eq (length word-to-insert) word-to-insert-pos) 
	(setq word-to-insert (concat word-to-insert 
				     srx-pd-path-teminator-string)))
    (setcdr entry (list (cons (elt prev-word 0) (substring prev-word 1))))
    (srx-pd-insert entry word-to-insert word-to-insert-pos max-level)))

(defun srx-pd-lookup (word &optional max-level dictionary)
"Insert a new word into a DICTIONARY. If DICTIONARY. is not
supplied, create a new dictionary.

Words are placed in lexicographical order; collisions, when detected,
resolved on the level of FIRST character; a corresponding entry spawns
out a new SUB-DICTIONARY. This process is recursive. Words that
may collide on the last character terminate with srx-pd-path-teminator-char.

When MAX-LEVEL supplied, however, it will limit the number of spanned
levels.

Example:

 insert the following in arbitrary order \"adba\" \"abba\" \"adda\"  \"baba\"
  ==>
    ((a (b . \"ba\") (d (b . \"a\") (d . \"a\"))) (b . \"aba\"))
"
  (if (not dictionary) (setq dictionary (srx-pd-make-dictionary)))
  (srx-pd-insert dictionary word 0 max-level)
  dictionary)

;;; tests...
;;; (setq dict (list 'keywords))
;;(srx-pd-lookup "baba" nil dict)
;;(srx-pd-lookup "varchar" 3 dict)
;;(srx-pd-lookup "varchar2" 3 dict)
;;(srx-pd-lookup "cada" nil dict)
;;(srx-pd-lookup "cadaxy" nil dict)
;;(srx-pd-lookup "bada" nil dict)
;;; (insert "\n" (srx-pd-print dict))
;;; (insert "\n" (setq regexp (srx-pd-print-as-regexp dict)))

(defun srx-pd-print-as-regexp (dictionary)
  "Having a proto-dictionary DICTIONARY, generate a
structured regexp that recognized any of the elements of the
dictionary.

Example:

  (srx-pd-print-as-regexp 
   '(my-dict ((a (b . \"ba\") (d (b . \"a\") (d . \"a\"))) (b . \"aba\"))))
==>
  \"\\\\(a\\\\(bba\\\\|d\\\\(ba\\\\|da\\\\)\\\\)\\\\|baba\\\\)\"

"
  (let ((res ""))
    (setq dictionary (cdr dictionary))
    (while dictionary
      (let* ((entry (car dictionary))
	     (char (car entry))
	     (tail (cdr entry)))
	(setq res 
	      (concat
	       res
	       (if (string-equal res "") "" "\\|")
	       (if (eq char srx-pd-path-teminator-char)
		   "" (make-string 1 char))
	       (if (stringp tail) tail
		 (srx-pd-print-as-regexp entry)))))
      (setq dictionary (cdr dictionary)))
    (concat "\\(" res "\\)")))

;;; (insert "\n" (srx-pd-print-as-regexp dict))
;;; (setq regexp (srx-pd-print-as-regexp dict))

(defun srx-pd-print (dictionary &optional indent)
  "Print out the contents of Proto-dictionary DICTIONARY"
  (let ((res ""))
    (setq dictionary (cdr dictionary))
    (if (not indent) (setq indent 0))
    (while dictionary
      (let* ((entry (car dictionary))
	     (char (car entry))
	     (tail (cdr entry)))
	(setq res 
	      (concat
	       res
	       "\n"
	       (make-string indent 32)
	       (make-string 1 char)
	       (if (stringp tail) tail
		 (srx-pd-print entry (1+ indent))))))
      (setq dictionary (cdr dictionary)))
    res))



(defvar  srx-special-chars '("+" "^" "[" "." "*" )
  "characters that must be prepended with \\\\ in reg-expressions")

(defun srx-fixup-special-char (regexp special)
  "scan REGEXP and turn off special meaning of special characters"
  (let ((result "")
	(pattern (concat "\\" special))
	pos)
    (while (setq pos (string-match pattern regexp))
      (setq result
	    (concat result 
		    (substring regexp 0 pos)
		    (if (and 
			 (> pos 0)
			 (eq (aref regexp (- pos 1)) ?\\ ))
			""
			"\\")
		    special))
      (setq regexp (substring regexp (1+ pos))))
    (concat result regexp)))

(defun srx-fixup-specials (regexp specials)
  (while specials
    (setq regexp (srx-fixup-special-char regexp (car specials))
	  specials (cdr specials)))
  regexp)
	    
;;; (load-file "structured-regexp.elc")
;;; (srx-fixup-special-char "+s\\+s+dd+" "+")      
;;; (srx-fixup-special-char (apply 'srx-or tkp-2pos-tokens) "+")      
;;; (srx-fixup-specials (apply 'srx-or tkp-2pos-tokens) srx-special-chars)




(provide 'structured-regexp)




