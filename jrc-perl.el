(defun jrc-perl-bye ()
  "Insert byebye(<uid>, \"\") at point"
  (interactive)
  (let ((error-number ()))
    (while (null error-number)
      (setq error-number (concat "&byebye(" 
                                 (number-to-string (+ 1 (random 200))) ","))
      (save-excursion
        (goto-char (point-min))
        (if (search-forward error-number () t 1)
            (setq error-number ()))))
    (insert error-number "\"\") ")
    (backward-char-command 3)))

(defun jrc-perl-single-sharp ()
  "Insert # at point"
  (interactive)
  (end-of-line)
  (insert " # "))

(defun jrc-perl-double-sharp ()
  "insert ## after indenting"
  (interactive)
  (cperl-indent-command)
  (insert "## "))

(defun jrc-perl-triple-sharp ()
  "insert ### after indenting"
  (interactive)
  (cperl-indent-command)
  (insert "### "))

(defun jrc-perl-move-comment (beg end targ-column)
  "move all perl comments in the region to a given column"
  (interactive "r\nnColumn: ")
  (jrc-perl-move-line beg end targ-column "#"))

;;
;; private func to move part of a line
(defun jrc-perl-move-line (beg end targ-column search-string)
  (save-excursion
    (goto-char beg)
    (let ((endmark (copy-marker end)))
      (while (< (point) endmark)
	(beginning-of-line)
	(search-forward search-string)
	(backward-char (length search-string))
	(delete-horizontal-space)
	(setq addn (- targ-column (current-column)))
	(backward-char) 
	(if (not (looking-at ":\\|<\\|>\\|!")) ;; keep :=, <=, etc intact
	  (forward-char 1))
	(insert (make-string addn ? ))
	(forward-line 1)))))

(defun jrc-perl-trace-prototype (name)
  "dump subroutine with trace calls"
  (interactive "sSubroutine name: ")
  (insert (concat "sub " name "
{
  trace::enter (\"" (file-name-sans-extension (buffer-name)) "::" name "\");

  trace::leave ();
}"))
  (search-backward name nil nil 2)
  (end-of-line))

(defun jrc-perl-module-template ()
  "insert module template"
  (interactive)
  (let ((module-name (file-name-sans-extension (buffer-name))))
    (insert (concat "package " module-name ";
#
#
#
#

###############################################################################
# Subroutine Declarations
###############################################################################


###############################################################################
# Module Variables
###############################################################################


###############################################################################
# Subroutine Definitions
###############################################################################

1;
"))))
