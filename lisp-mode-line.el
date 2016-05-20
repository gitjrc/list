;;;
(defun emacs-lisp-buffer-functions ()
  (make-local-variable 'buffer-find-functions)
  (setq buffer-find-functions 'emacs-lisp-find-function-name))

(defun emacs-lisp-show-function-name ()
  "Add to emacs-lisp-mode-hook to maintain function name in mode line"
  (make-local-variable 'post-command-hook)
  (make-local-variable 'emacs-lisp-current-function)
  (add-hook 'post-command-hook 'emacs-lisp-show-function-update)
  (setq mode-line-buffer-identification
        (append mode-line-buffer-identification
                '("(" emacs-lisp-current-function ") "))))

(defun emacs-lisp-show-function-update ()
  (setq emacs-lisp-current-function (emacs-lisp-find-function-name)))

(defun emacs-lisp-find-function-name (&optional move-point)
  "Return the name of the function containing the point."
  (interactive)
  (let (location result)
    (save-excursion
      (unless move-point 
        (end-of-line))
      (if (re-search-backward 
           "^(def[^ ]+[ \t]+\\([:-a-zA-Z_0-9]+\\)"
           nil t)
          (setq result (match-string 1)))
      (setq location (point)))
    (when move-point 
      (goto-char location))
    (if (interactive-p)
        (message result)
      result)))
