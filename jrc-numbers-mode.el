;;; 
;;; jrc-numbers-mode.el --- a minor mode for on-screen/window line numbering.
;;; 
;; based on line-numbers-mode.el, by Jerry Chen, which doesn't work.
;; This is xemacs specific.
;; 
(require 'cl)
(require 'linum)

(defvar jrc-numbers-mode nil
  "*Non-nil means to display line numbers on each line in current window.")
(make-variable-buffer-local 'jrc-numbers-mode)

(defvar jrc-numbers-cache nil
  "Cache used to store the extents for the line numbers")
(make-variable-buffer-local 'jrc-numbers-cache)


(defvar jrc-numbers-face 'bold ;(make-face 'jrc-numbers-face)
  "Face used to display line numbers")
;(set-face-background jrc-numbers-face "gray90")


(defun jrc-numbers-mode (&optional arg) 
  "Toggle jrc-numbers mode.
With arg, turn this mode on iff arg is positive.
When jrc-numbers-mode is enabled, a line number will appear at the left
margin of each line."
  (interactive "P")
  (let ((oldmode (not (not jrc-numbers-mode)))
	(inhibit-quit t))
    (setq jrc-numbers-mode
          (or (and arg (> (prefix-numeric-value arg) 0))
              (and (null arg) (null jrc-numbers-mode))))
;          (if (null arg) 
;            (not jrc-numbers-mode)
;	  (> (prefix-numeric-value arg) 0)))
    (if (not (eq oldmode jrc-numbers-mode))
        (if jrc-numbers-mode
            (progn
              (set-specifier left-margin-width 6 (current-buffer))
              (jrc-numbers-show))
          (remove-specifier left-margin-width (current-buffer))
          (jrc-numbers-hide)))))

(defun turn-on-jrc-numbers-mode ()
  "Turn on jrc-numbers-mode.
Useful for adding a major mode hook variable."
  (interactive)
;  (linum-mode 1))
  (jrc-numbers-mode 1))

(defun jrc-numbers-hide ()
  "Hide line numbers in current-buffer shown by `jrc-numbers-show'."
  (mapcar '(lambda (x)
            (and (extent-live-p x)
                (delete-extent x))) 
       jrc-numbers-cache)
  (setq jrc-numbers-cache nil))

(defun jrc-numbers-make-extent (begin finish line-number)
  (let (new-extent new-glyph)
;    ( message "extent %5d" line-number)
    (setq new-extent (make-extent begin finish)
          new-glyph (make-glyph (format "%5d " line-number)))
    (set-glyph-face new-glyph jrc-numbers-face)
;    (set-extent-property new-extent 'start-open t)
;    (set-extent-property new-extent 'end-open t)
    (set-extent-property new-extent 'begin-glyph new-glyph)
    (set-extent-property new-extent 'begin-glyph-layout
                         'outside-margin)
;                     (set-extent-begin-glyph new-extent new-glyph 
;                                             'outside-margin)
    (set-extent-property new-extent 'lnm line-number)
    (push new-extent jrc-numbers-cache)))

;(defun jrc-numbers-make-annotation (here line-number)
;  (make-annotation (make-glyph (format "%4d " line-number)) here 'text)

(defun jrc-numbers-show ()
  "Show line numbers in current-buffer"
;  ( message "numbers-show")
  (jrc-numbers-hide)
  (save-excursion
    (mapcar '(lambda (w)
               (set-buffer (window-buffer w))
               (goto-char (window-point w))
               ;(message "%d" (point)) ;(sleep-for 2)
               (let* ((go-backward-lines -60)
                      (go-forward-lines (* -2 go-backward-lines))
                      (ok t)
                      current-line 
                      here )
                 (beginning-of-line)
                 (forward-line go-backward-lines)
                 (setq current-line (1+ (count-lines 1 (point))))
                 (while (and (> go-forward-lines 0) ok)
                   (unless (extent-at (setq here (point)) nil 'lnm nil 'at)
                     (jrc-numbers-make-extent here here current-line))
;                     (jrc-numbers-make-annotation here current-line))
                   (setq ok (re-search-forward "\n" nil t))
                   (setq current-line (1+ current-line))
                   (setq go-forward-lines (1- go-forward-lines)))))
            (get-buffer-window-list (current-buffer)))))

(defun jrc-numbers-post-command-cb ()
  "Callback attached to `post-command-hook' to make `jrc-numbers-mode' working."
  (if jrc-numbers-mode
      (condition-case nil
	  (jrc-numbers-show)
	(error nil))))

(defun jrc-numbers-post-comint-cb (ignore)
  "Callback attached to `comint-output-filter-functions' for `jrc-numbers-mode'"
  (jrc-numbers-post-command-cb))

(add-hook 'post-command-hook 'jrc-numbers-post-command-cb)
(add-hook 'comint-output-filter-functions 'jrc-numbers-post-comint-cb)

(provide 'jrc-numbers-mode)



