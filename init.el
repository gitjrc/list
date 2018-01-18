(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 183 :width normal))))
 '(font-lock-comment-face ((t (:foreground "blue"))))
 '(font-lock-keyword-face ((t (:foreground "black" :weight bold))))
 '(linum ((t (:inherit (bold default))))))

(tool-bar-mode -1)
(require 'linum)
(global-linum-mode 1)
;(setq linum-format "%5d \u2502")
(setq linum-format "%5d ")

(defun jrc-normal-window ()
  "make selected frame normal size"
  (interactive)
  (set-frame-position (selected-frame) 770 45)
  (set-frame-size (selected-frame) 86 60 ))
(defun jrc-wide-window ()
  "make selected frame full-screen width"
  (interactive)
  (set-frame-position (selected-frame) 10 100)
  (set-frame-size (selected-frame) 190 60 ))  

(setq load-path 
      (append (list 
               (expand-file-name "/home/john/emacs-lisp"))
               load-path))

(load-library  "cache")
(load-library  "jrc-pad")
;
; No tabs
;
(setq-default indent-tabs-mode nil)
;
; Python
;
(load-library "pychecker") ;; 
;;(autoload 'pylint "pychecker" "Run pylint" t)
(autoload 'jrc-python-show-function-name "jrc-python")
(autoload 'python-fix-line  "python-fix" "fix Python line of code" t)
(autoload 'jrc-annotate-tags "jrc-annotate-tags" "Annotate with tags" t)
(add-hook 'python-mode-hook 'jrc-python-show-function-name)
;
; sql indenting
;
(autoload 'jrc-sql-indent-region "jrc-sql-indent" "indent sql region" t)
(autoload 'jrc-sql-indent-line "jrc-sql-indent" "indent sql line" t)
(autoload 'jrc-sql-indent-back "jrc-sql-indent" "indent sql" t)
;
; PL SQL
;
(autoload 'pls-show-function-name "pls-utils" "plsql utilities" t)
(autoload 'pls-buffer-functions "pls-utils" "plsql utilities" t)
(autoload 'pls-move-assign "pls-utils" "plsql utilities" t)
(autoload 'pls-move-comment "pls-utils" "plsql utilities" t)
(autoload 'pls-rename-package-original "pls-utils" "plsql utilities" t)
(autoload 'pls-rename-package-temporary "pls-utils" "plsql utilities" t)
(autoload 'pls-move-function-call "pls-utils" "plsql utilities" t)
(autoload 'pls-move-declarations "pls-utils" "plsql utilities" t)
(autoload 'plsql-compile' "plsql-compile" "Compile PL SQL" t)
(add-hook 'pls-mode-hook 'pls-show-function-name)
(add-hook 'pls-mode-hook 'pls-buffer-functions)
(put 'narrow-to-region 'disabled nil)
(setq pls-error-numbers-only t)  ;; 9/12/06
(autoload 'pls-mode  "ora-pls-mode" "PL/SQL Editing Mode" t)

(setq auto-mode-alist
   (append '(("\\.pls$"  . pls-mode)
             ("\\.sql$"  . pls-mode)
             ("\\.jpb$"  . pls-mode)
             ("\\.jps$"  . pls-mode)
             ) auto-mode-alist))
;; 
;;  uppercase
;; 
(defun upcase-region-or-word (arg)
  "Upcase the selected region or the following word (or ARG words)."
  (interactive "p")
  (if (region-active-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-word arg)))
;; 
;; JavaScript
;; 
(autoload 'javascript-buffer-functions "jrc-javascript" 
  "JavaScript startup hook")
(add-hook 'js-mode-hook 'javascript-buffer-functions)
;;
;; rebind up and down in shell mode and gdb
;;
(add-hook 'comint-mode-hook      
	  '(lambda ()
	     (define-key comint-mode-map 
	       [(up)] 'comint-previous-matching-input-from-input)
	     (define-key comint-mode-map 
	       [(down)] `comint-next-matching-input-from-input)))


(add-hook 'gdb-mode-hook      
	  '(lambda ()
	     (define-key gdb-mode-map 
	       [(up)] 'comint-previous-matching-input-from-input)
	     (define-key gdb-mode-map 
	       [(down)] `comint-next-matching-input-from-input)))
(setq comint-input-ring-size 1024)
(setq mouse-yank-at-point t) ; Paste where cursor is, not where mouse is

(defun pls ()
  (interactive)
  (pls-mode)
  (font-lock-mode))
;; 
;; Make ediff  split the way we want
;; 
(setq ediff-split-window-function 'split-window-horizontally)      
;; 
;; git
;; 
(autoload 'jrc-git-diff "jrc-git" "git diff current buffer" t)
;;
;; vocola
;;
(autoload 'jrc-vocola-equals "jrc-vocola" "insert = at column 25" t)
(autoload 'jrc-vocola-assign "jrc-vocola" "insert := at column 25" t)
(autoload 'jrc-vocola-generate "jrc-vocola" "run vocola generate program" t)
(autoload 'jrc-vocola-indent "jrc-vocola" "indent" t)
(autoload 'jrc-vocola-timing-and-fill "jrc-vocola" "show timing numbers" t)
;;
;; enscript
;;
(autoload 'enscript-buffer "enscript" "enscript" t)
(autoload 'enscript-buffer-widepage "enscript" "enscript" t)
(autoload 'enscript-region "enscript" "enscript" t)
(autoload 'enscript-region-widepage "enscript" "enscript" t)
;; 
;; JavaScript
;; 
(autoload 'javascript-buffer-functions "jrc-javascript" 
  "JavaScript startup hook")
(add-hook 'javascript-mode-hook 'javascript-buffer-functions)
;;
;; xcs
;;
(autoload 'xcs-find-bug "ez-bug-mode" "find bug description" t)
;; 
;; my speed stuff
;; 
(autoload 'jrc-speed-initialize "jrc-speed" "initialize speed" t)
(autoload 'jrc-speed-jump "jrc-speed" "speed jump to function" t)
;;
;; Lisp
;;
(autoload 'emacs-lisp-show-function-name "lisp-mode-line" "show fname")
(autoload 'emacs-lisp-buffer-functions "lisp-mode-line" "function hook")
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-show-function-name)
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-buffer-functions)
; Borrowed from xemacs
(defun downcase-region-or-word (arg)
  "Downcase the selected region or the following word (or ARG words)."
  (interactive "p")
  (if (region-active-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-word arg)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tool-bar-mode nil))
;;
;; Cscope
;;
;  Will probably need to make this more sophisticated someday.



;;;----------------------------------------------------------------------
;;;
;;; Vocola (.vcl) Mode support
;;;
;;;----------------------------------------------------------------------
(autoload 'vocola-mode "vocola-mode" "vocola mode" t)
(add-to-list 'auto-mode-alist '("\\.vcl\\'" . vocola-mode))
;;
;; grep
;;
(autoload 'jrc-grep-symbol "jrc-grep" "Grep for the symbol at point" t)
(autoload 'jrc-grep-region "jrc-grep" "Grep for the region" t)
;; Follow symbolic links when using rgrep
(setq grep-find-template "find . <X> -type l <F> -exec grep <C> -nH -e <R> {} +")
(set-frame-size (selected-frame) 86 62)
;;
;; Tramp
;;
(setq password-cache-expiry  12000)
;;
;; Nuke 10,000 lines
;; 
(autoload 'jrc-nuke-10-thousand "jrc-nuke-10-thousand" "Kill 10,000 lines" t)
