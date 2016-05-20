
;; the alist
(defvar jrc-pad-alist '())

;; filename variables
(defvar jrc-pad-filename (concat ".cpd."  (getenv "HOST")))
(defvar jrc-pad-writable (file-writable-p jrc-pad-filename))

;; frame variables
(defvar jrc-pad-frame nil)

;; face definitions
(defvar *jrc-pad-directory-face 'bold)
(defvar *jrc-pad-file-face 'blue)
(defvar *jrc-pad-identifier-face 'black)

;; jrc-pad-alist  is a list of element.
;; "element" is a Cons cell, but "cell" is hard to dictate because of sell.
;; element has element.name, element.face.  
;; all element.name are unique.
;; all element.name can be categorized into one of
;;  {directory, identifier, file}, and element.face is set based on that
;;  category.

;; element constructors
(defun jrc-pad-make-directory-element (name)
  (cons name *jrc-pad-directory-face))

(defun jrc-pad-make-identifier-element (name)
  (cons name *jrc-pad-identifier-face))

(defun jrc-pad-make-file-element (name)
  (cons name *jrc-pad-file-face))

(defun jrc-pad-make-proper-element (name)
  (cond ((jrc-pad-is-directory name)(jrc-pad-make-directory-element name))
        ((jrc-pad-is-file name)(jrc-pad-make-file-element name))
        (t (jrc-pad-make-identifier-element name))))

(defun jrc-pad-is-directory (name)
  (string-match "/" name))

(defun jrc-pad-is-file (name)
  (string-match "[.]" name))

(defun jrc-pad-is-identifier (name)
  (not (or (jrc-pad-is-directory name) (jrc-pad-is-file name))))

;; fetch element at index
(defun jrc-pad-fetch-element (index)
  (nth (- index 1) jrc-pad-alist))

;; fetch element at index, and move it to front
(defun jrc-pad-fetch-element-and-move (index)
  (let ((element (jrc-pad-delete-element index)))
    (jrc-pad-add-element element)
    element))

;; fetch name associated with element at index
(defun jrc-pad-fetch-name (index)
  (jrc-pad-element-get-name (jrc-pad-fetch-element index)))

;; delete and return element at index
(defun jrc-pad-delete-element (index)
  (let ((element (jrc-pad-fetch-element index)))
    (setq jrc-pad-alist  (delete element jrc-pad-alist))
    element))

(defun jrc-pad-element-get-name (element)
  (car element))

(defun jrc-pad-element-get-face (element)
  (cdr element))

(defun jrc-pad-add-element (element)
  (setq jrc-pad-alist (cons element
                            (remove (assoc (jrc-pad-element-get-name element)
                                           jrc-pad-alist) jrc-pad-alist))))

(defun jrc-pad-internal-sort (face)
  (setq jrc-pad-alist  
        (sort jrc-pad-alist
              '(lambda (first second)
                 (let ((first-face (jrc-pad-element-get-face first))
                       (second-face (jrc-pad-element-get-face second))
                       (first-name (jrc-pad-element-get-name first))
                       (second-name (jrc-pad-element-get-name second)))
                   (cond ((equal first-face face)
                          (if (equal second-face face)
                              (string< first-name second-name)
                            t))
                         ((equal second-face face) nil)
                         (t t))))))
  (jrc-pad-display-and-save))

(defun jrc-pad-display-and-save ()
  (jrc-pad-display-list)
  (when jrc-pad-writable
    (let ((temporary-frame (selected-frame))
          (temporary-window (selected-window)))
      (select-frame jrc-pad-frame)
      ; sometimes save-buffer throws weird dired errors
      (condition-case nil
          (save-buffer)
        (error nil))
      (select-frame temporary-frame)
      (select-window temporary-window))))


(defun jrc-pad-load-file ()
  (let ((element-list nil))
    (find-file jrc-pad-filename)
    (setq jrc-pad-alist  nil) 
    (setq buffer-read-only nil)
    (beginning-of-buffer)
    (while (re-search-forward "[ ]*[()0-9]+ \\(.*\\)"  nil t)
      (jrc-pad-add-element (jrc-pad-make-proper-element (match-string 1))))
    (setq jrc-pad-alist (reverse jrc-pad-alist))))

(defun jrc-pad-display-list ()
  (let ((original-window (selected-window))
        (original-frame (selected-frame))
        (the-list jrc-pad-alist)
        (counter 1))
    (select-frame jrc-pad-frame)
    (erase-buffer)
    (while the-list
      (insert (format "%2d " counter ))
      (let ((element (car  the-list))
            (here (point)))
        (insert (format "%s\n" (jrc-pad-element-get-name element)))
        (condition-case nil
            (set-extent-face (make-extent (point) here)
                             (jrc-pad-element-get-face element))
          (error nil)))
      (setq the-list (cdr the-list))
      (setq counter (+ counter 1)))
    (beginning-of-buffer)
    (select-frame original-frame)
    (select-window original-window)))

(defun jrc-pad-fetch-and-apply (index function &optional argument-list)
  (if (<= index (length jrc-pad-alist))
      (let ((word (jrc-pad-element-get-name 
                   (jrc-pad-fetch-element-and-move index))))
        (apply function (append (list word) argument-list))
        (jrc-pad-display-and-save))
    (error "index out of range")))

(defun jrc-pad-relative-line-to-absolute (relative-line radix)
  (let* ((base 100)
         (window-start-raw (count-lines 1 (window-start)))
         (window-start-tens (% window-start-raw base))
         (window-start-hundreds (- window-start-raw window-start-tens)))
    (if (< relative-line window-start-tens)
        (+ window-start-hundreds relative-line base)
      (+ window-start-hundreds relative-line))))    
  
;;;###autoload 
(defun jrc-pad-create-window ()
  "create pad window and frame, using cache frame if it exists"
  (interactive)
  (set-frame-position (selected-frame) 920 20)
  (set-frame-size (selected-frame) 86 62 )
  (let ((original-frame (selected-frame)))
    (if (and (boundp 'cache-frame) cache-frame)
        (setq jrc-pad-frame cache-frame)
      (setq jrc-pad-frame (make-frame))
      (set-frame-size jrc-pad-frame  63 63)
      (set-frame-position jrc-pad-frame  1885 20))
    (select-frame jrc-pad-frame)
    (jrc-pad-load-file)
    (jrc-pad-display-list)))

;;; functions that sort
;;;###autoload
(defun jrc-pad-directories-to-top ()
  "sort pad , moving directories to beginning"
  (interactive)
  (jrc-pad-internal-sort *jrc-pad-directory-face))

;;;###autoload
(defun jrc-pad-files-to-top ()
  "sort pad , moving files to beginning"
  (interactive)
  (jrc-pad-internal-sort *jrc-pad-file-face))

;;;###autoload
(defun jrc-pad-identifiers-to-top ()
  "sort pad , moving identifiers to beginning"
  (interactive)
  (jrc-pad-internal-sort *jrc-pad-identifier-face))

;;; functions that fetch an identifier and do something with it
;;;###autoload
(defun jrc-pad-find-file (index)
  "load file specified at index"
  (interactive "p")
  (jrc-pad-fetch-and-apply index 'find-file))

;;;###autoload
(defun jrc-pad-buffer (index)
  "Switch to buffer  specified at index"
  (interactive "p")
  (jrc-pad-fetch-and-apply index 'switch-to-buffer))

;;;###autoload
(defun jrc-pad-insert-identifier  (index)
  "Insert identifier at index"
  (interactive "p")
  (jrc-pad-fetch-and-apply index 'insert))

;;;###autoload
(defun jrc-pad-dired (index)
  "run dired on directory at index"
  (interactive "p")
  (jrc-pad-fetch-and-apply index 'dired (list "-alt")))

;;; functions that add elements -- use the word "enqueue", because we have
;;; already used "add" and "insert" in function names elsewhere
;;;###autoload
(defun jrc-pad-enqueue-directory ()
  "insert the current directory into pad"
  (interactive)
  (jrc-pad-add-element (jrc-pad-make-directory-element default-directory))
  (jrc-pad-display-and-save))

;;;###autoload
(defun jrc-pad-enqueue-buffer (buffer)
  "insert the current buffer name into pad"
  (interactive "b")
  (jrc-pad-add-element (jrc-pad-make-file-element buffer))
  (jrc-pad-display-and-save))


;;;###autoload
(defun jrc-pad-enqueue-region ()
  "Insert the current region into pad"
  (interactive)
  (jrc-pad-add-element (jrc-pad-make-identifier-element
                        (buffer-substring (point) (mark))))
  (jrc-pad-display-and-save))

;;;###autoload
(defun jrc-pad-enqueue-word ()
  "Inserts the word at cursor in cache-pad if 'word' is not there."
  (interactive)
  (let ((search-string "[0-9a-zA-Z/][a-z0-9A-Z_$-/]*")
        (bsearch-string "[^a-zA-Z0-9_$-/]"))
    (save-excursion
      (when (re-search-backward bsearch-string () t)
        (forward-char 1)
        (let ((start-pos (point)))
          (re-search-forward search-string)
          (jrc-pad-add-element (jrc-pad-make-identifier-element 
                                (buffer-substring start-pos (point))))
          (jrc-pad-display-and-save))))))

;;;###autoload
 (defun jrc-pad-remove-entry (index)
   "deletes the entry at index"
   (interactive "p")
   (jrc-pad-delete-element index)
   (jrc-pad-display-and-save))

;;;###autoload
(defun jrc-pad-jump-to-relative-line (num)
  "jump to relative line number"
  (interactive "p")
  (goto-line (jrc-pad-relative-line-to-absolute num 100)))
