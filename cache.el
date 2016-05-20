; cache pad, written by Jonathan Epstein (epstein@ncbi.nlm.nih.gov), partially
; based upon demacs, written by Thomas Rene Nielsen (trn@imada.ou.dk)

(setq cache-pad-list nil)
(setq cache-pad-filename nil)
;(setq cache-pad-default-filename "c:/default.cpd")
(setq cache-pad-default-filename (concat  ".cpd." (getenv "HOST")))
(defvar cache-pad-writable (file-writable-p cache-pad-default-filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window coordinates and size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq cache-num-rows 3)
;;(setq cache-num-columns 3)
;;(setq cache-column-width 27)
(setq cache-num-rows 150)
(setq cache-height-fudge-factor 1)
(setq cache-num-columns 1)
(setq cache-column-width 25)
(setq cache-window nil)
(setq cache-frame nil)

(defvar cache-place-point-before-rel-search t)

(defun region-to-x-selection ()
  "captures the current region into the X selection buffer"
  (interactive)
  (x-own-selection  (buffer-substring (point) (mark))))

(defun create-cache-window ()
  "Creates the cache window"
  (interactive)
  (jrc-normal-window)
;  (delete-other-windows)
;  (split-window-vertically)
;  (other-window 1)
  (let ((original-frame (selected-frame)))
;;    (set-frame-position (selected-frame) 480 100)
    (setq cache-frame (make-frame))
    (set-frame-size cache-frame 25 70);cache-column-width cache-num-rows)
    (set-frame-position cache-frame 1300 15)
;    (set-frame-position cache-frame 860 30)
    (select-frame cache-frame)
    (cache-pad-parse-input cache-pad-default-filename)
    (print-cache-pad)
    (linum-mode 0) ;; turn this off
;    (find-file cache-pad-default-filename)
    (select-frame original-frame)))
;;
;; copy word to pad
;; copy region to pad
;; copy buffer name to pad
;; yank pad to point
;; switched to buffer specified by pad entry
;; 

(defun print-cache-pad ()
  "prints the cache pad"
  (interactive)
  (let* ((num 1)
	 (str nil)
	 (row 0)
	 (col 0)
	 (temp-window (selected-window))
         (temp-other-window nil)
	 (temp-frame (selected-frame))
	 (the-list cache-pad-list))
	(select-frame cache-frame)
;       (setq temp-other-window (selected-window))
;	(select-window cache-window)
	(erase-buffer)
	(while (and the-list (< row cache-num-rows))
	  (if (> row 0) (open-line 1))
	  (if (> row 0) (forward-line 1))
	  (setq row (+ row 1))
	  (setq col 0)
	  (while (and the-list (< col cache-num-columns))
             (setq str (number-to-string num))
             (insert "(")
             (insert str)
             (insert ") ")
             (insert (car-safe the-list))
             (setq col (+ col 1))
	     (setq num (+ num 1))
;; insert padding as necessary
	     (if (< col cache-num-columns)
	       (let ((n 0))
	         (setq n (- cache-column-width
	                  (+ 3 (length str) (length (car-safe the-list)))))
	         (while (> n 0) (insert " ") (setq n (- n 1)))))
	     (setq the-list (cdr-safe the-list))))
;	(select-window temp-other-window)
        (beginning-of-buffer)
	(select-frame temp-frame)
	(select-window temp-window)))


;; 
(defun insert-directory-into-cache-pad ()
  "Insert the current directory into cache-pad"
  (interactive)
  (insert-word-into-cache-pad default-directory))

(defun insert-buffer-into-cache-pad (buf)
  "Insert the current buffer name into cache-pad"
  (interactive "b")
  (insert-word-into-cache-pad buf))

(defun jump-to-relative-line (num)
  "jump to relative line number"
  (interactive "p")
  (goto-line (relative-line-to-absolute num 100)))

(defun cache-pad-file (num)
  "load file specified by num"
  (interactive "p")
  (let ((word (nth (- num 1) cache-pad-list)))
       (if word (progn (find-file word)
                       (insert-word-into-cache-pad word)))))

(defun cache-pad-dired (num)
  "run dired on buffer specified by num"
  (interactive "p")
  (let ((word (nth (- num 1) cache-pad-list)))
       (if word (progn (dired word "-alt")
                       (insert-word-into-cache-pad word)))))

(defun cache-pad-nth-buffer (num)
  "switch to buffer specified by num"
  (interactive "p")
  (let ((word (nth (- num 1) cache-pad-list)))
       (if word (progn (switch-to-buffer word)
                       (insert-word-into-cache-pad word)))))


(defun insert-nth-cache (num)
  "fetch the nth element of the cache"
  (interactive "p")
  (let ((word (nth (- num 1) cache-pad-list)))
       (if word (progn
                  (insert word)
                  (insert-word-into-cache-pad word)))))

(defun add-word (word)
  (setq cache-pad-list (cons word (delete word cache-pad-list)))
  ;; prevent the list from growing too long
  (setq word (nth (+ 1 (* cache-num-rows cache-num-columns))
                  cache-pad-list))
  (if word (setq cache-pad-list (delete word cache-pad-list))))
  

(defun insert-word-into-cache-pad (word)
  (add-word word)
  (print-cache-pad)
  (cache-pad-print-parsable cache-pad-default-filename))


(defun insert-selection-into-cache-pad ()
 "Inserts the current selection into cache-pad if 'word' is not there."
 (interactive)
  (insert-word-into-cache-pad (x-get-selection)))
  
(defun insert-region-into-cache-pad ()
  "Inserts the current region into cache-pad if 'word' is not there."
  (interactive)
  (insert-word-into-cache-pad (buffer-substring (point) (mark))))

(defun insert-cache-pad ()
  "Inserts the word at cursor in cache-pad if 'word' is not there."
  (interactive)
  (let ((search-string "[a-zA-Z/][a-z0-9A-Z_$-/]*")
        (bsearch-string "[^a-zA-Z0-9_$-/]"))
    (save-excursion
      (if (re-search-backward bsearch-string () t)
          (progn 
            (forward-char 1)
            (let ((start-pos (point)))
              (re-search-forward search-string)
              (let ((end-pos (point)))
                (let ((word (buffer-substring start-pos end-pos)))
                  (insert-word-into-cache-pad word)))))))))

(defun relative-line-to-absolute (relative-line radix)
  (let* ((base 100)
         (window-start-raw (count-lines 1 (window-start)))
         (window-start-tens (% window-start-raw base))
         (window-start-hundreds (- window-start-raw window-start-tens)))
    (if (< relative-line window-start-tens)
        (+ window-start-hundreds relative-line base)
      (+ window-start-hundreds relative-line))))


;(defun relative-line-to-absolute (relative-line radix)
;  (let* ((my-window-end (window-end))
;         (top-line (count-lines 1 (window-start)))
;         (bottom-line (count-lines 1 my-window-end))
;         (middle-line (/ (+ top-line bottom-line) 2))
;         (x (+ middle-line (/ radix 2)))
;         (modulo (% x radix))
;         (base (- x modulo)))
    
;    (if (> relative-line modulo) 
;        (setq base (- base radix)))
;    (+ base relative-line)))

(defun cache-from-jumped-char-on-rel-line (relative-line char occurrence radix before-or-after)
  "cache the symbol at a certain character position on a relative line number, without moving (point)"
  (interactive "nRelative-line: \nsCharacter: \nnOccurrence: \nnRadix: \nnPlace cursor before (-1), after (1), or according to default (0): ")
  (save-excursion
    (if (jump-to-char-on-rel-line relative-line char occurrence radix before-or-after)
	(insert-cache-pad)))
)

(defun jump-to-char-on-rel-line (relative-line char occurrence radix before-or-after)
  "jump to a certain character position on a relative line number"
  (interactive "nRelative-line: \nsCharacter: \nnOccurrence: \nnRadix: \nnPlace cursor before (-1), after (1), or according to default (0): ")
  (let ((before (< before-or-after 0)))
    (if (< relative-line 0) (setq relative-line (% (+ (count-lines 1 (point)) 2 relative-line) radix)))
    (if (= before-or-after 0) (setq before cache-place-point-before-rel-search))
    (goto-line (relative-line-to-absolute relative-line radix))
    (if (> occurrence 0)
	(progn
	  (end-of-line)
	  (let ((endpoint (point)))
	    (beginning-of-line)
	    (if (search-forward char endpoint nil occurrence)
		(progn
		  (if before (backward-char))
		  t)
	      nil)
	    ))
      (progn
	(beginning-of-line)
	(let ((beginpoint (point))
	      (distance (if (< occurrence 0) (- 0 occurrence) 1)))
	  (end-of-line)
	  (if (search-backward char beginpoint nil distance)
	      (progn
		(if (null before) (forward-char))
		t)
	    nil)
	  )))))

(defun cache-pad-print-parsable (filename)
  " Write the cache pad in a format where it may be easily parsed by other applications.  The output format consists of one symbol per line"
  (interactive)
  (let ((temp-frame (selected-frame)))
    (select-frame cache-frame)
;    (find-file filename)
;    (erase-buffer)
;    (end-of-buffer nil)
;    (while the-list
;      (insert (concat (car the-list) "\n"))
;      (setq the-list (cdr the-list)))
    (if cache-pad-writable
        (save-buffer))
    (select-frame temp-frame)))


(defun cache-pad-parse-input (filename &optional reverse-it flush-it)
  "Input consists of one symbol per line.  The symbols are inserted into the cache pad, following the LRU algorithm."
  (let ((word-list nil))
    (find-file filename)
    (setq buffer-read-only nil)
    (beginning-of-buffer)
    (while (re-search-forward "(.*) \\(.*\\)" nil t)
;      (message (match-string 1)); danger: if string contains % , bad news!
;      (sleep-for 2)
      (setq word-list (append (list (match-string 1)) word-list)))
    (mapcar 'add-word word-list)))
    
;  (end-of-buffer)
;  (setq output-with-newlines (buffer-substring (point) (mark)))
;;  (kill-buffer nil)
;  (setq word-list
;;       (split-string
;	(ginel-string-split
;	 output-with-newlines "[\n\r]+"))
;  (if reverse-it (setq word-list (nreverse word-list)))
;  (if flush-it (setq cache-pad-list nil))
;  (erase-buffer)
;;  (mapcar 'insert-word-into-cache-pad word-list)
;  (mapcar 'add-word word-list)
;  )
;)

