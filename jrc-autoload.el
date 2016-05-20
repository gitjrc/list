;;;### (autoloads (jrc-pad-jump-to-relative-line jrc-pad-remove-entry jrc-pad-enqueue-word jrc-pad-enqueue-region jrc-pad-enqueue-buffer jrc-pad-enqueue-directory jrc-pad-dired jrc-pad-insert-identifier jrc-pad-buffer jrc-pad-find-file jrc-pad-identifiers-to-top jrc-pad-files-to-top jrc-pad-directories-to-top jrc-pad-create-window) "jrc-pad" "lisp/jrc-pad.el")

(autoload 'jrc-pad-create-window "jrc-pad" "\
create pad window and frame, using cache frame if it exists" t nil)

(autoload 'jrc-pad-directories-to-top "jrc-pad" "\
sort pad , moving directories to beginning" t nil)

(autoload 'jrc-pad-files-to-top "jrc-pad" "\
sort pad , moving files to beginning" t nil)

(autoload 'jrc-pad-identifiers-to-top "jrc-pad" "\
sort pad , moving identifiers to beginning" t nil)

(autoload 'jrc-pad-find-file "jrc-pad" "\
load file specified at index" t nil)

(autoload 'jrc-pad-buffer "jrc-pad" "\
Switch to buffer  specified at index" t nil)

(autoload 'jrc-pad-insert-identifier "jrc-pad" "\
Insert identifier at index" t nil)

(autoload 'jrc-pad-dired "jrc-pad" "\
run dired on directory at index" t nil)

(autoload 'jrc-pad-enqueue-directory "jrc-pad" "\
insert the current directory into pad" t nil)

(autoload 'jrc-pad-enqueue-buffer "jrc-pad" "\
insert the current buffer name into pad" t nil)

(autoload 'jrc-pad-enqueue-region "jrc-pad" "\
Insert the current region into pad" t nil)

(autoload 'jrc-pad-enqueue-word "jrc-pad" "\
Inserts the word at cursor in cache-pad if 'word' is not there." t nil)

(autoload 'jrc-pad-remove-entry "jrc-pad" "\
deletes the entry at index" t nil)

(autoload 'jrc-pad-jump-to-relative-line "jrc-pad" "\
jump to relative line number" t nil)

;;;***

