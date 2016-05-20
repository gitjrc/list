(defun jrc-git--chop-nl (string)
  "Return a copy of string with the last character removed."
  (replace-regexp-in-string "\n" "" string))

(defun jrc-git--current-branch ()
  (jrc-git--chop-nl (shell-command-to-string
		     "git rev-parse --abbrev-ref HEAD")))

(defun jrc-git--rev-list-string (branch)
  "Return 'git rev-list branch' as a single string."
  (shell-command-to-string (concat "git rev-list " branch)))

(defun jrc-git--toplevel  ()
  "Return 'git rev-parse --show-toplevel'."
  (jrc-git--chop-nl (shell-command-to-string "git rev-parse --show-toplevel")))

(defun jrc-git--relative-name  (file-name)
  "Return file-name, but with all non-git directory elements removed."
  (let ((repository-directory-list  (split-string (jrc-git--toplevel) "/"))
	(buffer-file-list (split-string file-name "/")))
    (while repository-directory-list
      (if (not (equal (car repository-directory-list)
		      (car buffer-file-list)))
	  (pop repository-directory-list) ;  extraneous directory; nuke it
	(pop repository-directory-list)
	(pop buffer-file-list)))
    (mapconcat 'identity  buffer-file-list  "/")))
	

(defun jrc-git-diff ()
  (interactive)
  (let* ((current-branch (jrc-git--current-branch))
	 (the-current-buffer (current-buffer))
	 (file-name (buffer-file-name))
	 (branch-rev-list-list
	  (split-string (jrc-git--rev-list-string current-branch) "\n"))
	 (master-rev-list-string (jrc-git--rev-list-string "master"))
	 (original-version-buffer (get-buffer-create "*master version*"))
	 master-commit local-commit )
    ;; Iterate over all commits in this branch looking for master commit.
    (dolist (local-commit branch-rev-list-list master-commit)
      (if (and (not master-commit) (string-match local-commit
						 master-rev-list-string))
	  (setq master-commit local-commit)))
    ;; Fetch file associated with the master commit.
    (save-excursion
      (set-buffer original-version-buffer)
      (delete-region (point-min) (point-max))
      (insert
	  (shell-command-to-string 
	   (concat "git show " master-commit ":" 
		   (jrc-git--relative-name file-name)))))
    (select-frame (make-frame '((width . 235) ( height . 50))))
    (ediff-buffers original-version-buffer the-current-buffer)))

