(magit-section :fetcher github
	       :repo "magit/magit"
	       :files ("lisp/magit-section.el"
		       "lisp/magit-section-pkg.el"
		       "docs/magit-section.texi"
		       "Documentation/magit-section.texi"))

(require 'magit-section)

(defmacro with-demo-buffer (&rest body)
  (declare (indent 0))
  `(let ((buffer (get-buffer-create "*section demo*")))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
	 (erase-buffer)
	 (magit-section-mode)
	 (magit-insert-section (demo-buffer)
	   ,@body)))
     (switch-to-buffer-other-window buffer)))

(with-demo-buffer)

(with-demo-buffer
  (magit-insert-section (demo-files)
    (magit-insert-heading
      (format "Files in %s\n" default-directory))
    (dolist (file (process-lines "ls"))
      (magit-insert-section (demo-file file)
	(insert (format "%s\n" file))))))

(with-demo-buffer
  (magit-insert-section (demo-files)
    (magit-insert-heading
      (format "Files in %s\n" default-directory))
    (save-excursion
      (call-process "ls" nil t))
    (magit-wash-sequence
     (lambda ()
       (unless (eobp)
	 (let ((file (buffer-substring (line-beginning-position)
				       (line-end-position))))
	   (magit-insert-section (demo-file file)
	     (forward-line))))))))

(with-demo-buffer
  (magit-insert-section (demo-files)
    (magit-insert-heading
      (format "Files in %s\n" default-directory))
    (save-excursion
      (call-process "ls" nil t))
    (magit-wash-sequence
     (lambda ()
       (unless (eobp)
	 (let* ((beg (line-beginning-position))
		(end (line-end-position))
		(file (buffer-substring beg end)))
	   (delete-region beg (1+ end))
	   (magit-insert-section (demo-file file)
	     (insert (if (eobp) "`- " "+- ") file "\n"))))))))

