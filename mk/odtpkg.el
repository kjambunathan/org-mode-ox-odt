(defun odtpkg-update-archive-contents (newdesc)
  (require 'cl-lib)
  (with-current-buffer (find-file-noselect "./elpa/archive-contents")
    (goto-char (point-min))
    (let* ((archive-contents (ignore-errors (read (current-buffer))))
	   (entry (assoc (car newdesc) (cdr archive-contents))))
      (erase-buffer)
      (cond
       (entry
	(setcdr entry (cdr newdesc)))
       (archive-contents
	(setcdr archive-contents (cons newdesc (cdr archive-contents))))
       (t
	(setq archive-contents (cons 1 (list newdesc)))))
      (pp  archive-contents (current-buffer))
      (save-buffer 0))))
