;;; org-fixup.el --- make life easier for folks without GNU make
;;
;; Author: Achim Gratz
;; Keywords: orgmode
;; URL: https://orgmode.org
;;
;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

(require 'autoload)
(require 'org-compat "org-compat.el")

(defun org-make-manual ()
  "Generate the Texinfo file out of the Org manual."
  (require 'ox-texinfo)
  (find-file "../doc/org-manual.org")
  (org-texinfo-export-to-texinfo))

(defun org-make-guide ()
  "Generate the Texinfo file out of the Org guide."
  (require 'ox-texinfo)
  (find-file "../doc/org-guide.org")
  (org-texinfo-export-to-texinfo))

(make-obsolete 'org-make-manuals
               "use org-make-manual and org-make-guide."
               "9.6")
(defun org-make-manuals (&optional manuals)
  "Generate the Texinfo files out of Org manuals."
  (require 'ox-texinfo)
  (setq manuals (or manuals '("../doc/org-manual.org" "../doc/org-guide.org")))
  (dolist (manual manuals)
    (find-file manual)
    (org-texinfo-export-to-texinfo)))

(defun org-make-org-version (org-release org-git-version)
  "Make the file org-version.el in the current directory.
This function is internally used by the build system and should
be used by foreign build systems or installers to produce this
file in the installation directory of Org mode.  Org will not
work correctly if this file is not present (except directly from
the Git work tree)."
  (with-temp-buffer
    (insert "\
;;; org-version.el --- autogenerated file, do not edit  -*- lexical-binding: t -*-
;;
;;; Code:
;;;\#\#\#autoload
\(defun org-release ()
  \"The release version of Org.
Inserted by installing Org mode or when a release is made.\"
   (let ((org-release \"" org-release "\"))
     org-release))
;;;\#\#\#autoload
\(defun org-git-version ()
  \"The Git version of Org mode.
Inserted by installing Org or when a release is made.\"
   (let ((org-git-version \"" org-git-version "\"))
     org-git-version))
\f\n\(provide 'org-version\)
\f\n;; Local Variables:\n;; version-control: never
;; no-byte-compile: t
;; coding: utf-8\n;; End:\n;;; org-version.el ends here\n")
    (let ((inhibit-read-only t))
      (write-file "org-version.el"))))

(defun org-make-org-loaddefs ()
  "Make the file org-loaddefs.el in the current directory.
This function is internally used by the build system and should
be used by foreign build systems or installers to produce this
file in the installation directory of Org mode.  Org will not
work correctly if this file is not up-to-date."
  (with-temp-buffer
    (set-visited-file-name "org-loaddefs.el")
    (insert ";;; org-loaddefs.el --- autogenerated file, do not edit\n;;\n;;; Code:\n")
    (let ((files (directory-files default-directory
				  nil "^\\(org\\|ob\\|ox\\|ol\\|oc\\)\\(-.*\\)?\\.el$")))
      (mapc (lambda (f) (generate-file-autoloads f)) files))
    (insert "\f\n(provide 'org-loaddefs)\n")
    (insert "\f\n;; Local Variables:\n;; version-control: never\n")
    (insert ";; no-byte-compile: t\n;; no-update-autoloads: t\n")
    (insert ";; coding: utf-8\n;; End:\n;;; org-loaddefs.el ends here\n")
    (let ((inhibit-read-only t))
      (save-buffer))))

(defun org-make-autoloads (&optional compile force)
  "Make the files org-loaddefs.el and org-version.el in the install directory.
Finds the install directory by looking for library \"org\".
Optionally byte-compile lisp files in the install directory or
force re-compilation.  This function is provided for easier
manual install when the build system can't be used."
  (let ((origin default-directory)
	(dirlisp (org-find-library-dir "org")))
    (unwind-protect
	(progn
	  (cd dirlisp)
	  (org-fixup)
	  (org-make-org-version (org-release) (org-git-version))
	  (org-make-org-loaddefs)
	  (when compile (byte-recompile-directory dirlisp 0 force)))
      (cd origin))))

(defun org-make-autoloads-compile ()
  "Call org-make-autoloads with compile argument.
Convenience function for easier invocation from command line."
  (org-make-autoloads 'compile nil))

(defun org-make-autoloads-compile-force ()
  "Call org-make-autoloads with compile force arguments.
Convenience function for easier invocation from command line."
  (org-make-autoloads 'compile 'force))

;; Internal functions

(defun org-make-local-mk ()
  "Internal function for the build system."
  (let ((default "mk/default.mk")
	(local   "local.mk"))
    (unwind-protect
	(with-temp-buffer
	  (insert-file-contents default)
	  (goto-char (point-min))
	  (when (search-forward "-8<-" nil t)
	    (forward-line 1)
	    (delete-region (point-min) (point)))
	  (when (search-forward "->8-" nil t)
	    (forward-line 0)
	    (delete-region (point) (point-max)))
	  (goto-char (point-min))
	  (insert "
# Remove \"oldorg:\" to switch to \"all\" as the default target.
# Change \"oldorg:\" to an existing target to make that target the default,
# or define your own target here to become the default target.
oldorg:	# do what the old Makefile did by default.

##----------------------------------------------------------------------
")
	  (goto-char (point-max))
	  (insert "\
# See default.mk for further configuration options.
")
	  (let ((inhibit-read-only t))
	    (write-file local)))
      nil)))

(defun org-make-letterformat (a4name lettername)
  "Internal function for the build system."
  (unwind-protect
      (with-temp-buffer
	(insert-file-contents a4name)
	(goto-char (point-min))
	(while (search-forward "\\pdflayout=(0l)" nil t)
	  (replace-match "\\pdflayout=(1l)" nil t))
	  (let ((inhibit-read-only t))
	    (write-file lettername)))
    nil))

;; redefine version functions

(defmacro org-fixup ()
  (let* ((origin default-directory)
	 (dirlisp (org-find-library-dir "org"))
	 (dirorg (concat dirlisp "../" ))
	 (dirgit (concat dirorg ".git/" ))
	 (org-version "N/A-fixup")
	 (org-git-version "N/A-fixup !!check installation!!"))
    (if (and (boundp 'org-fake-release)     (stringp org-fake-release)
	     (boundp 'org-fake-git-version) (stringp org-fake-git-version))
	(setq org-version     org-fake-release
	      org-git-version org-fake-git-version)
      (if (load (concat dirlisp "org-version.el") 'noerror 'nomessage 'nosuffix)
	  (setq org-version     (org-release)
		org-git-version (org-git-version))
	(when (and (file-exists-p dirgit)
		   (executable-find "git"))
	  (unwind-protect
	      (progn
		(cd dirorg)
		(let ((git6 (substring (shell-command-to-string "git describe --abbrev=6 HEAD") 0 -1))
		      (git0 (substring (shell-command-to-string "git describe --abbrev=0 HEAD") 0 -1))
		      (gitd (string-match "\\S-"
					  (shell-command-to-string "git status -uno --porcelain"))))
		  (setq org-git-version (concat git6 (when gitd ".dirty") "-git"))
		  (if (string-match "^release_" git0)
		      (setq org-version (substring git0 8))
		    (setq org-version git0))))
	    (cd origin)))))
    (message "org-fixup.el: redefined Org version")
    `(progn
       (defun org-release () ,org-version)
       (defun org-git-version () ,org-git-version))))

(provide 'org-fixup)

;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8
;; End:
;;; org-fixup.el ends here
