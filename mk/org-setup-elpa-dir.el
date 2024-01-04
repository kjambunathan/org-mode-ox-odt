;;;; org-setup-elpa-dir --- Enable linking with 3rd-party libraries -*- lexical-binding: t; coding: utf-8-emacs; -*-

;; Copyright (C) 2023  Jambuanthan K

;; Author: Jambunathan K <kjambunathan@gmail.com>
;; Version:
;; Homepage: https://github.com/kjambunathan/dotemacs
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `org-setup-elpa-dir' is a glue code for building the Enhanced
;; OpenDocument Exporters.
;;
;; The enhanced OpenDocument Exporter--particularly the `OpenDocument
;; Spreadsheet' exporter--relies on libraries like `peg' which are not
;; part of vanilla Emacs.  These libraries are available as add-on
;; packages through ELPA repos.
;;
;; Stock `org-mode'-s build process does NOT rely on any 3rd-party
;; packages, and make no provision for linking to ELPA libs.  This is
;; understandable because stock `org-mode' ships with vanilla Emacs.
;; This is not the case with the Advanced OpenDocument Exporter.
;;
;; This file (=`org-setup-elpa-dir') in conjunction with `local.mk'
;; permits build to link against 3rd-party packages.  Specifically, it
;; introduces a make variable `USER_ELPA_DIR' which is configurable.

;; Quick Overview
;; ==============
;;
;; The `ODS' (= "OpenDocument SpreadSheet") exporter that ships as part
;; of https://github.com/kjambunathan/org-mode-ox-odt relies on `peg' (=
;; "Parsing Expression Grammars in Emacs Lisp") library.
;;
;; If a user builds the OpenDocument exporter from above repo, and if
;; they have NOT installed `peg' or any other package then the
;; OpenDocument Exporters rely on, then the build may report errors.
;;
;; The glue code here suggests what corrective action an user may take
;; so that the build succeeds.
;;
;; The suggestions are:
;;
;; - Install `M-x package-install RET <peg-or-whatever> RET, and build again.
;;
;; - If the rebuild fails, it is most likely that the user is using
;;   non-standard paths for his ELPA packages.  In this case, encourage
;;   the user to teach the build system about their ELPA dir through an
;;   environment variable `USER_ELPA_DIR'.
;;
;;  Side-note: on why there is a need for `USER_ELPA_DIR'
;;  =======================================================
;;
;; The author of the ODT exporter uses a non-standard path for his Emacs
;; Lisp Packages.  He install all his extra packages through a standard
;; `git' checkout of these Urls,
;;
;;     GNU ELPA repo     : https://git.savannah.gnu.org/git/emacs/elpa.git
;;        See https://elpa.gnu.org/packages/index.html
;;
;;     Non-GNU ELPA repo : git://git.savannah.gnu.org/emacs/nongnu.git
;;        See https://elpa.nongnu.org/
;;
;;     GNU ELPA Devel repo:
;;        See https://elpa.gnu.org/devel/
;;
;;     MELPA repo        : https://github.com/melpa/melpa.git
;;       See https://melpa.org/
;;
;; and makes any of the available packages seamlessly through a single
;; `~/src/nongnu-elpa/packages/' (= `USER_ELPA_DIR') directory.  All
;; this is done using his own homegrown setup.  This means that the
;; author of the ODT exporters neither uses the standard native
;; installers like `package', or popular 3rd party installers like
;; `straight', `elpaca', `el-get' etc.
;;
;; The build system of this repo reflects Author's own prejudices and
;; preferences on how he likes his Emacs.
;;
;; How to build this repo if you have non-standard ELPA dirs
;; =========================================================
;;
;; Specifically, he uses the following config in his `.emacs':
;;
;;     (custom-set-variables
;;      '(package-archives
;;        '(("gnu" . "https://elpa.gnu.org/packages/")
;;          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
;;          ("melpa" . "https://melpa.org/packages/")
;;          ("gnu-devel" . "https://elpa.gnu.org/devel/")
;;          ("ox-odt" . "https://kjambunathan.github.io/elpa/")))
;;      '(package-directory-list
;;        '("~/src/elpa/packages" "~/src/nongnu-elpa/packages")))
;;
;; And he adds the following directive to his `~.bashrc'
;;
;;     export USER_ELPA_DIR=~/src/nongnu-elpa/packages
;;
;; This ensures that when he is in a Bash Shell, he can compile the
;; project with a simple `make' command.
;;
;; He also modifies this project's `.dir-locals.el' (see the root dir of
;; this project), such that `compile-command' is set to
;;
;;     "USER_ELPA_DIR=~/src/nongnu-elpa/packages make"
;;
;; This ensures that when he in the middle of making changes to this
;; project and is in a GUI Emacs session, he can build the project with
;; `M-x project-compile' (= `C-x p c').
;;
;; Occassionally, he also builds this repo with
;;
;;     USER_ELPA_DIR=~/src/nongnu-elpa/packages make
;;
;; You can use the above invocation if you don't want to mess around
;; with your `~/.bashrc' or make local changes to this repo.
;;
;; How to build when you use standard  ELPA dirs
;; =============================================
;;
;; Let me reiterate: If you are using the standard configuration of
;; `package-directory-list' or `package-user-dir', then the above
;; fuzzing with USER_ELPA_DIR does NOT apply for you; a simple `make'
;; shoud suffice as long as `peg' and other requisite packages can be
;; are in one of the in `package-directory-list' or `package-user-dir'
;; directories.

;;; Code:

(require 'find-func)
(require 'package)

;; Libraries that the OpenDocument Exporter relies on
(defvar odt-libs
  '(peg rnc-mode))

;; Configure the ELPA dir where `peg', `rnc-mode' can be found
(when (getenv "USER_ELPA_DIR")
  (setq package-user-dir (file-name-as-directory (getenv "USER_ELPA_DIR"))))

;; Put `peg', `rnc-mode' etc in the `load-path'
(setq package-load-list '(
                          (peg t)
                          (rnc-mode t)
                          ))
(package-initialize)

;; Load the above libraries
(dolist (l odt-libs)
  (cond
   ;; Case 1: The required packages are available.  Just log the
   ;; directory from which these libraries are picked up.
   ((require l nil t)
    (message "Library `%s' loaded from `%s'" l (find-library-name (symbol-name l))))
   ;; Case 2: The required libraries are NOT found.  Suggest corrective
   ;; action to the user.
   (t
    (error "
Library `%s' not found in

	- `package-directory-list' (= %S)
	- `package-user-dir' (= %S)

To fix this error, make `%s' available in above dirs by doing
`M-x package-install RET %s RET'

If you are using non-standard value for `package-directory-list'
or `package-user-dir' in your Emacs, then the re-build will fail
again even after the above `M-x package-install ...' step.  In
this case you need to configure the environment variable
`USER_ELPA_DIR' to point to your ELPA dir and compile again.

For example if your ELPA packages are available under
`~/my/elpa/packages' subdir, you can do any of the the following

    1. Add the following to your `~/.bashrc'

           export USER_ELPA_DIR=~/my/elpa/packages

       restart your shell, and do `make' again.

    2. Re-build with

           USER_ELPA_DIR=~/my/elpa/packages make

    3. Visit the `.dir-locals.el' file of this repo, and elide or
       change the `USER_ELPA_DIR' setting in `compile-command' and
       re-build the project with `M-x project-compile' (=`C-x p c')

See `mk/org-setup-elpa-dir.el' for further information.
"
	   l
	   package-directory-list
	   package-user-dir
           l
	   l))))

(provide 'org-setup-elpa-dir)
;;; org-setup-elpa-dir.el ends here
