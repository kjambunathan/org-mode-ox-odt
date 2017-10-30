;;; ox-jabref.el --- JabRef Citation Processor for Orgmode -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Vaidheeswaran C <vaidheeswaran.chinnaraju at gmail dot com>

;; Author: Vaidheeswaran C <vaidheeswaran.chinnaraju at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 8.3.6

;;; Commentary:

;; JabRef Homepage: http://jabref.sourceforge.net/
;;
;; Basic References:
;;
;; 1. [[http://jabref.sourceforge.net/help/CommandLine.php][JabRef command line syntax]]
;; 2. [[http://jabref.sourceforge.net/help/SearchHelp.php][Syntax for search query]]
;; 3. [[http://jabref.sourceforge.net/help/CustomExports.php][Custom export filters]]

;; * Introduction

;; This modules supports multiple cite keys and use of Pre/Post notes.

;; The table below illustrates the typical output when using "Chicago
;; (author-date)" as the style.  (See below for more information on
;; available Citation styles and how they may be specified on a per-file
;; basis.)


;; | Org Input                                  | ODT Output                |
;; |--------------------------------------------+---------------------------|
;; | \cite{suzuki_studies_1998, watts_way_1999} | (Suzuki 1998; Watts 1999) |
;; | \cite[See][Pg. 10]{watts_way_1999}         | (See Watts 1999, Pg. 10)  |


;; * Quick start guide
;;
;; 1. Install [[http://jabref.sourceforge.net/][JabRef]]
;;
;;    This module is tested with version JabRef-2.9.2.jar.
;;
;; 2. Install the JabRef plugin [[http://repo.or.cz/w/JabRefChicagoForOrgmode.git/blob_plain/HEAD:/net.sf.jabref.export.Chicago.ODF(English)-1.2.jar][Chicago Export filters for Org-mode]].
;;
;; 3. Enable JabRef support
;;
;;    #+BEGIN_EXAMPLE
;;      M-x customize-variable RET org-modules RET
;;    #+END_EXAMPLE
;;
;;    Enable the tick mark against JabRef.
;;
;;    Alternatively, you can add the following to your .emacs
;;
;;    #+BEGIN_EXAMPLE
;;      (require 'ox-jabref)
;;    #+END_EXAMPLE
;;
;;    Now when you invoke the Org exporter with C-c C-e, you will see
;;    the following in the dispatcher menu.
;;
;;      "Export to ODT (With Jabref Processing)"
;;
;; 4. Add the following line to your Org file and export.
;;
;;    #+BEGIN_SRC org
;;      ,#+ATTR_ODT: :style "Chicago (author-date)"
;;      ,#+BIBLIOGRAPHY: MyLibrary chicago option:-d
;;    #+END_SRC
;;
;;    Remember to replace the value of `:style' property to a style of
;;    your choosing.  The JabRef modules ships with following citation
;;    styles
;;
;;    - "Numeric"
;;    - "Chicago (full-note)"
;;    - "Chicago (author-date)"
;;
;; * Advanced configuration
;;
;; You can see a comprehensisve list of the user options with
;;
;; #+BEGIN_EXAMPLE
;;   M-x customize-group RET org-jabref RET
;; #+END_EXAMPLE
;;
;; Here is a quick overview of what you can achieve with above user
;; options.
;;
;; 1. `org-jabref-citation-styles'
;;
;;    To create annnotated bibliographies, replace "chicago.ODF.biblio"
;;    with "chicago.ODF.abstract" or "chicago.ODF.note".
;;
;;    To order bibliography entry by the order in which they are cited,
;;    set `:order-by-jabref' to t.
;;
;;    To include all bibliographic entries (whether or not they are
;;    cited) set `:dont-filter' to t.
;;
;; 2. `org-jabref-citation-formatters'
;;
;;    Modify the braces (round or square etc.), separators (semi-colon or
;;    comma etc.) to your taste.

;;; Code:

(require 'ox)
(require 'ox-odt)
(eval-when-compile
  (require 'cl))

;;; Internal Variables

(defvar org-jabref-formats
  '("Numeric"
    "chicago.ODF.biblio" "chicago.ODF.abstract" "chicago.ODF.note"
    "chicago.ODF.text" "chicago.ODF.footend" "chicago.ODF.footend.short"
    "chicago.ODF.reference")
  "Export formats avaiable from JabRef and used by this module.
Use

    \"java -jar ~/Downloads/JabRef-2.9.2.jar -n true -h\"

to see all available export formats.")

(defvar org-jabref-formats--type
  `(,@(mapcar (lambda (export-format)
		`(const ,export-format))
	      org-jabref-formats)
    (string :tag "Other" ))
  "Same as `org-jabref-formats', but for use with Emacs customizer.")

;;; User Configuration Options.

(defgroup org-jabref nil
  "Options for processing citations via JabRef."
  :tag "Org Jabref"
  :group 'org-export)

(defcustom org-jabref-command '("java" "-jar" "JabRef-2.9.2.jar" "-n" "true")
  "Jabref program."
  :type '(choice
	  (const :tag "Not configured" nil)
	  (repeat string))
  :group 'org-jabref
  :version "25.1"
  :package-version '(Org . "8.3"))

(defvar org-jabref-citation-formatter--params-type
  `((:substring (choice
		 (const :tag "Don't clip" nil)
		 (const :tag "Don't clip" (0 . nil))
		 (const :tag "Strip surrounding braces" (1 . -1))
		 (const :tag "Strip full-stop" (0 . -1))
		 (cons (integer :tag "From index")
		       (integer :tag "To index"))))
    (:braces (repeat :tag "List of braces"
		     (choice (const :tag "None" nil)
			     (const :tag "Round" ("(" . ")"))
			     (const :tag "Square" ("[" . "]"))
			     (const :tag "Add full-stop" (nil . "."))
			     (const :tag "ODT Text span"
				    ("<text:span text:style-name=\"OrgCite\">" .
				     "</text:span>"))
			     (const :tag "ODT Bibliography"
				    ("
<text:section text:style-name=\"OrgIndexSection\" text:name=\"Bibliography\">
<text:p text:style-name=\"Bibliography_20_Heading\">References</text:p>" . "
</text:section>
"))
			     (const :tag "ODT Bibliography (Numbered)"
				    ("
<text:section text:style-name=\"OrgIndexSection\" text:name=\"Bibliography\">
<text:p text:style-name=\"Bibliography_20_Heading\">References</text:p>
<text:list text:style-name=\"OrgBibliographyList\" text:continue-numbering=\"false\">" . "
</text:list>
</text:section>
"))
			     (const :tag "ODT List item"
				    ("
<text:list-item>"  . "
</text:list-item>"))
			     (cons (choice (const :tag "None" nil)
					   (string :tag "Opening brace"))
				   (choice (const :tag "None" nil)
					   (string :tag "Closing brace"))))
		     ))
    (:between-citations (choice (const :tag "None" nil)
				(const :tag "Comma" ", ")
				(const :tag "Semi-colon" "; ")
				(string :tag "Separator")))
    (:text-after-pre-note (choice (const :tag "None" nil)
				  (const :tag "Space" " ")
				  string))
    (:text-before-post-note (choice (const :tag "None" nil)
				    (const :tag "Space" " ")
				    (const :tag "Comma" ", ")
				    string))))

(defvar org-jabref-citation-formatters--value-type
  `(cons (function :tag "Multicite formatter")
	 (plist :options
		(,@org-jabref-citation-formatter--params-type
		 (:formatter
		  (choice
		   (function :tag "Key formatter")
		   (cons :tag "Key formatter with params"
			 (function :tag "Key formatter")
			 (plist :options ,org-jabref-citation-formatter--params-type))))))))

(defcustom org-jabref-citation-formatters
  `(("odt"
     ("Simple" .
      (org-jabref--multicite
       :braces (("<text:span text:style-name=\"OrgCite\">" . "</text:span>")
		("[" . "]"))
       :between-citations ", "
       :text-after-pre-note " "
       :text-before-post-note ", "
       :formatter org-jabref-odt-format-citation-as-link))
     ("Simple (but strip braces)" .
      (org-jabref--multicite
       :braces (("(" . ")"))
       :between-citations "; "
       :text-after-pre-note " "
       :text-before-post-note ", "
       :formatter (org-jabref-odt-format-citation-as-link
		   :substring (1 . -1))))
     ("Footnote" .
      (org-jabref--multicite
       :between-citations
       ,(format "<text:span text:style-name=\"%s\">%s</text:span>"
		"OrgSuperscript" ", ")
       :formatter (org-jabref-odt-format-citation-as-footnote-definition
		   :substring (0 . -1)
		   :braces ((nil . "."))
		   :text-after-pre-note " "
		   :text-before-post-note ", ")))
     ("Bibliography" .
      (org-jabref--multicite
       :braces
       (("
<text:section text:style-name=\"OrgIndexSection\" text:name=\"Bibliography\">
<text:p text:style-name=\"Bibliography_20_Heading\">References</text:p>
" . "
</text:section>
"))
       :formatter org-jabref-odt-format-bibliography--use-hanging-indent))
     ("Bibliography (Numbered)" .
      (org-jabref--multicite
       :braces
       (("
<text:section text:style-name=\"OrgIndexSection\" text:name=\"Bibliography\">
<text:p text:style-name=\"Bibliography_20_Heading\">References</text:p>
<text:list text:style-name=\"OrgBibliographyList\" text:continue-numbering=\"false\">" . "
</text:list>
</text:section>
"))
       :formatter (org-jabref-odt-format-bibliography--no-hanging-indent
		   :braces
		   (("
<text:list-item>"  . "
</text:list-item>")))))))
  "Citation formatters for various backends."
  :group 'org-jabref
  :version "25.1"
  :package-version '(Org . "8.3")
  :type `(alist :key-type (string :tag "Export backend")
		:options ("odt" "html")
		:value-type
		(alist :key-type (string :tag "Formatter style")
		       :value-type
		       ,org-jabref-citation-formatters--value-type)))

(defvar org-jabref-citation-formatters--default
  (let ((formatter-names (mapcar 'car (assoc-default "odt" org-jabref-citation-formatters))))
      `(choice :tag "Formatter name"
	   ,@(loop for formatter-name in formatter-names
		   collect `(const ,formatter-name))
	   (string :tag "Other" ,(car formatter-names)))))

(defcustom org-jabref-citation-styles
  `(("odt"
     ("Numeric"
      :in-text
      (:jabref-format "Numeric" :formatter "Simple")
      :bibliography
      (:jabref-format "chicago.ODF.reference" :formatter "Bibliography (Numbered)"))
     ("Chicago (full-note)"
      :in-text
      (:jabref-format ("chicago.ODF.footend" .
		       "chicago.ODF.footend.short") :formatter "Footnote")
      :bibliography
      (:jabref-format "chicago.ODF.biblio" :formatter "Bibliography"))
     ("Chicago (author-date)"
      :in-text
      (:jabref-format "chicago.ODF.text" :formatter "Simple (but strip braces)")
      :bibliography
      (:jabref-format "chicago.ODF.reference" :formatter "Bibliography"))))
  "Citation styles for various backends.
Each element in this list is of the form:

  \(EXPORT-BACKEND-NAME . BACKEND-STYLE-FITLERS\)

BACKEND-STYLE-FILTERS is of the form:

  \(CITATION-STYLE . EXPORT-FORMATS-PLIST\)

EXPORT-FORMATS-PLIST is property list with two well-known
properties - `:in-text' and `:bibliography'.  The values
of these properties are the export formats registered with
JabRef.

A typical value for this variable could be:

  '((\"odt\"
     (\"default\" :in-text \"chicago.ODF.text\"
      :bibliography \"chicago.ODF.reference\"))
    (\"html\"
     (\"default\" :bibliography \"html\")
     (\"simple\" :bibliography \"simplehtml\")))

For a list of export formats registered with JabRef use:

  java -jar JabRef-2.9.2.jar -n true -h."
  :group 'org-jabref
  :version "25.1"
  :package-version '(Org . "8.3")
  :type `(alist :key-type (string :tag "Export backend")
		:options ("odt" "html")
		:value-type
		(alist :key-type (string :tag "Citation style")
		       :options ("Chicago (author-date)"
				 "Chicago (full-note)")
		       :value-type
		       (plist
			:tag "Citation style configuration"
			:options
			((:in-text
			  (plist
			   :tag "In text format"
			   :options ((:jabref-format
				      (choice :tag "JabRef format"
					      ,@org-jabref-formats--type
					      (cons :tag "Long, short formatters"
						    (choice :tag "JabRef format (Long)"
							    ,@org-jabref-formats--type)
						    (choice :tag "JabRef format (Short)"
							    ,@org-jabref-formats--type))))
				     (:formatter ,org-jabref-citation-formatters--default))))
			 (:bibliography
			  (plist
			   :tag "Bibliogrpahy format"
			   :options ((:jabref-format
				      (choice :tag "JabRef export format"
					      ,@org-jabref-formats--type))
				     (:order-by-jabref
				      (choice
				       (const :tag "Order by reference" nil)
				       (const :tag "Order by JabRef settings" t)))
				     (:dont-filter
				      (choice
				       (const :tag "Filter entries" nil)
				       (const :tag "Do not filter" t)))
				     (:formatter ,org-jabref-citation-formatters--default)))))))))


;;; Loading and Unloaing

(defvar org-jabref-org-export-backends '(odt))

(defvar org-jabref--stock-backends
  (mapcar
   (lambda (backend)
     (cons backend (org-export-get-backend backend)))
   org-jabref-org-export-backends)
  "Backend definition of stock ODT exporter.")

(defvar org-jabref--enhanced-backends
  (mapcar
   (lambda (backend)
     (cons backend
	   (let* ((stock-backend (assoc-default backend org-jabref--stock-backends))
		  ;; Copy over the stock backend.
		  (enhanced-backend (copy-tree stock-backend t)))
	     ;; Override default citation transcoders with our own.
	     (setf (org-export-backend-transcoders enhanced-backend)
		   (append
		    '((keyword . org-jabref-keyword)
		      (citation . org-jabref-citation))
		    (org-export-backend-transcoders stock-backend)))
	     ;; Override default export options with our own.
	     (setf (org-export-backend-options enhanced-backend)
		   (append (org-export-backend-options stock-backend)
			   '((:jabref-citation-style
			      "ODT_JABREF_CITATION_STYLE" nil
			      ;; For the default value, use *all*
			      ;; supported Citation styles.  Once a
			      ;; user inserts the template with `C-C
			      ;; C-e # odt', he can edit this line to
			      ;; his needs.
			      (mapconcat (lambda (s)
					   (format "\"%s\"" (car s)))
					 (assoc-default "odt" org-jabref-citation-styles) " | ")
			      t))))	     
	     ;; Modify the menu description.
	     (let ((menu (org-export-backend-menu enhanced-backend)))
	       (setf (cadr menu) (concat (cadr menu) " (With Jabref Processing)")))
	     ;; Replace the existing ODT backend.
	     (org-export-register-backend enhanced-backend)
	     ;; Return the enhanced backend.
	     enhanced-backend)))
   org-jabref-org-export-backends)
  "Backend definition of ODT exporter with JabRef processing.")

(defun ox-jabref-unload-function ()
  "Restore the stock ODT backend."
  (prog1 nil
    (ad-deactivate 'org-export--collect-tree-properties)
    (dolist (backend org-jabref-org-export-backends)
      (org-export-register-backend
       (assoc-default backend org-jabref--stock-backends)))
    (message "ox-jabref: Unloaded")))


;;; Internal functions

(defun org-jabref--sanitize-options (info)
  ;; Sanitize value of #+BIB_FILE.
  (let ((bib-file (plist-get info :bib-file)))
    (when bib-file
      ;; Parse value.
      (setq bib-file (ignore-errors (read bib-file)))
      ;; Does bibfile exists?
      (unless (and (stringp bib-file)
		   (file-regular-p bib-file)
		   (file-readable-p bib-file))
	(user-error "(ox-jabref): Unreadable Bibliography file: %s" (or bib-file "")))
      ;; Convert bibfile to an absolute path and stash it.
      (plist-put info :bib-file (expand-file-name bib-file))))

  ;; Sanitize value of #+ODT_JABREF_CITATION_STYLE.
  (let ((citation-style (plist-get info :jabref-citation-style)))
    (when citation-style
      ;; Parse value.
      (setq citation-style (ignore-errors (read citation-style)))
      ;; Can I handle the requested Citation Style?
      (let* ((backend (plist-get info :back-end))
	     (backend-name (symbol-name (org-export-backend-name backend)))
	     (styles-alist (assoc-default backend-name org-jabref-citation-styles)))
	(unless (and (stringp citation-style) (assoc-string citation-style styles-alist t))
	  (message "(ox-jabref): Unsupported Citation style \"%s\"" citation-style)
	  ;; No.  Just use the first of the available citation styles.
	  (setq citation-style (caar styles-alist))))
      (plist-put info :jabref-citation-style citation-style)))
  
  (message "(ox-jabref): Bib file:  %s" (plist-get info :bib-file))
  (message "(ox-jabref): Citation style: %s" (plist-get info :jabref-citation-style)))

(defun org-jabref--get-export-format (info op prop)
  "Return the EXPORT-FORMAT configured for operation OP.
INFO is a plist holding contextual information.  OP can be one of
`:in-text' or `:bibliography'.  Return EXPORT-FORMAT is
that is registered for the current export backend.  See
`org-jabref-citation-styles'."
  (let* ((backend (plist-get info :back-end))
	 (backend-name (symbol-name (org-export-backend-name backend)))
	 (styles-alist (assoc-default backend-name org-jabref-citation-styles))
	 (citation-style (plist-get info :jabref-citation-style))
 	 ;; Get JabRef export format that match this backend and
	 ;; citation-style.
	 (export-formats-plist (assoc-default citation-style styles-alist)))
    (cond
     ((null op) export-formats-plist)
     ((null prop) (plist-get export-formats-plist op))
     (t (plist-get (plist-get export-formats-plist op) prop)))))

(defun org-jabref--get-citation-formatter (info formatter-name)
  "Return the EXPORT-FORMAT configured for operation OP.
INFO is a plist holding contextual information.  OP can be one of
`:in-text' or `:bibliography'.  Return EXPORT-FORMAT is
that is registered for the current export backend.  See
`org-jabref-citation-styles'."
  (let* ((backend (plist-get info :back-end))
	 (backend-name (symbol-name (org-export-backend-name backend)))
	 (formatters-alist (assoc-default backend-name org-jabref-citation-formatters)))
    (or (assoc-default formatter-name formatters-alist)
	(user-error "Unknown Citation formatter \"%s\"" formatter-name)
	(cdar formatters-alist))))

(defun org-jabref--run-cmd (&rest args)
  "Run JabRef command with ARGS.
Append ARGS to `org-jabref-command' and use `call-process'.
Return output, if the command succeeds.  Otherwise throw an
error."
  (let ((cmd (append org-jabref-command args)))
    (let (exitcode err-string)
      (message "Running %s" (mapconcat 'identity cmd " "))
      (setq err-string
	    (with-output-to-string
	      (setq exitcode
		    (apply 'call-process (car cmd)
			   nil standard-output nil (cdr cmd)))))
      (if (zerop exitcode) err-string
	  (error (format "JabRef command failed with error (%s)"
			 err-string))))))

(defun org-jabref--create-filtered-bibfile (bib-file cite-keys)
  "Truncate BIB-FILE so that it contains only CITE-KEYS.
Return name of the newly created bib file.

Specifically,

  - Create a temporary auxiliary file FILTERED.AUX file which
    contains just the CITE-KEYS

  - Run the following command (for example)

        jabref -n true -a FILTERED.AUX,FILTERED.BIB BIB-FILE

  - Return FILTERED.BIB."

  (let* ((aux-file (make-temp-file "jabref" nil ".aux"))
	 (filtered-bib-file (concat (file-name-sans-extension aux-file) ".bib")))
    ;; Create an .aux file out of CITE-KEYS.
    (with-current-buffer (find-file-noselect aux-file)
      (insert (mapconcat (lambda (cite-key)
			   (format "\\citation{%s}" cite-key))
			 cite-keys "\n"))
      (save-buffer 0))
    ;; Create a filtered bib file out of the aux file.
    (org-jabref--run-cmd "-a" (concat aux-file ","  filtered-bib-file) bib-file)
    ;; Delete aux file.
    (delete-file aux-file)
    ;; Return filtered bib file.
    filtered-bib-file))

;;;; JabRef Capabilities

(defun org-jabref-get-available-export-formats ()
  (let* ((help (org-jabref--run-cmd "-h" ))
	 (available-formats
	  (when (string-match
		 "\\(?:Available export formats:\\(?1:\\(?:.\\|\n\\)+?\\)\\(?:\n\n\\|\\'\\)\\)" help)
	    (match-string 1 help))))
    (when available-formats
      (mapcar 'org-trim (split-string available-formats "[,\f\t\n\r\v]+")))))

;;;; Export a BIB file, enbloc

(defun org-jabref-export-bib-file (bib-file export-format)
  "Export BIB-FILE to EXPORT-FORMAT.
CITE-KEYS is a list of cite keys that must occur in BIB-FILE.
Return an alist of (CITE-KEY . XML-STRING).  When CITE-KEYS is
non-nil, restrict CITE-KEYs to those that occur in CITE-KEYS.
Throw an error if a key in CITE-KEYS does not occur in BIB-FILE."
  (let ((xml (org-jabref-do-export-bib-file bib-file export-format)))
    (when xml
      (with-temp-buffer
	(insert xml)
	(let (result-alist)
	  (goto-char (point-min))
	  (condition-case nil
	      (while (and (not (eobp))
			  (re-search-forward "[ \n]*<!-- BEGIN: \\(.*?\\) -->[ \n]*"))
		(let ((cite-key (substring-no-properties (match-string 1)))
		      (begin (point))
		      (end (progn (re-search-forward "[ \n]*<!-- END: \\(.*?\\) -->[ \n]*")
				  (match-beginning 0))))
		  (push (cons cite-key (buffer-substring-no-properties begin end))
			result-alist)))
	    (error (user-error "Pls. upgrade your chicago.ODF plugin")))
	  (setq result-alist (nreverse result-alist)))))))

(defun org-jabref-do-export-bib-file (bib-file export-format &optional _dummy)
  "Export BIB-FILE to EXPORT-FORMAT.
Return the resulting XML as string.  Specifically,

  - Run the following command (for example)

        jabref -n true -o OUT.XML,EXPORT-FORMAT BIB-FILE

  - Return contents of OUT.XML as a string."
  (when (and bib-file (file-readable-p bib-file) export-format)
    (with-demoted-errors
      (let* ((xml-file (make-temp-file "jabref-" nil ".xml")))
	;; Export the Citation to it's XML representation.
	(org-jabref--run-cmd "-o"  (concat xml-file "," export-format) bib-file)
	;; Return the XML string.
	(prog1 (with-temp-buffer
		 (insert-file-contents xml-file)
		 (buffer-string))
	  ;; Delete temporary xml file.
	  (delete-file xml-file))))))

(defun org-jabref-do-export-bib-file-with-filtering (bib-file export-format
							   cite-keys)
  "Export BIB-FILE to EXPORT-FORMAT, but limit output to just the CITE-KEYS.
Use `org-jabref--create-filtered-bibfile' and `org-jabref-do-export-bib-file'."
  (when (and bib-file (file-readable-p bib-file) cite-keys export-format)
    (let ((filtered-bib-file (org-jabref--create-filtered-bibfile bib-file
								  cite-keys)))
      (prog1 (org-jabref-do-export-bib-file filtered-bib-file export-format)
	(delete-file filtered-bib-file)))))

;;;; Export a CITE-KEY

(defun org-jabref-export-cite-key (bib-file export-format cite-key)
  "Export CITE-KEY from BIB-FILE to EXPORT-FORMAT.
Return the XML representation as a string. Specifically,

  - Run the following command (for example)

        jabref -n true -m bibtexkey==CITE-KEY,OUT.XML,EXPORT-FORMAT BIB-FILE

  - Return contents of OUT.XML as a string."
  (when (and bib-file (file-readable-p bib-file) cite-key export-format)
    (condition-case nil
	(let* ((xml-file (file-name-nondirectory (make-temp-file "jabref-" nil
								 ".xml"))))
	  ;; Export the Citation to it's XML representation.
	  (org-jabref--run-cmd "-m" (format "bibtexkey==%s,%s,%s" cite-key xml-file
					    export-format) bib-file)
	  ;; Return the XML string.
	  (prog1 (with-temp-buffer
		   (insert-file-contents xml-file)
		   (buffer-string))
	    ;; Delete temporary xml file.
	    (delete-file xml-file)))
      (error (user-error "Export of cite key \"%s\" failed" cite-key)))))


;;; Citation Cache

;;;; Export Filters :: Read-in and Write-out of caches

(defadvice org-export--collect-tree-properties
    (around org-jabref-load-citation-cache activate)
  "Add `:citation-cache' property to INFO."

  (org-jabref--sanitize-options info)
  
  (let* ((info ad-do-it)
	 (bib-file (plist-get info :bib-file)))
    (if (not bib-file) info
      ;; A #+BIB_FILE file is specified.
      (let* ((in-text-jabref-format
	      (org-jabref--get-export-format info :in-text :jabref-format))
	     (bibliography-jabref-format
	      (org-jabref--get-export-format info :bibliography :jabref-format))
	     (citation-style (plist-get info :jabref-citation-style))
	     ;; Collect the list of JabRef export formats that will be
	     ;; used for the specified citation style.
	     (jabref-formats (nconc
			      (list bibliography-jabref-format)
			      (cond
			       ((consp in-text-jabref-format)
				(list (car in-text-jabref-format)
				      (cdr in-text-jabref-format)))
			       ((string= in-text-jabref-format "Numeric") nil)
			       (t (list in-text-jabref-format)))))
	     (cite-keys-used (mapcar 'car (plist-get info :citations-alist)))
	     citation-cache)
	;; Does JabRef export to formats that we expect?
	(let ((missing-formats (cl-set-difference jabref-formats
						  (org-jabref-get-available-export-formats)
						  :test #'string=)))
	  (when missing-formats
	    (user-error "(ox-jabref): No JabRef plugin for %S" missing-formats)))

	;; Export BIBFILE to each of the required formats and stash
	;; the results in citation cache.
	(dolist (jabref-format jabref-formats)
	  (unless (assoc-default jabref-format citation-cache)
	    (push (cons jabref-format (org-jabref-export-bib-file
				       bib-file jabref-format))
		  citation-cache)))

	;; Filter and Re-order the bibliographic entries.
	(let* ((dont-filter (org-jabref--get-export-format info :bibliography
							   :dont-filter))
	       (order-by-jabref (org-jabref--get-export-format info :bibliography
							       :order-by-jabref))
	       (bibliography-cache (assoc bibliography-jabref-format citation-cache))
	       (bib-alist (cdr bibliography-cache))
	       (citation-alist (plist-get info :citations-alist)))
	  (message "Bibliography is %sfiltered" (if dont-filter "not " ""))
	  (unless dont-filter
	    (setq bib-alist
		  (cond
		   (order-by-jabref
		    (loop for bib-entry in bib-alist
			  if (assoc-default (car bib-entry) citation-alist)
			  collect bib-entry))
		   (t
		    (reverse
		     (loop for citation-entry in citation-alist
			   if (assoc (car citation-entry) bib-alist)
			   collect it))))))
	  (setcdr bibliography-cache bib-alist))

	;; Calculate numeric citations.
	(when (and (atom in-text-jabref-format)
		   (string= in-text-jabref-format "Numeric"))
	  (let* ((bib-alist (assoc-default bibliography-jabref-format citation-cache ))
		 (cite-key-text-alist (loop for (cite-key . text) in bib-alist
					    with n = 0 collect
					    (cons cite-key (number-to-string (incf n))))))
	    (push (cons "Numeric" cite-key-text-alist)
		  citation-cache)))

	;; (pp-display-expression citation-cache "*citation-cache*")

	;; Stash the cache in to INFO for later use by the
	;; transcoders.
	(setq ad-return-value (nconc info (list :citation-cache citation-cache)))))))


;;; Transcoders

;;;; Internal functions

(defun org-jabref--strip-and-add-pre/post-notes (text pre-note post-note
						      formatter-info)
  (let* ((substring (plist-get formatter-info :substring))
	 (text-after-pre-note (plist-get formatter-info :text-after-pre-note))
	 (text-before-post-note (plist-get formatter-info :text-before-post-note))
	 ;; The first brace in the list is the outermost brace. i.e.,
	 ;; it gets applied while building the citation text.
	 (braces-list (reverse (plist-get formatter-info :braces)))
	 (allow-pre/post-notes-p (or text-after-pre-note text-before-post-note))
	 ;; Strip the text and apply pre/post-notes.
	 (text
	  (concat
	   ;; Add Pre-note and Text-after-pre-note, if needed.
	   (when allow-pre/post-notes-p
	     (if (org-string-nw-p pre-note) (concat pre-note text-after-pre-note) "" ))
	   ;; Add the Text (with stripping), if needed.
	   (if (not substring) text
	     (substring text (car substring) (cdr substring)))
	   ;; Add Post-note and Text-after-post-note, if needed.
	   (when allow-pre/post-notes-p
	     (if (org-string-nw-p post-note) (concat text-before-post-note post-note) "")))))
    (dolist (braces braces-list text)
      (setq text (concat
		  ;; Add Opening brace, if needed.
		  (or (car braces) "")
		  text
		  ;; Add Closing brace, if needed.
		  (or (cdr braces) ""))))))


(defun org-jabref--multicite (cite-key-text-alist pre-note post-note
						  formatter-info)
  (let* ((formatter (plist-get formatter-info :formatter))
	 (between-citations (plist-get formatter-info :between-citations)))
    (org-jabref--strip-and-add-pre/post-notes
     (mapconcat
      (lambda (cite-key-text)
	(let* ((cite-key (org-trim (car cite-key-text)))
	       (text (org-trim (cdr cite-key-text))))
	  (when (atom formatter) (setq formatter (list formatter)))
	  (funcall (car formatter) cite-key text pre-note post-note (cdr formatter))))
      cite-key-text-alist between-citations)
     pre-note post-note formatter-info)))

;;;; Bibliography

(defun org-jabref-bibliography (_bibliography _contents info)
  "Transcode a CITATION element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information.

Pass each CITE-KEY from CITATION in to `:in-text'
EXPORT-FORMAT.  Return the concatenated result, after adding some
separators."
  (let* ((export-format (org-jabref--get-export-format info :bibliography :jabref-format))
	 (formatter-name (org-jabref--get-export-format info :bibliography :formatter))
	 (formatter (org-jabref--get-citation-formatter info formatter-name))
	 (citation-cache (plist-get info :citation-cache))
	 (bib-alist (assoc-default export-format citation-cache))
	 (pre-note nil)
	 (post-note nil))
    (funcall (car formatter) bib-alist pre-note post-note (cdr formatter))))


;;;; Citation Reference

(defun org-jabref-citation (citation contents info)
  "Transcode a CITATION element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information.

Pass each CITE-KEY from CITATION in to `:in-text'
EXPORT-FORMAT.  Return the concatenated result, after adding some
separators."
  (let* ((pre-note (org-export-data (org-element-property :prefix citation) info))
	 (post-note (org-export-data (org-element-property :suffix citation) info))
	 (cite-keys (org-element-map citation 'citation-reference
		      (lambda (citation-reference) (org-element-property :key citation-reference))))
	 (export-formats (org-jabref--get-export-format info :in-text :jabref-format))
	 (formatter-name (org-jabref--get-export-format info :in-text :formatter))
	 (formatter (org-jabref--get-citation-formatter info formatter-name))
	 (citation-cache (plist-get info :citation-cache))
	 (cite-key-text-alist
	  (mapcar
	   (lambda (cite-key)
	     (let* ((export-format
		     (cond
		      ;; IN-TEXT uses only one export format.  Use it.
		      ((not (consp export-formats)) export-formats)
		      ;; IN-TEXT uses two export formats.  If the
		      ;; CITE-KEY is seen here for the first time, use
		      ;; the first of the export formats.
		      ((eq citation
			   (assoc-default cite-key
					  (plist-get info :citations-alist)))
		       (car export-formats))
		      ;; Otherwise, use the second of the export formats.
		      (t (cdr export-formats)))))
	       (cons cite-key
		     (assoc-default cite-key
				    (assoc-default export-format citation-cache)))))
	   cite-keys)))
    (or (when (cl-every (lambda (cite-key-text) (cdr cite-key-text)) cite-key-text-alist)
	  (funcall (car formatter) cite-key-text-alist pre-note post-note (cdr formatter)))
	(org-odt-citation citation contents info))))

;;;; Keyword

(defun org-jabref-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information.

If KEYWORD is a BIBLIOGRAPHY element, use
`org-jabref-bibliography'.  Otherwise, call the standard
transcoder for KEYWORD element."
  (let ((key (org-element-property :key keyword)))
    (cond
     ;; Handle BIBLIOGRAPHY code.
     ((string= key "BIBLIOGRAPHY")
      (org-jabref-bibliography keyword contents info))
     ;; Keyword is other than BIBLIOGRAPHY.  Use the stock transcoder.
     (t (let* ((backend (org-export-backend-name (plist-get info :back-end)))
	       (stock-backend (assoc-default backend org-jabref--stock-backends))
	       (transcoder (assoc-default 'keyword (org-export-backend-transcoders
						    stock-backend))))
	  (funcall transcoder keyword contents info))))))

;;; Jabref Citation Formatters (for ODT)

;;;; Bibliography

(defun org-jabref-odt-format-bibliography--no-hanging-indent
    (cite-key text pre-note post-note formatter-info)
  (org-jabref--strip-and-add-pre/post-notes
   (format
    "
<text:p text:style-name=\"%s\">
<text:bookmark-start text:name=\"cite-%s\"/>
%s
<text:bookmark-end text:name=\"cite-%s\"/>
</text:p>"
    "Bibliography_20_1" cite-key text cite-key)
   pre-note post-note formatter-info))


(defun org-jabref-odt-format-bibliography--use-hanging-indent
    (cite-key text pre-note post-note formatter-info)
  (org-jabref--strip-and-add-pre/post-notes
   (format
    "
<text:p text:style-name=\"%s\">
<text:bookmark-start text:name=\"cite-%s\"/>
%s
<text:bookmark-end text:name=\"cite-%s\"/>
</text:p>"
    "Bibliography" cite-key text cite-key)
   pre-note post-note formatter-info))

;;;; Citation reference :: Plain Text (for use with Chicago::Author-Date etc.)

(defun org-jabref-odt-format-citation-as-link
    (cite-key text pre-note post-note formatter-info)
  (format "<text:a xlink:type=\"simple\" text:style-name=\"Index_20_Link\"  xlink:href=\"#cite-%s\">%s</text:a>"
	  cite-key
	  ;; Strip the surrounding braces.
	  (org-jabref--strip-and-add-pre/post-notes
	   text pre-note post-note formatter-info)))

;;;; Citation reference: Footnote (for use with Chicago::Full note etc))

(defun org-jabref-odt-format-citation-as-footnote-definition
    (_cite-key text pre-note post-note formatter-info)
  (org-odt--format-footnote-definition
   nil (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
	       "Footnote"
	       (org-jabref--strip-and-add-pre/post-notes
		text pre-note post-note formatter-info))))

;;; Interactive function

(provide 'ox-jabref)

;;; ox-jabref.el ends here
