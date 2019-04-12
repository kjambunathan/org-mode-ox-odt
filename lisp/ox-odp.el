;;; ox-odp.el --- OpenDocument Presentation Exporter for Org Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jambunathan K <kjambunathan at gmail dot com>

;; Author: Jambunathan K <kjambunathan at gmail dot com>
;; Maintainer: Jambunathan K <kjambunathan at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: https://github.com/kjambunathan/org-mode-ox-odt

;; Package-Requires: ((org "9.2.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'ox-odt)

;;; Define Back-End

(org-export-define-derived-backend 'odp 'odt
  ;; :export-block "ODT"			; FIXME
  :menu-entry
  '(?o "Export to ODT"
       ((?p "As ODP file" org-odt-export-to-odp)
	(?P "As ODP file and open"
	    (lambda (a s v b)
	      (if a (org-odt-export-to-odp t s v)
		(org-open-file (org-odt-export-to-odp nil s v) 'system))))))
  :options-alist
  '((:headline-levels nil "H" org-odp-frame-level)
    (:description "DESCRIPTION" nil nil parse)
    (:keywords "KEYWORDS" nil nil parse)
    (:subtitle "SUBTITLE" nil nil parse)
    ;; Redefine regular option.
    (:with-latex nil "tex" org-odt-with-latex)
    ;; ODT-specific keywords
    ;; Keywords that affect styles.xml
    (:odt-styles-file "ODT_STYLES_FILE" nil (expand-file-name "OrgOdpStyles.xml" org-odt-styles-dir) t)
    (:odt-extra-images "ODT_EXTRA_IMAGES" nil nil split)
    (:odt-extra-styles "ODT_EXTRA_STYLES" nil nil newline)
    (:odt-extra-automatic-styles "ODT_EXTRA_AUTOMATIC_STYLES" nil nil newline)
    (:odt-master-styles "ODT_MASTER_STYLES" nil nil newline)
    ;; Keywords that affect content.xml
    (:odt-content-template-file "ODT_CONTENT_TEMPLATE_FILE" nil (expand-file-name "OrgOdpContentTemplate.xml" org-odt-styles-dir))
    (:odt-automatic-styles "ODT_AUTOMATIC_STYLES" nil nil newline)
    (:odt-display-outline-level "ODT_DISPLAY_OUTLINE_LEVEL" nil (number-to-string org-odt-display-outline-level))
    ;; Org has no *native* support Bibliographies and Citations .  So,
    ;; strictly speaking, the following "BIB_FILE" keyword is ODT only
    ;; and should be prefixed with "ODT_".  However, since the
    ;; Bibliography file option makes sense for *all* backends,
    ;; skipping the "ODT_" prefix, makes much sense.
    (:bib-file "BIB_FILE" nil nil t)
    ;; Other variables.
    (:odt-fontify-srcblocks nil nil org-odt-fontify-srcblocks)
    (:odt-format-drawer-function nil nil org-odt-format-drawer-function)
    (:odt-format-headline-function nil nil org-odp-format-headline-function)
    (:odt-format-inlinetask-function nil nil org-odt-format-inlinetask-function)
    (:odt-inline-formula-rules nil nil org-odt-inline-formula-rules)
    (:odt-inline-image-rules nil nil org-odt-inline-image-rules)
    (:odt-pixels-per-inch nil nil org-odt-pixels-per-inch)
    (:odt-styles-file nil nil org-odp-styles-file)
    (:odt-table-styles nil nil org-odt-table-styles)
    (:odt-use-date-fields nil nil org-odt-use-date-fields)
    ;; Variables that are used per-session of export.
    ;; Running counters for various objects.  Use this to generate
    ;; automatic names and style-names for those objects.
    (:odt-object-counters nil nil nil)
    (:odt-manifest-file-entries nil nil nil)
    ;; Initialize temporary workarea.  All files that end up in the
    ;; exported document are created here.
    (:odt-zip-dir nil nil (let ((dir (file-name-as-directory (make-temp-file "odt-" t))))
			    (prog1 dir (message  "ODT Zip Dir is %s" dir))))
    (:odt-hfy-user-sheet-assoc nil nil nil))
  :translate-alist '((headline . org-odp-headline)
		     (inner-template . org-odp-inner-template)
		     (section . org-odp-section)
		     ;; (template . org-odp-template)
		     ))

;;; Dependencies

;;; Hooks

;;; User Configuration Variables

(defgroup org-export-odp nil
  "Options for exporting Org mode files to ODP."
  :tag "Org Export ODP"
  :group 'org-export)

(defcustom org-odp-frame-level 1
  "The level at which headlines become a page of presentation."
  :group 'org-export-odp
  :type 'integer)

;;;; Document styles

(defcustom org-odp-content-template-file nil
  "Template file for \"content.xml\".
The exporter embeds the exported content just before
\"</office:text>\" element.

If unspecified, the file named \"OrgOdtContentTemplate.xml\"
under `org-odt-styles-dir' is used."
  :type '(choice (const nil)
		 (file))
  :group 'org-export-odp)

(defcustom org-odp-styles-file nil
  "Default styles file for use with ODT export.
Valid values are one of:
1. nil
2. path to a styles.xml file
3. path to a *.odt or a *.ott file
4. list of the form (ODT-OR-OTT-FILE (FILE-MEMBER-1 FILE-MEMBER-2
...))

In case of option 1, an in-built styles.xml is used. See
`org-odt-styles-dir' for more information.

In case of option 3, the specified file is unzipped and the
styles.xml embedded therein is used.

In case of option 4, the specified ODT-OR-OTT-FILE is unzipped
and FILE-MEMBER-1, FILE-MEMBER-2 etc are copied in to the
generated odt file.  Use relative path for specifying the
FILE-MEMBERS.  styles.xml must be specified as one of the
FILE-MEMBERS.

Use options 1, 2 or 3 only if styles.xml alone suffices for
achieving the desired formatting.  Use option 4, if the styles.xml
references additional files like header and footer images for
achieving the desired formatting.

Use \"#+ODT_STYLES_FILE: ...\" directive to set this variable on
a per-file basis.  For example,

#+ODT_STYLES_FILE: \"/path/to/styles.xml\" or
#+ODT_STYLES_FILE: (\"/path/to/file.ott\" (\"styles.xml\" \"image/hdr.png\"))."
  :group 'org-export-odp
  :version "24.1"
  :type
  '(choice
    (const :tag "Factory settings" nil)
    (file :must-match t :tag "styles.xml")
    (file :must-match t :tag "ODT or OTT file")
    (list :tag "ODT or OTT file + Members"
	  (file :must-match t :tag "ODF Text or Text Template file")
	  (cons :tag "Members"
		(file :tag "	Member" "styles.xml")
		(repeat (file :tag "Member"))))))

;;;; Headline

(defcustom org-odp-format-headline-function
  'org-odp-format-headline-default-function
  "Function to format headline text.

This function will be called with 5 arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags string, separated with colons (string or nil).

The function result will be used as headline text."
  :group 'org-export-odp
  :type 'function)

;;; Internal functions

(defun org-odp--get-layouts ()
  (cl-loop for (layout . rest) in org-odp-layout-properties collect layout))

(defun org-odp--get-layouts-with-n-frames (n)
  (cl-loop for (layout . rest) in org-odp-layout-properties
	   when (= (length (plist-get rest :cellname-and-dims)) n)
	   collect layout))

(defun org-odt--get-dimensions-of-nth-cell (layout n)
  (apply
   'org-odt--format-size-and-position
   (cdr (nth n (plist-get (cdr (assoc-string layout org-odp-layout-properties)) :cellname-and-dims )))))

(defun org-odt--get-dimensions (layout cell)
  (apply
   'org-odt--format-size-and-position
   (assoc-default cell
		  (plist-get (cdr (assoc-string
				   layout org-odp-layout-properties)) :cellname-and-dims ))))

;; Frame size for Presentation Notes
;; "svg:width=\"16.799cm\" svg:height=\"13.364cm\" svg:x=\"2.1cm\" svg:y=\"14.107cm\"

(defun org-odt--format-size-and-position (width height x y)
  (format "svg:width=\"%0.2fcm\" svg:height=\"%0.2fcm\" svg:x=\"%0.2fcm\" svg:y=\"%0.2fcm\""
	  width height x y))

(defvar org-odp-layout-properties
  '(
    ;; Row 1
    ("Blank Slide"
     :aid
     "
+-----------------------+
|                       |
|                       |
|                       |
|                       |
|           A1          |
|                       |
|                       |
|                       |
|                       |
|                       |
+-----------------------+
"
     )
    ("Title Slide"
     :aid
     "
+-----------------------+
|          A1           |
+-----------------------+
|                       |
|                       |
|                       |
|          A2           |
|                       |
|                       |
|                       |
+-----------------------+
"
     :cellname-and-dims
     (
      ("A1" . (25.199 2.629 1.4 0.628))
      ("A2" . (25.199 9.134 1.4 3.685))
      ))
    ("Title, Content"
     :aid
     "
+-----------------------+
|          A1           |
+-----------------------+
|                       |
|                       |
|                       |
|          A2           |
|                       |
|                       |
|                       |
+-----------------------+
 "
     :cellname-and-dims
     (
      ("A1" . (25.199 2.629 1.4 0.628))
      ("A2" . (25.199 9.134 1.4 3.685))
      ))
    ("Title and 2 Content"
     :aid "
+-----------------------+
|         A1:B1         |
+-----------+-----------+
|           |           |
|           |           |
|           |           |
|    A2     |     B2    |
|           |           |
|           |           |
|           |           |
+-----------+-----------+ "
     :cellname-and-dims (("A1:B1" . (25.199 2.629 1.4 0.628))
			 ("A2" . (12.297 9.134 1.4 3.685))
			 ("B2" . (12.297 9.134 14.313 3.685))))
    ;; Row 2
    ("Title Only"
     :aid
     "
+-----------------------+
|           A1          |
+-----------------------+
|                       |
|                       |
|                       |
|           A2          |
|                       |
|                       |
|                       |
+-----------------------+
"
     :cellname-and-dims
     (
      ("A1" . (25.199 2.629 1.4 0.628)))
     )
    ("Centered Text"
     :aid
     "
     +-----------------------+
     |                       |
     |                       |
     |                       |
     |                       |
     |          A1           |
     |                       |
     |                       |
     |                       |
     |                       |
     |                       |
     +-----------------------+
"
     :cellname-and-dims
     (
      ("A1" . (25.199 12.19 1.4 0.628)))
     )
    ("Title, 2 Content and Content"
     :aid
     "
     +-----------------------+
     |          A1:B1        |
     +-----------+-----------+
     |           |           |
     |    A2     |           |
     |           |           |
     +-----------+   B2:B3   |
     |           |           |
     |    A3     |           |
     |           |           |
     +-----------+-----------+
     "
     :cellname-and-dims
     (
      ("A1:B1" . (25.199 2.629 1.4 0.628))
      ("A2" . (12.297 4.356 1.4 3.685))
      ("B2:B3" . (12.297 9.134 14.313 3.685))
      ("A3" . (12.297 4.356 1.4 8.456))
      ))
    ("Title, Content and 2 Content"
     :aid
     "
     +-----------------------+
     |          A1:B1        |
     +-----------+-----------+
     |           |           |
     |           |     B2    |
     |           |           |
     |  A2:A3    +-----------+
     |           |           |
     |           |     B3    |
     |           |           |
     +-----------+-----------+
     "
     :cellname-and-dims
     (
      ("A1:B1" . (25.199 2.629 1.4 0.628))
      ("A2:A3" . (12.297 9.134 1.4 3.685))
      ("B2" . (12.297 4.356 14.313 3.685))
      ("B3" . (12.297 4.356 14.313 8.456))
      ))
    ;; Row 3
    ("Title, 2 Content over Content"
     :aid
     "
     +-----------------------+
     |          A1:B1        |
     +-----------+-----------+
     |           |           |
     |    A2     |     B2    |
     |           |           |
     +-----------+-----------+
     |                       |
     |          A3:B3        |
     |                       |
     +-----------------------+
     "
     :cellname-and-dims
     (
      ("A1:B1" . (25.199 2.629 1.4 0.628))
      ("A2" . (12.297 4.356 1.4 3.685))
      ("B2" . (12.297 4.356 14.313 3.685))
      ("A3:B3" . (25.199 4.356 1.4 8.456))
      ))
    ("Title, Content over Content"
     :aid
     "
     +-----------------------+
     |           A1          |
     +-----------------------+
     |                       |
     |           A2          |
     |                       |
     +-----------------------+
     |                       |
     |           A3          |
     |                       |
     +-----------------------+
     "
     :cellname-and-dims
     (
      ("A1" . (25.199 2.629 1.4 0.628))
      ("A2" . (25.199 4.356 1.4 3.685))
      ("A3" . (25.199 4.356 1.4 8.456))
      ))
    ("Title, 4 Content"
     :aid
     "
     +-----------------------+
     |          A1:B1        |
     +-----------+-----------+
     |           |           |
     |    A2     |     B2    |
     |           |           |
     +-----------+-----------+
     |           |           |
     |    A3     |     B3    |
     |           |           |
     +-----------+-----------+
     "
     :cellname-and-dims
     (
      ("A1:B1" . (25.199 2.629 1.4 0.628))
      ("A2" . (12.297 4.356 1.4 3.685))
      ("B2" . (12.297 4.356 14.313 3.685))
      ("A3" . (12.297 4.356 1.4 8.456))
      ("B3" . (12.297 4.356 14.313 8.456))
      ))
    ("Title, 6 Content"
     :aid
     "
+-----------------------+
|          A1:C1        |
+-------+-------+-------+
|       |       |       |
|   A2  |  B2   |   C2  |
|       |       |       |
+-------+-------+-------+
|       |       |       |
|   A3  |  B3   |   C3  |
|       |       |       |
+--  ---+--- ---+--- ---+
"
     :cellname-and-dims
     (
      ("A1:C1" . (25.199 2.629 1.4 0.628))
      ("A2" . (8.113 4.356 1.4 3.685))
      ("B2" . (8.113 4.356 9.92 3.685))
      ("C2" . (8.113 4.356 18.439 3.685))
      ("A3" . (8.113 4.356 1.4 8.456))
      ("B3" . (8.113 4.356 9.92 8.456))
      ("C3" . (8.113 4.356 18.439 8.456))
      ))))

(defun org-odp--draw:page (contents &optional dont-close-page
				    masterpage  style layout)
  (format "
<draw:page draw:master-page-name=\"%s\" %s>
%s
%s
"
	  (or masterpage "Default")
	  (mapconcat #'identity
		     (list
		      (when style	; style belonging to family `drawing-page'
			(format " draw:style-name=\"%s\"" style))
		      (when layout
			(format "presentation:presentation-page-layout-name=\"%s\""
				layout)))

		     " ")
	  contents
	  (or (when dont-close-page "")
	      "</draw:page>")))

(defun org-odp--draw:frame  (contents presentation-class presentation-style dimensions &optional text-style)
  (format "
<draw:frame presentation:class=\"%s\" presentation:style-name=\"%s\" draw:layer=\"layout\" %s>
%s
</draw:frame>"
	  presentation-class
	  presentation-style
	  (concat
	   (or (when text-style
		 (format "draw:text-style-name=\"%s\"" text-style))
	       "")
	   " "
	   dimensions)
	  contents))

(defun org-odp--draw:text-box (contents &optional extra)
  (format "\n<draw:text-box%s>\n%s\n</draw:text-box>"
	  (or extra "")
	  contents))

;;; Template

;;;; Inner Template

(defun org-odp-inner-template (contents info)
  "Return body of document string after ODT conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ( ;; `org-display-custom-times' should be accessed right
	 ;; within the context of the Org buffer.  So obtain its
	 ;; value before moving on to temp-buffer context down below.
	 (_custom-time-fmts
	  (if org-display-custom-times
	      (cons (substring (car org-time-stamp-custom-formats) 1 -1)
		    (substring (cdr org-time-stamp-custom-formats) 1 -1))
	    '("%Y-%M-%d %a" . "%Y-%M-%d %a %H:%M"))))
    (with-temp-buffer
      (insert-file-contents
       (let ((content-template-file
	      (let ((file (plist-get info :odt-content-template-file)))
		(cond
		 ((or (not file) (string= file ""))
		  (expand-file-name "OrgOdpContentTemplate.xml" org-odt-styles-dir))
		 ((file-name-absolute-p file) file)
		 (t (expand-file-name
		     file (file-name-directory (plist-get info :input-file))))))))
	 (message "ox-odt: Content template file is %s" content-template-file)
	 content-template-file))

      ;; Update display level.
      ;; - Remove existing sequence decls.  Also position the cursor.
      (goto-char (point-min))
      (when (re-search-forward "<office:presentation>" nil t)
	(delete-region (match-end 0)
		       (save-excursion
			 (re-search-forward "</office:presentation>" nil nil)
			 (match-beginning 0))))

      ;; Preamble - Title, Author, Date etc.
      (insert
       (let* ((title (and (plist-get info :with-title)
      			  (org-export-data (plist-get info :title) info)))
      	      (author (and (plist-get info :with-author)
      			   (let ((auth (plist-get info :author)))
      			     (and auth (org-export-data auth info)))))
      	      (email (plist-get info :email))
      	      ;; Switch on or off above vars based on user settings
      	      (author (and (plist-get info :with-author) (or author email)))
      	      (_email (and (plist-get info :with-email) email)))

	 (org-odp--draw:page
	  (org-odp--draw:frame
	   (org-odp--draw:text-box
	    (cl-loop for c in (list title author) concat
		     (when (org-string-nw-p c)
		       (format "\n<text:p>%s</text:p>" c))))
	   "title"			; presentation-class
	   "Default-title" 		; presentation-style
	   (org-odt--get-dimensions "Title, Content" "A1")))))
      ;; Contents.
      (insert "\n<!-- begin -->\n" contents "\n<!-- end -->\n")
      ;; Prettify buffer contents, if needed.
      (when org-odt-prettify-xml
	(nxml-mode)
	(indent-region (point-min) (point-max)))
      ;; Cleanup, when export is body-only.
      (when (memq 'body-only (plist-get info :export-options))
	(org-odt-cleanup-xml-buffers nil nil info))
      ;; Return contents.
      (buffer-substring-no-properties (point-min) (point-max)))))



;;; Transcode Functions

;;;; Headline

(defun org-odp-format-headline-default-function
    (todo _todo-type priority text tags)
  "Default format function for a headline.
See `org-odp-format-headline-function' for details."
  (concat
   ;; Todo.
   (when todo
     (let ((style (if (member todo org-done-keywords) "OrgDone" "OrgTodo")))
       (format "<text:span text:style-name=\"%s\">%s</text:span> "
	       style todo)))
   (when priority
     (let* ((style (format "OrgPriority-%s" priority))
	    (priority (format "[#%c]" priority)))
       (format "<text:span text:style-name=\"%s\">%s</text:span> "
	       style priority)))
   ;; Title.
   text
   ;; Tags.
   (when tags
     (concat
      "<text:tab/>"
      (format "<text:span text:style-name=\"%s\">[%s]</text:span>"
	      "OrgTags" (mapconcat
			 (lambda (tag)
			   (format
			    "<text:span text:style-name=\"%s\">%s</text:span>"
			    "OrgTag" tag)) tags " : "))))))

(defun org-odp-format-headline--wrap (headline backend info
					       &optional format-function)
  "Transcode a HEADLINE element using BACKEND.
INFO is a plist holding contextual information."
  (setq backend (or backend (plist-get info :back-end)))
  (let* ((level (+ (org-export-get-relative-level headline info)))
	 (headline-number (org-export-get-headline-number headline info))
	 (section-number (and (org-export-numbered-headline-p headline info)
			      (mapconcat 'number-to-string
					 headline-number ".")))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo
			   (org-export-data-with-backend todo backend info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (text (org-export-data-with-backend
		(org-element-property :title headline) backend info))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags headline info)))
	 (headline-label (org-export-get-reference headline info))
	 (format-function
	  (if (functionp format-function) format-function
	    (cl-function
	     (lambda (todo todo-type priority text tags
			   &key _level _section-number _headline-label)
	       (funcall (plist-get info :odt-format-headline-function)
			todo todo-type priority text tags))))))
    (funcall format-function
	     todo todo-type  priority text tags
	     :level level :section-number section-number
	     :headline-label headline-label)))

(defun org-odt-export--get-num-frames (headline _info)
  (1+ (length (cl-loop for el in (org-element-contents headline)
		       when (eq (org-element-type el) 'headline)
		       collect el))))

(defun org-odt-export--get-num-frames-of-parent (headline info)
  (org-odt-export--get-num-frames (org-export-get-parent headline) info))

(defun org-odp-headline (headline contents info)
  "Transcode a HEADLINE element from Org to ODT.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  ;; Case 1: This is a footnote section: ignore it.
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
	   (frame-level (plist-get info :headline-levels))
	   (_text (org-export-data (org-element-property :title headline) info))
	   ;; Create the headline text.
	   (full-text (org-odp-format-headline--wrap headline nil info)))

      (message "Handling node %s" full-text)
      (cond
       ((< level frame-level)
	(message "<")
	(concat
	 (let* ((layout ;; "Title Slide"
		 (car (org-odp--get-layouts-with-n-frames 2))))
	   (org-odp--draw:page
	    (concat
	     (org-odp--draw:frame
	      (org-odp--draw:text-box
	       (format "\n<text:p>%s</text:p>" full-text))
	      "title"			; presentation-class
	      "Default-title" 		; presentation-style
	      ;; (org-odt--get-dimensions layout "A1")
	      (org-odt--get-dimensions-of-nth-cell layout 0)
	      )
	     (let ((section-contents (cl-loop for c in (org-element-contents headline)
					      when (eq (org-element-type c) 'section)
					      return (when (org-element-contents c)
						       (org-export-data (org-element-contents c) info)))))
	       (when (org-string-nw-p section-contents)
		 (org-odp--draw:frame
		  (org-odp--draw:text-box section-contents)
		  "subtitle"			; presentation-class
		  "Default-subtitle" 		; presentation-style
		  ;; (org-odt--get-dimensions layout "A2")
		  (org-odt--get-dimensions-of-nth-cell layout 1)))))))
	 contents))
       ((= level frame-level)
	(let* ((layout ;; "Title and 2 Content"
		(car (org-odp--get-layouts-with-n-frames
		      (org-odt-export--get-num-frames headline info))))

	       (_cellname "A1:B1"))
	  (message "=")
	  (org-odp--draw:page
	   (concat
	    (org-odp--draw:frame
	     (org-odp--draw:text-box
	      (format "\n<text:p>%s</text:p>" full-text))
	     "title"			; presentation-class
	     "Default-title" 		; presentation-style
	     (org-odt--get-dimensions-of-nth-cell layout 0)
	     ;; (org-odt--get-dimensions layout cellname)
	     )
	    contents))))
       ((> level frame-level)
	(cond
	 ((= frame-level (org-export-get-relative-level (org-export-get-parent-headline headline) info))
	  (let* ((layout ;; "Title and 2 Content"
		  (car (org-odp--get-layouts-with-n-frames
			(org-odt-export--get-num-frames
			 (org-export-get-parent-headline headline) info))))
		 (n-elders (let ((elders (cl-loop for el in (org-export-get-previous-element headline info t)
						  when (eq (org-element-type headline) 'headline)
						  collect el)))
			     (if elders (length elders) 0)))
		 ;; (cellname (format "%c2" (+ ?A n-elders)))

		 )
	    (message "> %s" (list layout (1+ n-elders)))
	    (org-odp--draw:frame
	     (org-odp--draw:text-box
	      (concat
	       (format "\n<text:p>%s</text:p>" full-text)
	       contents))
	     "subtitle"			; presentation-class
	     "Default-subtitle" 		; presentation-style
	     ;; (org-odt--get-dimensions layout cellname)
	     (org-odt--get-dimensions-of-nth-cell layout (1+ n-elders)))))
	 (t (org-odt-headline headline contents info))))))))

;;;; Section

(defun org-odp-section (section contents info)
  "Transcode a SECTION element from Org to ODT.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let ((parent (org-export-get-parent section)))
    (unless (and (eq (org-element-type parent) 'headline)
		 (< (org-export-get-relative-level parent info)
		    (plist-get info :headline-levels)))
      contents)))

;;; Interactive functions

;;;; Export to OpenDocument master

;;;###autoload
(defun org-odt-export-to-odp
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a ODP or a OpenDocument XML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, write a OpenDocument
XML file holding just the contents.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

The function returns a file name in any one of the BACKEND
format, `org-odt-preferred-output-format' or XML format."
  (interactive)
  (let* ((backend 'odp))
    (org-odt-export-to-odt-backend backend async subtreep
				   visible-only body-only ext-plist)))

(cl-pushnew "odp" org-odt-supported-file-types)

(provide 'ox-odp)

;;; ox-odt.el ends here
