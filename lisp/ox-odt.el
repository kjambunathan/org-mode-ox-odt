;;; ox-odt.el --- OpenDocument Text Exporter for Org Mode -*- lexical-binding: t; coding: utf-8-emacs; -*-

;; Copyright (C) 2010-2022 Jambunathan K <kjambunathan at gmail dot com>

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
(eval-when-compile
  (require 'rx)
  (require 'table nil 'noerror))
(require 'format-spec)
(require 'xml)
(require 'ox)
(require 'ox-ascii)
(require 'org-compat)

;;; Useful Functions

;;;; XML to Lisp & vice versa

(defun org-odt--xml-to-lisp (&optional xml)
  (setq xml (or xml (buffer-substring-no-properties (region-beginning) (region-end))))
  (with-temp-buffer
    (insert xml)
    (libxml-parse-xml-region (point-min) (point-max))))

(defun org-odt--lisp-to-xml (element &optional depth prettify)
  (let* ((newline (if prettify "\n" ""))
	 (print-attributes
	  (lambda (attributes)
	    (mapconcat #'identity (cl-loop for (attribute . value) in attributes collect
					   (format "%s=\"%s\"" attribute value))
		       " "))))
    (setq depth (or depth 0))
    (cond
     ((stringp element)
      element)
     ((symbolp (car element))
      (let* ((name (car element))
	     (attributes (cadr element))
	     (contents (cddr element)))
	(let ((prefix (if prettify (make-string depth ? ) "")))
	  (cond
	   ((null contents)
	    (format "%s%s<%s %s/>"
		    newline
		    prefix name (funcall print-attributes attributes)))
	   (t
	    (format "%s%s<%s %s>%s%s%s</%s>"
		    newline
		    prefix
		    name
		    (funcall print-attributes attributes)
		    (if (stringp contents) contents
		      ;; (print-element contents (1+ depth))
		      (org-odt--lisp-to-xml contents (1+ depth) prettify))
		    newline
		    prefix
		    name))))))
     (t
      (mapconcat #'identity
		 (cl-loop for el in element collect
			  ;; (print-element el (1+ depth))
			  (org-odt--lisp-to-xml el (1+ depth) prettify))
		 "")))))


;;; Define Back-End

(org-export-define-backend 'odt
  '((bold . org-odt-bold)
    (center-block . org-odt-center-block)
    (clock . org-odt-clock)
    (code . org-odt-code)
    (drawer . org-odt-drawer)
    (dynamic-block . org-odt-dynamic-block)
    (entity . org-odt-entity)
    (example-block . org-odt-example-block)
    (export-block . org-odt-export-block)
    (export-snippet . org-odt-export-snippet)
    (fixed-width . org-odt-fixed-width)
    (footnote-definition . org-odt-footnote-definition)
    (footnote-reference . org-odt-footnote-reference)
    (headline . org-odt-headline)
    (horizontal-rule . org-odt-horizontal-rule)
    (inline-src-block . org-odt-inline-src-block)
    (inlinetask . org-odt-inlinetask)
    (inner-template . org-odt-inner-template)
    (italic . org-odt-italic)
    (item . org-odt-item)
    (keyword . org-odt-keyword)
    (latex-environment . org-odt-latex-environment)
    (latex-fragment . org-odt-latex-fragment)
    (line-break . org-odt-line-break)
    (link . org-odt-link)
    (node-property . org-odt-node-property)
    (paragraph . org-odt-paragraph)
    (plain-list . org-odt-plain-list)
    (plain-text . org-odt-plain-text)
    (planning . org-odt-planning)
    (property-drawer . org-odt-property-drawer)
    (quote-block . org-odt-quote-block)
    (radio-target . org-odt-radio-target)
    (section . org-odt-section)
    (special-block . org-odt-special-block)
    (src-block . org-odt-src-block)
    (statistics-cookie . org-odt-statistics-cookie)
    (strike-through . org-odt-strike-through)
    (subscript . org-odt-subscript)
    (superscript . org-odt-superscript)
    (table . org-odt-table)
    (table-cell . org-odt-table-cell)
    (table-row . org-odt-table-row)
    (target . org-odt-target)
    (template . org-odt-template)
    (timestamp . org-odt-timestamp)
    (underline . org-odt-underline)
    (verbatim . org-odt-verbatim)
    (verse-block . org-odt-verse-block)
    ;; (citation . org-odt-citation)
    )
  :filters-alist '((:filter-parse-tree
		    . (org-odt--translate-clickable-images
                       org-odt--translate-latex-fragments
		       org-odt--translate-description-lists ; Dummy symbol
		       org-odt--translate-list-tables
		       org-odt--transclude-sole-footnote-references-in-a-table)))
  :menu-entry
  '(?o "Export to ODT"
       ((?o "As ODT file" org-odt-export-to-odt)
	(?O "As ODT file and open"
	    (lambda (a s v b)
	      (if a (org-odt-export-to-odt t s v b)
		(org-open-file (org-odt-export-to-odt a s v b) 'system))))
	(?x "As XML buffer" org-odt-export-as-odt)))
  :options-alist
  '((:description "DESCRIPTION" nil nil parse)
    (:keywords "KEYWORDS" nil nil parse)
    (:subtitle "SUBTITLE" nil nil parse)
    ;; Redefine regular option.
    (:with-latex nil "tex" org-odt-with-latex)
    (:odt-math-syntax "ODT_MATH_SYNTAX" nil org-odt-math-syntax t)
    ;; ODT-specific keywords
    ;; Keywords that affect styles.xml
    (:odt-preferred-output-format "ODT_PREFERRED_OUTPUT_FORMAT" nil org-odt-preferred-output-format t)
    (:odt-styles-file "ODT_STYLES_FILE" nil org-odt-styles-file t)
    (:odt-extra-images "ODT_EXTRA_IMAGES" nil nil split)
    (:odt-extra-styles "ODT_EXTRA_STYLES" nil org-odt-extra-styles newline)
    (:odt-extra-automatic-styles "ODT_EXTRA_AUTOMATIC_STYLES" nil org-odt-extra-automatic-styles newline)
    (:odt-master-styles "ODT_MASTER_STYLES" nil org-odt-master-styles newline)
    ;; Keywords that affect content.xml
    (:odt-content-template-file "ODT_CONTENT_TEMPLATE_FILE" nil org-odt-content-template-file)
    (:odt-automatic-styles "ODT_AUTOMATIC_STYLES" nil nil newline)
    (:odt-display-outline-level "ODT_DISPLAY_OUTLINE_LEVEL" nil (number-to-string org-odt-display-outline-level))
    (:odt-app "ODT_APP" nil nil t)
    ;; Keywords that affect meta.xml
    (:odt-document-properties "ODT_DOCUMENT_PROPERTIES" nil nil split)
    (:odt-extra-meta "ODT_EXTRA_META" nil nil newline)
    ;; Keywords for debugging
    ;; - Pretty print XML files
    ;;     Value can either be empty, or one of `tidy' or `tidy+indent'.
    ;;     See `org-odt-prettify-xml-buffer'.
    (:odt-prettify-xml "ODT_PRETTIFY_XML" nil org-odt-prettify-xml t)
    ;; - Check for 'well-formed'-ness of a OpenDocument file
    ;;     Value can either be empty, or one of `noabort' or `abort'.
    (:odt-validate "ODT_VALIDATE" nil org-odt-validate t)

    ;; Keys associated with Endnotes
    (:odt-endnote-anchor-format nil nil org-odt-endnote-anchor-format t)
    (:odt-endnote-braces nil nil org-odt-endnote-braces t)
    (:odt-endnote-regexp "ODT_ENDNOTE_REGEXP" nil org-odt-endnote-regexp t)
    (:odt-endnote-separator nil nil org-odt-endnote-separator t)

    ;; Keys associated with Footnotes
    (:odt-footnote-anchor-format nil nil org-odt-footnote-anchor-format t)
    (:odt-footnote-braces nil nil org-odt-footnote-braces t)
    (:odt-footnote-separator nil nil org-odt-footnote-separator t)

    ;; Other variables.
    (:odt-fontify-srcblocks nil nil org-odt-fontify-srcblocks)
    (:odt-format-drawer-function nil nil org-odt-format-drawer-function)
    (:odt-format-headline-function nil nil org-odt-format-headline-function)
    (:odt-format-inlinetask-function nil nil org-odt-format-inlinetask-function)
    (:odt-inline-formula-rules nil nil org-odt-inline-formula-rules)
    (:odt-inline-image-rules nil nil org-odt-inline-image-rules)
    (:odt-link-org-files-as-odt nil nil org-odt-link-org-files-as-odt)
    (:odt-pixels-per-inch nil nil org-odt-pixels-per-inch)
    (:odt-table-styles nil nil org-odt-table-styles)
    (:odt-use-date-fields nil nil org-odt-use-date-fields)
    (:odt-indices nil nil org-odt-indices)
    ;; Variables that are used per-session of export.
    ;; Running counters for various objects.  Use this to generate
    ;; automatic names and style-names for those objects.
    (:odt-object-counters nil nil nil)
    (:odt-manifest-file-entries nil nil nil)
    ;; Initialize temporary workarea.  All files that end up in the
    ;; exported document are created here.
    (:odt-zip-dir nil nil (let ((dir (file-name-as-directory (make-temp-file "odt-" t))))
			    (prog1 dir (message "ODT Zip Dir is %s" dir))))
    (:odt-hfy-user-sheet-assoc nil nil nil)))


;;; Dependencies

;;; Hooks

;;; Function and Dynamically Scoped Variables Declarations

(declare-function hfy-face-to-style "htmlfontify" (fn))
(declare-function hfy-face-or-def-to-name "htmlfontify" (fn))
(declare-function archive-zip-extract "arc-mode" (archive name))
(declare-function org-create-math-formula "org" (latex-frag &optional mathml-file))
(declare-function browse-url-file-url "browse-url" (file))

(defvar nxml-auto-insert-xml-declaration-flag) ; nxml-mode.el
(defvar archive-zip-extract)		       ; arc-mode.el
(defvar hfy-end-span-handler)		       ; htmlfontify.el
(defvar hfy-begin-span-handler)		       ; htmlfontify.el
(defvar hfy-face-to-css)		       ; htmlfontify.el
(defvar hfy-html-quote-map)		       ; htmlfontify.el
(defvar hfy-html-quote-regex)		       ; htmlfontify.el



;;; Internal Variables

(defconst org-odt-lib-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Location of ODT exporter.
Use this to infer values of `org-odt-styles-dir' and
`org-odt-schema-dir'.")

(defconst org-odt-special-string-regexps
  '(("\\\\-" . "&#x00ad;\\1")		; shy
    ("---\\([^-]\\)" . "&#x2014;\\1")	; mdash
    ("--\\([^-]\\)" . "&#x2013;\\1")	; ndash
    ("\\.\\.\\." . "&#x2026;"))		; hellip
  "Regular expressions for special string conversion.")

(defconst org-odt-schema-dir-list
  (list (expand-file-name "../etc/schema/" org-odt-lib-dir) ; git
	(expand-file-name "./etc/schema/" org-odt-lib-dir)  ; elpa
	)
  "List of directories to search for OpenDocument schema files.
Use this list to set the default value of `org-odt-schema-dir'.
Directories in this list are determined based on the value of
`org-odt-lib-dir'.")

(defconst org-odt-styles-dir-list
  (list
   (expand-file-name "../etc/styles/" org-odt-lib-dir) ; git
   (expand-file-name "./etc/styles/" org-odt-lib-dir)  ; elpa
   (expand-file-name "./org/" data-directory)	       ; system
   )
  "List of directories to search for OpenDocument styles files.
See `org-odt-styles-dir'.  Directories in this list are
determined based on the value of `org-odt-lib-dir'.")

(defconst org-odt-styles-dir
  (cl-loop for styles-dir in org-odt-styles-dir-list
	   when (cl-every (lambda (file-name)
			    (file-readable-p (expand-file-name file-name styles-dir)))
			  '("OrgOdtContentTemplate.xml" "OrgOdtStyles.xml"))
	   return styles-dir)
  "Directory that holds auxiliary XML files used by the ODT exporter.

This directory contains the following XML files -
 \"OrgOdtStyles.xml\" and \"OrgOdtContentTemplate.xml\".  These
 XML files are used as the default values of
 `org-odt-styles-file' and `org-odt-content-template-file'.

The default value of this variable varies depending on the
version of org in use and is initialized from
`org-odt-styles-dir-list'.  Note that the user could be using org
from one of: org's own private git repository, GNU ELPA tar or
standard Emacs.")

(defconst org-odt-manifest-file-entry-tag
  "\n<manifest:file-entry manifest:media-type=\"%s\" manifest:full-path=\"%s\"%s/>")

(defconst org-odt-file-extensions-alist
  '(("odt" "application/vnd.oasis.opendocument.text"                  "Text document")
    ("ott" "application/vnd.oasis.opendocument.text-template"         "Text document used as template")
    ("odg" "application/vnd.oasis.opendocument.graphics"              "Graphics document (Drawing)")
    ("otg" "application/vnd.oasis.opendocument.graphics-template"     "Drawing document used as template")
    ("odp" "application/vnd.oasis.opendocument.presentation"          "Presentation document")
    ("otp" "application/vnd.oasis.opendocument.presentation-template" "Presentation document used as template")
    ("ods" "application/vnd.oasis.opendocument.spreadsheet"           "Spreadsheet document")
    ("ots" "application/vnd.oasis.opendocument.spreadsheet-template"  "Spreadsheet document used as template")
    ("odc" "application/vnd.oasis.opendocument.chart"                 "Chart document")
    ("otc" "application/vnd.oasis.opendocument.chart-template"        "Chart document used as template")
    ("odi" "application/vnd.oasis.opendocument.image"                 "Image document")
    ("oti" "application/vnd.oasis.opendocument.image-template"        "Image document used as template")
    ("odf" "application/vnd.oasis.opendocument.formula"               "Formula document")
    ("otf" "application/vnd.oasis.opendocument.formula-template"      "Formula document used as template")
    ("odm" "application/vnd.oasis.opendocument.text-master"           "Global Text document.")
    ("oth" "application/vnd.oasis.opendocument.text-web"              "Text document used as template for HTML documents")
    ("odb" "application/vnd.oasis.opendocument.base"                  "Database front end document"))
  "Map a OpenDocument file extension, to it's mimetype and description.")

(defconst org-odt-supported-file-types
  '("odt" "odf" "odm" "ods")
  "List of OpenDocument file extensions that this backend can generate.")

(defconst org-odt-page-break-style-format "
<style:style style:name=\"%s\" style:family=\"paragraph\" style:parent-style-name=\"%s\" style:master-page-name=\"%s\">
 <style:paragraph-properties %s/>
</style:style>")

(defconst org-odt-table-style-format
  "
<style:style style:name=\"%s\" style:family=\"table\" %s>
  <style:table-properties fo:margin-top=\"0cm\" fo:margin-bottom=\"0.20cm\" table:align=\"center\" %s/>
</style:style>
"
  "Template for auto-generated Table styles.")

(defconst org-odt-graphic-style-format
  "
<style:style style:name=\"%s\" style:parent-style-name=\"%s\"
             style:family=\"graphic\">
  <style:graphic-properties draw:fill=\"bitmap\"
                            draw:fill-color=\"#ffffff\"
                            draw:fill-image-height=\"100%%\"
                            draw:fill-image-name=\"%s\"
                            draw:fill-image-ref-point=\"top-left\"
                            draw:fill-image-width=\"100%%\"
                            style:repeat=\"no-repeat\"  
                            >
  </style:graphic-properties>
</style:style>

"
  "Template for auto-generated graphic styles.")

(defvar org-odt-src-block-style-format
  (org-odt--lisp-to-xml
   `(style:style
     ((style:name . "OrgSrcBlock")
      (style:family . "paragraph")
      (style:parent-style-name . "Preformatted_20_Text"))
     (style:paragraph-properties
      ((fo:background-color . "%s")
       (fo:padding . "0.049cm")
       (fo:border . "0.51pt solid #000000")
       (style:shadow . "none"))
      (style:background-image nil))
     "\n   %s\n  "))
  "Custom paragraph style for colorized source and example blocks.
This style is much the same as that of \"OrgFixedWidthBlock\"
except that the foreground and background colors are set
according to the default face identified by the `htmlfontify'.")

(defvar hfy-optimizations)
(defvar org-odt-image-size-probe-method
  (append (and (executable-find "identify") '(imagemagick)) ; See Bug#10675
	  '(emacs fixed))
  "Ordered list of methods for determining image sizes.")

(defvar org-odt-default-image-sizes-alist
  '(("as-char" . (5 . 0.4))
    ("paragraph" . (5 . 5)))
  "Hardcoded image dimensions one for each of the anchor
  methods.")

;; A4 page size is 21.0 by 29.7 cms
;; The default page settings has 2cm margin on each of the sides. So
;; the effective text area is 17.0 by 25.7 cm
(defvar org-odt-max-image-size '(17.0 . 20.0)
  "Limiting dimensions for an embedded image.")

(defvar org-odt-caption-and-numbering-settings
  '((:TABLE:        :variable "Table"        :entity-name "Table"    :caption-style "Table"     :use-outline-levelp t   :seq-num-format "1")
    (:FIGURE:       :variable "Figure"       :entity-name "Figure"   :caption-style "Figure"    :use-outline-levelp t   :seq-num-format "1")
    (:SUBENTITY:    :variable "SubEntity"    :entity-name ""         :caption-style "Figure"    :use-outline-levelp nil :seq-num-format "a")
    (:MATH-FORMULA: :variable "Text"         :entity-name "Equation" :caption-style "Formula"    :use-outline-levelp t   :seq-num-format "1")
    (:DVIPNG-IMAGE: :variable "Equation"     :entity-name "Equation" :caption-style "Figure"    :use-outline-levelp t   :seq-num-format "1")
    (:LISTING:      :variable "Listing"      :entity-name "Listing"  :caption-style "Listing"   :use-outline-levelp t   :seq-num-format "1"))
  "Map a CATEGORY-HANDLE to CATEGORY-PROPS.

This is an alist where each entry is of the form:

  (CATEGORY . CATEGORY-PROPS)

CATEGORY, a symbol, identifies the type of a captionable entity.

CATEGORY-PROPS, a plist, controls how the captions of type
CATEGORY are enumerated and typeset.

CATEGORY-PROPS allows following properties:

  - `:variable' :: a string. This is the OpenDocument sequence
       counter associated with the entity.  This counter
       corresponds to the counters declared within
       \"<text:sequence-decls>...</text:sequence-decls>\" block
       of `org-odt-content-template-file'.

  - `:use-outline-levelp' :: a boolean.  When t, the counter is
       reset at headline level specified by
       `ODT_DISPLAY_OUTLINE_LEVEL' or
       `org-odt-display-outline-level'.

  - `:seq-num-format' :: a string.  This specifies how the number
       in captions is formatted.  Use \"1\" for numerals, \"a\"
       for alphabets etc.

  - `:entity-name' :: a string.  This qualifies captions on
       export.

  - `:caption-style' :: a string.  This is the paragraph style
       used for typesetting the captions.

See `org-odt-format-label'.")

(defvar org-odt-category-attribute-to-category
  (cl-loop for (category . category-props) in org-odt-caption-and-numbering-settings
	   unless (eq category :SUBENTITY:)
	   collect (cons (downcase (plist-get category-props :variable)) category))
  "See `org-odt--element-category'.")

(cl-defun org-odt-toc/category (&key category localp index-title contents)
  (let ((category1 (assoc-default category '(("figures" . :FIGURE:)
					     ("listings" . :LISTING:)
					     ("tables" . :TABLE:)))))
    (when category1
      (let* ((category-props (alist-get category1 org-odt-caption-and-numbering-settings))
	     (entity-name (plist-get category-props :entity-name))
	     (variable (plist-get category-props :variable))
	     (style-prefix (or "Org" variable))
	     (caption-sequence-format "text")
	     ;; Fill in the default values for unspecified params
	     (scope (if localp "chapter" "document"))
	     (index-title (or index-title
			      (org-trim (format "List of %s" (capitalize entity-name)))))
	     (contents (or contents
			   (org-odt--lisp-to-xml
			    `(text:p
			      ((text:style-name . "Standard"))
			      "PLS. UPDATE THIS INDEX"))))
	     (index-style "OrgIndexSection"))
	(org-odt--lisp-to-xml
	 `(text:illustration-index
	   ((text:style-name . ,index-style)
	    (text:protected . "true")
	    (text:name . ,(format "Table of %s" (capitalize entity-name))))
	   (text:illustration-index-source
	    ((text:caption-sequence-name . ,variable)
	     (text:caption-sequence-format . ,caption-sequence-format)
	     (text:index-scope . ,scope))
	    (text:index-title-template
	     ((text:style-name . ,(format "%s_20_Index_20_Heading" style-prefix)))
	     ,index-title)
	    (text:illustration-index-entry-template
	     ((text:style-name . ,(format "%s_20_Index" style-prefix)))
	     (text:index-entry-link-start
	      ((text:style-name . "Index_20_Link")))
	     (text:index-entry-text nil)
	     (text:index-entry-tab-stop
	      ((style:type . "right")
	       (style:leader-char . ".")))
	     (text:index-entry-page-number nil)
	     (text:index-entry-link-end nil)))
	   (text:index-body nil
			    (text:index-title
			     ((text:style-name . ,index-style)
			      (text:name . ,(format "Table of %s Heading" (capitalize entity-name))))
			     (text:p
			      ((text:style-name . ,(format "%s_20_Index_20_Heading" style-prefix)))
			      ,index-title))
			    ,contents)))))))

(cl-defun org-odt-define-page-layout (style-name &key page-dimensions
						 print-orientation
						 (page-usage "mirrored")
						 (num-format "1")
						 (margin-bottom "2cm")
						 (margin-left "2cm")
						 (margin-right "2cm")
						 (margin-top "2cm")
						 (header-style)
                                                 (footer-style
						  '(style:header-footer-properties
						    ((fo:min-height . "0.6cm")
						     (fo:margin-left . "0cm")
						     (fo:margin-right . "0cm")
						     (fo:margin-top . "0.499cm")
						     (style:dynamic-spacing . "false")))))
  ;; See `org-odt-master-styles' for how to use this API.
  (pcase-let*
      ((print-orientation (or print-orientation 'portrait))
       (normalize-page-dimensions
	(lambda (page-dimensions)
	  (message "test: %S" page-dimensions)
	  (pcase-let* ((`(,x . ,y) page-dimensions)
		       (parse
			(lambda (x)
			  (cons x
				(cond
				 ((and (stringp x)
				       (string-match (rx (group (one-or-more (or digit ".")))
							 (group (optional (or "cm" "in"))))
						     x))
				  (list (string-to-number (match-string 1 x))
					(if (string= "" (match-string 2 x)) "cm"
					  (match-string 2 x))))
				 ((numberp x)
				  (list x "cm"))
				 (t (error "Invalid page dimension `%S'" x))))))
		       (ordered (sort (list (funcall parse x)
					    (funcall parse y))
				      (lambda (x y) (< (nth 1 x) (nth 1 y))))))
	    (list (format "%0.2f%s"
			  (nth 1 (nth 0 ordered))
			  (nth 2 (nth 0 ordered)))
		  (format "%0.2f%s"
			  (nth 1 (nth 1 ordered))
			  (nth 2 (nth 1 ordered)))))))
       (`(,width ,height)
	(let* ((dims
		(cond
		 ((stringp page-dimensions)
		  (or (cdr
		       (assoc-string page-dimensions
				     '(("a4" . (21.0 . 29.7))
				       ("letter" . (21.59 . 27.94))
				       ("a5" . (14.8 . 21.0)))
				     t))
		      page-dimensions))
		 ((consp page-dimensions)
		  page-dimensions)))
	       (dims (funcall normalize-page-dimensions dims)))
	  (pcase print-orientation
	    (`landscape
	     (nreverse dims))
	    (`portrait dims)
	    (_ (error "Invalid print-orientation `%S'" print-orientation))))))
    (org-odt--lisp-to-xml
     `(style:page-layout
       ((style:name . ,style-name)
	,@(when page-usage
            `((style:page-usage . ,page-usage))))
       (style:page-layout-properties
	((fo:page-width . ,width)
	 (fo:page-height . ,height)
	 ,@(when num-format
	     `((style:num-format . ,num-format)))
	 (style:print-orientation . ,(format "%s" print-orientation))
	 ,@(when margin-bottom
	     `((fo:margin-bottom . ,margin-bottom)))
	 ,@(when margin-left
	     `((fo:margin-left . ,margin-left)))
	 ,@(when margin-right
	     `((fo:margin-right . ,margin-right)))
	 ,@(when margin-top
	     `((fo:margin-top . ,margin-top)))
	 (style:writing-mode . "lr-tb")
	 (style:footnote-max-height . "0cm"))
	(style:footnote-sep
	 ((style:width . "0.018cm")
	  (style:distance-before-sep . "0.101cm")
	  (style:distance-after-sep . "0.101cm")
	  (style:line-style . "solid")
	  (style:adjustment . "left")
	  (style:rel-width . "25%")
	  (style:color . "#000000"))))
       ,@(when header-style
	   `((style:header-style nil ,header-style)))
       ,@(when footer-style
	   `((style:footer-style nil ,footer-style)))))))

;; `(text:p
;;   ((text:style-name . "OrgFooter"))
;;   (text:page-number
;;    ((text:select-page . "current"))))

(cl-defun org-odt-define-page-style (page-style &key
						layout-style
						(header-contents org-odt-header-contents)
						(footer-contents org-odt-footer-contents))
  ;; See `org-odt-master-styles' for how to use this API.
  (let* ((header
	  (cond
	   ((stringp header-contents)
	    (org-odt--xml-to-lisp
	     (org-odt-export-string-as-odt-string header-contents)))
	   (t header-contents)))
         (footer
	  (cond
	   ((stringp footer-contents)
	    (org-odt--xml-to-lisp
	     (org-odt-export-string-as-odt-string footer-contents)))
	   (t footer-contents)))
	 (page-style (or page-style "Standard"))
	 (layout-style (or layout-style "A4PortraitLayout")))

;; (style:master-page
;;  ((style:name . "Standard")
;;   (style:page-layout-name . "A4PortraitLayout"))
;;  (style:footer nil
;; 	       (text:p
;; 		((text:style-name . "OrgFooter"))
;; 		(text:page-number
;; 		 ((text:select-page . "current"))))))

    (org-odt--lisp-to-xml
     `(style:master-page
       ((style:name . ,page-style)
	(style:page-layout-name . ,layout-style))
       ,@(when header
	    `((style:header nil ,header)))
	,@(when footer
	    `((style:footer nil ,footer)))))))

(defvar hfy-user-sheet-assoc)

(defvar org-odt-locales-alist
  '(("aa_DJ"    "Afar (Djibouti)"                                                       western )
    ("aa_ER"    "Afar (Eritrea)"                                                        western )
    ("aa_ET"    "Afar (Ethiopia)"                                                       western )
    ("af_ZA"    "Afrikaans (South Africa)"                                              western )
    ("agr_PE"   "Aguaruna (Peru)"                                                       western )
    ("ak_GH"    "Akan (Ghana)"                                                          western )
    ("am_ET"    "Amharic (Ethiopia)"                                                    ctl     )
    ("an_ES"    "Aragonese (Spain)"                                                     western )
    ("anp_IN"   "Angika (India)"                                                        ctl     )
    ("ar_AE"    "Arabic (United Arab Emirates)"                                         ctl     )
    ("ar_BH"    "Arabic (Bahrain)"                                                      ctl     )
    ("ar_DZ"    "Arabic (Algeria)"                                                      ctl     )
    ("ar_EG"    "Arabic (Egypt)"                                                        ctl     )
    ("ar_IN"    "Arabic (India)"                                                        ctl     )
    ("ar_IQ"    "Arabic (Iraq)"                                                         ctl     )
    ("ar_JO"    "Arabic (Jordan)"                                                       ctl     )
    ("ar_KW"    "Arabic (Kuwait)"                                                       ctl     )
    ("ar_LB"    "Arabic (Lebanon)"                                                      ctl     )
    ("ar_LY"    "Arabic (Libya)"                                                        ctl     )
    ("ar_MA"    "Arabic (Morocco)"                                                      ctl     )
    ("ar_OM"    "Arabic (Oman)"                                                         ctl     )
    ("ar_QA"    "Arabic (Qatar)"                                                        ctl     )
    ("ar_SA"    "Arabic (Saudi Arabia)"                                                 ctl     )
    ("ar_SD"    "Arabic (Sudan)"                                                        ctl     )
    ("ar_SS"    "Arabic (South Sudan)"                                                  ctl     )
    ("ar_SY"    "Arabic (Syrian Arab Republic)"                                         ctl     )
    ("ar_TN"    "Arabic (Tunisia)"                                                      ctl     )
    ("ar_YE"    "Arabic (Yemen)"                                                        ctl     )
    ("as_IN"    "Assamese (India)"                                                      ctl     )
    ("ast_ES"   "Asturian (Spain)"                                                      western )
    ("ayc_PE"   "Southern Aymara (Peru)"                                                western )
    ("az_AZ"    "Azerbaijani (Azerbaijan)"                                              western )
    ("az_IR"    "Azerbaijani (Iran, Islamic Republic of)"                               western )
    ("be_BY"    "Belarusian (Belarus)"                                                  western )
    ("bem_ZM"   "Bemba (Zambia) (Zambia)"                                               western )
    ("bg_BG"    "Bulgarian (Bulgaria)"                                                  western )
    ("bhb_IN"   "Bhili (India)"                                                         ctl     )
    ("bho_IN"   "Bhojpuri (India)"                                                      ctl     )
    ("bho_NP"   "Bhojpuri (Nepal)"                                                      ctl     )
    ("bi_VU"    "Bislama (Vanuatu)"                                                     western )
    ("bn_BD"    "Bengali (Bangladesh)"                                                  ctl     )
    ("bn_IN"    "Bengali (India)"                                                       ctl     )
    ("bo_CN"    "Tibetan (China)"                                                       ctl     )
    ("bo_IN"    "Tibetan (India)"                                                       ctl     )
    ("br_FR"    "Breton (France)"                                                       western )
    ("brx_IN"   "Bodo (India) (India)"                                                  ctl     )
    ("bs_BA"    "Bosnian (Bosnia and Herzegovina)"                                      western )
    ("byn_ER"   "Bilin (Eritrea)"                                                       western )
    ("ca_AD"    "Catalan (Andorra)"                                                     western )
    ("ca_ES"    "Catalan (Spain)"                                                       western )
    ("ca_FR"    "Catalan (France)"                                                      western )
    ("ca_IT"    "Catalan (Italy)"                                                       western )
    ("ce_RU"    "Chechen (Russian Federation)"                                          western )
    ("chr_US"   "Cherokee (United States)"                                              western )
    ("ckb_IQ"   "Central Kurdish (Iraq)"                                                western )
    ("cmn_TW"   "Mandarin Chinese (Taiwan, Province of China)"                          cjk     )
    ("crh_UA"   "Crimean Tatar (Ukraine)"                                               western )
    ("cs_CZ"    "Czech (Czechia)"                                                       western )
    ("csb_PL"   "Kashubian (Poland)"                                                    western )
    ("cv_RU"    "Chuvash (Russian Federation)"                                          western )
    ("cy_GB"    "Welsh (United Kingdom)"                                                western )
    ("da_DK"    "Danish (Denmark)"                                                      western )
    ("de_DE"    "German (Germany)"                                                      western )
    ("de_AT"    "German (Austria)"                                                      western )
    ("de_BE"    "German (Belgium)"                                                      western )
    ("de_CH"    "German (Switzerland)"                                                  western )
    ("de_IT"    "German (Italy)"                                                        western )
    ("de_LI"    "German (Liechtenstein)"                                                western )
    ("de_LU"    "German (Luxembourg)"                                                   western )
    ("doi_IN"   "Dogri (macrolanguage) (India)"                                         ctl     )
    ("dsb_DE"   "Lower Sorbian (Germany)"                                               western )
    ("dv_MV"    "Dhivehi (Maldives)"                                                    ctl     )
    ("dz_BT"    "Dzongkha (Bhutan)"                                                     ctl     )
    ("el_CY"    "Modern Greek (1453-) (Cyprus)"                                         western )
    ("el_GR"    "Modern Greek (1453-) (Greece)"                                         western )
    ("en_US"    "English (United States)"                                               western )
    ("en_AG"    "English (Antigua and Barbuda)"                                         western )
    ("en_AU"    "English (Australia)"                                                   western )
    ("en_BW"    "English (Botswana)"                                                    western )
    ("en_CA"    "English (Canada)"                                                      western )
    ("en_DK"    "English (Denmark)"                                                     western )
    ("en_GB"    "English (United Kingdom)"                                              western )
    ("en_HK"    "English (Hong Kong)"                                                   western )
    ("en_IE"    "English (Ireland)"                                                     western )
    ("en_IL"    "English (Israel)"                                                      western )
    ("en_IN"    "English (India)"                                                       western )
    ("en_NG"    "English (Nigeria)"                                                     western )
    ("en_NZ"    "English (New Zealand)"                                                 western )
    ("en_PH"    "English (Philippines)"                                                 western )
    ("en_SC"    "English (Seychelles)"                                                  western )
    ("en_SG"    "English (Singapore)"                                                   western )
    ("en_ZA"    "English (South Africa)"                                                western )
    ("en_ZM"    "English (Zambia)"                                                      western )
    ("en_ZW"    "English (Zimbabwe)"                                                    western )
    ("es_ES"    "Spanish (Spain)"                                                       western )
    ("es_AR"    "Spanish (Argentina)"                                                   western )
    ("es_BO"    "Spanish (Bolivia, Plurinational State of)"                             western )
    ("es_CL"    "Spanish (Chile)"                                                       western )
    ("es_CO"    "Spanish (Colombia)"                                                    western )
    ("es_CR"    "Spanish (Costa Rica)"                                                  western )
    ("es_CU"    "Spanish (Cuba)"                                                        western )
    ("es_DO"    "Spanish (Dominican Republic)"                                          western )
    ("es_EC"    "Spanish (Ecuador)"                                                     western )
    ("es_GT"    "Spanish (Guatemala)"                                                   western )
    ("es_HN"    "Spanish (Honduras)"                                                    western )
    ("es_MX"    "Spanish (Mexico)"                                                      western )
    ("es_NI"    "Spanish (Nicaragua)"                                                   western )
    ("es_PA"    "Spanish (Panama)"                                                      western )
    ("es_PE"    "Spanish (Peru)"                                                        western )
    ("es_PR"    "Spanish (Puerto Rico)"                                                 western )
    ("es_PY"    "Spanish (Paraguay)"                                                    western )
    ("es_SV"    "Spanish (El Salvador)"                                                 western )
    ("es_US"    "Spanish (United States)"                                               western )
    ("es_UY"    "Spanish (Uruguay)"                                                     western )
    ("es_VE"    "Spanish (Venezuela, Bolivarian Republic of)"                           western )
    ("et_EE"    "Estonian (Estonia)"                                                    western )
    ("eu_ES"    "Basque (Spain)"                                                        western )
    ("eu_FR"    "Basque (France)"                                                       western )
    ("fa_IR"    "Persian (Iran, Islamic Republic of)"                                   ctl     )
    ("ff_SN"    "Fulah (Senegal)"                                                       western )
    ("fi_FI"    "Finnish (Finland)"                                                     western )
    ("fil_PH"   "Filipino (Philippines)"                                                western )
    ("fo_FO"    "Faroese (Faroe Islands)"                                               western )
    ("fr_FR"    "French (France)"                                                       western )
    ("fr_BE"    "French (Belgium)"                                                      western )
    ("fr_CA"    "French (Canada)"                                                       western )
    ("fr_CH"    "French (Switzerland)"                                                  western )
    ("fr_LU"    "French (Luxembourg)"                                                   western )
    ("fur_IT"   "Friulian (Italy)"                                                      western )
    ("fy_DE"    "western Frisian (Germany)"                                             western )
    ("fy_NL"    "western Frisian (Netherlands)"                                         western )
    ("ga_IE"    "Irish (Ireland)"                                                       western )
    ("gd_GB"    "Scottish Gaelic (United Kingdom)"                                      western )
    ("gez_ER"   "Geez (Eritrea)"                                                        western )
    ("gez_ET"   "Geez (Ethiopia)"                                                       western )
    ("gl_ES"    "Galician (Spain)"                                                      western )
    ("gu_IN"    "Gujarati (India)"                                                      ctl     )
    ("gv_GB"    "Manx (United Kingdom)"                                                 western )
    ("ha_NG"    "Hausa (Nigeria)"                                                       western )
    ("hak_TW"   "Hakka Chinese (Taiwan, Province of China)"                             cjk     )
    ("he_IL"    "Hebrew (Israel)"                                                       ctl     )
    ("hi_IN"    "Hindi (India)"                                                         ctl     )
    ("hif_FJ"   "Fiji Hindi (Fiji)"                                                     western )
    ("hne_IN"   "Chhattisgarhi (India)"                                                 ctl     )
    ("hr_HR"    "Croatian (Croatia)"                                                    western )
    ("hsb_DE"   "Upper Sorbian (Germany)"                                               western )
    ("ht_HT"    "Haitian (Haiti)"                                                       western )
    ("hu_HU"    "Hungarian (Hungary)"                                                   ctl     )
    ("hy_AM"    "Armenian (Armenia)"                                                    western )
    ("ia_FR"    "Interlingua (International Auxiliary Language Association) (France)"   western )
    ("id_ID"    "Indonesian (Indonesia)"                                                western )
    ("ig_NG"    "Igbo (Nigeria)"                                                        western )
    ("ik_CA"    "Inupiaq (Canada)"                                                      western )
    ("is_IS"    "Icelandic (Iceland)"                                                   western )
    ("it_CH"    "Italian (Switzerland)"                                                 western )
    ("it_IT"    "Italian (Italy)"                                                       western )
    ("iu_CA"    "Inuktitut (Canada)"                                                    western )
    ("ja_JP"    "Japanese (Japan)"                                                      cjk     )
    ("ka_GE"    "Georgian (Georgia)"                                                    western )
    ("kab_DZ"   "Kabyle (Algeria)"                                                      western )
    ("kk_KZ"    "Kazakh (Kazakhstan)"                                                   western )
    ("kl_GL"    "Kalaallisut (Greenland)"                                               western )
    ("km_KH"    "Central Khmer (Cambodia)"                                              ctl     )
    ("kn_IN"    "Kannada (India)"                                                       ctl     )
    ("ko_KR"    "Korean (Korea, Republic of)"                                           cjk     )
    ("kok_IN"   "Konkani (macrolanguage) (India)"                                       ctl     )
    ("ks_IN"    "Kashmiri (India)"                                                      ctl     )
    ("ku_TR"    "Kurdish (Turkey)"                                                      ctl     )
    ("kw_GB"    "Cornish (United Kingdom)"                                              western )
    ("ky_KG"    "Kirghiz (Kyrgyzstan)"                                                  western )
    ("lb_LU"    "Luxembourgish (Luxembourg)"                                            western )
    ("lg_UG"    "Ganda (Uganda)"                                                        western )
    ("li_BE"    "Limburgan (Belgium)"                                                   western )
    ("li_NL"    "Limburgan (Netherlands)"                                               western )
    ("lij_IT"   "Ligurian (Italy)"                                                      western )
    ("ln_CD"    "Lingala (Congo, The Democratic Republic of the)"                       western )
    ("lo_LA"    "Lao (Lao People's Democratic Republic)"                                western )
    ("lt_LT"    "Lithuanian (Lithuania)"                                                western )
    ("lv_LV"    "Latvian (Latvia)"                                                      western )
    ("lzh_TW"   "Literary Chinese (Taiwan, Province of China)"                          cjk     )
    ("mag_IN"   "Magahi (India)"                                                        ctl     )
    ("mai_IN"   "Maithili (India)"                                                      ctl     )
    ("mai_NP"   "Maithili (Nepal)"                                                      ctl     )
    ("mfe_MU"   "Morisyen (Mauritius)"                                                  western )
    ("mg_MG"    "Malagasy (Madagascar)"                                                 western )
    ("mhr_RU"   "Eastern Mari (Russian Federation)"                                     western )
    ("mi_NZ"    "Maori (New Zealand)"                                                   western )
    ("miq_NI"   "Mískito (Nicaragua)"                                                   western )
    ("mjw_IN"   "Karbi (India)"                                                         ctl     )
    ("mk_MK"    "Macedonian (North Macedonia)"                                          western )
    ("ml_IN"    "Malayalam (India)"                                                     ctl     )
    ("mn_MN"    "Mongolian (Mongolia)"                                                  ctl     )
    ("mni_IN"   "Manipuri (India)"                                                      ctl     )
    ("mnw_MM"   "Mon (Myanmar)"                                                         ctl     )
    ("mr_IN"    "Marathi (India)"                                                       ctl     )
    ("ms_MY"    "Malay (macrolanguage) (Malaysia)"                                      ctl     )
    ("mt_MT"    "Maltese (Malta)"                                                       western )
    ("my_MM"    "Burmese (Myanmar)"                                                     ctl     )
    ("nan_TW"   "Min Nan Chinese (Taiwan, Province of China)"                           cjk     )
    ("nb_NO"    "Norwegian Bokmål (Norway)"                                             western )
    ("nds_DE"   "Low German (Germany)"                                                  western )
    ("nds_NL"   "Low German (Netherlands)"                                              western )
    ("ne_NP"    "Nepali (macrolanguage) (Nepal)"                                        ctl     )
    ("nhn_MX"   "Central Nahuatl (Mexico)"                                              western )
    ("niu_NU"   "Niuean (Niue)"                                                         western )
    ("niu_NZ"   "Niuean (New Zealand)"                                                  western )
    ("nl_AW"    "Dutch (Aruba)"                                                         western )
    ("nl_BE"    "Dutch (Belgium)"                                                       western )
    ("nl_NL"    "Dutch (Netherlands)"                                                   western )
    ("nn_NO"    "Norwegian Nynorsk (Norway)"                                            western )
    ("nr_ZA"    "South Ndebele (South Africa)"                                          western )
    ("nso_ZA"   "Pedi (South Africa)"                                                   western )
    ("oc_FR"    "Occitan (post 1500) (France)"                                          western )
    ("om_ET"    "Oromo (Ethiopia)"                                                      western )
    ("om_KE"    "Oromo (Kenya)"                                                         western )
    ("or_IN"    "Oriya (macrolanguage) (India)"                                         ctl     )
    ("os_RU"    "Ossetian (Russian Federation)"                                         western )
    ("pa_IN"    "Panjabi (India)"                                                       western )
    ("pa_PK"    "Panjabi (Pakistan)"                                                    western )
    ("pap_AW"   "Papiamento (Aruba)"                                                    western )
    ("pap_CW"   "Papiamento (Curaçao)"                                                  western )
    ("pl_PL"    "Polish (Poland)"                                                       western )
    ("ps_AF"    "Pushto (Afghanistan)"                                                  ctl     )
    ("pt_BR"    "Portuguese (Brazil)"                                                   western )
    ("pt_PT"    "Portuguese (Portugal)"                                                 western )
    ("quz_PE"   "Cusco Quechua (Peru)"                                                  western )
    ("raj_IN"   "Rajasthani (India)"                                                    ctl     )
    ("ro_RO"    "Romanian (Romania)"                                                    western )
    ("ru_RU"    "Russian (Russian Federation)"                                          western )
    ("ru_UA"    "Russian (Ukraine)"                                                     western )
    ("rw_RW"    "Kinyarwanda (Rwanda)"                                                  western )
    ("sa_IN"    "Sanskrit (India)"                                                      ctl     )
    ("sah_RU"   "Yakut (Russian Federation)"                                            western )
    ("sat_IN"   "Santali (India)"                                                       ctl     )
    ("sc_IT"    "Sardinian (Italy)"                                                     western )
    ("sd_IN"    "Sindhi (India)"                                                        ctl     )
    ("se_NO"    "Northern Sami (Norway)"                                                western )
    ("sgs_LT"   "Samogitian (Lithuania)"                                                western )
    ("shn_MM"   "Shan (Myanmar)"                                                        ctl     )
    ("shs_CA"   "Shuswap (Canada)"                                                      western )
    ("si_LK"    "Sinhala (Sri Lanka)"                                                   ctl     )
    ("sid_ET"   "Sidamo (Ethiopia)"                                                     western )
    ("sk_SK"    "Slovak (Slovakia)"                                                     western )
    ("sl_SI"    "Slovenian (Slovenia)"                                                  western )
    ("sm_WS"    "Samoan (Samoa)"                                                        western )
    ("so_SO"    "Somali (Somalia)"                                                      western )
    ("so_DJ"    "Somali (Djibouti)"                                                     western )
    ("so_ET"    "Somali (Ethiopia)"                                                     western )
    ("so_KE"    "Somali (Kenya)"                                                        western )
    ("sq_AL"    "Albanian (Albania)"                                                    western )
    ("sq_MK"    "Albanian (North Macedonia)"                                            western )
    ("sr_ME"    "Serbian (Montenegro)"                                                  western )
    ("sr_RS"    "Serbian (Serbia)"                                                      western )
    ("ss_ZA"    "Swati (South Africa)"                                                  western )
    ("st_ZA"    "Southern Sotho (South Africa)"                                         western )
    ("sv_SE"    "Swedish (Sweden)"                                                      western )
    ("sv_FI"    "Swedish (Finland)"                                                     western )
    ("sw_KE"    "Swahili (macrolanguage) (Kenya)"                                       western )
    ("sw_TZ"    "Swahili (macrolanguage) (Tanzania, United Republic of)"                western )
    ("szl_PL"   "Silesian (Poland)"                                                     western )
    ("ta_IN"    "Tamil (India)"                                                         ctl     )
    ("ta_LK"    "Tamil (Sri Lanka)"                                                     ctl     )
    ("tcy_IN"   "Tulu (India)"                                                          ctl     )
    ("te_IN"    "Telugu (India)"                                                        ctl     )
    ("tg_TJ"    "Tajik (Tajikistan)"                                                    western )
    ("th_TH"    "Thai (Thailand)"                                                       western )
    ("the_NP"   "Chitwania Tharu (Nepal)"                                               ctl     )
    ("ti_ER"    "Tigrinya (Eritrea)"                                                    western )
    ("ti_ET"    "Tigrinya (Ethiopia)"                                                   western )
    ("tig_ER"   "Tigre (Eritrea)"                                                       western )
    ("tk_TM"    "Turkmen (Turkmenistan)"                                                western )
    ("tl_PH"    "Tagalog (Philippines)"                                                 western )
    ("tn_ZA"    "Tswana (South Africa)"                                                 western )
    ("to_TO"    "Tonga (Tonga Islands) (Tonga)"                                         western )
    ("tpi_PG"   "Tok Pisin (Papua New Guinea)"                                          western )
    ("tr_TR"    "Turkish (Turkey)"                                                      western )
    ("tr_CY"    "Turkish (Cyprus)"                                                      western )
    ("ts_ZA"    "Tsonga (South Africa)"                                                 western )
    ("tt_RU"    "Tatar (Russian Federation)"                                            western )
    ("ug_CN"    "Uighur (China)"                                                        ctl     )
    ("uk_UA"    "Ukrainian (Ukraine)"                                                   western )
    ("unm_US"   "Unami (United States)"                                                 western )
    ("ur_IN"    "Urdu (India)"                                                          ctl     )
    ("ur_PK"    "Urdu (Pakistan)"                                                       ctl     )
    ("uz_UZ"    "Uzbek (Uzbekistan)"                                                    western )
    ("ve_ZA"    "Venda (South Africa)"                                                  western )
    ("vi_VN"    "Vietnamese (Viet Nam)"                                                 western )
    ("wa_BE"    "Walloon (Belgium)"                                                     western )
    ("wae_CH"   "Walser (Switzerland)"                                                  western )
    ("wal_ET"   "Wolaytta (Ethiopia)"                                                   western )
    ("wo_SN"    "Wolof (Senegal)"                                                       western )
    ("xh_ZA"    "Xhosa (South Africa)"                                                  western )
    ("yi_US"    "Yiddish (United States)"                                               ctl     )
    ("yo_NG"    "Yoruba (Nigeria)"                                                      western )
    ("yue_HK"   "Yue Chinese (Hong Kong)"                                               cjk     )
    ("yuw_PG"   "Yau (Morobe Province) (Papua New Guinea)"                              western )
    ("zh_CN"    "Chinese (China)"                                                       cjk     )
    ("zh_HK"    "Chinese (Hong Kong)"                                                   cjk     )
    ("zh_SG"    "Chinese (Singapore)"                                                   cjk     )
    ("zh_TW"    "Chinese (Taiwan, Province of China)"                                   cjk     )
    ("zu_ZA"    "Zulu (South Africa)"                                                   western ))
  "Locale info.

A list of entries (LOCALE-CODE LOCALE-NAME LIBO-SCRIPT-TYPE).
Here is one of the entries in the list:

  (\"ta_IN\" \"Tamil (India)\" ctl )

LOCALE-CODE and LOCALE-NAME are used in `org-odt-update-locale'
for configuring the LANGUAGE keyword of an Org file.  These
LOCALE values together with LIBO-SCRIPT-TYPE is used to configure
the default language/script for body text in exported document.
See `org-odt-write-styles-file'

LOCALE-CODE, a string => LANG-CODE UNDERSCORE COUNTRY-CODE .
Example: \"ta_IN\".

     - LOCALE-CODEs are sourced from file `/usr/share/i18n/SUPPORTED'.
       This file is part of `locales' package on Debian.  For more
       information see https://en.wikipedia.org/wiki/ISO/IEC_15897.

LOCALE-NAME, a string =>  LANG-NAME OPEN-PAREN COUNTRY-NAME CLOSE-PAREN.
Example: \"Tamil (India)\".

    - LANG-NAME is sourced by looking up LANG-CODE in file
      `/usr/share/iso-codes/json/iso_639-3.json'.  This file is
      part of `iso-codes' package on Debian.  For more
      information, see https://en.wikipedia.org/wiki/ISO_639-3.

    - COUNTRY-NAME is sourced by looking up COUNTRY-CODE in file
      `/usr/share/iso-codes/json/iso_3166-1.json'.  This file is
      part of `iso-codes' package on Debian. For more
      information, see https://en.wikipedia.org/wiki/ISO_3166-1.

LIBO-SCRIPT-TYPE, a symbol, is one of `western', `cjk' or `ctl'.
It is populated by consulting the font configuration menu in
LibreOffice UI. The entries can also be cross-checked against the
following file:

https://raw.githubusercontent.com/LibreOffice/core/master/i18nlangtag/source/isolang/mslangid.cxx.")



;;; User Configuration Variables

(defgroup org-export-odt nil
  "Options for exporting Org mode files to ODT."
  :tag "Org Export ODT"
  :group 'org-export)


;;;; Debugging

(defcustom org-odt-prettify-xml ""
  "Specify whether or not the xml output should be prettified.

Value, a string, can be one of

    - \"\"		:: an empty string. Don't prettify
    - \"tidy\"		:: Run HTML tidy on component XML buffers
    - \"tidy+indent\"	:: Run HTML tidy followed with `indent-region'
                           on component XML buffers

When this option is non nil, use
`org-odt-prettify-xml-buffer' (i.e., HTML tidy) on component xml
buffers -- `content.xml', `styles.xml', `meta.xml', and
`mainifest.mxl' -- before they are zipped up in to a OpenDocument
file.  Note that this function creates line breaks, and will
introduce extraneous and undesirable whitespace in exported
document.  So, don't use this option in production.  Use this for
developing ODT-specific features or to make sense of OpenDocument
XML produced by a third party, say LibreOffice."
  :group 'org-export-odt
  :type '(choice
	  (const :tag "Don't prettify" "")
	  (const :tag "Prettify with HTML Tidy only" "tidy")
	  (const :tag "Prettify with HTML Tidy; Also run `indent-region'" "tidy+indent")))


;;;; Document schema

(require 'rng-loc)
(defcustom org-odt-schema-dir
  (cl-loop for schema-dir in org-odt-schema-dir-list
	   when (cl-every
		 (lambda (file-name-*)
		   (file-expand-wildcards (expand-file-name file-name-* schema-dir)))
		 '("od-manifest-schema*.rnc" "od-schema*.rnc" "schemas.xml"))
	   return schema-dir)
  "Directory that contains OpenDocument schema files.

This directory contains:
1. rnc files for OpenDocument schema
2. a \"schemas.xml\" file that specifies locating rules needed
   for auto validation of OpenDocument XML files.

Use the customize interface to set this variable.  This ensures
that `rng-schema-locating-files' is updated and auto-validation
of OpenDocument XML takes place based on the value
`rng-nxml-auto-validate-flag'.

The default value of this variable varies depending on the
version of org in use and is initialized from
`org-odt-schema-dir-list'.  The OASIS schema files are available
only in the org's private git repository.  It is *not* bundled
with GNU ELPA tar or standard Emacs distribution."
  :type '(choice
	  (const :tag "Not set" nil)
	  (directory :tag "Schema directory"))
  :group 'org-export-odt
  :version "24.1"
  :set
  (lambda (var schema-dir)
    "Set `org-odt-schema-dir'.
Also add it to `rng-schema-locating-files'."
    (if (not schema-dir)
	(set var schema-dir)
      (if (cl-every
	   (lambda (file-name-*)
	     (file-expand-wildcards (expand-file-name file-name-* schema-dir)))
	   '("od-manifest-schema*.rnc" "od-schema*.rnc" "schemas.xml"))
	  (set var schema-dir)
	(message "ox-odt: %s has no OpenDocument schema files, value ignored" schema-dir)))

    (when org-odt-schema-dir
      (eval-after-load 'rng-loc
	'(add-to-list 'rng-schema-locating-files
		      (expand-file-name "schemas.xml"
					org-odt-schema-dir))))))


;;;; Document styles

(defcustom org-odt-content-template-file nil
  "Template file for \"content.xml\".
The exporter embeds the exported content just before
\"</office:text>\" element.

If unspecified, the file named \"OrgOdtContentTemplate.xml\"
under `org-odt-styles-dir' is used."
  :type '(choice (const nil)
		 (file))
  :group 'org-export-odt
  :version "24.3")

(defcustom org-odt-styles-file nil
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
  :group 'org-export-odt
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

(defcustom org-odt-extra-styles nil
  "Extra styles, an XML string.
The styles specified here are prepended to in-buffer styles
specified with the following keywords

    #+ODT_EXTRA_STYLES: ...

       and

    #+ATTR_ODT: :target \"extra_styles\"
    #+begin_src nxml
    ...
    #+end_src
."
  :group 'org-export-odt
  :type
  '(choice
    (const :tag "None" nil)
    (string :tag "XML string")))

(defcustom org-odt-master-styles nil
  "Extra styles, an XML string.
The styles specified here are prepended to in-buffer styles
specified with the following keywords

    #+ODT_MASTER_STYLES: ...

       and

    #+ATTR_ODT: :target \"master_styles\"
    #+begin_src nxml
    ...
    #+end_src

The styles defined here are master page styles.  

ODT exporter produces pages in A4 (Portrait) layout.

By customizing this variable `org-odt-master-styles', and
`org-odt-extra-automatic-styles', you can generate documents with
custom page layouts.

Use `org-odt-master-styles' to define custom page styles.  Use
`org-odt-define-page-layout' to define custom page layouts.

Down below you see recipes for producing documents in the US
Letter (Portrait), A4 (Landscape). Recipe below works by
re-associating the `Standard' page style, to a layout style
different from the default `A4PortraitLayout'.

If you would like to produce documents in A4 (Landscape) layout,
then do this

    (setq org-odt-master-styles
          (org-odt-define-page-style \"Standard\"
                                     :layout-style \"A4LandscapeLayout\"
                                     :footer-template t))
    (setq org-odt-extra-automatic-styles nil)

If you would like to produce documents in US Letter (Portrait)
layout, then do this

    (setq org-odt-extra-automatic-styles
          (org-odt-define-page-layout \"LetterPortraitLayout\"
                                      :page-dimensions \"letter\"
                                      :print-orientation \\='portrait))

    (setq org-odt-master-styles
          (org-odt-define-page-style \"Standard\"
                                     :layout-style \"LetterPortraitLayout\"
                                     :footer-template t))

If you would like to produce documents in A5 (Portrait) layout,
then do this

    (setq org-odt-extra-automatic-styles
          (org-odt-define-page-layout \"A5PortraitLayout\"
                                      :page-dimensions \"a5\"))

    (setq org-odt-master-styles
          (org-odt-define-page-style \"Standard\"
                                     :layout-style \"A5PortraitLayout\"
                                     :footer-template t))

If you would like to produce documents in a non-standard 10cm x
10cm layout, then do this

    (setq org-odt-extra-automatic-styles
          (org-odt-define-page-layout \"MyCustomSquareLayout\"
                                      :page-dimensions \\='(10 . 10)))

    (setq org-odt-master-styles
          (org-odt-define-page-style \"Standard\"
                                     :layout-style \"MyCustomSquareLayout\"
                                     :footer-template t))

Note that the `A4LandscapeLayout' is already defined by the
default `OrgOdtStyles.xml', so there is no need to define the
page layout.

In the cases of `LetterPortraitLayout', `A5PortraitLayout',
`MyCustomSquareLayout' layouts above, the corresponding page
layout styles are _not_ already defined for you.  So, you need to
install _both_ the page layout, and page styles."
  :group 'org-export-odt
  :type
  '(choice
    (const :tag "None" nil)
    (string :tag "XML string")))

(defcustom org-odt-extra-automatic-styles nil
    "Extra styles, an XML string.
The styles specified here are prepended to in-buffer styles
specified with the following keywords

    #+ODT_EXTRA_AUTOMATIC_STYLES: ...

       and

    #+ATTR_ODT: :target \"extra_automatic_styles\"
    #+begin_src nxml
    ...
    #+end_src

The styles defined here are page layout styles.  

ODT exporter produces pages in A4 (Portrait) layout.

By customizing this variable `org-odt-extra-automatic-styles',
and `org-odt-master-styles', you can generate documents with
custom page layouts.

Use `org-odt-master-styles' to define custom page styles.  Use
`org-odt-define-page-layout' to define custom page layouts.

See `org-odt-master-styles' for recipes on how to produce
Letter (Portrait), A4 (Landscape), and other page layouts."
    :group 'org-export-odt
    :type
    '(choice
      (const :tag "None" nil)
      (string :tag "XML string")))

;; (defcustom org-odt-header-contents nil
;;   "
;; ."
;;   :group 'org-export-odt
;;   :type
;;   (let ((contents
;; 	 "
;; #+ATTR_ODT: :style \"Footer\"
;; {{{ODTDate}}}{{{ODTTab}}}{{{ODTPageNumber}}} of {{{ODTPageCount}}}{{{ODTTab}}}{{{ODTChapter}}}
;; "))
;;     `(choice
;;       (const :tag "None" nil)
;;       (string :tag "Org string" ,contents)
;;       (sexp :tag "XML (as Lisp)"
;; 	    ,(org-odt--xml-to-lisp
;; 	      (org-odt-export-string-as-odt-string contents))))))

(defcustom org-odt-header-contents nil
  "
."
  :group 'org-export-odt
  :type
  `(choice
    (const :tag "None" nil)
    (string :tag "Org string")
    (sexp :tag "XML (as Lisp)")))

;; (defcustom org-odt-footer-contents nil
;;   "
;; ."
;;   :group 'org-export-odt
;;   :type
;;   (let ((contents
;; 	 "
;; #+ATTR_ODT: :style \"Footer\"
;; {{{ODTDate}}}{{{ODTTab}}}{{{ODTPageNumber}}} of {{{ODTPageCount}}}{{{ODTTab}}}{{{ODTChapter}}}
;; "))
;;     `(choice
;;       (const :tag "None" nil)
;;       (string :tag "Org string" ,contents)
;;       (sexp :tag "XML (as Lisp)"
;; 	    ,(org-odt--xml-to-lisp
;; 	      (org-odt-export-string-as-odt-string contents))))))

(defcustom org-odt-footer-contents nil
  "
."
  :group 'org-export-odt
  :type
  `(choice
    (const :tag "None" nil)
    (string :tag "Org string")
    (sexp :tag "XML (as Lisp)")))

(defcustom org-odt-display-outline-level 2
  "Outline levels considered for enumerating captioned entities."
  :group 'org-export-odt
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'integer)


;;;; Document validation

(defcustom org-odt-validate-process nil
  "Shell command to validate a OpenDocument format.

Get ODF Validator from ODF Toolkit project.  See
https://odftoolkit.org/downloads.html.

A typical command with the above validator looks like this:

    java -jar /some/dir/odfvalidator-<whatever>.jar -v %i

Value is of the form (SHELL-CMD CMD-ARG1 CMD-ARG2 ...).
SHELL-CMD is the name of the executable.
CMD-ARG-1 etc. are the arguments to the command.  These arguments
can contain format specifiers.  These format specifiers are
interpreted as below:

    %i input file name in full.

Customize option `org-odt-validate' to control the post-export
validation process.

Use command `org-odt-validate' to validate any OpenDocument file."
  :group 'org-export-odt
  :type
  '(choice
    (const :tag "None" nil)
    (cons :tag "Command"
	  (string :tag "Executable") (repeat :tag "Arguments " (string :tag "Argument")))))

(defcustom org-odt-validate ""
  "Speccify if ODF Validator is run, and what to do on validation failures.

Validate with `org-odt-validate-process'.  Validation happens
immediately after a OpenDocument format file is created, but
before `org-odt-convert' and `org-odt-transform-processes'.

It's value, a string, can be one of

    - \"\"		:: Don't run ODF Validator on exported documents.

    - \"noabort\"	:: Run ODF Validator, but ignore error status.
                             In other words, proceed ahead with `org-odt-convert' 
                             and `org-odt-transform-processes' even if document 
			     is NOT well-formed.

    - \"abort\"		:: Run ODF Validator; Abort further processing if
                             validation fails.  In other words, don't run 
                             `org-odt-convert'and `org-odt-transform-processes'
			      if document is NOT well-formed."
  :group 'org-export-odt
  :type '(choice
	  (const :tag "Skip validation checks" "")
	  (const :tag "Run Validator;  Don't abort, even if document in NOT well-formed" "noabort")
	  (const :tag "Run Validator;  Abort export, if document is NOT well-formed" "abort")))


;;;; Document transformation

(defcustom org-odt-transform-processes nil
  "List of shell commands that transforms an exported document.

Pipe an exported document through these commands in the order
they are listed.  These tranformations occurs right before
conversion to `org-odt-preferred-output-format'.

The customization buffer suggests some useful transformers (See
below for details).  To use these transformers install the
LibreOffice extension, \"OrgModeUtilities.oxt\" found under
\"./contrib/odt/\" subdirectory of this repository.

I. Reload
---------

  This macro is the equivalent to running Menu-> File-> Reload.

  During document production, modify-export-check cycle is a rule
  rather than an exception.  When an exported document is opened
  in LibreOffice for *the first time*, LibreOffice displays the
  intended contents.  However, when there is a subsequent edit,
  and the document is exported and opened in LibreOffice,
  LibreOffice continues to display the old content, overlooking
  the new changes on the disk.  Given this behaviour, on every
  export-and-open, the user has to manually do a `File -> Reload'
  to view the new changes.  This macro takes care of this manual
  step.

  Don't add this macro to `org-odt-transform-processes'.  The
  macro is listed here for your information.  This macro is best
  used in conjunction with `xdg-open'.

  Here is a quick guide on how to configure existing LibreOffice
  session to reload ODT files on fresh export.

  1. Configure `xdg-open' as the system-dependent command for opening
     files

	 (setcdr (assq \\='system org-file-apps-gnu) \"xdg-open %s\")

	 (advice-add \\='org-open-file :around
		     (lambda (orig-fun &rest args)
		       ;; Work around a weird problem with xdg-open.
		       (let ((process-connection-type nil))
			 (apply orig-fun args))))

  2. Install `OrgModeUtilities.oxt' as an add-on to LibreOffice with
     `Tools' -> `Extension Manager' -> `Add-on' -> [pick
     `OrgModeUtilities.oxt' from you filesystem].

     You can obtain from `OrgModeUtilities.oxt' either from
     `contrib/odt/LibreOffice/' (git) or from
     `.emacs.d/elpa/ox-odt-x.x.x.xxx/LibreOffice/' (elpa).

  3. Create a custom desktop file, say
     `my-libreoffice-writer.desktop', and configure the `Exec' command
     to execute the `Reload' macro on file open.  Associate this
     custom desktop file with all OpenDocument files. In other words,
     open a terminal, and do the following:
     
	 ~$ cp /usr/share/applications/libreoffice-writer.desktop ~/.local/share/applications/my-libreoffice-writer.desktop

	 Open `my-libreoffice-writer.desktop', and modify the `Exec' command which looks like

	     Exec=libreoffice --writer %U

	 to

	     Exec=soffice --nologo --norestore %u macro:///OrgMode.Utilities.Reload()

	 ~$ xdg-mime default my-libreoffice-writer.desktop application/vnd.oasis.opendocument.text

	 ~$ update-desktop-database ~/.local/share/applications

II. Update All
-------------

  This transformer, a LibreOffice macro, fills out

     1. index templates created with following Org directives

  	  #+TOC: headlines
  	  #+TOC: figures
  	  #+TOC: tables
  	  #+TOC: listings

     2. xrefs created with `org-odt-caption-and-xref-settings'

     3. Update all transcluded links and break those links.

  This macro is the equivalent to running Menu-> Tools-> Update->
  Update All.

III. Update All and Break Links
------------------------------

  This transformer, a LibreOffice macro, does what \"Update All\"
  macro does.  Additionally, it also breaks links to the external
  files.

  This macro is the equivalent to running Menu-> Tools-> Update->
  Update All, followed by Menu->Edit->Links->[Select All]->Break
  Link from within LibreOffice GUI.

IV. Optimize Column Width of all Tables
---------------------------------------

  This transformer, a LibreOffice macro, optimizes the column width
  of all tables in the document.  This macro is equivalent to
  running LibreOffice's Menu-> Table-> Size-> Optimal Column Width
  on all the tables within the document.

Each element is of the form (PURPOSE SHELL-CMD CMD-ARG1 CMD-ARG2
...).  PURPOSE, a human-readable string, summarizes what the
command accomplishes.  SHELL-CMD is the name of the executable.
CMD-ARG-1 etc. are the arguments to the command.  These arguments
can contain format specifiers.  These format specifiers are
interpreted as below:

%i input file name in full
%I input file name as a URL."
  :group 'org-export-odt
  :type
  '(choice
    (const :tag "None" nil)
    (repeat :tag "Transformers"
	    (choice :tag "Transformer"
		    (const :tag "Reload"
			   ("Reload" "soffice" "--norestore" "--invisible" "--headless"
			    "macro:///OrgMode.Utilities.Reload(%I)"))
		    (const :tag "Update All"
			   ("Update All" "soffice" "--norestore" "--invisible" "--headless"
			    "macro:///OrgMode.Utilities.UpdateAll(%I)"))
		    (const :tag "Update All and Break Links"
			   ("Update All and Break Links" "soffice" "--norestore" "--invisible" "--headless"
			    "macro:///OrgMode.Utilities.UpdateAll(%I, 1)"))
		    (const :tag "Optimize Column Width of all Tables"
			   ("Optimize Column Width of all Tables" "soffice" "--norestore" "--invisible" "--headless"
			    "macro:///OrgMode.Utilities.OptimizeColumnWidth(%I)"))
		    (cons :tag "Other"
			  (string :tag "Name")
			  (cons :tag "Command"
				(string :tag "Executable") (repeat :tag "Arguments " (string :tag "Argument"))))))))

;;;; Document conversion

(defcustom org-odt-convert-processes
  '(("LibreOffice"
     "%l soffice --headless --convert-to %f%x --outdir %d %i")
    ("unoconv"
     "%l unoconv -f %f -o %d %i")
    ("Gnumeric"
     "%l ssconvert %i %o"))
  "Specify a list of document converters and their usage.
The converters in this list are offered as choices while
customizing `org-odt-convert-process'.

This variable is a list where each element is of the
form (CONVERTER-NAME CONVERTER-CMD).  CONVERTER-NAME is the name
of the converter.  CONVERTER-CMD is the shell command for the
converter and can contain format specifiers.  These format
specifiers are interpreted as below:

%i input file name in full
%I input file name as a URL
%f format of the output file
%o output file name in full
%O output file name as a URL
%d output dir in full
%D output dir as a URL.
%x extra options as set in `org-odt-convert-capabilities'."
  :group 'org-export-odt
  :version "24.1"
  :type
  '(choice
    (const :tag "None" nil)
    (alist :tag "Converters"
	   :key-type (string :tag "Converter Name")
	   :value-type (group (string :tag "Command line")))))

(defcustom org-odt-convert-process "LibreOffice"
  "Use this converter to convert from \"odt\" format to other formats.
During customization, the list of converter names are populated
from `org-odt-convert-processes'."
  :group 'org-export-odt
  :version "24.1"
  :type '(choice :convert-widget
		 (lambda (w)
		   (apply 'widget-convert (widget-type w)
			  (eval (car (widget-get w :args)))))
		 `((const :tag "None" nil)
		   ,@(mapcar (lambda (c)
			       `(const :tag ,(car c) ,(car c)))
			     org-odt-convert-processes))))

(defcustom org-odt-convert-capabilities
  '(("Text"
     ("odt" "ott" "doc" "rtf" "docx")
     (("pdf" "pdf") ("odt" "odt") ("rtf" "rtf") ("ott" "ott")
      ("doc" "doc" ":\"MS Word 97\"") ("docx" "docx") ("html" "html")
      ("txt" "txt" ":\"Text (encoded)\"")))
    ("Web"
     ("html")
     (("pdf" "pdf") ("odt" "odt") ("html" "html")))
    ("Spreadsheet"
     ("ods" "ots" "xls" "csv" "xlsx")
     (("pdf" "pdf") ("ots" "ots") ("html" "html") ("csv" "csv") ("ods" "ods")
      ("xls" "xls") ("xlsx" "xlsx")))
    ("Presentation"
     ("odp" "otp" "ppt" "pptx")
     (("pdf" "pdf") ("swf" "swf") ("odp" "odp") ("otp" "otp") ("ppt" "ppt")
      ("pptx" "pptx") ("odg" "odg")))
    ("Formula"
     ("odf" "mml")
     (("pdf" "pdf") ("mml" "mml") ("odf" "odf"))))
  "Specify input and output formats of `org-odt-convert-process'.
More correctly, specify the set of input and output formats that
the user is actually interested in.

This variable is an alist where each element is of the
form (DOCUMENT-CLASS INPUT-FMT-LIST OUTPUT-FMT-ALIST).
INPUT-FMT-LIST is a list of INPUT-FMTs.  OUTPUT-FMT-ALIST is an
alist where each element is of the form (OUTPUT-FMT
OUTPUT-FILE-EXTENSION EXTRA-OPTIONS).

The variable is interpreted as follows:
`org-odt-convert-process' can take any document that is in
INPUT-FMT-LIST and produce any document that is in the
OUTPUT-FMT-LIST.  A document converted to OUTPUT-FMT will have
OUTPUT-FILE-EXTENSION as the file name extension.  OUTPUT-FMT
serves dual purposes:
- It is used for populating completion candidates during
  `org-odt-convert' commands.
- It is used as the value of \"%f\" specifier in
  `org-odt-convert-process'.

EXTRA-OPTIONS is used as the value of \"%x\" specifier in
`org-odt-convert-process'.

DOCUMENT-CLASS is used to group a set of file formats in
INPUT-FMT-LIST in to a single class.

Note that this variable inherently captures how LibreOffice based
converters work.  LibreOffice maps documents of various formats
to classes like Text, Web, Spreadsheet, Presentation etc and
allow document of a given class (irrespective of its source
format) to be converted to any of the export formats associated
with that class.

See default setting of this variable for an typical
configuration."
  :group 'org-export-odt
  :version "24.1"
  :type
  '(choice
    (const :tag "None" nil)
    (alist :tag "Capabilities"
	   :key-type (string :tag "Document Class")
	   :value-type
	   (group (repeat :tag "Input formats" (string :tag "Input format"))
		  (alist :tag "Output formats"
			 :key-type (string :tag "Output format")
			 :value-type
			 (group (string :tag "Output file extension")
				(choice
				 (const :tag "None" nil)
				 (string :tag "Extra options"))))))))

(defcustom org-odt-preferred-output-format nil
  "Automatically post-process to this format after exporting to \"odt\".
Command `org-odt-export-to-odt' exports first to \"odt\" format
and then uses `org-odt-convert-process' to convert the
resulting document to this format.  During customization of this
variable, the list of valid values are populated based on
`org-odt-convert-capabilities'.

You can set this option on per-file basis using file local
values.  See Info node `(emacs) File Variables'."
  :group 'org-export-odt
  :version "24.1"
  :type '(choice :convert-widget
		 (lambda (w)
		   (apply 'widget-convert (widget-type w)
			  (eval (car (widget-get w :args)))))
		 `((const :tag "None" nil)
		   ,@(mapcar (lambda (c)
			       `(const :tag ,c ,c))
			     (org-odt-reachable-formats "odt")))))
;;;###autoload
(put 'org-odt-preferred-output-format 'safe-local-variable 'stringp)


;;;; Drawers

(defcustom org-odt-format-drawer-function
  (lambda (_name contents) contents)
  "Function called to format a drawer in ODT code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

The default value simply returns the value of CONTENTS."
  :group 'org-export-odt
  :version "24.4"
  :package-version '(Org . "8.3")
  :type 'function)

;;;; Footnotes / Endnotes

;;;;; Endnotes

(defcustom org-odt-endnote-regexp "^$"
  "A footnote whose label matches this regexp is exported as an endnote.

    |---------+-----------------------------------+-----------------------------------------------|
    | REGEXP  | COMMENTS                          | BEHAVIOUR                                     |
    |---------+-----------------------------------+-----------------------------------------------|
    | \".\"   | Match all footnote labels         | Typeset all footnotes  as endnotes              |
    |---------+-----------------------------------+-----------------------------------------------|
    | \"^$\"  | Match none of the footnote labels | Typeset none of the footnotes as endnotes       |
    |         |                                   | i.e., typeset all footnotes  as footnotes     |
    |         |                                   |                                               |
    |---------+-----------------------------------+-----------------------------------------------|
    | \"^en\" | Match all footnote labels         | A footnote labeled with [fn:en1] etc            |
    |         | that start with \"en\"            | will be typeset as a endnote, but those         |
    |         |                                   | like [fn:1] etc will be typeset as footnotes. |
    |---------+-----------------------------------+-----------------------------------------------|

By default, typeset all footnotes as footnotes.
."

  :group 'org-export-odt
  :type '(choice
	  (const :tag "All footnotes as footnotes" "^$")
	  (const :tag "All footnotes as endnotes" ".")
	  (regexp :tag "Footnotes matching this regexp as endnotes" "^en")))

(defcustom org-odt-endnote-anchor-format
  (format "<text:span text:style-name=\"%s\">%%s</text:span>" "OrgSuperscript")
  "Format string to typeset a endnote numeral in text area.
%s-specifier in the format string is replaced with the endnote numeral.

For example, if you want to enclose each endnote numeral in
square braces, and embolden it, you need to set this variable to

    \"<text:span text:style-name=\"Bold\">[%s]</text:span>\"

  or

    #+BIND: org-odt-endnote-anchor-format \"<text:span text:style-name=\\\"Bold\\\">[%s]</text:span>\"


Note that this variable doesn't affect the style of the endnote
numeral in the notes area.  In order to control the style of the
endnote numeral in notes area, you need to tweak
\"text:notes-configuration\" element of the endnote note-class.

For example, if you want the style in the notes area to match
with the style in text area as defined in in the preceding
example, you need to do the following:


    #+odt_extra_styles: <style:style style:name=\"Endnote_20_Symbol\"
    #+odt_extra_styles: 	     style:display-name=\"Endnote Symbol\" style:family=\"text\">
    #+odt_extra_styles:   <style:text-properties fo:font-weight=\"bold\"/>
    #+odt_extra_styles: </style:style>

    #+odt_extra_styles: <text:notes-configuration
    #+odt_extra_styles:     text:note-class=\"endnote\"
    #+odt_extra_styles:     text:default-style-name=\"Endnote\"
    #+odt_extra_styles:     text:citation-style-name=\"Endnote_20_Symbol\"
    #+odt_extra_styles:     text:citation-body-style-name=\"Endnote_20_anchor\"
    #+odt_extra_styles:     text:master-page-name=\"Endnote\"
    #+odt_extra_styles:     style:num-prefix=\"[\" style:num-suffix=\"] \"
    #+odt_extra_styles:     style:num-format=\"1\" text:start-value=\"0\"/>
."
  :group 'org-export-odt
  :type '(choice
	  (const :tag "As superscript" "<text:span text:style-name=\"OrgSuperscript\">%s</text:span>")
	  (const :tag "Within Square Brackets" "[%s]")
	  (string :tag "Other")))

(defcustom org-odt-endnote-separator
  (format "<text:span text:style-name=\"%s\">, </text:span>" "OrgSuperscript")
  "String that separates individual endnote anchor in a cluster.

A cluster of endnotes is a sequence of two or more endnotes
appearing back-to-back in body text.

For example, when you export the following org snippet

    #+odt_endnote_regexp: ^en

    Body text[fn:en1][fn:en2]

    [fn:en1] Endnote one 
    [fn:en2] Endnote two 

    #+BIND: org-odt-endnote-separator \", \"
    #+BIND: org-odt-endnote-braces (\"<text:span text:style-name=\\\"Bold\\\">[\" . \"]</text:span>\")
    #+BIND: org-odt-endnote-anchor-format \"%s\"

you will get

    Body text*[1, 2]*

with the text between asterisks typeset in bold.

See also `org-odt-endnote-anchor-format',
`org-odt-endnote-braces' and `org-odt-endnote-regexp'."
  :group 'org-export-odt
  :type '(string :tag "Endnote separator"))

(defcustom org-odt-endnote-braces '("" . "")
  "Braces to enclose a cluster of endnotes.

A cluster of endnotes is a sequence of two or more endnotes
appearing back-to-back in body text.

For example, when you export the following org snippet

    #+odt_endnote_regexp: ^en

    Body text[fn:en1][fn:en2]

    [fn:en1] Endnote one 
    [fn:en2] Endnote two 

    #+BIND: org-odt-endnote-separator \", \"
    #+BIND: org-odt-endnote-braces (\"<text:span text:style-name=\\\"Bold\\\">[\" . \"]</text:span>\")
    #+BIND: org-odt-endnote-anchor-format \"%s\"

you will get

    Body text*[1, 2]*

with the text between asterisks typeset in bold.

See also `org-odt-endnote-anchor-format' and
`org-odt-endnote-separator' and `org-odt-endnote-regexp'."
  :group 'org-export-odt
  :type '(choice
	  (const :tag "None" ("" . ""))
	  (const :tag "Square Brackets" ("[" . "]"))
	  (string :tag "Other")))

;;;;; Footnotes

(defcustom org-odt-footnote-anchor-format
  (format "<text:span text:style-name=\"%s\">%%s</text:span>" "OrgSuperscript")
  "Format string to typeset a footnote numeral in text area.
%s-specifier in the format string is replaced with the footnote numeral.

For example, if you want to enclose each footnote numeral in
square braces, and embolden it, you need to set this variable to

    \"<text:span text:style-name=\"Bold\">[%s]</text:span>\"

  or

    #+BIND: org-odt-footnote-anchor-format \"<text:span text:style-name=\\\"Bold\\\">[%s]</text:span>\"


Note that this variable doesn't affect the style of the footnote
numeral in the notes area.  In order to control the style of the
footnote numeral in notes area, you need to tweak
\"text:notes-configuration\" element of the footnote note-class.

For example, if you want the style in the notes area to match
with the style in text area as defined in the previous example,
you need to do the following:

    #+odt_extra_styles: <style:style style:name=\"Footnote_20_Symbol\"
    #+odt_extra_styles: 	     style:display-name=\"Footnote Symbol\" style:family=\"text\">
    #+odt_extra_styles:   <style:text-properties fo:font-weight=\"bold\"/>
    #+odt_extra_styles: </style:style>

    #+odt_extra_styles: <text:notes-configuration
    #+odt_extra_styles:     text:note-class=\"footnote\"
    #+odt_extra_styles:     text:default-style-name=\"Footnote\"
    #+odt_extra_styles:     text:citation-style-name=\"Footnote_20_Symbol\"
    #+odt_extra_styles:     text:citation-body-style-name=\"Footnote_20_anchor\"
    #+odt_extra_styles:     text:master-page-name=\"Footnote\" style:num-prefix=\"[\"
    #+odt_extra_styles:     style:num-suffix=\"] \" style:num-format=\"1\" text:start-value=\"0\"
    #+odt_extra_styles:     text:footnotes-position=\"page\" text:start-numbering-at=\"document\"/>

See also `org-odt-footnote-braces' and `org-odt-footnote-separator'."
  :group 'org-export-odt
  :type '(choice
	  (const :tag "As superscript" "<text:span text:style-name=\"OrgSuperscript\">%s</text:span>")
	  (const :tag "Within Square Brackets" "[%s]")
	  (string :tag "Other")))


(defcustom org-odt-footnote-separator
  (format "<text:span text:style-name=\"%s\">, </text:span>" "OrgSuperscript")
  "String that separates individual footnote anchor in a cluster.

A cluster of footnotes is a sequence of two or more footnotes
appearing back-to-back in body text.

For example, when you export the following org snippet

    Body text[fn:1][fn:2]

    [fn:1] Footnote one 
    [fn:2] Footnote two 

    #+BIND: org-odt-footnote-separator \", \"
    #+BIND: org-odt-footnote-braces (\"<text:span text:style-name=\\\"Bold\\\">[\" . \"]</text:span>\")
    #+BIND: org-odt-footnote-anchor-format \"%s\"

you will get

    Body text*[1, 2]*

with the text between asterisks typeset in bold.

See also `org-odt-footnote-anchor-format' and `org-odt-footnote-braces'."
  :group 'org-export-odt
  :type '(string :tag "Footnote separator"))

(defcustom org-odt-footnote-braces '("" . "")
  "Braces to enclose a cluster of footnotes.

A cluster of footnotes is a sequence of two or more footnotes
appearing back-to-back in body text.

For example, when you export the following org snippet

    Body text[fn:1][fn:2]

    [fn:1] Footnote one 
    [fn:2] Footnote two 

    #+BIND: org-odt-footnote-separator \", \"
    #+BIND: org-odt-footnote-braces (\"<text:span text:style-name=\\\"Bold\\\">[\" . \"]</text:span>\")
    #+BIND: org-odt-footnote-anchor-format \"%s\"

you will get

    Body text*[1, 2]*

with the text between asterisks typeset in bold.

See also `org-odt-footnote-anchor-format' and `org-odt-footnote-separator'."
  :group 'org-export-odt
  :type '(choice
	  (const :tag "None" ("" . ""))
	  (const :tag "Square Brackets" ("[" . "]"))
	  (string :tag "Other")))

;;;; Headline

(defcustom org-odt-format-headline-function
  'org-odt-format-headline-default-function
  "Function to format headline text.

This function will be called with 5 arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags string, separated with colons (string or nil).

The function result will be used as headline text."
  :group 'org-export-odt
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'function)


;;;; Inlinetasks

(defcustom org-odt-format-inlinetask-function
  'org-odt-format-inlinetask-default-function
  "Function called to format an inlinetask in ODT code.

The function must accept six parameters:
  TODO      the todo keyword, as a string
  TODO-TYPE the todo type, a symbol among `todo', `done' and nil.
  PRIORITY  the inlinetask priority, as a string
  NAME      the inlinetask name, as a string.
  TAGS      the inlinetask tags, as a string.
  CONTENTS  the contents of the inlinetask, as a string.

The function should return the string to be exported."
  :group 'org-export-odt
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'function)


;;;; LaTeX

(defcustom org-odt-with-latex org-export-with-latex
  "Non-nil means process LaTeX math snippets.

When set, the exporter will process LaTeX environments and
fragments.

This option can also be set with the +OPTIONS line,
e.g. \"tex:mathjax\".  Allowed values are:

nil            Ignore math snippets.
`verbatim'     Keep everything in verbatim
`dvipng'       Process the LaTeX fragments to images.  This will also
               include processing of non-math environments.
`imagemagick'  Convert the LaTeX fragments to pdf files and use
               imagemagick to convert pdf files to png files.
`mathjax'      Do MathJax preprocessing and arrange for MathJax.js to
               be loaded.
t              Synonym for `mathjax'."
  :group 'org-export-odt
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "Do not process math in any way" nil)
	  (const :tag "Use dvipng to make images" dvipng)
	  (const :tag "Use imagemagick to make images" imagemagick)
	  (const :tag "Use MathJax to display math" mathjax)
	  (const :tag "Leave math verbatim" verbatim)))

(defcustom org-odt-math-syntax "latex"
  "Syntax of the text between LaTeX math delimiters.

The value, a string can be one of \"latex\" or \"starmath\".
When the value is \"latex\", the interpretation is same as that
specified in the following node.

    (info \"(org) LaTeX fragments\") 

When the value is \"starmath\", the string between the LaTeX math
delimiters are assumed to be in Starmath format.  Use this option
if you find that the LaTeX-to-MathML conversion achieved by
`org-latex-to-mathml-convert-command' is error-prone or
unsatisfactory.

Here is a sample Org snippet that uses starmath for typesetting
the math.

    #+options: tex:t
    #+odt_math_syntax: starmath

    When ${ a <> 0 }$, there are two solutions to \\( { a x ^ 2 + b x + c = 0 } \\)
    and they are

    $${ x  = frac { { - b +- sqrt { b ^ 2 - 4 a c } } } { { 2 a } }}$$

    A trigonometric equation

    \\begin{equation}
     { nitalic cos { ( θ + ϕ ) } = nitalic cos { ( θ ) } nitalic cos { ( ϕ ) } - nitalic sin { ( θ ) } nitalic sin { ( ϕ ) } }
    \\end{equation}

When you use Starmath fragments, the value of `org-odt-with-latex'
is ignored, and the output is *always* set to MathML.  In other
words, you lose the ability to embed a starmath fragment verbatim
or as an image in the exported file."
  :group 'org-export-odt
  :type '(choice
	  (const :tag "LaTeX" "latex")
	  (const :tag "Starmath" "starmath")))


;;;; Links

(defcustom org-odt-inline-formula-rules
  '(("file" . "\\.\\(mathml\\|mml\\|odf\\)\\'"))
  "Rules characterizing formula files that can be inlined into ODT.

A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path."
  :group 'org-export-odt
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(alist :key-type (string :tag "Type")
		:value-type (regexp :tag "Path")))

(defcustom org-odt-inline-image-rules
  '(("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'"))
  "Rules characterizing image files that can be inlined into ODT.

A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path."
  :group 'org-export-odt
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(alist :key-type (string :tag "Type")
		:value-type (regexp :tag "Path")))

(defcustom org-odt-pixels-per-inch 96.0
  "Scaling factor for converting images pixels to inches.
Use this for sizing of embedded images.  See Info node `(org)
Images in ODT export' for more information."
  :type 'float
  :group 'org-export-odt
  :version "24.4"
  :package-version '(Org . "8.1"))

(defcustom org-odt-caption-and-xref-settings
  '((:LISTING: :caption-position below :caption-format
	       (category " " counter ": " caption)
	       :xref-format
	       (value))
    (:DVIPNG-IMAGE: :caption-position below :caption-format
		    (category " " counter ": " caption)
		    :xref-format
		    (value))
    (:MATH-FORMULA: :caption-position below :caption-format
		    (caption)
		    :xref-format
		    (text)
		    :label-format
		    ("(" counter ")"))
    (:FIGURE: :caption-position below :caption-format
	      (category " " counter ": " caption)
	      :xref-format
	      (value))
    (:SUBENTITY: :caption-position below :caption-format
		 ("(" counter ") " caption)
		 :xref-format
		 (value))
    (:TABLE: :caption-position above
	     :caption-format
	     (category " " counter ": " caption)
	     :xref-format
	     (value)))
  "Specify how to format caption and cross-references.

Use this, for example, to control various aspects of caption (the
numbering format, its position etc.) or to generate page numbers
as part of cross-references.  For a quick overview of this
variable, see examples towards the end of this docstring.

If you customize this option, the following text—\"[PLS. UPDATE
FIELDS]\"—is used as a placeholder for unresolvable
cross-reference fields (like page number etc).  When using
LibreOffice, use Menu -> Tools -> Update-> Fields / Update All
manually or leave `org-odt-transform-processes' at it's factory
value.

This variable is an alist of pairs (RULE-TAG . RULE-PLIST).
RULE-TAG is a symbol.  RULE-PLIST is a property list, the allowed
properties of which depend on the value of RULE-TAG.  The details
are as below.

RULE-TAG takes following one of the values:

  `:TABLE:' `:FIGURE:' `:MATH-FORMULA:' `:DVIPNG-IMAGE:'
   `:LISTING:' `:TARGET:'.

The `:TARGET:' rule specifies how a cross-reference to a
HEADLINE, a TARGET or a non-captionable ELEMENT is typeset.  Its
RULE-PLIST allow a single property `:xref-format'.

All RULE-TAGs (except for `:TARGET:') specify how a caption and a
cross-reference to the corresponding entity is typeset.  Its
RULE-PLIST allow following properties.

  `:caption-position' - a symbol - one of `above' or `below'
  `:caption-format'   - a mixed list of symbols and strings
  `:xref-format'      - a mixed list of symbols and strings

`:caption-format' and `:xref-format' are but format
specifiers (in disguise) and specify how a caption or a
cross-reference is transcoded.  Their form and function are
better illustrated than described.  So, consider the following
examples:

A `:caption-format' with the following value

          (category \" \" counter \": \" caption)

will result in following caption.

          Table 1: An Example Table
          ^^^^^ ^  ^^^^^^^^^^^^^^^^
            ^   |           ^
            |   |           |
       category |       caption
              counter

A `:xref-format' with the following value

      (\"Section \" chapter \" [\" text \"]\", \" page, \" t)

will result in following cross-reference.

          See Section 3.1 [Tropical Storms], page 24.
                       ^   ^^^^^^^^^^^^^^^        ^^^
                       |            |                |
                 chapter no.   chapter title       page number

See `org-odt-link--infer-description' (specifically
`org-odt--xref-target') and `org-odt-format-label' for
implementation details."
  :type
  `(alist :options
	  ((:TARGET:
	    (plist :options
		   ((:xref-format
		     (choice
		      (const :tag "Simple page number" ("page " t))
		      (const :tag "TexInfo style"
			     ("Section " chapter " [" text "]," " page " t))
		      (repeat :tag "Format string"
			      (choice
			       (const :tag "Chapter" chapter)
			       (const :tag "Direction" direction)
			       (const :tag "Number" number)
			       (const :tag "Number (All superior)" number-all-superior)
			       (const :tag "Number (No superior)" number-no-superior)
			       (const :tag "Page" page)
			       (const :tag "Page style" t)
			       (const :tag "Text" text)
			       (string :tag "String" ""))))))))
	   ,@(mapcar
	      (lambda (dc)
		`(,dc
		  (plist :options
			 ((:caption-position
			   (choice (const :tag "Below" below)
				   (const :tag "Above" above)))
			  (:caption-format
			   (repeat (choice
				    (const :tag "Category" category)
				    (const :tag "Counter" counter)
				    (const :tag "Caption" caption)
				    (string :tag "String" ""))))
			  (:xref-format
			   (repeat :tag "Format string"
				   (choice
				    (const :tag "Caption" caption)
				    (const :tag "Category & Value" category-and-value)
				    (const :tag "Chapter" chapter)
				    (const :tag "Direction" direction)
				    (const :tag "Page" page)
				    (const :tag "Text" text)
				    (const :tag "Value" value)
				    (string :tag "String" "")))))
			 )))
	      '(:TABLE: :FIGURE: :MATH-FORMULA: :DVIPNG-IMAGE: :LISTING:))))
  :group 'org-export-odt
  :version "24.4")

(defcustom org-odt-link-org-files-as-odt t
  "When Non-nil, treat a \"file.org\" link as if it is a \"file.odt\" link."
  :group 'org-export-odt
  :type 'boolean)

;;;; Lists

(defcustom org-odt-description-list-style #'org-odt--translate-description-lists/html
  "Specify how description lists are rendered.
Choose one of HTML or LaTeX style."
  :type '(choice
          (const :tag "Use HTML style" org-odt--translate-description-lists/html )
          (const :tag "Use LaTeX style" org-odt--translate-description-lists/latex ))
  :group 'org-export-odt
  :set (lambda (symbol value)
	 "Alias `org-odt--translate-description-lists'."
	 (set-default symbol value)
	 (fset 'org-odt--translate-description-lists value))
  :version "24.1")

;;;; Macros

(defconst org-odt--native-fields
  '(
    ;; Document Title
    ("ODTTitle" text:title nil)
    ;; Chapter Name, Number etc
    ("ODTChapter" text:chapter
     ((text:display . "name")
      (text:outline-level . "1")))
    ("ODTChapterNumber" text:chapter
     ((text:display . "number")
      (text:outline-level . "1")))
    ("ODTChapterNumberAndName" text:chapter
     ((text:display . "number-and-name")
      (text:outline-level . "1")))
    ("ODTChapterPlainNumber" text:chapter
     ((text:display . "plain-number")
      (text:outline-level . "1")))
    ;; Page Number
    ("ODTPageNumber" text:page-number
     ((text:select-page . "current"))
     "1")
    ("ODTPageNumberLowerAlpha" text:page-number
     ((style:num-format . "a")
      (text:select-page . "current"))
     "a")
    ("ODTPageNumberUpperAlpha" text:page-number
     ((style:num-format . "A")
      (text:select-page . "current"))
     "A")
    ("ODTPageNumber-1" text:page-number
     ((style:num-format . "1")
      (text:select-page . "current"))
     "1")
    ("ODTPageNumberLowerRoman" text:page-number
     ((style:num-format . "i")
      (text:select-page . "current"))
     "i")
    ("ODTPageNumberUpperRoman" text:page-number
     ((style:num-format . "I")
      (text:select-page . "current"))
     "I")
    ;; Whitespaces
    ("ODTSpace" text:s
     ((text:c . "1")))
    ("ODTTab" text:tab nil)
    ("ODTLineBreak" text:line-break nil)
    ;; Document Meta - File name, Path etc
    ("ODTFileName" text:file-name
     ((text:display . "name-and-extension"))
     "nativefields.odt")
    ("ODTFileNameAndExtension" text:file-name
     ((text:display . "name-and-extension"))
     "nativefields.odt")
    ("ODTFileNameSansExtension" text:file-name
     ((text:display . "name"))
     "nativefields")
    ("ODTFileNamePath" text:file-name
     ((text:display . "path"))
     "/home/kjambunathan/src/org-mode-ox-odt/testing/examples/odt/")
    ("ODTFileNameFull" text:file-name
     ((text:display . "full"))
     "/home/kjambunathan/src/org-mode-ox-odt/testing/examples/odt/nativefields.odt")
    ;; Document Statistics - Character, Word, Paragraph and Page counts
    ("ODTCharacterCount" text:character-count nil "422")
    ("ODTWordCount" text:word-count
     ((style:num-format . "1"))
     "51")
    ("ODTParagraphCount" text:paragraph-count nil "38")
    ("ODTPageCount" text:page-count nil "2")
    ;; Object Counts - Image, Table and Object counts
    ("ODTImageCount" text:image-count
     ((style:num-format . "1"))
     "0")
    ("ODTObjectCount" text:object-count
     ((style:num-format . "1")))
    ("ODTTableCount" text:table-count nil "0")
    ;; Date and/or Time
    ("ODTDate" text:time
     ((style:data-style-name . "OrgDate1")
      (text:time-value . "2022-05-16T09:50:12.615588982")
      (text:fixed . "false"))
     "09:50:12")
    ("ODTDateAndTime" text:time
     ((style:data-style-name . "OrgDate2")
      (text:time-value . "2022-05-16T09:50:36.528331437")
      (text:fixed . "false"))
     "09:50:36 AM"))
  "Definition of OpenDocument fields.

This is an alist of elements (FIELD-NAME . FIELD-DEFINITION).

FIELDNAME is available as an Org macro.  For example, you can
insert document title as a field anywhere in the exported file
with the following construct

    {{{ODTTitle}}}

FIELD-DEFINITION is lispified version created by passing the XML
definition through `org-odt--xml-to-lisp'

The FIELD-NAME is converted in an Org macro of same name as part
of `org-odt-global-macros'.")

(defcustom org-odt-global-macros
  `(
    ("keyword-to-documentproperty" .
     "(eval (if (org-export-derived-backend-p
		org-export-current-backend 'odt)
	       (format \"@@odt:%s@@\"
		       (org-odt--use-custom-field
			$1 (org-macro--find-keyword-value $1)))
	     (org-macro--find-keyword-value $1)))")
    ,@(mapcar (lambda (e)
		(pcase-let ((`(,name . ,value) e))
		  (cons name (format "@@odt:%s@@" (org-odt--lisp-to-xml value)))))
	      org-odt--native-fields))
  "Alist of ODT specific macro names and expansion templates.

This variable defines macro expansion templates installed by the
ODT backend, that are available globally.  Entries here are
appended to `org-export-global-macros' as part of
`org-odt-before-processing-hook', which is hooked on to
`org-export-before-processing-hook'.

Currently, following macros are defined:

 - `keyword-to-documentproperty' :: Usage
   {{{keyword-to-documentproperty(NAME)}}}.  Generate a reference
   to keyword NAME in content.xml.  In order for the reference to
   be functional, the keyword NAME (and it's value) need to be
   defined, and the keyword NAME has to be marked as exportable
   by appearing as a value of `ODT_DOCUMENT_PROPERTIES' keyword.
   In non-ODT backends this macro behaves like the standard
   {{{keyword(NAME)}}}.

   For example, to define and reference a document property
   called `DOC-TITLE' with value \"Custom fields\", you can do
   the following:

	#+ODT_DOCUMENT_PROPERTIES: DOC-TITLE 
	#+DOC-TITLE: Custom fields

	#+MACRO: DocTitle {{{keyword-to-documentproperty(DOC-TITLE)}}}

	The name of the document is {{{DocTitle}}}.

- Various OpenDocument-specific fields as given below :: Usage

  You can generate a TAB character with

        {{{ODTTab}}}

  You can generate a page number like `1 of 3' with

        {{{ODTPageNumber}}} of {{{ODTPageCount}}}

  These FIELD-NAME macros are useful for auto-inserting
  document-specific metadata in the Page Header and Footer of the
  exported file.  You can use {{{ODTTab}}} to position the fields
  either to the left, center or right of the header/footer area.

        |--------------------------+-------------------------------------------------------|
        | FIELD NAME / MACRO NAME  | PURPOSE                                               |
        | =======================  | =======                                               |
        |--------------------------+-------------------------------------------------------|
        | ODTTitle                 | Document Title                                        |
        |--------------------------+-------------------------------------------------------|
        | ODTChapter               | Chapter Name and/or Number                            |
        | ODTChapterNumber         |                                                       |
        | ODTChapterNumberAndName  |                                                       |
        | ODTChapterPlainNumber    |                                                       |
        |--------------------------+-------------------------------------------------------|
        | ODTPageNumber            | PageNumber (in Arabic, Alphabetic, and Roman formats) |
        | ODTPageNumberLowerAlpha  |                                                       |
        | ODTPageNumberUpperAlpha  |                                                       |
        | ODTPageNumber-1          |                                                       |
        | ODTPageNumberLowerRoman  |                                                       |
        | ODTPageNumberUpperRoman  |                                                       |
        |--------------------------+-------------------------------------------------------|
        | ODTSpace                 | Space                                                 |
        | ODTTab                   | Tab                                                   |
        | ODTLineBreak             | Line Break                                            |
        |--------------------------+-------------------------------------------------------|
        | ODTFileName              | FileName (with/without Directory and Extension)       |
        | ODTFileNameAndExtension  |                                                       |
        | ODTFileNameSansExtension |                                                       |
        | ODTFileNamePath          |                                                       |
        | ODTFileNameFull          |                                                       |
        |--------------------------+-------------------------------------------------------|
        | ODTCharacterCount        | Document Statistics like Word Count etc               |
        | ODTWordCount             |                                                       |
        | ODTParagraphCount        |                                                       |
        | ODTPageCount             |                                                       |
        | ODTImageCount            |                                                       |
        | ODTObjectCount           |                                                       |
        | ODTTableCount            |                                                       |
        |--------------------------+-------------------------------------------------------|
        | ODTDate                  | Date                                                  |
        | ODTDateAndTime           | Date and Time                                         |
        |--------------------------+-------------------------------------------------------|

Associations follow the pattern

  (NAME . TEMPLATE)

where NAME is a string beginning with a letter and consisting of
alphanumeric characters only.

TEMPLATE is the string to which the macro is going to be
expanded.  Inside, \"$1\", \"$2\"... are place-holders for
macro's arguments.  Moreover, if the template starts with
\"(eval\", it will be parsed as an Elisp expression and evaluated
accordingly."
  :group 'org-export-odt
  :type '(repeat
	  (cons (string :tag "Macro Name")
		(string :tag "Expansion Template"))))

;;;; Src Block

(defcustom org-odt-create-custom-styles-for-srcblocks t
  "Whether custom styles for colorized source blocks be automatically created.
When this option is turned on, the exporter creates custom styles
for source blocks based on the advice of `htmlfontify'.  Creation
of custom styles happen as part of `org-odt-hfy-face-to-css'.

When this option is turned off exporter does not create such
styles.

Use the latter option if you do not want the custom styles to be
based on your current display settings.  It is necessary that the
styles.xml already contains needed styles for colorizing to work.

This variable is effective only if `org-odt-fontify-srcblocks' is
turned on."
  :group 'org-export-odt
  :version "24.1"
  :type 'boolean)

(defcustom org-odt-fontify-srcblocks t
  "Specify whether or not source blocks need to be fontified.
Turn this option on if you want to colorize the source code
blocks in the exported file.  For colorization to work, you need
to make available an enhanced version of `htmlfontify' library."
  :type 'boolean
  :group 'org-export-odt
  :version "24.1")


;;;; Table

(defcustom org-odt-table-template-props
  '(("Org" border valign)
    ("Custom" valign))
  "Org ODT Table Template Props."
  :group 'org-export-odt
  :type '(choice
	  (const :tag "None" nil)
	  (repeat :tag "Table Templates"
		  (cons :tag "Table Template Properties"
			(string :tag "Table Template Name")
			(set (const :tag "Provides Cell Borders" border)
			     (const :tag "Provides Valigns" valign))))))

(defcustom org-odt-table-styles
  '(("OrgEquation" "OrgEquation"
     ((use-first-column-styles . t)
      (use-last-column-styles . t)))
    ("TableWithHeaderRowAndColumn" "Custom"
     ((use-first-row-styles . t)
      (use-first-column-styles . t)))
    ("TableWithFirstRowandLastRow" "Custom"
     ((use-first-row-styles . t)
      (use-last-row-styles . t)))
    ("GriddedTable" "Custom" nil))
  "Specify how Table Styles should be derived from a Table Template.
This is a list where each element is of the
form (TABLE-STYLE-NAME TABLE-TEMPLATE-NAME TABLE-CELL-OPTIONS).

TABLE-STYLE-NAME is the style associated with the table through
\"#+ATTR_ODT: :style TABLE-STYLE-NAME\" line.

TABLE-TEMPLATE-NAME is a set of - upto 9 - automatic
TABLE-CELL-STYLE-NAMEs and PARAGRAPH-STYLE-NAMEs (as defined
below) that is included in `org-odt-content-template-file'.

TABLE-CELL-STYLE-NAME := TABLE-TEMPLATE-NAME + TABLE-CELL-TYPE +
                         \"TableCell\"
PARAGRAPH-STYLE-NAME  := TABLE-TEMPLATE-NAME + TABLE-CELL-TYPE +
                         \"Contents\"
TABLE-CELL-TYPE       := \"FirstRow\"   | \"LastColumn\" |
                         \"FirstRow\"   | \"LastRow\"    |
                         \"EvenRow\"    | \"OddRow\"     |
                         \"EvenColumn\" | \"OddColumn\"  | \"\"
where \"+\" above denotes string concatenation.

TABLE-CELL-OPTIONS is an alist where each element is of the
form (TABLE-CELL-STYLE-SELECTOR . ON-OR-OFF).
TABLE-CELL-STYLE-SELECTOR := `use-first-row-styles'       |
                             `use-last-row-styles'        |
                             `use-first-column-styles'    |
                             `use-last-column-styles'     |
                             `use-banding-rows-styles'    |
                             `use-banding-columns-styles' |
                             `use-first-row-styles'
ON-OR-OFF                 := t | nil

For example, with the following configuration

\(setq org-odt-table-styles
      \\='((\"TableWithHeaderRowsAndColumns\" \"Custom\"
         ((use-first-row-styles . t)
          (use-first-column-styles . t)))
        (\"TableWithHeaderColumns\" \"Custom\"
         ((use-first-column-styles . t)))))

1. A table associated with \"TableWithHeaderRowsAndColumns\"
   style will use the following table-cell styles -
   \"CustomFirstRowTableCell\", \"CustomFirstColumnTableCell\",
   \"CustomTableCell\" and the following paragraph styles
   \"CustomFirstRowContents\",
   \"CustomFirstColumnContents\", \"CustomContents\"
   as appropriate.

2. A table associated with \"TableWithHeaderColumns\" style will
   use the following table-cell styles -
   \"CustomFirstColumnTableCell\", \"CustomTableCell\" and the
   following paragraph styles
   \"CustomFirstColumnContents\", \"CustomContents\"
   as appropriate..

Note that TABLE-TEMPLATE-NAME corresponds to the
\"<table:table-template>\" elements contained within
\"<office:styles>\".  The entries (TABLE-STYLE-NAME
TABLE-TEMPLATE-NAME TABLE-CELL-OPTIONS) correspond to
\"table:template-name\" and \"table:use-first-row-styles\" etc
attributes of \"<table:table>\" element.  Refer ODF-1.2
specification for more information.  Also consult the
implementation filed under `org-odt--table-cell-get-cell-style'.

The TABLE-STYLE-NAME \"OrgEquation\" is used internally for
formatting of numbered display equations.  Do not delete this
style from the list."
  :group 'org-export-odt
  :version "24.1"
  :type '(choice
          (const :tag "None" nil)
          (repeat :tag "Table Styles"
                  (list :tag "Table Style Specification"
			(string :tag "Table Style Name")
			(string  :tag "Table Template Name")
			(alist :options (use-first-row-styles
					 use-last-row-styles
					 use-first-column-styles
					 use-last-column-styles
					 use-banding-rows-styles
					 use-banding-columns-styles)
			       :key-type symbol
			       :value-type (const :tag "True" t))))))

;;;; Timestamps

(defcustom org-odt-use-date-fields nil
  "Non-nil, if timestamps should be exported as date fields.

When nil, export timestamps as plain text.

When non-nil, map `org-time-stamp-custom-formats' to a pair of
OpenDocument date-styles with names \"OrgDate1\" and \"OrgDate2\"
respectively.  A timestamp with no time component is formatted
with style \"OrgDate1\" while one with explicit hour and minutes
is formatted with style \"OrgDate2\".

This feature is experimental.  Most (but not all) of the common
%-specifiers in `format-time-string' are supported.
Specifically, locale-dependent specifiers like \"%c\", \"%x\" are
formatted as canonical Org timestamps.  For finer control, avoid
these %-specifiers.

Textual specifiers like \"%b\", \"%h\", \"%B\", \"%a\", \"%A\"
etc., are displayed by the application in the default language
and country specified in `org-odt-styles-file'.  Note that the
default styles file uses language \"en\" and country \"GB\".  You
can localize the week day and month strings in the exported
document by setting the default language and country either using
the application UI or through a custom styles file.

See `org-odt--build-date-styles' for implementation details."
  :group 'org-export-odt
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)


;;;; Experimental features: LANGUAGE keyword

(defcustom org-odt-experimental-features '(language)
  "List of EXPERIMENTAL features that are enabled.

This is a list of symbols, with each symbol representing a
feature.  Currently the following EXPERIMENTAL features are
available:

    - `language' :: Honor LANGUAGE setting.  See inline comments in
      `org-odt-write-styles-file'
      
    - `short-caption-as-label' :: Use short caption as image label.
      The effect of this option, by default, is to \"overlay\" the
      short caption as a text on the top-right (i.e., north-east)
      corner of an image.  You can use this feature to add an
      \"index\" to a subfigure.  For example, with the snippet like
      the one below:

          #+NAME: table
          #+CAPTION: Animals
          #+ATTR_ODT: :category \"figure\"
          #+ATTR_ODT: :list-table t
          -
              -
                  #+NAME: dog
                  #+CAPTION[([[dog]])]: A Dog
                  [[./org-mode-unicorn.png]]
              -
                  #+NAME: goat
                  #+CAPTION[([[goat]])]: A Goat        
                  [[./org-mode-unicorn.png]]

    - `transclude-sole-footnote-references-in-a-table' :: Enable
      Transcluded tables.  When enabled, a table cell whose
      content is a _sole_ footnote reference will be expanded as
      if it held the associated footnote definition.

      An Org table that is expanded like this is called as a \"Transcluded
      table\".

      A Transcluded table like this,

      #+ATTR_ODT: :widths \"2,1,1,8\"
      | /       | <        | >        |         |
      | Day     | Min Temp | Max Temp | Summary |
      |---------+----------+----------+---------|
      | Monday  | 11C      | 22C      | [fn:1]  |
      |---------+----------+----------+---------|
      | Tuesday | 9C       | 19C      | [fn:2]  |

      [fn:1]

      1. A clear day with lots of sunshine.
      2. Late in the day, a strong breeze will bring down the temperatures.

      [fn:2]

      1. Cloudy with rain, across many northern regions.
      2. Clear spells across most of Scotland and Northern Ireland, but
	 rain reaching the far northwest.

      when exported, will be typeset \"as if\" it is like the table below:

      | /       | <        | >        |                                                    |
      | Day     | Min Temp | Max Temp | Summary                                            |
      |---------+----------+----------+----------------------------------------------------|
      | Monday  | 11C      | 22C      | 1. A clear day with lots of sunshine.              |
      |         |          |          | 2. Late in the day, a strong breeze will bring     |
      |         |          |          |    down the temperatures.                          |
      |---------+----------+----------+----------------------------------------------------|
      | Tuesday | 9C       | 19C      | 1. Cloudy with rain, across many northern regions. |
      |         |          |          | 2. Clear spells across most of Scotland and        |
      |         |          |          |    Northern Ireland, but rain reaching the far     |
      |         |          |          |    northwest.                                      |

 ."

  :group 'org-export-odt
  :type '(set (const :tag "Honor LANGUAGE keyword" language)
	      (choice (const :tag "Ignore short caption" nil)
		      ;; (symbol :tag "Honor short caption" short-caption)
		      (const :tag "Short captions as object label" short-caption-as-label))
	      (const :tag "Enable Transcluded tables" transclude-sole-footnote-references-in-a-table)))


;;; Internal functions

;;;; Date

(defun org-odt--format-timestamp (timestamp &optional end iso-date-p)
  (let* ((format-timestamp
	  (lambda (timestamp format &optional end utc)
	    (if timestamp
		(org-timestamp-format timestamp format end utc)
	      (format-time-string format nil utc))))
	 (has-time-p (or (not timestamp)
			 (org-timestamp-has-time-p timestamp)))
	 (iso-date (let ((format (if has-time-p "%Y-%m-%dT%H:%M:%S"
				   "%Y-%m-%dT%H:%M:%S")))
		     (funcall format-timestamp timestamp format end))))
    (if iso-date-p iso-date
      (let* ((style (if has-time-p "OrgDate2" "OrgDate1"))
	     ;; LibreOffice does not care about end goes as content
	     ;; within the "<text:date>...</text:date>" field.  The
	     ;; displayed date is automagically corrected to match the
	     ;; format requested by "style:data-style-name" attribute.  So
	     ;; don't bother about formatting the date contents to be
	     ;; compatible with "OrgDate1" and "OrgDateTime" styles.  A
	     ;; simple Org-style date should suffice.
	     (date (let* ((formats
			   (if org-display-custom-times
			       (cons (substring
				      (car org-time-stamp-custom-formats) 1 -1)
				     (substring
				      (cdr org-time-stamp-custom-formats) 1 -1))
			     '("%Y-%m-%d %a" . "%Y-%m-%d %a %H:%M")))
			  (format (if has-time-p (cdr formats) (car formats))))
		     (funcall format-timestamp timestamp format end)))
	     (repeater (let ((repeater-type (org-element-property
					     :repeater-type timestamp))
			     (repeater-value (org-element-property
					      :repeater-value timestamp))
			     (repeater-unit (org-element-property
					     :repeater-unit timestamp)))
			 (concat
			  (cl-case repeater-type
			    (catchup "++") (restart ".+") (cumulate "+"))
			  (when repeater-value
			    (number-to-string repeater-value))
			  (cl-case repeater-unit
			    (hour "h") (day "d") (week "w") (month "m")
			    (year "y"))))))
	(concat
	 (format "<text:date text:date-value=\"%s\" style:data-style-name=\"%s\" text:fixed=\"true\">%s</text:date>"
		 iso-date style date)
	 (and (not (string= repeater ""))  " ")
	 repeater)))))


;;;; Locale-specific

(defun org-odt--translate (s encoding info)
  "Translate string S according to language specification.

ENCODING is a symbol among `:ascii', `:html', `:latex', `:latin1'
and `:utf-8'.  In the context of this exporter, ENCODING is
always `:utf-8'.  INFO is a plist used as a communication
channel.  Translation depends on `:language' property.  Return
the translated string.  If no translation is found, try to fall
back to `:default' encoding.  If it fails, return S.

This function is a thin wrapper around `org-export-translate'.
The syntax of LANGUAGE keyword (see
`org-export-default-language') is not codified at the time of
this writing.  Based on entries on `org-export-dictionary', it
can either be a LANG-CODE, or LOCALE-CODE.  The LOCALE-CODE _may_
use hyphen instead of underscore to separate the LANG-CODE and
COUNTRY-CODE.

This wrapper does the right thing for all the above variations in
LANGUAGE keyword."
  (or
   (cl-block outer
     (cl-loop with lang = (plist-get info :language)
	      with s-entry = (cdr (assoc s org-export-dictionary))
	      with try-langs = (cl-delete-duplicates
				;; Try looking up following variations of LANGUAGE value in
				;; `org-export-dictionary'.
				(list
				 ;; LANGUAGE, as it is.
				 lang
				 ;; LANGUAGE, but with hyphen instead of underscore.
				 (replace-regexp-in-string "-" "_" lang t t)
				 ;; LANGUAGE, but with underscore instead of underscore.
				 (replace-regexp-in-string "_" "-" lang t t)
				 ;; LANGUAGE, but with just the LANG-CODE.
				 (car (split-string lang "[_-]+")))
				:test #'string=)
	      with try-encodings = (list encoding :default)
	      for encoding in try-encodings
	      do (cl-loop for lang in try-langs
			  for translation = (plist-get (assoc-default lang s-entry)
						       encoding)
			  ;; Return the first succesful translation.
			  when translation do (cl-return-from outer translation))))
   s))


;;;; Frame

(cl-defun org-odt--draw:frame (text &key width height
				      rel-width rel-height
				      style extra anchor
				      title desc)
  (format
   "\n<draw:frame %s>\n%s\n</draw:frame>"
   (mapconcat #'identity
	      (list extra
		    (when style (format "draw:style-name=\"%s\"" style))
		    (when width (format "svg:width=\"%0.2fcm\"" width))
		    (when height (format "svg:height=\"%0.2fcm\"" height))
		    (when rel-width (format "style:rel-width=\"%f%%\"" rel-width))
		    (when rel-height "style:rel-height=\"scale\"")
		    (format "text:anchor-type=\"%s\"" (or anchor "char")))
	      " ")
   (concat text
	   (when title
	     (format "<svg:title>%s</svg:title>"
		     (org-odt--encode-plain-text title t)))
	   (when desc
	     (format "<svg:desc>%s</svg:desc>"
		     (org-odt--encode-plain-text desc t))))))

;;;; Library wrappers :: Arc Mode

(defun org-odt--zip-extract (archive members target)
  (when (atom members) (setq members (list members)))
  (dolist (member members)
    (require 'arc-mode)
    (let* ((--quote-file-name
	    ;; This is shamelessly stolen from `archive-zip-extract'.
	    (lambda (name)
	      (if (or (not (memq system-type '(windows-nt ms-dos)))
		      (and (boundp 'w32-quote-process-args)
			   (null w32-quote-process-args)))
		  (shell-quote-argument name)
		name)))
	   (target (funcall --quote-file-name target))
	   (archive (expand-file-name archive))
	   (archive-zip-extract
	    (list "unzip" "-qq" "-o" "-d" target))
	   exit-code command-output)
      (setq command-output
	    (with-temp-buffer
	      (setq exit-code (archive-zip-extract archive member))
	      (buffer-string)))
      (unless (zerop exit-code)
	(message command-output)
	(error "Extraction failed")))))


;;;; Library wrappers :: Ox

(defun org-odt--read-attribute (element &optional property collectp)
  (let ((attrs (cl-case (org-element-type element)
		 (headline
		  (let* ((value (org-element-property :ATTR_ODT element)))
		    (when value
		      (ignore-errors (read (format "(%s)" value))))))
		 (t (cl-loop for x on (org-export-read-attribute :attr_odt element) by 'cddr
			     append (list (car x) (read (cadr x))))))))
    (cond
     ((null property) attrs)
     ((null collectp) (plist-get attrs property))
     (t (cl-loop for x = (memq property attrs) then (memq property (cddr x))
		 while x collecting (cadr x) into value
		 finally (return value))))))


;;;; Target

(defun org-odt--target (text label)
  (cond
   ;; Empty label.
   ((not (and label (org-string-nw-p label))) text)
   ;; Bookmark pointing to a range of text.
   ((and text (not (string= text "")))
    (concat (format "<text:bookmark-start text:name=\"%s\"/>" label) text
	    (format "<text:bookmark-end text:name=\"%s\"/>" label)))
   ;; Bookmark at a location.
   (t (format "<text:bookmark text:name=\"%s\"/>" label))))

(defun org-odt--xref-target (category text label)
  (let* ((xref-format (plist-get
		       (assoc-default category
				      org-odt-caption-and-xref-settings)
		       :xref-format)))
    (when xref-format
      (mapconcat
       (lambda (%)
	 (cond
	  ((stringp %) %)
	  ((eq t %)
	   (format "<text:bookmark-ref text:ref-name=\"%s\">%s</text:bookmark-ref>"
		   label text))
	  ((symbolp %)
	   (format "<text:bookmark-ref text:reference-format=\"%s\" text:ref-name=\"%s\">%s</text:bookmark-ref>"
		   % label text))))
       xref-format ""))))

;;;; Textbox

(cl-defun org-odt-build-graphic-properties (&key
					    ;;  "as-char" "char" "page" "paragraph"
					    anchor-type
					    ;;  "center" "from-left" "right"
					    horizontal-pos
					    ;;  "page" "paragraph" "paragraph-content"
					    horizontal-rel
					    ;;  "bottom" "from-top" "middle" "top"
					    vertical-pos
					    ;;  "baseline" "page" "paragraph" "paragraph-content" "text"
					    vertical-rel
					    (x "0cm")
					    (y "0cm")
					    min-height
					    rel-width
					    width
					    flow-with-text
					    wrap
					    wrap-influence-on-position
					    number-wrapped-paragraphs
					    wrap-contour
					    margin-bottom
					    margin-left
					    margin-right
					    margin-top
                                            (padding  "0cm")
                                            (border  "none")
                                            (shadow-opacity  "100%")
                                            (shadow  "none")
					    background-color
					    fill
					    fill-color
					    opacity
					    background-transparency
					    run-through)
  `(style:graphic-properties
    ((
      (text:anchor-type ,anchor-type)
      (style:horizontal-pos ,horizontal-pos)
      (style:horizontal-rel ,horizontal-rel)
      (style:vertical-pos ,vertical-pos)
      (style:vertical-rel ,vertical-rel)
      (svg:x ,x)
      (svg:y ,y)
      ;;  "0.499cm" "0cm"
      ,@(when min-height
          `((fo:min-height ,min-height)))
      ;;  "100%"
      ,@(when rel-width
          `((style:rel-width ,rel-width)))
      ;;  "0cm" "2cm"
      ,@(when width
          `((svg:width ,width)))
      ;;  "true"
      ,@(when flow-with-text
          `((style:flow-with-text ,flow-with-text)))
      ;;  "none" "parallel" "run-through"
      ,@(when wrap
          `((style:wrap ,wrap)))
      ;;  "once-concurrent"
      ,@(when wrap-influence-on-position
          `((draw:wrap-influence-on-position ,wrap-influence-on-position)))
      ;;  "1" "no-limit"
      ,@(when number-wrapped-paragraphs
          `((style:number-wrapped-paragraphs ,number-wrapped-paragraphs)))
      ;;  "false"
      ,@(when wrap-contour
          `((style:wrap-contour ,wrap-contour)))
      ,@(when margin-bottom
          `((fo:margin-bottom ,margin-bottom)))
      ,@(when margin-left
          `((fo:margin-left ,margin-left)))
      ,@(when margin-right
          `((fo:margin-right ,margin-right)))
      ,@(when margin-top
          `((fo:margin-top ,margin-top)))
      ,@(when padding
          `((fo:padding ,padding)))
      ;;  "0.06pt solid #000000" "0.26pt solid #000000" "none"
      ,@(when border
          `((fo:border ,border)))
      ;;  "100%"
      ,@(when shadow-opacity
          `((draw:shadow-opacity ,shadow-opacity)))
      ;;  "none"
      ,@(when shadow
          `((style:shadow ,shadow)))
      ;;  "#ffffcc" "transparent"
      ,@(when background-color
          `((fo:background-color ,background-color)))
      ;;  "none" "solid"
      ,@(when fill
          `((draw:fill ,fill)))
      ;;  "#729fcf" "#ffffcc"
      ,@(when fill-color
          `((draw:fill-color ,fill-color)))
      ;;  "100%"
      ,@(when opacity
          `((draw:opacity ,opacity)))
      ;;  "0%"
      ,@(when background-transparency
          `((style:background-transparency ,background-transparency)))
      ;;  "foreground"
      ,@(when run-through
          `((style:run-through ,run-through)))))))

(cl-defun org-odt-build-margin-note (name &key parent-style-name)
  `(style:style
    ((style:name . ,name)
     (style:parent-style-name . ,parent-style-name)
     (style:family . "graphic"))
    ,(org-odt-build-graphic-properties
      :anchor-type "paragraph"
      :horizontal-pos "from-left"
      :horizontal-rel "page-start-margin"
      :vertical-pos "top"
      :vertical-rel "paragraph-content"
      :min-height "0.041cm"
      :width "3cm")))

(cl-defun org-odt--draw:textbox (text &key min-width min-height)
  (format "\n<draw:text-box %s>%s\n</draw:text-box>"
	  (mapconcat #'identity
		     (list (format "fo:min-height=\"%0.2fcm\"" (or min-height 0.2))
			   (unless min-width
			     (format "fo:min-width=\"%0.2fcm\"" (or min-width 0.2))))
		     " ")
	  text))

(cl-defun org-odt--textbox (text &key width height rel-width
				   style extra anchor)
  (org-odt--draw:frame
   (org-odt--draw:textbox text :min-width width :min-height height)
   :width width :height nil :rel-width rel-width
   :style style :extra extra :anchor anchor))

;;;; Customshape

(defconst org-odt-shape-geometries-alist
  `((rectangle .
	       ,(org-odt--lisp-to-xml
		 '(draw:enhanced-geometry
		   ((svg:viewBox . "0 0 21600 21600")
		    (draw:type . "rectangle")
		    (draw:enhanced-path . "M 0 0 L 21600 0 21600 21600 0 21600 0 0 Z N")))))
    (ellipse .
	     ,(org-odt--lisp-to-xml
	       '(draw:enhanced-geometry
		 ((svg:viewBox . "0 0 21600 21600")
		  (draw:glue-points . "10800 0 3163 3163 0 10800 3163 18437 10800 21600 18437 18437 21600 10800 18437 3163")
		  (draw:text-areas . "3163 3163 18437 18437")
		  (draw:type . "ellipse")
		  (draw:enhanced-path . "U 10800 10800 10800 10800 0 360 Z N")))))))

(cl-defun org-odt--graphic-style (&key style image height width)
  (org-odt--lisp-to-xml
   `(style:style
     ((style:name . ,style)
      (style:family . "graphic"))
     (style:graphic-properties
      ((draw:auto-grow-height . "false")
       (svg:stroke-color . "#000000")
       (svg:stroke-width . "0cm")
       (draw:fill-color . "#ffffff")
       ,@(when image
	   `((draw:fill . "bitmap")
	     (draw:fill-image-height . "100%")
	     (draw:fill-image-name . ,image)
	     (draw:fill-image-ref-point . "center")
	     (draw:fill-image-width . "100%")))
       (draw:textarea-horizontal-align . "justify")
       (draw:textarea-vertical-align . "middle")
       (fo:min-height . ,(format "%scm" (or width 1)))
       (fo:min-width . ,(format "%scm" (or height 1)))
       (style:horizontal-pos . "from-left")
       (style:horizontal-rel . "paragraph")
       (style:number-wrapped-paragraphs . "no-limit")
       (style:repeat . "no-repeat")
       (style:run-through . "foreground")
       (style:vertical-pos . "from-top")
       (style:wrap . "run-through"))))))

(cl-defun org-odt--draw:custom-shape (&key text width height style
					   anchor-type id other-id shape)
  ;; WARNING: `customshape' blocks are EXPERIMENTAL and pre-ALPHA
  ;; quality.  Don't use it in production.  See
  ;; `org-odt--draw:custom-shape' sample usage and other details.
  ;;
  ;; A *simple* document produced by LibreOffice UI (or this
  ;; exporter) that contains *even* a single captioned image is
  ;; *not* properly in Google Documents.  See
  ;; https://github.com/kjambunathan/org-mode-ox-odt/issues/136
  ;;
  ;; Google Documents uses a draw object which is a combination of
  ;; image and it's caption to produce captioned images. See
  ;;
  ;; - How to Add Captions to Images in Google Docs ::
  ;;     https://www.howtogeek.com/725451/how-to-add-captions-to-images-in-google-docs/
  ;;
  ;; - A typical ODT document produced by Google docs ::
  ;;     https://github.com/kjambunathan/org-mode-ox-odt/files/7594408/A.Simple.Captioned.Image.as.produced.by.Google.Docs.odt
  ;;
  ;; In order to produce such a document, this exporter needs to
  ;; emit a custom rectangulare shape for holding the caption.
  ;; Here is a sample snippet to produce a document that has
  ;; custom shapes:
  ;;
  ;;     #+odt_automatic_styles: <style:style style:name="OrgShape"
  ;;     #+odt_automatic_styles:              style:family="graphic">
  ;;     #+odt_automatic_styles:   <style:graphic-properties draw:auto-grow-height="true"
  ;;     #+odt_automatic_styles:                             draw:textarea-horizontal-align="justify"
  ;;     #+odt_automatic_styles:                             draw:textarea-vertical-align="middle"
  ;;     #+odt_automatic_styles:                             draw:wrap-influence-on-position="once-concurrent"
  ;;     #+odt_automatic_styles:                             fo:min-height="0cm"
  ;;     #+odt_automatic_styles:                             fo:min-width="0cm"
  ;;     #+odt_automatic_styles:                             fo:padding-bottom="0.125cm"
  ;;     #+odt_automatic_styles:                             fo:padding-left="0.25cm"
  ;;     #+odt_automatic_styles:                             fo:padding-right="0.25cm"
  ;;     #+odt_automatic_styles:                             fo:padding-top="0.125cm"
  ;;     #+odt_automatic_styles:                             fo:wrap-option="wrap"
  ;;     #+odt_automatic_styles:                             style:flow-with-text="false"
  ;;     #+odt_automatic_styles:                             style:horizontal-pos="center"
  ;;     #+odt_automatic_styles:                             style:horizontal-rel="paragraph"
  ;;     #+odt_automatic_styles:                             style:number-wrapped-paragraphs="no-limit"
  ;;     #+odt_automatic_styles:                             style:run-through="foreground"
  ;;     #+odt_automatic_styles:                             style:vertical-pos="top"
  ;;     #+odt_automatic_styles:                             style:vertical-rel="paragraph"
  ;;     #+odt_automatic_styles:                             style:wrap="none" />
  ;;     #+odt_automatic_styles:   <style:paragraph-properties style:writing-mode="lr-tb" />
  ;;     #+odt_automatic_styles: </style:style>
  ;;
  ;;     [[./org-mode-unicorn.png]]
  ;;
  ;;     #+ATTR_ODT: :anchor "paragraph" :style "OrgShape" :width 3
  ;;     #+begin_customshape
  ;;       Aliqua esse aute non lorem ullamco sint consequat in incididunt
  ;;       qui excepteur reprehenderit
  ;;     #+end_customshape
  ;;
  ;; Note that to produce a captioned image, the image and caption
  ;; (contained within custom shape) have to be grouped together.
  ;; Unfortunately, even before I could proceed with grouping the
  ;; objects, I realized that LibreOffice 7.2.2 has multiple issues
  ;; with rendering custom shapes.  One of the key issue is that
  ;; LibreOffice, even though it honors the relative positioning--both
  ;; horizontal and vertical position--of text (i.e., caption text)
  ;; within the custom shape, it re-writes the styles in an
  ;; inconvenient way.  See ['Writer typesets identically defined
  ;; automatic and custom graphic-styles
  ;; differently'](https://bugs.documentfoundation.org/show_bug.cgi?id=145987).
  ;;
  ;; Support for `customshape' is added to produce the ODT document
  ;; annexed in above LibreOffice bug report.  In other words,
  ;; `customshape' aren't meant for production use.
  (concat (org-odt--lisp-to-xml
	   `(draw:custom-shape
	     ((text:anchor-type . ,(or anchor-type "as-char"))
	      (svg:y . "0cm")
	      (draw:z-index . "0")
	      ;; (draw:name . "Shape 1")
	      (draw:style-name . ,style)
	      (draw:text-style-name . "Standard")
	      ,@(when id
		  `((xml:id . ,id)))
	      ,@(when id
		  `((draw:id . ,id)))
	      (svg:width . ,(format "%scm" (or width 0.2)))
	      (svg:height . ,(format "%scm" (or height 0.2))))
	     ,(concat
	       text
	       (let ((shape (or shape 'rectangle)))
		 (or (alist-get (intern (downcase shape))
				org-odt-shape-geometries-alist)
		     "")))))
	  (when other-id
	    (let* ((id1 id) (pt1 nil) (id2 nil) (pt2 nil))
	      (when (string-match
		     (rx-to-string
		      '(and (optional (group (one-or-more (not ":")))) ":"
			(group (one-or-more (any "0-9")))
			(optional "<" "-" (optional ">"))
			(group (one-or-more (not ":"))) ":"
			(group (one-or-more (any "0-9")))))
		     other-id)
		(setq id1 id
		      pt1 (match-string 2 other-id)
		      id2 (match-string 3 other-id)
		      pt2 (match-string 4 other-id)))
	      (org-odt--lisp-to-xml
	       `(draw:connector
		 ((text:anchor-type . "paragraph")
		  (draw:z-index . "2")
		  (draw:name . "Shape2")
		  (draw:style-name . "OrgConnector")
		  (draw:text-style-name . "Standard")
		  (draw:type . "line")
		  (draw:start-shape . ,id1)
		  (draw:end-shape . ,id2)
		  (draw:start-glue-point . ,pt1)
		  (draw:end-glue-point . ,pt2)
		  (svg:d . "M2499 395l5999-3425")
		  (svg:viewBox . "0 0 6001 3427"))
		 (text:p nil)))))))

;;;; Table of Contents

(defun org-odt-toc/headlines (index-title rel-max-depth &optional parent-level toc-text need-page-number)
  (let* ((max-depth (+ parent-level rel-max-depth))
         (scope (if (zerop parent-level) "document" "chapter"))
	 (style "OrgIndexSection"))
    (org-odt--lisp-to-xml
     `(text:table-of-content
       ((text:style-name . ,style)
	(text:protected . "false")
	(text:name . "Table of Contents"))
       (text:table-of-content-source
	((text:outline-level . ,(format "%d" rel-max-depth))
	 (text:use-outline-level . "false")
	 (text:use-index-source-styles . "true")
	 (text:index-scope . ,scope))
	(text:index-title-template
	 ((text:style-name . "Contents_20_Heading"))
	 ,index-title)
	,@(cl-loop for level from 1 to 10 collect
		   (org-odt--lisp-to-xml
		    `(text:table-of-content-entry-template
		      ((text:outline-level . ,(format "%d" level))
		       (text:style-name . ,(format "Contents_20_%d" level)))
		      (text:index-entry-link-start
		       ((text:style-name . ,(if need-page-number "Index_20_Link" "Internet_20_link"))))
		      (text:index-entry-chapter nil)
		      (text:index-entry-text nil)
		      ,@(when need-page-number
			  `((text:index-entry-tab-stop
			     ((style:type . "right")
			      (style:leader-char . ".")))
			    (text:index-entry-page-number nil)))
		      (text:index-entry-link-end nil))))
	,@(cl-loop for depth from (1+ parent-level) to max-depth
		   for level from 1 collect
		   (org-odt--lisp-to-xml
		    `(text:index-source-styles
		      ((text:outline-level . ,(format "%d" level)))
		      (text:index-source-style
		       ((text:style-name . ,(format "Heading_20_%d" depth))))))))
       (text:index-body nil
			(text:index-title
			 ((text:style-name . ,style)
			  (text:name . "Table of Contents1_Head"))
			 (text:p
			  ((text:style-name . "Contents_20_Heading"))
			  ,index-title))
			,toc-text)))))

(cl-defun org-odt-format-toc-headline
    (todo todo-type priority text tags
	  &key _level section-number headline-label)
  (setq text
	(concat
	 ;; Section number.
	 (when section-number (concat section-number ". "))
	 ;; Todo.
	 (when todo
	   (let ((style (if (eq todo-type 'done) "OrgDone" "OrgTodo")))
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
	    (format " <text:span text:style-name=\"%s\">[%s]</text:span>"
		    "OrgTags"
		    (mapconcat
		     (lambda (tag)
		       (format
			"<text:span text:style-name=\"%s\">%s</text:span>"
			"OrgTag" tag)) tags " : "))))))
  (format "<text:a xlink:type=\"simple\" xlink:href=\"#%s\">%s</text:a>"
	  headline-label text))

(defun org-odt-toc (rel-max-depth info &optional scope)
  (cl-assert (wholenump rel-max-depth))
  ;; When a headline is marked as a radio target, as in the example below:
  ;;
  ;; ** <<<Some Heading>>>
  ;;    Some text.
  ;;
  ;; suppress generation of radio targets.  i.e., Radio targets are to
  ;; be marked as targets within /document body/ and *not* within
  ;; /TOC/, as otherwise there will be duplicated anchors one in TOC
  ;; and one in the document body.
  ;;
  ;; Likewise, links, footnote references and regular targets are also
  ;; suppressed.
  (let* ((title (org-odt--translate "Table of Contents" :utf-8 info))
	 (headlines (org-export-collect-headlines
		     info (and (wholenump rel-max-depth) rel-max-depth) scope))
	 (parent-level (or (when scope
			     (let* ((parent-headline (org-export-get-parent-headline scope)))
			       (when parent-headline
				 (org-export-get-relative-level parent-headline info))))
			   0))
	 (backend (org-export-create-backend
		   :parent (org-export-backend-name (plist-get info :back-end))
		   :transcoders '((footnote-reference . ignore)
				  (link . (lambda (object c i) c))
				  (radio-target . (lambda (object c i) c))
				  (target . ignore)))))
    (when headlines
      (let ((toc-text (mapconcat
		       (lambda (headline)
			 (let* ((entry (org-odt-format-headline--wrap
					headline backend info 'org-odt-format-toc-headline))
				(level (org-export-get-relative-level headline info))
				(style (format "Contents_20_%d" (- level parent-level))))
			   (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
				   style entry)))
		       headlines "\n")))
	(org-odt-toc/headlines title rel-max-depth parent-level toc-text 'need-page-number)))))


;;;; Document styles

(defun org-odt--count-object (info object)
  (cl-assert (symbolp object))
  (let* ((counters (plist-get info :odt-object-counters))
	 (seqno (1+ (or (plist-get counters object) 0))))
    (plist-put info :odt-object-counters
	       (plist-put counters object seqno))
    seqno))

(defun org-odt--name-object (info object &optional base-style-name)
  (cl-assert (and (symbolp object)
		  (not (keywordp object))))
  (format "%s%d"
	  (or base-style-name
	      (capitalize (symbol-name object)))
	  (org-odt--count-object info object)))


;;;; Checkbox

;; | Ascii | LaTeX      | Html Entity | Html Hex  | Unicode Name                  | Unicode Character |
;; |-------+------------+-------------+-----------+-------------------------------+-------------------|
;; | x     | \checkmark | &check;     | &#x2713;  | CHECK MARK                    | ✓                 |
;; |-------+------------+-------------+-----------+-------------------------------+-------------------|
;; | [x]   | \boxminus  | &boxtimes;  | &#x022A0; | SQUARED TIMES                 | ⊠                 |
;; | [ ]   | \square    | &square;    | &#x025A1; | WHITE  SQUARE                 | □                 |
;; | [-]   | \boxtimes  | &boxminus;  | &#x0229F; | SQUARED MINUS                 | ⊟                 |
;; |-------+------------+-------------+-----------+-------------------------------+-------------------|
;; | [x]   |            |             | &#x1F5F7; | BALLOT BOX WITH BOLD SCRIPT X | 🗷                 |
;; | [ ]   |            |             | &#x1F78F; | MEDIUM WHITE SQUARE           | 🞏                 |
;; | [-]   |            |             | &#x2BBD;  | BALLOT BOX WITH LIGHT X       | ⮽                 |

(defun org-odt--checkbox (item)
  "Return check-box string associated to ITEM.

You can control how the checkboxes are typeset by customizing the
`utf-8' vaule of following entities:

  (1) \"checkboxon\"
  (2) \"checkboxoff\"
  (3) \"checkboxwip\"

in `org-entities-user'.

The default setting is equivalent to the following custom value
of `org-entities-user':

  (dolist (new-entity
	   \\='(
	     ;; (\"name\"         \"LaTeX\"         \"LaTeX mathp\"  \"HTML\"        \"ASCII\"  \"Latin1\"  \"utf-8\")
		(\"checkboxon\"   \"\\\\boxtimes\"    t              \"&boxtimes;\"  \"[x]\"    \"[x]\"     \"&#x022A0;\") ; ⊠ - SQUARED TIMES
		(\"checkboxoff\"  \"\\\\square\"      t              \"&square;\"    \"[ ]\"    \"[ ]\"     \"&#x025A1;\") ; □ - WHITE  SQUARE
		(\"checkboxwip\"  \"\\\\boxminus\"    t              \"&boxminus;\"  \"[-]\"    \"[-]\"     \"&#x0229F;\") ; ⊟ - SQUARED MINUS
		))
    (map-delete org-entities-user (car new-entity))
    (customize-set-variable \\='org-entities-user (cons new-entity org-entities-user))
    (customize-save-variable \\='org-entities-user org-entities-user))

Here are possible alternative settings:

  (dolist (new-entity
	   \\='(
	     ;; (\"name\"         \"LaTeX\"         \"LaTeX mathp\"  \"HTML\"        \"ASCII\"  \"Latin1\"  \"utf-8\")
		(\"checkboxon\"   \"\\\\boxtimes\"    t              \"&#x1F5F7;\"  \"[x]\"    \"[x]\"     \"&#x1F5F7;\") ; 🗷 - BALLOT BOX WITH BOLD SCRIPT X
		(\"checkboxoff\"  \"\\\\square\"      t              \"&#x1F78F;\"  \"[ ]\"    \"[ ]\"     \"&#x1F78F;\") ; 🞏 - MEDIUM WHITE SQUARE          
		(\"checkboxwip\"  \"\\\\boxminus\"    t              \"&#x2BBD;\"   \"[-]\"    \"[-]\"     \"&#x2BBD;\" ) ; ⮽ - BALLOT BOX WITH LIGHT X      
		))
    (map-delete org-entities-user (car new-entity))
    (customize-set-variable \\='org-entities-user (cons new-entity org-entities-user))
    (customize-save-variable \\='org-entities-user org-entities-user))

  (dolist (new-entity
	   \\='(
	     ;; (\"name\"         \"LaTeX\"         \"LaTeX mathp\"  \"HTML\"        \"ASCII\"  \"Latin1\"  \"utf-8\")
		(\"checkboxon\"   \"\\\\boxtimes\"    t              \"[&#x2713;]\"  \"[x]\"    \"[x]\"     \"[&#x2713;]\") ; [✓] - CHECK MARK
		(\"checkboxoff\"  \"\\\\square\"      t              \"[ ]\"         \"[ ]\"    \"[ ]\"     \"[ ]\"       ) ; [ ] - SPACE
		(\"checkboxwip\"  \"\\\\boxminus\"    t              \"[-]\"         \"[-]\"    \"[-]\"     \"[-]\"       ) ; [-] - HYPHEN-MINUS
		))
    (map-delete org-entities-user (car new-entity))
    (customize-set-variable \\='org-entities-user (cons new-entity org-entities-user))
    (customize-save-variable \\='org-entities-user org-entities-user))

For the pupose of this exporter, only the `utf-8' values are of
significance.  All other values are ignored."
  (let ((checkbox (org-element-property :checkbox item)))
    (if (not checkbox) ""
      (format "<text:span text:style-name=\"%s\">%s </text:span>"
	      "OrgCode" (cl-case checkbox
			  (on (or (nth 6 (org-entity-get "checkboxon"))
				  "&#x022A0;"
				  "[&#x2713;]"))
			  (off (or (nth 6 (org-entity-get "checkboxoff"))
				   "&#x025A1;"
				   "[ ]"))
			  (trans (or
				  (nth 6 (org-entity-get "checkboxwip"))
				  "&#x0229F;"
				  "[-]")))))))

;;; Template

;;;; Template Helpers

(defvar org-odt-table-paragraph-styles
  '(("OrgTableFirstRowContents" .
     "
  <style:style style:name=\"OrgTableFirstRowContents\" style:family=\"paragraph\" style:parent-style-name=\"OrgTableContents\">
   <style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\"/>
   <style:text-properties fo:font-weight=\"bold\" style:font-weight-asian=\"bold\" style:font-weight-complex=\"bold\"/>
  </style:style>
")
    ("OrgTableFirstRowContentsLeft" .
     "
  <style:style style:name=\"OrgTableFirstRowContentsLeft\" style:family=\"paragraph\" style:parent-style-name=\"OrgTableFirstRowContents\">
   <style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\"/>
  </style:style>
")
    ("OrgTableFirstRowContentsCenter" .
     "
  <style:style style:name=\"OrgTableFirstRowContentsCenter\" style:family=\"paragraph\" style:parent-style-name=\"OrgTableFirstRowContents\">
   <style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\"/>
  </style:style>
")
    ("OrgTableFirstRowContentsRight" .
     "
  <style:style style:name=\"OrgTableFirstRowContentsRight\" style:family=\"paragraph\" style:parent-style-name=\"OrgTableFirstRowContents\">
   <style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\"/>
  </style:style>
")
    ("OrgTableFirstColumnContents" .
     "
  <style:style style:name=\"OrgTableFirstColumnContents\" style:family=\"paragraph\" style:parent-style-name=\"OrgTableContents\">
   <style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\"/>
   <style:text-properties fo:font-weight=\"bold\" style:font-weight-asian=\"bold\" style:font-weight-complex=\"bold\"/>
  </style:style>
")
    ("OrgTableFirstColumnContentsLeft" .
     "
  <style:style style:name=\"OrgTableFirstColumnContentsLeft\" style:family=\"paragraph\" style:parent-style-name=\"OrgTableFirstColumnContents\">
   <style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\"/>
  </style:style>
")
    ("OrgTableFirstColumnContentsCenter" .
     "
  <style:style style:name=\"OrgTableFirstColumnContentsCenter\" style:family=\"paragraph\" style:parent-style-name=\"OrgTableFirstColumnContents\">
   <style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\"/>
  </style:style>
")
    ("OrgTableFirstColumnContentsRight" .
     "
  <style:style style:name=\"OrgTableFirstColumnContentsRight\" style:family=\"paragraph\" style:parent-style-name=\"OrgTableFirstColumnContents\">
   <style:paragraph-properties fo:text-align=\"center\" style:justify-single-word=\"false\"/>
  </style:style>
")))

(defun org-odt--table-cell-build-paragraph-styles (base-style)
  ;; Given a paragraph style BASE-STYLE, define following paragraph
  ;; that inherit from BASE-STYLE.
  ;;
  ;;     BASE-STYLE ⨯ VARIANT ⨯ SUFFIX2 ⨯ HALIGN
  ;;
  ;;     ⨯ above is the cross-product operator
  ;;
  ;;    - VARIANT	⇒ ["" "FirstRow" "FirstColumn" "LastRow" "LastColumn"]
  ;;    - SUFFIX2	⇒ ["Contents"]
  ;;    - HALIGN	⇒ ["Left" "Center" "Right"]
  ;;
  ;; This function is invoked with following BASE-STYLE values
  ;;
  ;; BASE-STYLE ⇒ TABLE-TEMPLAT-NAME x SUFFIX1
  ;;
  ;; - TABLE-TEMPLAT-NAME	⇒ ["Org" "Custom"]
  ;; - SUFFIX1	⇒ "Table"
  ;;
  ;; So, this function effectively generates 30 (= 2 x 5 x 1 x 3 )
  ;; paragraph styles.  Here are some of the paragraph styles
  ;; generated by this function:
  ;;
  ;; - "OrgTableContents"
  ;; - "OrgTableLastRowContentsCenter"
  ;; - "CustomTableFirstColumnContents"
  ;; - "CustomTableLastRowContentsCenter"
  (with-temp-buffer
    (cl-loop for variant in '(nil "FirstColumn" "LastColumn"
				  "FirstRow" "LastRow"
				  "EvenRow" "OddRow"
				  "EvenColumn" "OddColumn")
	     do
	     (insert
	      (or (assoc-default (concat base-style variant "Contents") org-odt-table-paragraph-styles)
		  (format
		   "
  <style:style style:name=\"%s\" style:family=\"paragraph\" style:parent-style-name=\"%s\"/>
"
		   (concat base-style variant "Contents")
		   (if variant (concat base-style "Contents") "Table_20_Contents"))))
	     (cl-loop for halign in '(left center right) do
		      (insert
		       (or (assoc-default (concat base-style variant "Contents" (capitalize (symbol-name halign)))
					  org-odt-table-paragraph-styles)
			   (format "
  <style:style style:name=\"%s\" style:family=\"paragraph\" style:parent-style-name=\"%s\">
    <style:paragraph-properties fo:text-align=\"%s\" style:justify-single-word=\"false\"/>
  </style:style>
"
				   (concat base-style variant "Contents" (capitalize (symbol-name halign)))
				   (concat base-style variant "Contents")
				   halign)))))
    (buffer-string)))

(defun org-odt--table-cell-build-table-cell-styles (base-style-name border-value &optional base-table-cell-properties)
  ;; Given a table-cell style BASE-STYLE-NAME, define the following
  ;; table-cell styles that "inherit" from BASE-STYLE-NAME.
  ;;
  ;;     BASE-STYLE-NAME ⨯ VALIGN x BORDER-STYLES
  ;;
  ;;     ⨯ above is the cross-product operator
  ;;
  ;;    - VALIGN	⇒ ["" "Top" "Middle" "Bottom"]
  ;;    - BORDER-STYLES ⇒ ["T" "B" "L" "R"] | [""]
  ;;
  ;; This function is invoked with the following BASE-STYLE-NAME values of
  ;;
  ;;     BASE-STYLE-NAME ⇒ TABLE-TEMPLAT-NAME x SUFFIX1 x VARIANT x SUFFIX2
  ;;
  ;; - TABLE-TEMPLAT-NAME	⇒ ["Org" "Custom"]
  ;; - SUFFIX1	⇒ ["Table"]
  ;; - VARIANT	⇒ ["" "FirstRow" "FirstColumn" "LastRow" "LastColumn"]
  ;; - SUFFIX2	⇒ ["Cell"]
  ;;
  ;; So, this function effectively generates 400 (= 1 x 5 x 1 x 4 x 16 +
  ;; 						 1 x 5 x 1 x 1 x 16)
  ;; table-cell styles.  Here are a few random table-cell styles
  ;; generated by this from the table styles generated by this
  ;; function:
  ;;
  ;; - "OrgTableCellMiddleTLR"
  ;; - "OrgTableCellBottomBL"
  ;; - "OrgFirstRowTableCellBottomLR"
  ;; - "OrgFirstColumnTableCellBottomTL"
  ;; - "OrgLastColumnTableCellTR"
  ;; - "OrgLastColumnTableCellTopTBLR"
  ;; - "OrgLastColumnTableCellMiddleTBLR"
  ;; - "OrgLastColumnTableCellBottomTBL"
  ;; - "CustomLastRowTableCellTop"
  ;; - "CustomLastRowTableCellMiddle"
  (let ((borders
	 (if (null border-value)
	     (list (cons "" ""))
	   (cl-loop for n from 0 to 15 collect
		    (cl-loop with border-styles = `((8 . "fo:border-top=\"%s\"")
						    (4 . "fo:border-bottom=\"%s\"")
						    (2 . "fo:border-left=\"%s\"")
						    (1 . "fo:border-right=\"%s\""))
			     with cell-name-suffixes = '((8 . "T")
							 (4 . "B")
							 (2 . "L")
							 (1 . "R"))
			     for x in '(8 4 2 1)
			     for what = (logand n x)
			     collecting (format (assoc-default x border-styles)
						(if (zerop what) "none" border-value))
			     into table-cell-properties
			     collecting (if (not (zerop what))
					    (assoc-default x cell-name-suffixes)
					  "")
			     into cell-name-suffix
			     finally (cl-return
				      (cons
				       (mapconcat #'identity cell-name-suffix "")
				       (mapconcat #'identity table-cell-properties " ")))))))
	(valigns
	 (cl-loop for valign in '(nil top middle bottom) collect
		  (if (not valign)
		      (cons "" "")
		    (cons (capitalize (symbol-name valign))
			  (format "style:vertical-align=\"%S\"" valign))))))
    (with-temp-buffer
      (insert (format "\n\n<!-- BEGIN: table-cell styles %s -->\n\n" base-style-name))
      (cl-loop for (valign-suffix . valign-properties) in valigns do
	       (cl-loop for (border-suffix . border-properties) in borders 
                        for style-name = (concat base-style-name valign-suffix border-suffix) do
                        (insert
			 (format
			  "
    <style:style style:name=\"%s\" style:family=\"table-cell\">
      <style:table-cell-properties %s/>
    </style:style>"
			  style-name
			  (mapconcat #'identity
				     (list
				      base-table-cell-properties
				      border-properties
				      valign-properties)
				     " ")))
                        (cl-loop with data-types = '(date)
                                 for data-type in data-types
                                 for data-type-as-string = (capitalize (symbol-name data-type)) do
                                 (insert
                                  (org-odt--lisp-to-xml
                                   `(style:style
                                     ((style:name . ,(format "%s%s" style-name  data-type-as-string))
                                      (style:parent-style-name . ,style-name)
                                      (style:family . "table-cell")
                                      (style:data-style-name . ,(format "%s%s%s" "Org" data-type-as-string "Style")))))))))
      (insert (format "\n\n<!-- END: table-cell styles %s -->\n\n" base-style-name))
      (buffer-string))))

(defun org-odt--build-date-styles (fmt style)
  ;; In LibreOffice 3.4.6, there doesn't seem to be a convenient way
  ;; to modify the date fields.  A date could be modified by
  ;; offsetting in days.  That's about it.  Also, date and time may
  ;; have to be emitted as two fields - a date field and a time field
  ;; - separately.

  ;; One can add Form Controls to date and time fields so that they
  ;; can be easily modified.  But then, the exported document will
  ;; become tightly coupled with LibreOffice and may not function
  ;; properly with other OpenDocument applications.

  ;; I have a strange feeling that Date styles are a bit flaky at the
  ;; moment.

  ;; The feature is experimental.
  (when (and fmt style)
    (let* ((fmt-alist
	    '(("%A" . "<number:day-of-week number:style=\"long\"/>")
	      ("%B" . "<number:month number:textual=\"true\" number:style=\"long\"/>")
	      ("%H" . "<number:hours number:style=\"long\"/>")
	      ("%M" . "<number:minutes number:style=\"long\"/>")
	      ("%S" . "<number:seconds number:style=\"long\"/>")
	      ("%V" . "<number:week-of-year/>")
	      ("%Y" . "<number:year number:style=\"long\"/>")
	      ("%a" . "<number:day-of-week number:style=\"short\"/>")
	      ("%b" . "<number:month number:textual=\"true\" number:style=\"short\"/>")
	      ("%d" . "<number:day number:style=\"long\"/>")
	      ("%e" . "<number:day number:style=\"short\"/>")
	      ("%h" . "<number:month number:textual=\"true\" number:style=\"short\"/>")
	      ("%k" . "<number:hours number:style=\"short\"/>")
	      ("%m" . "<number:month number:style=\"long\"/>")
	      ("%p" . "<number:am-pm/>")
	      ("%y" . "<number:year number:style=\"short\"/>")))
	   (case-fold-search nil)
	   (re (mapconcat 'identity (mapcar 'car fmt-alist) "\\|"))
	   match rpl (start 0) (filler-beg 0) filler-end filler output)
      (mapc
       (lambda (pair)
	 (setq fmt (replace-regexp-in-string (car pair) (cdr pair) fmt t t)))
       '(("\\(?:%[[:digit:]]*N\\)" . "") ; strip ns, us and ns
	 ("%C" . "Y")			 ; replace century with year
	 ("%D" . "%m/%d/%y")
	 ("%G" . "Y")		      ; year corresponding to iso week
	 ("%I" . "%H")		      ; hour on a 12-hour clock
	 ("%R" . "%H:%M")
	 ("%T" . "%H:%M:%S")
	 ("%U\\|%W" . "%V")	      ; week no. starting on Sun./Mon.
	 ("%Z" . "")		      ; time zone name
	 ("%c" . "%Y-%M-%d %a %H:%M" ) ; locale's date and time format
	 ("%g" . "%y")
	 ("%X" . "%x" )	 ; locale's pref. time format
	 ("%j" . "")	 ; day of the year
	 ("%l" . "%k")	 ; like %I blank-padded
	 ("%s" . "")	 ; no. of secs since 1970-01-01 00:00:00 +0000
	 ("%n" . "<text:line-break/>")
	 ("%r" . "%I:%M:%S %p")
	 ("%t" . "<text:tab/>")
	 ("%u\\|%w" . "")  ; numeric day of week - Mon (1-7), Sun(0-6)
	 ("%x" . "%Y-%M-%d %a")		; locale's pref. time format
	 ("%z" . "")			; time zone in numeric form
	 ))
      (while (string-match re fmt start)
	(setq match (match-string 0 fmt))
	(setq rpl (assoc-default match fmt-alist))
	(setq start (match-end 0))
	(setq filler-end (match-beginning 0))
	(setq filler (substring fmt (prog1 filler-beg
				      (setq filler-beg (match-end 0)))
				filler-end))
	(setq filler (and (not (string= filler ""))
			  (format "<number:text>%s</number:text>"
				  (org-odt--encode-plain-text filler))))
	(setq output (concat output "\n" filler "\n" rpl)))
      (setq filler (substring fmt filler-beg))
      (unless (string= filler "")
	(setq output (concat output
			     (format "\n<number:text>%s</number:text>"
				     (org-odt--encode-plain-text filler)))))
      (format "\n<number:date-style style:name=\"%s\" %s>%s\n</number:date-style>"
	      style
	      (concat " number:automatic-order=\"true\""
		      " number:format-source=\"fixed\"")
	      output ))))

(defun org-odt-create-manifest-file-entry (info &rest args)
  (plist-put info :odt-manifest-file-entries
	     (cons args (plist-get info :odt-manifest-file-entries))))

(defun org-odt--define-custom-field (key value)
  "Install a user-define variable NAME with value VALUE in meta.xml.

See entry titled `keyword-to-documentproperty' in
`org-odt-global-macros' for more information."
  (when (org-string-nw-p key)
    (let ((value-type "string"))
      (format "\n<meta:user-defined meta:value-type=\"%s\" meta:name=\"%s\" >%s</meta:user-defined>\n"
	      value-type
	      (org-odt--encode-plain-text key)
	      (org-odt--encode-plain-text (or value ""))))))

(defun org-odt--use-custom-field (name &optional value style)
  "Reference a user-defined variable NAME.

See entry titled `keyword-to-documentproperty' in
`org-odt-global-macros' for more information."
  (when (org-string-nw-p name)
    (format "<text:user-defined %s text:name=\"%s\">%s</text:user-defined>"
	    (if style (format "style:data-style-name=\"%s\"" style) "")
	    (org-odt--encode-plain-text name)
	    (org-odt--encode-plain-text value))))

(defun org-odt-get-backend-property (backend property)
  (let* ((config (list
                  ;; ODS backend
		  (list 'ods
			:styles-file (expand-file-name "ods/styles.xml" org-odt-styles-dir)
			:content-template-file (expand-file-name "ods/content.xml" org-odt-styles-dir)
			:content-tags '("<office:spreadsheet>" . "</office:spreadsheet>"))
                  ;; ODT and other backends
		  (list nil
			:styles-file (expand-file-name "OrgOdtStyles.xml" org-odt-styles-dir)
			:content-template-file (expand-file-name "OrgOdtContentTemplate.xml" org-odt-styles-dir)
			:content-tags '("<office:text>" . "</office:text>")))))
    (or (plist-get (alist-get backend config) property)
	(plist-get (alist-get nil config) property))))


;;;; Inner Template

(defun org-odt-inner-template (contents info)
  "Return body of document string after ODT conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* (;; `org-display-custom-times' should be accessed right
	 ;; within the context of the Org buffer.  So obtain its
	 ;; value before moving on to temp-buffer context down below.
	 (custom-time-fmts
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
                  (org-odt-get-backend-property org-export-current-backend :content-template-file))
		 ((file-name-absolute-p file) file)
		 (t (expand-file-name
		     file (file-name-directory (plist-get info :input-file))))))))
	 (message "ox-odt: Content template file is %s" content-template-file)
	 content-template-file))
      ;; Write automatic styles.
      ;; - Position the cursor.
      (goto-char (point-min))
      (re-search-forward "<office:automatic-styles>" nil t)
      (goto-char (match-end 0))
      (newline)

      ;; - Dump these automatic styles:
      (cl-loop for table-cell-style in
	       (cl-loop with cell-preferences = nil
			for x in `(("OrgTable%sCell" "0.002cm solid #000000" "fo:padding=\"0.159cm\"")
				   ("CustomTable%sCell" nil
				    ,(mapconcat #'identity
						'("fo:background-color=\"transparent\""
						  "fo:border-bottom=\"0.002cm solid #000000\""
						  "fo:border-left=\"0.002cm solid #000000\""
						  "fo:border-right=\"0.002cm solid #000000\""
						  "fo:border-top=\"0.002cm solid #000000\""
						  "fo:padding=\"0.097cm\"")
						" ")))
			appending (cl-loop for variant in
					   '("" "FirstColumn" "LastColumn"
					     "FirstRow" "LastRow"
					     "EvenRow" "OddRow"
					     "EvenColumn" "OddColumn")
					   for y = (copy-tree x) do
					   (let* ((base-style (format (car y) variant))
						  (cell-props (plist-get (assoc-default base-style cell-preferences)
									 :table-cell-properties)))
					     (setcar y base-style)
					     (when cell-props (setcar (last y) cell-props)))
					   collect y))
	       do (insert (apply #'org-odt--table-cell-build-table-cell-styles table-cell-style)))

      ;;   1. styles specified with "#+ODT_AUTOMATIC_STYLES: ..."
      ;;   2. paragraph styles that request pagebreaks
      ;;   3. table styles that specify `:rel-width'
      (insert "\n<!-- BEGIN: OFFICE:AUTOMATIC-STYLES -->\n")
      (insert (org-element-normalize-string (or (plist-get info :odt-automatic-styles) "")))

      ;; - Dump automatic styles for paragraphs within a table.  See
      ;;   `org-odt--table-cell-get-paragraph-style'.
      (let* ((p-styles (cl-delete-duplicates
			(org-element-map (plist-get info :parse-tree) 'table
			  (lambda (table)
			    (org-odt--read-attribute table :p-style))
			  info)
                        :test 'string=)))
	(cl-loop for p-style in p-styles do
		 (when p-style
		   (cl-loop for cell-type in '("Heading" "Contents") do
			    (cl-loop for cell-alignment in '(left right center) do
				     (insert
				      (format "
  <style:style style:name=\"%s\" style:family=\"paragraph\" style:parent-style-name=\"%s\">
    <style:paragraph-properties fo:text-align=\"%s\" style:justify-single-word=\"false\"/>
  </style:style>
"
					      (concat p-style cell-type
						      (capitalize (symbol-name cell-alignment)))
					      (concat p-style cell-type)
					      (assoc-default cell-alignment
							     '((left . "start")
							       (right . "end")
							       (center . "center"))))))))))

      ;; - Dump date-styles.
      (when (or t (plist-get info :odt-use-date-fields))
	(insert (org-odt--build-date-styles (car custom-time-fmts)
					    "OrgDate1")
		(org-odt--build-date-styles (cdr custom-time-fmts)
					    "OrgDate2")))
      (insert "\n<!-- END: OFFICE:AUTOMATIC-STYLES -->\n")

      ;; Update display level.
      ;; - Remove existing sequence decls.  Also position the cursor.
      (unless (eq org-export-current-backend 'ods)
        (goto-char (point-min))
        (when (re-search-forward "<text:sequence-decls" nil t)
	  (delete-region (match-beginning 0)
		         (re-search-forward "</text:sequence-decls>" nil nil)))
        (insert "\n<!-- BEGIN: TEXT:SEQUENCE-DECLS -->\n")
        ;; Update sequence decls according to user preference.
        (insert
         (format
	  "\n<text:sequence-decls>%s\n</text:sequence-decls>"
	  (cl-loop for (_category . category-props) in org-odt-caption-and-numbering-settings concat
		   (format "\n<text:sequence-decl text:display-outline-level=\"%d\" text:name=\"%s\"/>"
			   (if (plist-get category-props :use-outline-levelp)
			       (string-to-number (plist-get info :odt-display-outline-level))
			     0)
			   (plist-get category-props :variable)))))
        (insert "\n<!-- END: TEXT:SEQUENCE-DECLS -->\n"))
      
      ;; Position the cursor to document body.
      (goto-char (point-min))
      (let ((end-tag (cdr (org-odt-get-backend-property org-export-current-backend :content-tags))))
        (re-search-forward end-tag nil nil))
      (goto-char (match-beginning 0))

      (unless (eq org-export-current-backend 'ods)
        ;; Preamble - Title, Author, Date etc.
        (insert "\n<!-- BEGIN: OFFICE:TEXT/METADATA -->\n")
        (insert
         (let* ((author (when (plist-get info :with-author)
			  (let ((data (plist-get info :author)))
			    (org-odt--encode-plain-text
			     (org-element-interpret-data data)))))
	        (_creator (when (plist-get info :with-creator)
			    (let ((data (plist-get info :creator)))
			      (org-odt--encode-plain-text
			       (org-element-interpret-data data)))))
	        (_description (let ((data (plist-get info :description)))
			        (org-odt--encode-plain-text
			         (org-element-interpret-data data))))
	        (email (when (plist-get info :with-email)
		         (plist-get info :email)))
	        (date (when (plist-get info :with-date)
		        (let ((date (plist-get info :date)))
			  (when date
			    (let ((timestamp
				   (unless (cdr date)
				     (when (eq (org-element-type (car date)) 'timestamp)
				       (car date)))))
			      (or (when (and (plist-get info :odt-use-date-fields)
					     timestamp)
				    (org-odt--format-timestamp timestamp)))
			      (org-export-data (plist-get info :date) info))))))
	        (_keywords (let ((data (plist-get info :keywords)))
			     (org-odt--encode-plain-text
			      (org-element-interpret-data data))))
	        (subtitle (let ((data (plist-get info :subtitle)))
			    (org-odt--encode-plain-text
			     (org-element-interpret-data data))))
	        (title (when (plist-get info :with-title)
		         (let ((data (plist-get info :title)))
			   (org-odt--encode-plain-text
			    (org-element-interpret-data data))))))
	   (concat
	    ;; Title.
	    (when (org-string-nw-p title)
	      (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		      "OrgTitle" (format "\n<text:title>%s</text:title>" title)))
	    ;; Subtitle.
	    (when (org-string-nw-p subtitle)
	      (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		      "OrgSubtitle"
		      (format "<text:user-defined text:name=\"Subtitle\">%s</text:user-defined>"
			      subtitle)))
	    ;; Author and E-mail.
	    (when (org-string-nw-p author)
	      (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		      "OrgDocInfo"
		      (let ((author (format "<text:initial-creator>%s</text:initial-creator>" author)))
		        (or (when email
			      (format "<text:a xlink:type=\"simple\" xlink:href=\"%s\">%s</text:a>"
				      email author))
			    author))))
	    ;; Date
	    (when (org-string-nw-p date)
	      (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		      "OrgDocInfo" date)))))
        (insert "\n<!-- END: OFFICE:TEXT/METADATA -->\n")      
        ;; Table of Contents
        (let* ((with-toc (plist-get info :with-toc))
	       (depth (and with-toc (if (wholenump with-toc)
				        with-toc
				      (plist-get info :headline-levels)))))
	  (when depth (insert (or (org-odt-toc depth info) "")))))
      ;; Contents.
      (insert "\n<!-- BEGIN: OFFICE:TEXT/CONTENTS -->\n")
      (insert contents)
      (insert "\n<!-- END: OFFICE:TEXT/CONTENTS -->\n")
      
      ;; Prettify buffer contents, if needed.
      (org-odt-prettify-xml-buffer (plist-get info :odt-prettify-xml))
      ;; Cleanup, when export is body-only.
      (when (memq 'body-only (plist-get info :export-options))
	(org-odt-cleanup-xml-buffers nil nil info))
      ;; Return contents.
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;; Write content.xml

(defun org-odt-write-contents-file (contents _backend info)
  (with-temp-buffer
    (insert contents)
    ;; Write content.xml.
    (let ((coding-system-for-write 'utf-8))
      (write-region nil nil
       (concat (plist-get info :odt-zip-dir) "content.xml")))
    ;; Create a manifest entry for content.xml.
    (org-odt-create-manifest-file-entry info "text/xml" "content.xml")))


;;;; Write styles.xml

(defun org-odt-write-styles-file (_contents _backend info)
  (let* ((styles-file
	  (let ((file (plist-get info :odt-styles-file)))
	    (cond
	     ((or (not file) (string= file ""))
              (org-odt-get-backend-property org-export-current-backend :styles-file))
	     ((file-name-absolute-p file) file)
	     (t (expand-file-name
		 file (file-name-directory (plist-get info :input-file)))))))
	 (styles-file-type (file-name-extension styles-file))
	 (extra-images (plist-get info :odt-extra-images)))

    ;; Complain if the styles file doesn't exist.
    (unless (file-readable-p styles-file)
      (user-error "Cannot read styles file: %s" styles-file))

    (message "ox-odt: Styles file is %s" styles-file)

    ;; Check the type of styles file.
    (pcase styles-file-type
      ;; If it is of type `odt' or `ott' (i.e., a zip file), then the
      ;; styles.xml within the zip file becomes the styles.xml of the
      ;; target file.  Extra images, if any, also comes from within
      ;; this zip file.
      ((or "odt" "ott")
       (let ((archive styles-file)
	     (members (cons "styles.xml" extra-images)))
	 (org-odt--zip-extract archive members (plist-get info :odt-zip-dir))
	 (dolist (member members)
	   (when (org-file-image-p member)
	     (let* ((image-type (file-name-extension member))
		    (media-type (format "image/%s" image-type)))
	       (org-odt-create-manifest-file-entry info media-type member))))))
      ;; If it is of type `xml', then it becomes the styles.xml of the
      ;; target file.  Extra images, if any, comes from the user's
      ;; file system.
      ("xml"
       ;; User-provided styles-file could be read only, but we want the
       ;; copy of this file in zip dir to be writable.  So, first create
       ;; an "empty" styles file in the zip dir and then overwrite it
       ;; with the user-provided styles file.
       (with-temp-buffer
         (write-region nil nil (concat (plist-get info :odt-zip-dir) "styles.xml")))
       (copy-file styles-file (concat (plist-get info :odt-zip-dir) "styles.xml") t)
       ;; Some styles.xml elements (like
       ;; "<draw:fill-image>...</draw:fill-image>") may reference images.
       ;; Such images are specified with #+ODT_EXTRA_IMAGES: ... lines.
       ;; Copy over these image files to the exported file.
       (dolist (path extra-images)
	 (let* ((input-dir (file-name-directory (plist-get info :input-file)))
		(full-path (if (file-name-absolute-p path) path
			     (expand-file-name path input-dir)))
		(target-path (file-relative-name path input-dir)))
	   (org-odt--copy-image-file info full-path target-path))))
      (_ (error "Styles file is invalid: %s" styles-file)))

    ;; create a manifest entry for styles.xml
    (org-odt-create-manifest-file-entry info "text/xml" "styles.xml")

    ;; Update styles.xml
    (with-temp-buffer
      (insert-file-contents (concat (plist-get info :odt-zip-dir) "styles.xml"))

      ;; Position the cursor.
      (goto-char (point-min))
      (when (re-search-forward "</office:master-styles>" nil t)
	(goto-char (match-beginning 0)))

      ;; Write master styles.
      (insert (or (org-element-normalize-string (plist-get info :odt-master-styles)) ""))
      
      ;; Position the cursor.
      ;; (goto-char (point-min))
      ;; (when (re-search-forward "</office:automatic-styles>" nil t)
      ;; 	(goto-char (match-beginning 0)))

      ;; Position the cursor.
      (goto-char (point-min))
      ;; When there are duplicate page layouts with same name defined,
      ;; LibreOffice seems to go with the first definition it
      ;; encounters.  So, inserting at the beginning of automatic
      ;; styles section, gives us the flexibility of redefining page
      ;; layouts using '#+ODT_EXTRA_AUTOMATIC_STYLES: ...'.  You can
      ;; see this in action in doc/org-odt-manual/list-table-1.org etc
      ;; where I change the page height to create an image file that
      ;; is aesthetically pleasing for inclusion in the manuals.
      (when (re-search-forward "<office:automatic-styles>" nil t)
	(goto-char (match-end 0)))

      ;; Write automatic styles.
      (insert (or (org-element-normalize-string (plist-get info :odt-extra-automatic-styles)) ""))

      ;; Write custom styles for source blocks
      ;; Save STYLES used for colorizing of source blocks.
      ;; Update styles.xml with styles that were collected as part of
      ;; `org-odt-hfy-face-to-css' callbacks.
      (goto-char (point-min))
      (when (re-search-forward "</office:styles>" nil t)
	(goto-char (match-beginning 0))
	(insert "\n<!-- Org Htmlfontify Styles -->\n"
		(cl-loop for style in (plist-get info :odt-hfy-user-sheet-assoc)
			 concat (format " %s\n" (cddr style)))
		"\n"))

      ;; Position the cursor.
      (goto-char (point-min))
      (when (re-search-forward "</office:styles>" nil t)
	(goto-char (match-beginning 0)))

      ;;    Style structure when                  Style structure when
      ;;    `language' feature is OFF             `language' feature is ON
      ;;                        
      ;;                                                                                    o 
      ;;                                                                                   /|\
      ;;                                                                                    |
      ;;                                                                                 ORG FILE
      ;;                                                                                    |
      ;;                                                                                    |
      ;;                                                                                    |
      ;;            O OrgUser                               O OrgUser                       |   OrgUser =>  User sets this style in Org file
      ;;            |                                       |                               |               through #+ODT_EXTRA_STYLES:
      ;;            |                                       |                               |
      ;;            |                                       |                               |
      ;;    ========|=======================================|=======================================
      ;;            |                                       |                               o
      ;;            |                                       |                              /|\
      ;;            |                                       |                               |
      ;;            |                                       |                             EXPORTER 
      ;;            |                                       +------> OrgLocale              |     
      ;;            |                                              |                        |     
      ;;            |                                              |                        |    OrgLocale => Exporter introduces this style     
      ;;            |                                              |                        |                 to honour the LANGUAGE KEYWORD
      ;;            |                                              |                       \|/
      ;;            |                                              |                        o
      ;;    ========|==============================================|==================================
      ;;            |                                              |                        |
      ;;            |                                              |                        |
      ;;            |                                              |                        |
      ;;            +----> Standard                                +----> Standard          |     Standard => Defined in OrgOdtStyles.xml. 
      ;;                                                                                    |                 The root style from which
      ;;                                                                                    |                 all other styles inherit from.
      ;;                                                                                    |
      ;;                                                                                    |
      ;;                                                                                   STYLES.XML
      ;;                                                                                    |
      ;;                                                                                   \|/
      ;;                                                                                    o
      ;; 

      ;; Write the default LANGUAGE for body text.
      (if (not (memq 'language org-odt-experimental-features))
	  (insert "
<style:style style:name=\"Standard\" style:family=\"paragraph\" style:parent-style-name=\"OrgUser\"/>
")
	(insert (or
		 (let* ((lang-or-locale (plist-get info :language))
			;; Convert user-string to canonical format with hyphen as separator.
			(locale (replace-regexp-in-string "-" "_" lang-or-locale t t))
			(entry (when locale
				 (assoc locale org-odt-locales-alist))))
		   (let* ((entry (or
				  ;; LOCALE-CODE is well-known.
				  entry
				  ;; LOCALE-CODE is not known to us.
				  ;;
				  ;; It is most likely a LOCALE-CODE in degenerate form.  That is, it
				  ;; is most likely a LANG-CODE without any COUNTRY-CODE.
				  ;;
				  ;; In thi case, look up LANG-CODE in our locales list and map it to
				  ;; the first locale that shares the user-specified language.
				  (cl-some (lambda (it)
					     (when (string-prefix-p (format "%s_" lang-or-locale) (car it))
					       it))
					   org-odt-locales-alist)))
			  ;; Split LOCALE-CODE in to component parts.
			  (country (when entry (cadr (split-string (car entry) "_"))))
			  (lang (when entry (car (split-string (car entry) "_"))))
			  (script-type (when entry (nth 2 entry))))
		     (when script-type
		       (format "
<style:style style:name=\"OrgLocale\" style:family=\"paragraph\" style:parent-style-name=\"OrgUser\">
  <style:text-properties %s/>
</style:style>
<style:style style:name=\"Standard\" style:family=\"paragraph\" style:parent-style-name=\"OrgLocale\"/>
"
			       (mapconcat #'identity
					  (list
					   (format "fo:language=\"%s\" fo:country=\"%s\""
						   (if (eq script-type 'western) lang "none")
						   (if (eq script-type 'western) country "none"))
					   (format "style:language-asian=\"%s\" style:country-asian=\"%s\""
						   (if (eq script-type 'cjk) lang "none")
						   (if (eq script-type 'cjk) country "none"))
					   (format "style:language-complex=\"%s\" style:country-complex=\"%s\""
						   (if (eq script-type 'ctl) lang "none")
						   (if (eq script-type 'ctl) country "none")))
					  " ")))))
		 "")))

      ;; Write extra styles.
      (cl-loop for p-style in '("OrgTable" "CustomTable")
	       do (insert (org-odt--table-cell-build-paragraph-styles p-style)))
      ;;
      ;; Note: If the user has defined the `Standard' style in ODT_EXTRA_STYLES then that is what is
      ;; effective in the final exported document.  What this means is that when the EXPERIMENTAL
      ;; `language' feature is ON, the re-definition of `Standard' via `OrgLocale' inserted by the
      ;; exporter gets overridden.  This behavour is deliberate so that existing org files continue
      ;; to function in the same way as they were well-before the availability of the EXPERIMENTAL
      ;; `language' feature.
      ;;
      ;; A WORD OF ADVICE TO THE USER: Don't define `Standard' style in ODT_EXTRA_STYLES.  Instead
      ;; define the style `OrgUser'.  This way you can enable have best of both worlds: custom
      ;; inline definition of styles, AND setting up of locale according to LANGUAGE keyword in the
      ;; document.
      (insert (or (org-element-normalize-string (plist-get info :odt-extra-styles)) ""))

      ;; Update styles.xml - take care of outline numbering

      ;; Don't make automatic backup of styles.xml file. This setting
      ;; prevents the backed-up styles.xml file from being zipped in to
      ;; odt file. This is more of a hackish fix. Better alternative
      ;; would be to fix the zip command so that the output odt file
      ;; includes only the needed files and excludes any auto-generated
      ;; extra files like backups and auto-saves etc etc. Note that
      ;; currently the zip command zips up the entire temp directory so
      ;; that any auto-generated files created under the hood ends up in
      ;; the resulting odt file.
      (setq-local backup-inhibited t)

      ;; Outline numbering is retained only upto LEVEL.
      ;; To disable outline numbering pass a LEVEL of 0.

      (goto-char (point-min))
      (let ((regex
	     "<text:outline-level-style\\([^>]*\\)text:level=\"\\([^\"]*\\)\"\\([^>]*\\)>")
	    (replacement
	     "<text:outline-level-style\\1text:level=\"\\2\" style:num-format=\"\">"))
	(while (re-search-forward regex nil t)
	  (unless (let ((sec-num (plist-get info :section-numbers))
			(level (string-to-number (match-string 2))))
		    (if (wholenump sec-num) (<= level sec-num) sec-num))
	    (replace-match replacement t nil))))
      ;; Prettify buffer contents, if needed
      (org-odt-prettify-xml-buffer (plist-get info :odt-prettify-xml))
      ;; Write styles.xml
      (let ((coding-system-for-write 'utf-8))
	(write-region nil nil (concat (plist-get info :odt-zip-dir) "styles.xml"))))))


;;;; Write meta.xml

(defun org-odt-write-meta-file (_contents _backend info)
  (let ((author (when (plist-get info :with-author)
		  (let ((data (plist-get info :author)))
		    (org-odt--encode-plain-text
		     (org-element-interpret-data data)))))
	(creator (when (plist-get info :with-creator)
		   (let ((data (plist-get info :creator)))
		     (org-odt--encode-plain-text
		      (org-element-interpret-data data)))))
	(description (let ((data (plist-get info :description)))
		       (org-odt--encode-plain-text
			(org-element-interpret-data data))))
	(email (when (plist-get info :with-email)
		 (plist-get info :email)))
	(iso-date (when (plist-get info :with-date)
		    (let ((date (plist-get info :date)))
		      (when (and (not (cdr date))
				 (eq (org-element-type (car date)) 'timestamp))
			(org-odt--format-timestamp (car date) nil 'iso-date)))))
	(keywords (let ((data (plist-get info :keywords)))
		    (org-odt--encode-plain-text
		     (org-element-interpret-data data))))
	(subtitle (let ((data (plist-get info :subtitle)))
		    (org-odt--encode-plain-text
		     (org-element-interpret-data data))))
	(title (when (plist-get info :with-title)
		 (let ((data (plist-get info :title)))
		   (org-odt--encode-plain-text
		    (org-element-interpret-data data))))))
    (with-temp-buffer
      (insert
       (concat
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
     <office:document-meta
         xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\"
         xmlns:xlink=\"http://www.w3.org/1999/xlink\"
         xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
         xmlns:meta=\"urn:oasis:names:tc:opendocument:xmlns:meta:1.0\"
         xmlns:ooo=\"http://openoffice.org/2004/office\"
         office:version=\"1.2\">
       <office:meta>\n"
	(concat
	 ;; Order the items here by their XML element name.
	 (when (org-string-nw-p author)
	   (format "<dc:creator>%s</dc:creator>\n" author))
	 (when (org-string-nw-p iso-date)
	   (format "<dc:date>%s</dc:date>\n" iso-date))
	 (when (org-string-nw-p description)
	   (format "<dc:description>%s</dc:description>\n" description))
	 (when (org-string-nw-p description)
	   (format "<dc:subject>%s</dc:subject>\n" description))
	 (when (org-string-nw-p title)
	   (format "<dc:title>%s</dc:title>\n" title))
	 (when (org-string-nw-p iso-date)
	   (format "<meta:creation-date>%s</meta:creation-date>\n" iso-date))
	 (when (org-string-nw-p creator)
	   (format "<meta:generator>%s</meta:generator>\n" creator))
	 (when (org-string-nw-p author)
	   (format "<meta:initial-creator>%s</meta:initial-creator>\n" author))
	 (when (org-string-nw-p keywords)
	   (format "<meta:keyword>%s</meta:keyword>\n" keywords))
	 (when (org-string-nw-p email)
	   (format "<meta:user-defined meta:name=\"E-Mail\">%s</meta:user-defined>\n" email))
	 (when (org-string-nw-p subtitle)
	   (format "<meta:user-defined meta:name=\"Subtitle\">%s</meta:user-defined>\n" subtitle))
	 ;; Additional user-defined entries.
	 (plist-get info :odt-extra-meta))
	"  </office:meta>\n" "</office:document-meta>"))
      ;; Prettify buffer contents, if needed
      (org-odt-prettify-xml-buffer (plist-get info :odt-prettify-xml))
      ;; Write meta.xml.
      (let ((coding-system-for-write 'utf-8))
	(write-region nil nil (concat (plist-get info :odt-zip-dir) "meta.xml"))))

    ;; Add meta.xml in to manifest.
    (org-odt-create-manifest-file-entry info "text/xml" "meta.xml")))

;;;; Write mimetype

(defun org-odt-write-mimetype-file (_contents _backend info)
  (let* ((mimetype
	  (let ((ext (file-name-extension (plist-get info :output-file))))
	    (unless (member ext org-odt-supported-file-types)
	      (setq ext "odt"))
	    (nth 1 (assoc-string ext org-odt-file-extensions-alist)))))
    (let ((coding-system-for-write 'utf-8))
      (write-region mimetype nil (concat (plist-get info :odt-zip-dir) "mimetype")))
    (org-odt-create-manifest-file-entry info mimetype "/" "1.2")))

;;;; Write manifest.xml

(defun org-odt-write-manifest-file (_contents _backend info)
  (make-directory (concat (plist-get info :odt-zip-dir) "META-INF"))
  (with-temp-buffer
    (insert
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
     <manifest:manifest xmlns:manifest=\"urn:oasis:names:tc:opendocument:xmlns:manifest:1.0\" manifest:version=\"1.2\">\n")
    (dolist (file-entry (plist-get info :odt-manifest-file-entries))
      (let* ((version (nth 2 file-entry))
	     (extra (if (not version) ""
		      (format " manifest:version=\"%s\"" version))))
	(insert
	 (format org-odt-manifest-file-entry-tag
		 (nth 0 file-entry) (nth 1 file-entry) extra))))
    (insert "\n</manifest:manifest>")
    ;; Prettify buffer contents, if needed
    (org-odt-prettify-xml-buffer (plist-get info :odt-prettify-xml))
    ;; Write manifest.xml
    (let ((coding-system-for-write 'utf-8))
      (write-region nil nil (concat (plist-get info :odt-zip-dir) "META-INF/manifest.xml")))))

;;;; Prettify XML files

(defun org-odt-prettify-xml-files-maybe (_contents _backend info)
  (when org-odt-prettify-xml
    (dolist (file '("META-INF/manifest.xml" "content.xml" "meta.xml" "styles.xml"))
      (setq file (concat (plist-get info :odt-zip-dir) file))
      (when (file-readable-p file)
	(with-temp-buffer
	  (insert-file-contents file)
	  ;; Prettify output if needed.
	  (indent-region (point-min) (point-max))
	  (let ((coding-system-for-write 'utf-8))
	    (write-region nil nil file)))))))

;;;; Zip XML files to OpenDocument format

(defun org-odt-zip (_contents _backend info)
  ;; Ensure that the program named zip is available
  (unless (executable-find "zip")
    ;; Not at all OSes ship with zip by default
    (error "Executable \"zip\" needed for creating OpenDocument files"))
  ;; Run zip.
  (let* ((target (plist-get info :output-file))
	 (target-name (file-name-nondirectory target))
	 (cmds `(("zip" "-mX0" ,target-name "mimetype")
		 ("zip" "-rmTq" ,target-name "."))))
    ;; If a file with same name as the desired output file
    ;; exists, remove it.
    (when (file-exists-p target)
      (delete-file target))
    ;; Zip up the xml files.
    (let ((coding-system-for-write 'no-conversion) exitcode err-string)
      (message "Create OpenDocument file `%s'..." target)
      ;; Switch temporarily to content.xml.  This way Zip
      ;; process will inherit `org-odt-zip-dir' as the current
      ;; directory.
      (with-current-buffer
	  (find-file-noselect (concat (plist-get info :odt-zip-dir) "content.xml") t)
	(dolist (cmd cmds)
	  (message "Running %s" (mapconcat 'identity cmd " "))
	  (setq err-string
		(with-output-to-string
		  (setq exitcode
			(apply 'call-process (car cmd)
			       nil standard-output nil (cdr cmd)))))
	  (or (zerop exitcode)
	      (error (concat "Unable to create OpenDocument file."
			     "  Zip failed with error (%s)")
		     err-string)))))
    ;; Move the zip file from temporary work directory to
    ;; user-mandated location.
    (rename-file (concat (plist-get info :odt-zip-dir) target-name) target)
    (message "Created %s" (expand-file-name target))
    ;; Cleanup work directory and work files.
    (org-odt-cleanup-xml-buffers nil nil info)
    target))

;;;; Cleanup temp files

(defun org-odt-cleanup-xml-buffers (target _backend info)
  (prog1 target
    ;; Kill all XML buffers.
    (dolist (file '("META-INF/manifest.xml" "content.xml" "meta.xml" "styles.xml"))
      (let ((buf (find-buffer-visiting
		  (concat (plist-get info :odt-zip-dir) file))))
	(when buf
	  (with-current-buffer buf
	    (set-buffer-modified-p nil)
	    (kill-buffer buf)))))
    ;; Delete temporary directory and also other embedded
    ;; files that get copied there.
    (delete-directory (plist-get info :odt-zip-dir) t)))

;;;; Wrapper for File Validation

(defun org-odt--validate-target (target _backend info)
  (let* ((validate (let ((v (plist-get info :odt-validate)))
		     (cl-assert (member v '(nil "" "noabort" "abort")))
		     v)))
    (prog1 target
      (when (org-string-nw-p validate)
	(org-odt-validate target (string= "abort" validate))))))

;;;; Wrapper for File Transformation

(defun org-odt--transform-target (target _backend _info)
  (org-odt-transform target))

;;;; Wrapper for File Conversion

(defun org-odt--convert (target _backend info)
  (condition-case-unless-debug err
      (let ((output-fmt (plist-get info :odt-preferred-output-format)))
	(if (org-string-nw-p output-fmt)
	    (org-odt-convert target output-fmt)
	  target))
    (error (message (error-message-string err))
	   target)))

;;;; Outer Template

(defun org-odt-template (contents info)
  "Return complete document string after ODT conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (condition-case-unless-debug err
      (cl-reduce (lambda (target f)
		   (funcall f target nil info))
		 '(org-odt--validate-target
		   org-odt--transform-target
		   org-odt--convert)
		 :initial-value
		 (cl-reduce (lambda (contents f)
			      (funcall f contents nil info))
			    '(org-odt-write-contents-file
			      org-odt-write-styles-file
			      org-odt-write-meta-file
			      org-odt-write-mimetype-file
			      org-odt-write-manifest-file
			      ;; org-odt-prettify-xml-files-maybe
			      org-odt-zip
			      org-odt-cleanup-xml-buffers)
			    :initial-value contents))

    ((error)
     ;; Cleanup work directory and work files.
     (org-odt-cleanup-xml-buffers nil nil info)
     (error "OpenDocument export failed with error: `%s'"
	    (error-message-string err)))))



;;; Transcode Functions

;;;; Bold

(defun org-odt-bold (bold contents _info)
  "Transcode BOLD from Org to ODT.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  ;; Internally, `org-odt--translate-description-lists/html'
	  ;; or `org-odt--translate-description-lists/latex' requests
	  ;; a custom style for bold.
	  (or (org-element-property :style bold) "Bold")
	  contents))


;;;; Center Block

(defun org-odt-center-block (_center-block contents _info)
  "Transcode a CENTER-BLOCK element from Org to ODT.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  contents)


;;;; Clock

(defun org-odt-clock (clock contents info)
  "Transcode a CLOCK element from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((timestamp (org-element-property :value clock))
	(duration (org-element-property :duration clock)))
    (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
	    (if (eq (org-element-type (org-export-get-next-element clock info))
		    'clock) "OrgClock" "OrgClockLastLine")
	    (concat
	     (format "<text:span text:style-name=\"%s\">%s</text:span>"
		     "OrgClockKeyword" org-clock-string)
	     (org-odt-timestamp timestamp contents info)
	     (and duration (format " (%s)" duration))))))


;;;; Code

(defun org-odt-code (code _contents _info)
  "Transcode a CODE object from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "OrgCode" (org-odt--encode-plain-text
		     (org-element-property :value code))))


;;;; Comment

;; Comments are ignored.


;;;; Comment Block

;; Comment Blocks are ignored.


;;;; Drawer

(defun org-odt-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to ODT.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-property :drawer-name drawer))
	 (output (funcall (plist-get info :odt-format-drawer-function)
			  name contents)))
    output))


;;;; Dynamic Block

(defun org-odt-dynamic-block (_dynamic-block contents _info)
  "Transcode a DYNAMIC-BLOCK element from Org to ODT.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  contents)


;;;; Entity

(defun org-odt-entity (entity _contents _info)
  "Transcode an ENTITY object from Org to ODT.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (org-element-property :utf-8 entity))


;;;; Example Block

(defun org-odt-example-block (example-block _contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-odt-format-code example-block info))


;;;; Export Snippet

(defun org-odt-export-snippet (export-snippet _contents _info)
  "Transcode a EXPORT-SNIPPET object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'odt)
    (org-element-property :value export-snippet)))


;;;; Export Block

(defun org-odt-export-block (export-block _contents _info)
  "Transcode a EXPORT-BLOCK element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "ODT")
    (org-remove-indentation (org-element-property :value export-block))))


;;;; Fixed Width

(defun org-odt-fixed-width (fixed-width _contents info)
  "Transcode a FIXED-WIDTH element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-odt-do-format-code (org-element-property :value fixed-width) info))

(defun org-odt--endnote-p (footnote-reference info)
  (let ((label (org-element-property :label footnote-reference)))
    (when (stringp label)
      (string-match-p (plist-get info :odt-endnote-regexp) label))))

;;;; Footnote Definition

;; Footnote Definitions are ignored.


;;;; Footnote Reference

(defun org-odt--format-footnote-definition (n def &optional note-class)
  (format
   "<text:note %s text:note-class=\"%s\">%s</text:note>"
   (if n (format "text:id=\"fn%d\"" n) "")
   (or note-class 'footnote)
   (concat
    (format "<text:note-citation>%d</text:note-citation>" (or n 0))
    (format "<text:note-body>%s</text:note-body>" def))))

(defun org-odt-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((--format-footnote-anchor
	  (lambda (anchor note-class)
	    (format (plist-get info (cl-case note-class
				      (footnote :odt-footnote-anchor-format)
				      (endnote :odt-endnote-anchor-format)))
		    anchor)))
	 (--format-footnote-reference
	  (lambda (n note-class)
	    (setq n (format "%d" n))
	    (let ((ref-format "text")
		  (ref-name (concat "fn" n)))
	      (funcall --format-footnote-anchor
		       (format "<text:note-ref text:note-class=\"%s\" text:reference-format=\"%s\" text:ref-name=\"%s\">%s</text:note-ref>"
			       note-class ref-format ref-name n)
		       note-class))))
	 (note-class (if (org-odt--endnote-p footnote-reference info) 'endnote 'footnote))
	 (prevp (eq (org-element-type (org-export-get-previous-element footnote-reference info)) 'footnote-reference))
	 (nextp (eq (org-element-type (org-export-get-next-element footnote-reference info)) 'footnote-reference)))
    (concat
     ;; Insert a opening brace before the first footnote reference in a cluster.
     (and (not prevp) ; not preceded by a footnote reference
	  (car (plist-get info (cl-case note-class
				 (footnote :odt-footnote-braces)
				 (endnote :odt-endnote-braces)))))
     ;; Insert separator between two footnotes in a row.
     (and prevp			; preceded by a footnote reference
	  (plist-get info (cl-case note-class
			    (footnote :odt-footnote-separator)
			    (endnote :odt-endnote-separator))))
     ;; Transcode footnote reference.
     (let ((n (org-export-get-footnote-number footnote-reference info nil t)))
       (cond
	((not
	  (org-export-footnote-first-reference-p footnote-reference info nil t))
	 (funcall --format-footnote-reference n note-class))
	;; Inline definitions are secondary strings.
	;; Non-inline footnotes definitions are full Org data.
	(t
	 (let* ((raw (org-export-get-footnote-definition
		      footnote-reference info))
		(def
		 (let ((def (org-trim (org-export-data raw info))))
		   (if (eq (org-element-class (car (org-element-contents raw)))
			   'element)
		       def
		     (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
			     (assoc-default note-class '((endnote . "Endnote")
							 (footnote . "Footnote")))
			     def)))))
	   (funcall --format-footnote-anchor
		    (org-odt--format-footnote-definition n def note-class)
		    note-class)))))
     ;; Insert a closing brace after the last footnote reference in a cluster.
     (and (not nextp) ; not followed by a footnote reference
	  (cdr (plist-get info (cl-case note-class
				 (footnote :odt-footnote-braces)
				 (endnote :odt-endnote-braces))))))))


;;;; Citation Reference

(defun org-odt-citation (citation _contents _info)
  "Transcode a CITATION element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; Just interpret the citation object.
  ;; Citation processors (like ox-jabref.el) may handle citation
  ;; by registering their own transcoders.
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "OrgCode" (org-odt--encode-plain-text
		     (org-element-interpret-data
		      (or (org-element-property :replaces citation)
			  citation)))))

;;;; Headline

(defun org-odt-format-headline-default-function
    (todo todo-type priority text tags)
  "Default format function for a headline.
See `org-odt-format-headline-function' for details."
  (concat
   ;; Todo.
   (when todo
     (let ((style (if (eq todo-type 'done) "OrgDone" "OrgTodo")))
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

(defun org-odt-format-headline--wrap (headline backend info
					       &optional format-function)
  "Transcode a HEADLINE element using BACKEND.
INFO is a plist holding contextual information."
  (let* ((export-data (lambda (data backend info)
			(if backend
			    (org-export-data-with-backend data backend info)
			  (org-export-data data info))))
	 (level (+ (org-export-get-relative-level headline info)))
	 (headline-number (org-export-get-headline-number headline info))
	 (section-number (and (org-export-numbered-headline-p headline info)
			      (mapconcat 'number-to-string
					 headline-number ".")))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo
			   (funcall export-data todo backend info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (text (funcall export-data
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

(defun org-odt-headline (headline contents info)
  "Transcode a HEADLINE element from Org to ODT.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  ;; Case 1: This is a footnote section: ignore it.
  (unless (org-element-property :footnote-section-p headline)
    (let* (;; (_text (org-export-data (org-element-property :title headline) info))
	   ;; Create the headline text.
	   (full-text (org-odt-format-headline--wrap headline nil info))
	   ;; Get level relative to current parsed data.
	   ;; (level (org-export-get-relative-level headline info))
	   ;; Get canonical label for the headline.
	   (id (org-export-get-reference headline info))
	   ;; Get user-specified labels for the headline.
	   (extra-ids (list (org-element-property :CUSTOM_ID headline)
			    (org-element-property :ID headline)))
	   ;; Extra targets.
	   (extra-targets
	    (mapconcat (lambda (x)
			 (when x (org-odt--target
				  "" (if (org-uuidgen-p x) (concat "ID-" x) x))))
		       extra-ids ""))
	   ;; Title.
	   (anchored-title (org-odt--target full-text id)))
      (cond
       ;; Case 2. This is a deep sub-tree: export it as a list item.
       ;;         Also export as items headlines for which no section
       ;;         format has been found.
       ((org-export-low-level-p headline info)
	;; Build the real contents of the sub-tree.
	(concat
	 (and (org-export-first-sibling-p headline info)
	      (format "\n<text:list text:style-name=\"%s\" %s>"
		      ;; Choose style based on list type.
		      (if (org-export-numbered-headline-p headline info)
			  "OrgNumberedList" "OrgBulletedList")
		      ;; If top-level list, re-start numbering.  Otherwise,
		      ;; continue numbering.
		      (format "text:continue-numbering=\"%s\""
			      (let* ((parent (org-export-get-parent-headline
					      headline)))
				(if (and parent
					 (org-export-low-level-p parent info))
				    "true" "false")))))
	 (let ((headline-has-table-p
		(let ((section (assq 'section (org-element-contents headline))))
		  (assq 'table (and section (org-element-contents section))))))
	   (format "\n<text:list-item>\n%s\n%s"
		   (concat
		    (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
			    "Text_20_body"
			    (concat extra-targets anchored-title))
		    contents)
		   (if headline-has-table-p
		       "</text:list-header>"
		     "</text:list-item>")))
	 (and (org-export-last-sibling-p headline info)
	      "</text:list>")))
       ;; Case 3. Standard headline.  Export it as a section.
       (t
	(let* ((format-headline
		(lambda (h hc info i)
		  (let* ((level (org-export-get-relative-level h info))
			 (style (let* ((style (org-odt--read-attribute h :style))
				       (prefix (org-odt--read-attribute h :style-prefix))
				       (suffix (org-odt--read-attribute h :style-suffix))
				       (prefix-i (and i (org-odt--read-attribute i :style-prefix)))
				       (suffix-i (and i (org-odt--read-attribute i :style-suffix))))
				  (cond
				   ((stringp style) style)
				   ((stringp prefix)
				    (concat prefix (number-to-string level) suffix))
				   ((stringp suffix)
				    (concat "Heading_20_" (number-to-string level) suffix))
				   ((stringp prefix-i)
				    (concat prefix-i (number-to-string level) suffix-i))
				   ((stringp suffix-i)
				    (concat "Heading_20_" (number-to-string level) suffix-i))
				   (t (concat "Heading_20_" (number-to-string level)))))))
		    (format
		     "\n<text:h text:style-name=\"%s\" text:outline-level=\"%s\">%s</text:h>"
		     (org-odt--get-derived-paragraph-style h info style) level hc))))
	       (headline-contents (concat extra-targets anchored-title))
	       (immediate-list-style
		(org-odt--read-attribute headline :list-style))
	       (inherit-from
		(let ((h headline) (inherit-from nil))
		  (while (and (null inherit-from) h)
		    (setq inherit-from (or (and (org-odt--read-attribute h :list-style) h)
					   (cl-loop for el in (reverse (org-export-get-previous-element h info t))
						    thereis (and (eq 'headline (org-element-type el))
								 (org-odt--read-attribute el :list-style)
								 el)))
			  h (org-export-get-parent-headline h)))
		  inherit-from))
	       (inherited-list-style (and inherit-from (org-odt--read-attribute inherit-from :list-style)))
	       (text-h (funcall format-headline headline headline-contents info inherit-from)))
	  (if (or (null inherited-list-style) (string= inherited-list-style ""))
	      (concat text-h contents)
	    (concat
	     (format "<text:list xml:id=\"%s\" %s text:style-name=\"%s\"><text:list-item>%s</text:list-item></text:list>"
		     (org-export-get-reference headline info)
		     (if immediate-list-style ""
		       (let ((prev-headline-or-parent
			      (or (cl-loop for el in (reverse (org-export-get-previous-element headline info t))
					   thereis (and (eq 'headline (org-element-type el)) el))
				  (org-export-get-parent-headline headline))))
			 (cl-assert prev-headline-or-parent)
			 (format "text:continue-list=\"%s\""
				 (org-export-get-reference prev-headline-or-parent info))))
		     inherited-list-style
		     (dotimes (_i (- (org-export-get-relative-level headline info) 1) text-h)
		       (setq text-h (format "<text:list><text:list-item>%s</text:list-item></text:list>"
					    text-h))))
	     contents))))))))


;;;; Horizontal Rule

(defun org-odt-horizontal-rule (_horizontal-rule _contents _info)
  "Transcode an HORIZONTAL-RULE  object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
	  "Horizontal_20_Line" ""))


;;;; Inline Babel Call

;; Inline Babel Calls are ignored.


;;;; Inline Src Block

(defun org-odt--find-verb-separator (s)
  "Return a character not used in string S.
This is used to choose a separator for constructs like \\verb."
  (let ((ll "~,./?;':\"|!@#%^&-_=+abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>()[]{}"))
    (cl-loop for c across ll
	     when (not (string-match (regexp-quote (char-to-string c)) s))
	     return (char-to-string c))))

(defun org-odt-inline-src-block (inline-src-block _contents _info)
  "Transcode an INLINE-SRC-BLOCK element from Org to ODT.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((_org-lang (org-element-property :language inline-src-block))
	 (code (org-element-property :value inline-src-block))
	 (_separator (org-odt--find-verb-separator code)))
    (error "FIXME")))


;;;; Inlinetask

(defun org-odt-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to ODT.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword inlinetask)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type inlinetask)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority inlinetask)))
	 (name (org-export-data (org-element-property :title inlinetask) info))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags inlinetask info))))
    (funcall (plist-get info :odt-format-inlinetask-function)
	     todo todo-type priority name tags contents)))

(defun org-odt-format-inlinetask-default-function
    (todo todo-type priority name tags contents)
  "Transcode an INLINETASK element from Org to ODT.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
	  "Text_20_body"
	  (org-odt--textbox
	   (concat
	    (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		    "OrgInlineTaskHeading"
		    (org-odt-format-headline-default-function
		     todo todo-type priority name tags))
	    contents)
	   :width nil :height nil
	   :style "OrgInlineTaskFrame" :extra nil :anchor nil
	   :rel-width 100)))

;;;; Italic

(defun org-odt-italic (_italic contents _info)
  "Transcode ITALIC from Org to ODT.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "Emphasis" contents))


;;;; Item

(defun org-odt-item (item contents info)
  "Transcode an ITEM element from Org to ODT.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((plain-list (org-export-get-parent item))
	 (type (org-element-property :type plain-list))
	 (counter (org-element-property :counter item))
	 (_tag (let ((tag (org-element-property :tag item)))
		 (and tag
		      (concat (org-odt--checkbox item)
			      (org-export-data tag info))))))
    (cl-case type
      ((ordered unordered descriptive-1 descriptive-2)
       (format "\n<text:list-item %s>\n%s\n%s"
	       (if (numberp counter) (format "text:start-value=\"%d\"" counter) "")
	       contents
	       (let* ((--element-has-a-table-p
		       (lambda (element _info)
			 (cl-loop for el in (org-element-contents element)
				  thereis (eq (org-element-type el) 'table)))))
		 (cond
		  ((funcall --element-has-a-table-p item info)
		   "</text:list-header>")
		  (t "</text:list-item>")))))
      (t (error "Unknown list type: %S" type)))))

;;;; Keyword

(defcustom org-odt-indices
  '((:keyword "INDEX"
	      :comment "Default index"
	      :alias "ii" :separator "," :alphap nil :index-name "")
    (:keyword "AINDEX"
	      :comment "Alphabetcal index"
	      :alias "ai" :separator "," :alphap t :index-name "Alphabetical")
    (:keyword "UINDEX"
	      :comment "User-Defined index"
	      :alias "ui" :separator "," :alphap nil :index-name "User-Defined")
    (:keyword "CINDEX"
	      :comment "Concept index, for general concepts"
	      :alias "cp" :separator nil :alphap t :index-name "Concept")
    (:keyword "FINDEX"
	      :comment "Function index, for function and function-like names (such as entry points of libraries)"
	      :alias "fn" :separator nil :alphap nil :index-name "Function")
    (:keyword "KINDEX"
	      :comment "Keystroke index, for keyboard commands"
	      :alias "ky" :separator nil :alphap nil :index-name "Key")
    (:keyword "PINDEX"
	      :comment "Program index, for names of programs"
	      :alias "pg" :separator nil :alphap nil :index-name "Program")
    (:keyword "TINDEX"
	      :comment "Data type index, for type names (such as structures defined in header files)"
	      :alias "tp" :separator nil :alphap nil :index-name "Data Type")
    (:keyword "VINDEX"
	      :comment "Variable index, for variable names (such as library global variables)"
	      :alias "vr" :separator nil :alphap nil :index-name "Variable"))
  "ODT Indices."
  :group 'org-export-odt
  :type '(repeat (list :tag ""
		       ;; Keyword
		       (const :format "" :keyword)
		       (string :tag "Name" :format "%v")
		       ;; Comment
		       (const :format "" :comment)
		       (string :tag "Comment" :format "%v")
		       ;; Alias
		       (const :format "" :alias)
		       (string :tag "PRINT_INDEX name")
		       ;; Separator
		       (const :format "" :separator)
		       (choice :tag "Separator"
			       (const :tag "None" nil)
			       (string :tag "Comma" ",")
			       (string :tag "Other"))
		       ;; Is Alphabetical
		       (const :format "" :alphap)
		       (choice :tag "Is Alphabetical?"
			       (const :tag "No" nil)
			       (const :tag "Yes" t))
		       ;; ODT Index Name
		       (const :format "" :index-name)
		       (string :tag "Index Name used in ODT XML"))))

(defun org-odt--index-keyword->plist (index-keyword info)
  (cl-loop for entry in (plist-get info :odt-indices)
	   when (string= index-keyword (plist-get entry :keyword))
	   return entry))

(defun org-odt--index-name->plist (index-name info)
  (cl-loop for entry in (plist-get info :odt-indices)
	   when (string= index-name (plist-get entry :index-name))
	   return entry))

(defun org-odt--index-alias->plist (alias info)
  (cl-loop for entry in (plist-get info :odt-indices)
	   when (string= alias (plist-get entry :alias))
	   return entry))

(defun org-odt--pop-index-entries (info)
  (let ((indices (plist-get info :index-entries)))
    (prog1 (or indices "")
      (plist-put info :index-entries nil))))

(defun org-odt--push-index-entries (index-entry info)
  (plist-put info :index-entries
	     (concat (or (plist-get info :index-entries) "")
		     index-entry)))

(cl-defun org-odt--format-index-entry (info index-plist &key alttext bodytext mainp)
  (cl-assert index-plist)
  (cl-assert (null bodytext))
  (cl-assert (org-string-nw-p alttext))
  (let* ((index-name (org-odt--encode-plain-text (plist-get index-plist :index-name)))
	 (index-entries
	  (let* ((separator (plist-get index-plist :separator))
		 (entries (when (org-string-nw-p separator)
			    (split-string alttext separator t "[ \t]"))))
	    (mapcar #'org-odt--encode-plain-text entries)))
	 (index-len (length index-entries))
	 (max-indices 3)
	 (key3 (or (nth 0 index-entries)
		   (org-odt--encode-plain-text alttext)))
	 (key1 (nth 1 index-entries))
	 (key2 (nth 2 index-entries)))
    (when (> index-len max-indices)
      (user-error "Number of Index entries (%s) exceeds the maximum supported (%s)"
		  index-len max-indices))
    (cond
      ;; Alphabetical Index
      ((plist-get index-plist :alphap)
       (let* ((extra (mapconcat #'identity
				(list
				 (when key1
				   (format "text:key1=\"%s\"" key1))
				 (when key2
				   (format "text:key2=\"%s\"" key2))
				 (format "text:main-entry=\"%s\"" (if mainp "true" "false"))
				 (when key3
				   (format "text:string-value=\"%s\"" key3)))
				" ")))
	 (cond
	   (alttext
	    (format "<text:alphabetical-index-mark %s/>" extra))
	   (t
	    (let* ((id (org-odt--name-object info 'index-mark)))
	      (concat
	       (format "<text:alphabetical-index-mark-start text:id=\"%s\" %s/>" id extra)
	       bodytext
	       (format "<text:alphabetical-index-mark-end text:id=\"%s\"/>" id)))))))
      ;; User-Defined Index
      (t
       (cond
	 (index-entries
	  (mapconcat #'identity
		     (cl-loop for outline-level from 1 to index-len
			   for i in (assoc-default index-len
						   '((1 . (1))
						     (2 . (2 1))
						     (3 . (2 3 1))))
			   for index-entry = (nth (1- i) index-entries)
			   collect (format "<text:user-index-mark text:string-value=\"%s\" text:index-name=\"%s\" text:outline-level=\"%d\"/>"
					   index-entry index-name outline-level))
		     ""))
	 (t
	  (format "<text:user-index-mark text:string-value=\"%s\" text:index-name=\"%s\" text:outline-level=\"1\"/>"
		  key3 index-name)))))))

(defun org-odt--format-index-entries (element info)
  (mapconcat #'identity
	     (cl-loop for aline in (org-element-property :attr_odt element)
		      for plist = (ignore-errors (when aline (read (format "(%s)" aline))))
		      for alttext = (plist-get plist :index-entry)
		      for index-name = (or (plist-get plist :index-name)
					   (plist-get (car (plist-get info :odt-indices)) :index-name))
		      for index-plist = (org-odt--index-name->plist index-name info)
		      when alttext
		      collect (org-odt--format-index-entry info index-plist
							   :alttext alttext
							   :mainp (plist-get plist :mainp)))
	     ""))

(defun org-odt--format-alphabetical-index (_index-name _info)
  (org-odt--lisp-to-xml
   `(text:alphabetical-index
     ((text:style-name . "Sect1")
      (text:protected . "false")
      (text:name . "Alphabetical Index1"))
     (text:alphabetical-index-source
      ((text:main-entry-style-name . "Main_20_index_20_entry")
       (text:sort-algorithm . "alphanumeric")
       (fo:language . "en")
       (fo:country . "IN"))
      (text:index-title-template
       ((text:style-name . "Index_20_Heading"))
       "Alphabetical Index")
      (text:alphabetical-index-entry-template
       ((text:outline-level . "separator")
	(text:style-name . "Index_20_Separator"))
       (text:index-entry-text nil))
      ,@(cl-loop for level from 1 to 3 collect
		 (org-odt--lisp-to-xml
		  `(text:alphabetical-index-entry-template
		    ((text:outline-level . ,(format "%d" level))
		     (text:style-name . ,(format "Index_20_%d" level)))
		    (text:index-entry-text nil)
		    (text:index-entry-tab-stop
		     ((style:type . "right")
		      (style:leader-char . ".")))
		    (text:index-entry-page-number nil)))))
     (text:index-body nil
		      (text:index-title
		       ((text:style-name . "Sect1")
			(text:name . "Alphabetical Index1_Head"))
		       (text:p
			((text:style-name . "Index_20_Heading"))
			"Alphabetical Index"))))))

(defun org-odt--format-user-defined-index (index-name info)
  (let* ((_style "OrgIndexSection")
	 (object-name (org-odt--name-object info 'index))
	 (index-name (if (org-string-nw-p index-name)
			 (org-trim index-name)
		       "User-Defined"))
	 (index-title (format "%s Index" index-name)))
    (org-odt--lisp-to-xml
     `(text:user-index
       ((text:style-name . "Sect1")
	(text:protected . "false")
	(text:name . ,object-name))
       (text:user-index-source
	((text:use-index-marks . "true")
	 (text:index-name . ;; "User-Defined"
			  ,index-name))
	(text:index-title-template
	 ((text:style-name . "User_20_Index_20_Heading"))
	 ;; "User-Defined Index"
	 ,index-title)
	,@(cl-loop for level from 1 to 10 collect
		   (org-odt--lisp-to-xml
		    `(text:user-index-entry-template
		      ((text:outline-level . ,(format "%d" level))
		       (text:style-name . ,(format "User_20_Index_20_%d" level)))
		      ,@(cl-loop with indent = 0.5
				 for i from 1 to (1- level)
				 collect `(text:index-entry-tab-stop
					   ((style:type . "left")
					    (style:position . ,(format "%scm" (* i indent)))
					    (style:leader-char . " "))))
		      (text:index-entry-text nil)
		      (text:index-entry-tab-stop
		       ((style:type . "right")
			(style:leader-char . ".")))
		      (text:index-entry-link-start
		       ((text:style-name . "Index_20_Link")))
		      (text:index-entry-page-number nil)
		      (text:index-entry-link-end nil)))))
       (text:index-body nil
			(text:index-title
			 ((text:style-name . "Sect1")
			  (text:name . "User-Defined3_Head"))
			 (text:p
			  ((text:style-name . "User_20_Index_20_Heading"))
			  ,index-title)))))))

(defun org-odt--format-index (index-plist info)
  (let* ((index-name (plist-get index-plist :index-name)))
    (cond
     ((plist-get index-plist :alphap)
      (org-odt--format-alphabetical-index index-name info))
     (t (org-odt--format-user-defined-index index-name info)))))

(defun org-odt-keyword (keyword _contents info)
  "Transcode a KEYWORD element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ;; If KEYWORD is part of ODT_DOCUMENT_PROPERTIES add it's name
     ;; and value in meta.xml.
     ((member key (mapcar #'upcase (plist-get info :odt-document-properties)))
      (plist-put info :odt-extra-meta (concat
				       (plist-get info :odt-extra-meta)
				       (org-odt--define-custom-field key value)))
      nil)
     ((string= key "ODT") value)
     ((string= key "TOC")
      (let* ((value (downcase value))
	     (entity-name (when (string-match (rx-to-string
					       '(or "headlines" "tables" "figures" "listings"))
					      value)
			    (match-string 0 value)))
	     (localp (string-match-p "\\<local\\>" value))
	     (rel-depth (when (string-match "[0-9]+" value)
			  (string-to-number (match-string 0 value)))))
	(pcase entity-name
	  ("headlines"
	   (let* ((depth (let ((with-toc (plist-get info :with-toc)))
			   (cond
			    (rel-depth rel-depth)
			    ((wholenump with-toc) with-toc)
			    (t (plist-get info :headline-levels))))))
	     (when (wholenump depth)
	       (org-odt-toc depth info (and localp keyword)))))
	  (_
	   (org-odt-toc/category :category entity-name
				 :localp localp
				 :index-title (org-odt--translate
					       (org-trim (format "List of %s" (capitalize entity-name)))
					       :utf-8 info))))))
     ;; Indices
     (;; (member key '("CINDEX" "FINDEX" "KINDEX" "PINDEX" "TINDEX" "VINDEX"))
      (org-odt--index-keyword->plist key info)
      (let* ((index-plist (org-odt--index-keyword->plist key info)))
	(unless index-plist
	  (user-error "Index keyword %S not defined" key))
	(org-odt--push-index-entries
	 (org-odt--format-index-entry info index-plist :alttext value)
	 info))
      (ignore))
     ;; Print Index
     ((string= key "PRINT_INDEX")
      (let* ((index-plist (org-odt--index-alias->plist value info)))
	(unless index-plist
	  (user-error "Unknown keyword %S in PRINT_INDEX" value))
	(when index-plist
	  (org-odt--format-index index-plist info))))
     ;; Ad-hoc Page break
     ((string= key "PAGEBREAK")

      ;; Pagebreaks created this way are a mere expedience.  These
      ;; create extraneous "empty" paragraphs which take up "extra
      ;; space".  A typographer will chide you for resorting to such
      ;; underhanded means to create pagebreaks.
      ;;
      ;; As an expedience it has it's uses.  See
      ;; `org-odt-special-block' for a realistic example of how
      ;; pagebreak can be to service.
      ;;
      ;; The right way to create pagebreaks is to create new styles -
      ;; custom or automatic - that set the "before/after" pagebreak
      ;; of an element (a paragraph, table etc).
      ;;
      ;; For example, consider pagebreaks created as below.
      ;;
      ;; Text in first page.
      ;;
      ;; #+ATTR_ODT: :style "OrgPageBreakDefault"
      ;; #+PAGEBREAK:
      ;;
      ;; This text goes in next page.

      ;; Now look at the page that is introduced with forced page
      ;; break.  You will realize that the first line of text in that
      ;; page is a bit displaced from other pages created by
      ;; LibreOffice.  A keen eye will definitely catch this
      ;; aberration.

      (unless (org-odt--read-attribute keyword :page-break)
	(org-element-put-property keyword :attr_odt (list ":page-break \"after\"")))

      (org-odt-paragraph keyword "" info)))))


;;;; Latex Environment


;; (eval-after-load 'ox-odt '(ad-deactivate 'org-format-latex-as-mathml))
;; (defadvice org-format-latex-as-mathml	; FIXME
;;   (after org-odt-protect-latex-fragment activate)
;;   "Encode LaTeX fragment as XML.
;; Do this when translation to MathML fails."
;;   (unless (> (length ad-return-value) 0)
;;     (setq ad-return-value (org-odt--encode-plain-text (ad-get-arg 0)))))

(defun org-odt-latex-environment (latex-environment _contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((latex-frag (org-remove-indentation
		      (org-element-property :value latex-environment))))
    (org-odt-do-format-code latex-frag info)))


;;;; Latex Fragment

;; (when latex-frag			; FIXME
;; 	(setq href (propertize href :title "LaTeX Fragment"
;; 				   :description latex-frag)))
;; handle verbatim
;; provide descriptions

(defun org-odt-latex-fragment (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((latex-frag (org-element-property :value latex-fragment))
	 (citation? (org-element-property :replaced-by latex-fragment)))
    (if citation? (org-export-data citation? info)
      (format "<text:span text:style-name=\"%s\">%s</text:span>"
	      "OrgCode" (org-odt--encode-plain-text latex-frag t)))))


;;;; Line Break

(defun org-odt-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "<text:line-break/>")


;;;; Link

;;;; Links :: Label references

(defun org-odt--element-category (element info)
  (let ((category
	 (cl-case (org-element-type element)
	   (table
	    (when (org-odt--enumerable-p element info)
	      :TABLE:))
	   (src-block
	    (when (org-odt--enumerable-p element info)
	      :LISTING:))
	   ((link paragraph)
	    (cond
	     ((org-odt--enumerable-latex-image-p element info)
	      :DVIPNG-IMAGE:)
	     ((org-odt--enumerable-image-p element info)
	      :FIGURE:)
	     ((org-odt--enumerable-formula-p element info)
	      :MATH-FORMULA:))))))
    (when category
      (let ((caption-from
	     (cl-case (org-element-type element)
	       (link (org-export-get-parent-element element))
	       (t element))))
	(or
	 ;; An element can forced to be of a certain category by the
	 ;; use of `:category' property.  For example, the following
	 ;; table will be captioned as if it is a figure.
	 ;;
	 ;; 	   #+NAME: table
	 ;; 	   #+CAPTION: A Table
	 ;; 	   #+ATTR_ODT: :category "figure"
	 ;; 	   | 1 | 2 |
	 ;; 	   | 3 | 4 |
	 ;;
	 ;; This property is particularly useful while typesetting
	 ;; side-by-side figures using a table like the one below.
	 ;;
	 ;; #+NAME: table
	 ;; #+CAPTION: Animals
	 ;; #+ATTR_ODT: :category "figure"
	 ;; #+ATTR_ODT: :list-table t
	 ;; -
	 ;;   -
	 ;;     #+NAME: dog
	 ;;     #+CAPTION: A Dog
	 ;;     [[./dog.png]]
	 ;;   -
	 ;;     #+NAME: goat
	 ;;     #+CAPTION: A Goat
	 ;;     [[./goat.png]]
	 ;;
	 ;; See `org-odt-category-attribute-to-category' for a list of
	 ;; allowed values of `:category'.
	 (assoc-default (org-odt--read-attribute caption-from :category)
			org-odt-category-attribute-to-category)
	 category)))))

(defun org-odt--get-captioned-parent (element info)
  (let ((caption-from
	 (cl-case (org-element-type element)
	   (link (org-export-get-parent-element element))
	   (t element))))
    (cl-loop for parent in (org-element-lineage caption-from)
	     when (org-odt--element-category parent info)
	     return parent)))

(defun org-odt--element-secondary-category (element info)
  (let ((category (org-odt--element-category element info)))
    (when category
      (if (org-odt--get-captioned-parent element info) :SUBENTITY: category))))

(defun org-odt-format-label (element info op &optional format-prop)
  "Return a label for ELEMENT.

ELEMENT is a `link', `table', `src-block' or `paragraph' type
element.  INFO is a plist used as a communication channel.  OP is
either `definition' or `reference', depending on the purpose of
the generated string.

Return value is a string if OP is set to `reference' or a cons
cell like CAPTION . SHORT-CAPTION) where CAPTION and
SHORT-CAPTION are strings."
  (cl-assert (memq (org-element-type element) '(link table src-block paragraph)))
  (let* ((caption-from
	  (cl-case (org-element-type element)
	    (link (org-export-get-parent-element element))
	    (t element)))
	 ;; Get label and caption.
	 (label (org-export-get-reference caption-from info))
	 (caption (org-export-get-caption caption-from))
	 (short-caption (org-export-get-caption caption-from t))
	 ;; Transcode captions.
	 (caption (and caption (org-export-data caption info)))
	 ;; Currently short caption are sneaked in as object names.
	 ;;
	 ;; The advantages are:
	 ;;
	 ;; - Table Of Contents: Currently, there is no support for
	 ;;   building TOC for figures, listings and tables.  See
	 ;;   `org-odt-keyword'.  User instead has to rely on
	 ;;   external application for building such indices.  Within
	 ;;   LibreOffice, building an "Illustration Index" or "Index
	 ;;   of Tables" will create a table with long captions (only)
	 ;;   and building a table with "Object names" will create a
	 ;;   table with short captions.
	 ;;
	 ;; - Easy navigation: In LibreOffice, object names are
	 ;;   offered via the navigation bar.  This way one can
	 ;;   quickly locate and jump to object of his choice in the
	 ;;   exported document.
	 ;;
	 ;; The main disadvantage is that there cannot be any markups
	 ;; within object names i.e., one cannot embolden, italicize
	 ;; or underline text within short caption.  So suppress
	 ;; generation of <text:span >...</text:span> and other
	 ;; markups by overriding the default translators.  We
	 ;; probably shouldn't be suppressing translators for all
	 ;; elements in `org-element-all-objects', but for now this
	 ;; will do.
	 (short-caption
	  (when short-caption
	    (cond
	     ;; Short captions, as they are commonly understood in
	     ;; LaTeX world, is not supported by this exporter.
	     ((memq 'short-caption org-odt-experimental-features)
	      ;; Sneaking in short-caption as name attribute is
	      ;; problematic with LibreOffice > 4.0.4.2.  So ignore
	      ;; short-captions.  See following thread:
	      ;; http://lists.gnu.org/archive/html/emacs-orgmode/2013-12/msg00100.html
	      ;; (ignore
	      ;;  (let ((short-caption (or short-caption caption))
	      ;; 	   (backend (org-export-create-backend
	      ;; 		     :parent (org-export-backend-name
	      ;; 			      (plist-get info :back-end))
	      ;; 		     :transcoders
	      ;; 		     (mapcar (lambda (type) (cons type (lambda (_o c _i) c)))
	      ;; 			     org-element-all-objects))))
	      ;;    (when short-caption
	      ;; 	 (org-export-data-with-backend short-caption backend info))))
	      )
	     ;; Use short caption as text "overlay" over the object
	     ;; underneath.
	     ((memq 'short-caption-as-label org-odt-experimental-features)
	      (when short-caption
		(org-export-data short-caption info)))))))
    (when (org-odt--enumerable-p caption-from info)
      (let* ((category (or (org-odt--element-category element info)
			   (error "Refusing to enumerate the uncategorizable element: %S"
				  element)))
	     (captioned-parent (org-odt--get-captioned-parent element info))
	     (secondary-category (if captioned-parent :SUBENTITY: category))
	     ;; Compute sequence number of the element.
	     (scope (or captioned-parent
			(cl-loop for x in (org-element-lineage element)
				 with n = (string-to-number
					   (plist-get info :odt-display-outline-level))
				 thereis (and (eq (org-element-type x) 'headline)
					      (<= (org-export-get-relative-level x info) n)
					      (org-export-numbered-headline-p x info)
					      x))))
	     (ordinal (let ((counter 0))
			(let* ((org-element-all-objects (remq 'table-cell org-element-all-objects))
			       (org-element-all-elements (cons 'table-cell org-element-all-elements))
			       (org-element-greater-elements (append '(table-row table-cell)
								     org-element-greater-elements)))
			  (org-element-map (or scope (plist-get info :parse-tree))
			      '(link src-block table)
			    (lambda (el)
			      (and (eq secondary-category (org-odt--element-secondary-category el info))
				   (cl-incf counter)
				   (or (eq element el)
				       (eq element (cl-case (org-element-type el)
						     (link (org-export-get-parent-element el))
						     (t el))))
				   counter))
			    info 'first-match))))
	     (seqno (pcase secondary-category
		      (':SUBENTITY: (number-to-string ordinal))
		      (_ (concat
			  ;; Section number.
			  (and scope
			       (mapconcat 'number-to-string
					  (org-export-get-headline-number scope info) "."))
			  ;; Separator.
			  (and scope ".")
			  ;; Ordinal.
			  (number-to-string ordinal))))))
	(let* ((category-props (assoc-default secondary-category org-odt-caption-and-numbering-settings)))
	  (cl-case op
	    ;; Case 1: Handle Label definition.
	    (definition
	     (let* ((caption-text
		     ;; Label definition: Typically formatted as below:
		     ;;     ENTITY-NAME SEQ-NO: LONG CAPTION
		     ;; with translation for correct punctuation.
		     (cl-loop for % in (plist-get
					(assoc-default secondary-category
						       org-odt-caption-and-xref-settings)
					(or format-prop :caption-format))
			      concat
			      (pcase %
				('category
				 ;; Localize entity name.
				 (org-odt--translate (plist-get category-props :entity-name) :utf-8 info))
				('counter
				 (concat
				  ;; Sneak in a bookmark.  The bookmark is used when the
				  ;; labeled element is referenced with a link that
				  ;; provides its own description.
				  (org-odt--target "" label)
				  (if (and captioned-parent (= ordinal 1))
				      (format
				       "<text:sequence text:ref-name=\"%s\" text:name=\"%s\" style:num-format=\"%s\">%s</text:sequence>"
				       label
				       (plist-get category-props :variable)
				       (plist-get category-props :seq-num-format)
				       seqno)
				    (format
				     "<text:sequence text:ref-name=\"%s\" text:name=\"%s\" text:formula=\"ooow:%s+1\" style:num-format=\"%s\">%s</text:sequence>"
				     label
				     (plist-get category-props :variable)
				     (plist-get category-props :variable)
				     (plist-get category-props :seq-num-format)
				     seqno))))
				('caption (or caption ""))
				(_ %)))))
	       (list
		:caption
		(unless (string= caption-text "")
		  (cl-case (or format-prop :caption-format)
		    (:caption-format
		     (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
			     (let ((style (plist-get category-props :caption-style)))
			       (if (eq secondary-category :SUBENTITY:)
				   (format "OrgSub%s" style)
				 style))
			     caption-text))
		    (t caption-text)))
		:short-caption
		short-caption
		:caption-position
		(plist-get (assoc-default secondary-category org-odt-caption-and-xref-settings)
			   :caption-position)
		:p-style
		;; Paragraph style for the "overlaid" short caption,
		;; and the associated image/formula.
		(or (org-odt--read-attribute caption-from :style)
		    (format "Org%sText" (plist-get category-props :caption-style)))
		:caption-text
		caption-text
		:caption-style
		(plist-get category-props :caption-style)
		:subentityp
		(eq secondary-category :SUBENTITY:))))
	    ;; Case 2: Handle Label reference.
	    (reference
	     (cl-loop for % in (plist-get
				(assoc-default secondary-category org-odt-caption-and-xref-settings)
				(or format-prop :xref-format))
		      concat
		      (pcase %
			((pred stringp) %)
			((pred symbolp)
			 (format
			  "<text:sequence-ref text:reference-format=\"%s\" text:ref-name=\"%s\">%s</text:sequence-ref>"
			  %
			  label
			  (if (equal (assoc-default secondary-category org-odt-caption-and-xref-settings)
				     (assoc-default secondary-category
						    (eval (car (get 'org-odt-caption-and-xref-settings
								    'standard-value)))))
			      seqno
			    "[PLS. UPDATE FIELDS]"))))))
	    (t (error "Unknown %S on label" op))))))))


;;;; Links :: Inline Images

(defun org-odt--copy-image-file (info full-path target-file)
  "Returns the internal name of the file"
  (let* ((image-type (file-name-extension full-path))
	 (media-type (format "image/%s" image-type)))
    (message "Embedding %s as %s..."
	     (substring-no-properties full-path) target-file)

    (let* ((target-dir (file-name-directory target-file)))
      (unless (file-exists-p (concat (plist-get info :odt-zip-dir) target-dir))
	(make-directory (concat (plist-get info :odt-zip-dir) target-dir))
	(org-odt-create-manifest-file-entry info "" target-dir)))

    (copy-file full-path (concat (plist-get info :odt-zip-dir) target-file) 'overwrite)
    (org-odt-create-manifest-file-entry info media-type target-file)
    target-file))

(defun org-odt--image-size (file info &optional user-widths
				 user-heights scale dpi embed-as)
  (cl-assert (consp user-widths))
  (cl-assert (consp user-heights))
  (let* ((user-width (car user-widths))
	 (user-height (car user-heights))
	 (size (org-odt---image-size file info user-width user-height scale dpi embed-as)))
    (setcar user-widths (car size))
    (setcar user-heights (cdr size))

    (when (cdr user-widths)
      (setcdr user-widths (max (car user-widths) (cdr user-widths))))

    (when (cdr user-heights)
      (setcdr user-heights (max (car user-heights) (cdr user-heights))))

    (cons user-widths user-heights)))

(defun org-odt---image-size (file info &optional user-width
				  user-height scale dpi embed-as)
  (let* ((--pixels-to-cms
	  (lambda (pixels dpi)
	    (let ((cms-per-inch 2.54)
		  (inches (/ pixels dpi)))
	      (* cms-per-inch inches))))
	 (--size-in-cms
	  (lambda (size-in-pixels dpi)
	    (and size-in-pixels
		 (cons (funcall --pixels-to-cms (car size-in-pixels) dpi)
		       (funcall --pixels-to-cms (cdr size-in-pixels) dpi)))))
	 (dpi (or dpi (plist-get info :odt-pixels-per-inch)))
	 (anchor-type (or embed-as "paragraph"))
	 (user-width (and (not scale) user-width))
	 (user-height (and (not scale) user-height))
	 (size
	  (and
	   (not (and user-height user-width))
	   (or
	    ;; Use Imagemagick.
	    (and (executable-find "identify")
		 (let ((size-in-pixels
			(let ((dim (shell-command-to-string
				    (format "identify -format \"%%w:%%h\" \"%s\""
					    file))))
			  (when (string-match "\\([0-9]+\\):\\([0-9]+\\)" dim)
			    (cons (string-to-number (match-string 1 dim))
				  (string-to-number (match-string 2 dim)))))))
		   (funcall --size-in-cms size-in-pixels dpi)))
	    ;; Use Emacs.
	    (let ((size-in-pixels
		   (ignore-errors	; Emacs could be in batch mode
		     (clear-image-cache)
		     (image-size (create-image file) 'pixels))))
	      (funcall --size-in-cms size-in-pixels dpi))
	    ;; Use hard-coded values.
	    (cdr (assoc-string anchor-type
			       org-odt-default-image-sizes-alist))
	    ;; Error out.
	    (error "Cannot determine image size, aborting"))))
	 (width (car size)) (height (cdr size)))
    (cond
     (scale
      (setq width (* width scale) height (* height scale)))
     ((and user-height user-width)
      (setq width user-width height user-height))
     (user-height
      (setq width (* user-height (/ width height)) height user-height))
     (user-width
      (setq height (* user-width (/ height width)) width user-width))
     (t (ignore)))
    ;; ensure that an embedded image fits comfortably within a page
    (let ((max-width (car org-odt-max-image-size))
	  (max-height (cdr org-odt-max-image-size)))
      (when (or (> width max-width) (> height max-height))
	(let* ((scale1 (/ max-width width))
	       (scale2 (/ max-height height))
	       (scale (min scale1 scale2)))
	  (setq width (* scale width) height (* scale height)))))
    (cons width height)))

(defun org-odt--do-image-size (path attributes info)
  (setq path
	(let* ((expanded-path (when path
				(cond
				 ((file-name-absolute-p path)
				  (expand-file-name path))
				 (t (expand-file-name path (file-name-directory
							    (plist-get info :input-file))))))))
	  (when (and expanded-path (file-readable-p expanded-path)) expanded-path)))
  (when path
    (let* ((width (plist-get attributes :width))
	   (height (plist-get attributes :height))
	   (scale (plist-get attributes :scale))
	   ;; Handle `:width', `:height' and `:scale' properties.
	   (size
	    (let ((--user-dim
		   (lambda (x)
		     (pcase x
		       ((and (or (pred null) (pred numberp)) x1)
			(cons x1 nil))
		       (`(,(and (or (pred null) (pred numberp)) x1) . ,(and (or (pred null) (pred numberp)) x2))
			(cons x1 x2))
		       (`(,(and (or (pred null) (pred numberp)) x1) ,(and (or (pred null) (pred numberp)) x2))
			(cons x1 x2))
		       (_ (cons nil nil))))))
	      (org-odt--image-size
	       path info
	       (funcall --user-dim width)
	       (funcall --user-dim height)
	       scale
	       nil			; embed-as
	       "paragraph"		; FIXME
	       )))
	   (widths (car size)) (heights (cdr size)))
      (list path widths heights))))

(defun org-odt-link--inline-image (element info)
  "Return ODT code for an inline image.
LINK is the link pointing to the inline image.  INFO is a plist
used as a communication channel."
  (cl-assert (eq (org-element-type element) 'link))
  (pcase-let* ((src (let* ((type (org-element-property :type element))
			   (raw-path (org-element-property :path element)))
		      (cond ((member type '("http" "https"))
			     (concat type ":" raw-path))
			    ((file-name-absolute-p raw-path)
			     (expand-file-name raw-path))
			    (t raw-path))))
	       (attr-from (cl-case (org-element-type element)
			    (link (org-export-get-parent-element element))
			    (t element)))
	       ;; Extract attributes from #+ATTR_ODT line.
	       ;; Handle `:width', `:height' and `:scale' properties.
	       (`(,file-path ,widths ,heights)
		(org-odt--do-image-size src (org-odt--read-attribute attr-from) info))
	       (href (format
		      "\n<draw:image xlink:href=\"%s\" xlink:type=\"simple\" xlink:show=\"embed\" xlink:actuate=\"onLoad\"/>"
		      (org-odt--copy-image-file info
						file-path
						(format "Images/%04d.%s"
							(org-odt--count-object info :images)
							(file-name-extension file-path)))))
	       ;; Gather `:inner-frame' and `outer-frame' attributes.
	       ;; A frame attribute is a plist with three keys
	       ;; `:style',`:extra' and `:anchor'.
	       (inner-frame-params
		(let ((value (org-odt--read-attribute attr-from :inner-frame)))
		  (cl-loop for (a b . rest) on value by #'cddr
			   append (list (intern (format ":%s" a)) b))))
	       (outer-frame-params
		(let ((value (org-odt--read-attribute attr-from :outer-frame)))
		  (cl-loop for (a b . rest) on value by #'cddr
			   append (list (intern (format ":%s" a)) b))))
	       (standalone-link-p (org-odt--standalone-link-p element info))
	       (embed-as (if standalone-link-p "to-char" "as-char"))
	       (captions-plist (org-odt-format-label element info 'definition))
	       (caption (plist-get captions-plist :caption))
	       (label (plist-get captions-plist :label))
	       (entity (concat (and caption "Captioned") embed-as "Image"))
	       ;; Check if this link was created by LaTeX-to-PNG converter.
	       (title-and-desc
		(let* ((replaces (org-element-property
				  :replaces (if (not standalone-link-p) element
					      (org-export-get-parent-element element)))))
		  (when replaces
		    (list
		     ;; If yes, note down the type of the element - LaTeX Fragment
		     ;; or LaTeX environment.  It will go in to frame title.
		     :title (capitalize (symbol-name (org-element-type replaces)))
		     ;; If yes, note down its contents.  It will go in to frame
		     ;; description.  This quite useful for debugging.
		     :desc (org-element-property :value replaces)))))
	       (app (or (plist-get info :odt-app) "lo"))
	       (libreofficep (pcase app ("lo" t) (_ nil)))
	       (inner-frame-size
		(let ((rel-size (when (and (or caption label)
					   (null (cdr widths))
					   (null (cdr heights))
					   libreofficep)
				  (list :rel-width 100 :rel-height 'scale))))
		  (nconc (list :width (car widths) :height (car heights))
			 rel-size)))
	       (outer-frame-size (list :width (or (cdr widths) (car widths))
				       :height (or (cdr heights) (car heights)))))
    (let ((contents (org-odt--render-image app entity
					   href
					   captions-plist
					   (nconc inner-frame-params title-and-desc
						  inner-frame-size)
					   (nconc outer-frame-params outer-frame-size))))
      (if standalone-link-p
	  ;; Decorate contents with the paragraph style it needs to be
	  ;; enclosed in.  The value of `:p-style' is probed in
	  ;; `org-odt-paragraph'.
	  (propertize contents :p-style
		      (concat "Org"
			      (when (plist-get captions-plist :subentityp) "Sub")
			      (plist-get captions-plist :caption-style)
			      "Body"))
	contents))))


;;;; Links :: Math formula

(defun org-odt-link--inline-formula (element info)
  (let* ((src (let* ((_type (org-element-property :type element))
		     (raw-path (org-element-property :path element)))
		(cond
		 ((file-name-absolute-p raw-path)
		  (expand-file-name raw-path))
		 (t raw-path))))
	 (src-expanded (if (file-name-absolute-p src) src
			 (expand-file-name src (file-name-directory
						(plist-get info :input-file)))))
	 (href
	  (format
	   "\n<draw:object %s xlink:href=\"%s\" xlink:type=\"simple\"/>"
	   " xlink:show=\"embed\" xlink:actuate=\"onLoad\""
	   (file-name-directory (org-odt--copy-formula-file
				 info
				 src-expanded
				 (format "Formula-%04d/"
					 (org-odt--count-object info :formulas))))))
	 (standalone-link-p (org-odt--standalone-link-p element info))
	 (enumerable-link-p (when standalone-link-p
			      (let ((caption-from (cl-case (org-element-type element)
						    (link (org-export-get-parent-element element))
						    (t element))))
				(org-odt--enumerable-p caption-from info))))
	 (captions-plist (org-odt-format-label element info 'definition))
	 (label (let ((plist (org-odt-format-label element info 'definition :label-format)))
		  (plist-get plist :caption)))
	 (entity
	  (concat
	   (when enumerable-link-p "Captioned")
	   (if standalone-link-p "Paragraph" "As-Char")
	   "Formula"))
	 (title-and-desc
	  ;; Check if this link was created by LaTeX-to-MathML
	  ;; converter.
	  (let ((replaces (org-element-property
			   :replaces (if (not standalone-link-p) element
				       (org-export-get-parent-element element)))))
	    (when replaces
	      (list
	       ;; If yes, note down the type of the element - LaTeX Fragment
	       ;; or LaTeX environment.  It will go in to frame title.
	       :title (capitalize (symbol-name (org-element-type replaces)))
	       ;; If yes, note down its contents.  It will go in to frame
	       ;; description.  This quite useful for debugging.
	       :desc (org-element-property :value replaces)))))
	 (app (or (plist-get info :odt-app) "lo"))
	 (equation (org-odt--render-formula
		    app entity href
		    (append captions-plist (list :label label))
		    (let ((inner-frame-params nil))
		      (append inner-frame-params title-and-desc))
		    (let ((outer-frame-params nil))
		      outer-frame-params))))
    (if (not standalone-link-p) equation
      (org-odt--lisp-to-xml
       `(table:table
	 ((table:style-name . "OrgEquationTable"))
	 (table:table-column
	  ((table:style-name . "OrgEquationTableColumn1")))
	 (table:table-column
	  ((table:style-name . "OrgEquationTableColumn2")))
	 (table:table-rows nil
			   (table:table-row
			    ((table:style-name . "OrgTableRow"))
			    (table:table-cell
			     ((table:style-name . "OrgTableCellMiddle"))
			     (text:p
			      ((text:style-name . "OrgTableContentsCenter"))
			      ,equation))
			    (table:table-cell
			     ((table:style-name . "OrgTableCellMiddle"))
			     (text:p
			      ((text:style-name . "OrgTableContentsLeft"))
			      ,(if (org-string-nw-p label) label ""))))))))))

(defun org-odt--copy-formula-file (info src-file target-dir)
  "Returns the internal name of the file"
  (let* ((target-file (concat target-dir "content.xml")))
    ;; Create a directory for holding formula file.  Also enter it in
    ;; to manifest.
    (make-directory (concat (plist-get info :odt-zip-dir) target-dir))
    (org-odt-create-manifest-file-entry
     info
     "application/vnd.oasis.opendocument.formula" target-dir "1.2")
    ;; Copy over the formula file from user directory to zip
    ;; directory.
    (message "Embedding %s as %s..." src-file target-file)
    (let ((ext (file-name-extension src-file)))
      (cond
       ;; Case 1: Mathml.
       ((member ext '("mathml" "mml"))
	(copy-file src-file (concat (plist-get info :odt-zip-dir) target-file) 'overwrite))
       ;; Case 2: OpenDocument formula.
       ((string= ext "odf")
	(org-odt--zip-extract src-file "content.xml"
			      (concat (plist-get info :odt-zip-dir) target-dir)))
       (t (error "%s is not a formula file" src-file))))
    ;; Enter the formula file in to manifest.
    (org-odt-create-manifest-file-entry info "text/xml" target-file)
    target-file))

;;;; Targets

(defun org-odt--render-image (app cfg-key href
				  captions-plist
				  inner-frame-params
				  outer-frame-params)
  (let* ((libreofficep (pcase app
			 ("lo" t)
			 (_ nil)))
	 (caption (plist-get captions-plist :caption))
	 (_short-caption (plist-get captions-plist :short-caption))
	 (caption-position (plist-get captions-plist :caption-position))
	 (label (plist-get captions-plist :label))
	 (inner-frame-cfg-alist
	  `(("As-CharImage" :style "OrgInlineImage" :extra nil :anchor "as-char")
	    ("CaptionedAs-CharImage" :style "OrgDisplayImage" :extra nil :anchor "paragraph")
	    ("ParagraphImage" :style "OrgDisplayImage" :extra nil :anchor "paragraph")
	    ("CaptionedParagraphImage" :style "OrgDisplayImage" :extra nil :anchor "paragraph")
	    ("To-CharImage" :style "OrgToCharImage" :extra nil :anchor "char")
	    ("CaptionedTo-CharImage" :style ,(cl-case caption-position
					       (above "OrgToCharImageCaptionAbove")
					       (below "OrgToCharImage"))
	     :extra nil :anchor "char")
	    ("PageImage" :style "OrgPageImage" :extra nil :anchor "page")
	    ("CaptionedPageImage" :style "OrgDisplayImage" :extra nil :anchor "paragraph")))
	 (outer-frame-cfg-alist
	  '(("CaptionedAs-CharImage" :style "OrgInlineImage" :extra nil :anchor "as-char")
	    ("CaptionedParagraphImage" :style "OrgImageCaptionFrame" :extra nil :anchor "paragraph")
	    ("CaptionedTo-CharImage" :style "OrgToCharImage" :extra nil :anchor "char")
	    ("CaptionedPageImage" :style "OrgPageImageCaptionFrame" :extra nil :anchor "page")))
	 ;; Retrieve inner and outer frame params, from configuration.
	 (inner (cdr (assoc-string cfg-key inner-frame-cfg-alist t)))
	 (outer (cdr (assoc-string cfg-key outer-frame-cfg-alist t)))
	 ;; User-specified frame params (from #+ATTR_ODT spec)
	 (inner-user inner-frame-params)
	 (outer-user outer-frame-params))
    (cond
     ;; Case 1: Image/Formula has no caption.
     ;;         There is only one frame, one that surrounds the image
     ;;         or formula.
     ((and (null caption) (null label))
      ;; Merge user frame params with that from configuration.
      (setq inner (org-combine-plists inner inner-user))
      (apply 'org-odt--draw:frame href inner))
     ;; Case 2: Image/Formula is captioned or labeled.
     ;;         There are two frames: The inner one surrounds the
     ;;         image or formula.  The outer one contains the
     ;;         caption/sequence number.
     (t
      ;; Merge user frame params with outer frame params.
      (setq outer (org-combine-plists outer outer-user))
      ;; Short caption, if specified, goes as part of inner frame.
      (setq inner (org-combine-plists inner inner-user))
      (let* ((text (concat
		    (apply 'org-odt--draw:frame href inner)
		    (plist-get captions-plist :caption-text))))
	(cond
	 (libreofficep (apply 'org-odt--textbox
			      (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
				      (concat
				       (when (plist-get captions-plist :subentityp) "Sub")
				       (plist-get captions-plist :caption-style))
				      text)
			      outer))
	 (t text)))))))

(defun org-odt--render-formula (app cfg-key
				    href
				    captions-plist
				    inner-frame-params outer-frame-params)
  (let* ((libreofficep (pcase app ("lo" t) (_ nil)))
	 (caption (plist-get captions-plist :caption))
	 (_short-caption (plist-get captions-plist :short-caption))
	 (caption-position (plist-get captions-plist :caption-position))
	 (label (plist-get captions-plist :label))
	 (inner-frame-cfg-alist
	  `(("As-CharFormula" :style "OrgInlineFormula" :extra nil :anchor "as-char")
	    ("ParagraphFormula" :style "OrgCaptionedFormula" :extra nil :anchor "as-char")
            ("CaptionedParagraphFormula" :style "OrgCaptionedFormula" :extra nil :anchor "as-char")))
	 (outer-frame-cfg-alist
	  '(("CaptionedParagraphFormula" :style "OrgFormulaCaptionFrame" :extra nil :anchor "as-char")))
	 ;; Retrieve inner and outer frame params, from configuration.
	 (inner (cdr (assoc-string cfg-key inner-frame-cfg-alist t)))
	 (outer (cdr (assoc-string cfg-key outer-frame-cfg-alist t)))
	 ;; User-specified frame params (from #+ATTR_ODT spec)
	 (inner-user inner-frame-params)
	 (outer-user outer-frame-params))
    (cond
     ;; Case 1: Image/Formula has no caption.
     ;;         There is only one frame, one that surrounds the image
     ;;         or formula.
     ((and (null caption) (null label))
      ;; Merge user frame params with that from configuration.
      (setq inner (org-combine-plists inner inner-user))
      (apply 'org-odt--draw:frame href inner))
     ;; Case 2: Image/Formula is captioned or labeled.
     ;;         There are two frames: The inner one surrounds the
     ;;         image or formula.  The outer one contains the
     ;;         caption/sequence number.
     (t
      ;; Merge user frame params with outer frame params.
      (setq outer (org-combine-plists outer outer-user))
      (setq inner (org-combine-plists inner inner-user))
      (let* ((text
	      (let* ((caption-text (plist-get captions-plist :caption-text))
		     (formula (apply 'org-odt--draw:frame href inner))
		     (separator (when (org-string-nw-p caption-text)
				  "<text:line-break/>")))
		(mapconcat #'identity
			   (cl-case caption-position
			     (above
			      (list caption-text separator formula))
			     (below
			      (list formula separator caption-text)))
			   ""))))
	(cond
	 (libreofficep (apply 'org-odt--textbox
			      (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
				      (concat
				       (when (plist-get captions-plist :subentityp) "Sub")
				       (plist-get captions-plist :caption-style))
				      text)
			      outer))
	 (t text)))))))

(defun org-odt--enumerable-p (element _info)
  ;; Element should have a caption or label.
  (or (org-element-property :caption element)
      (org-element-property :name element)))

(defun org-odt--enumerable-image-p (element info)
  (org-odt--standalone-link-p
   element info
   ;; Paragraph should have a caption or label.  It SHOULD NOT be a
   ;; replacement element. (i.e., It SHOULD NOT be a result of LaTeX
   ;; processing.)
   (lambda (p)
     (and (not (org-element-property :replaces p))
	  (or (org-element-property :caption p)
	      (org-element-property :name p))))
   ;; Link should point to an image file.
   (lambda (l)
     (cl-assert (eq (org-element-type l) 'link))
     (org-export-inline-image-p l (plist-get info :odt-inline-image-rules)))))

(defun org-odt--enumerable-latex-image-p (element info)
  (org-odt--standalone-link-p
   element info
   ;; Paragraph should have a caption or label.  It SHOULD also be a
   ;; replacement element. (i.e., It SHOULD be a result of LaTeX
   ;; processing.)
   (lambda (p)
     (and (org-element-property :replaces p)
	  (or (org-element-property :caption p)
	      (org-element-property :name p))))
   ;; Link should point to an image file.
   (lambda (l)
     (cl-assert (eq (org-element-type l) 'link))
     (org-export-inline-image-p l (plist-get info :odt-inline-image-rules)))))

(defun org-odt--enumerable-formula-p (element info)
  (org-odt--standalone-link-p
   element info
   ;; Paragraph should have a caption or label.
   (lambda (p)
     (or (org-element-property :caption p)
	 (org-element-property :name p)))
   ;; Link should point to a MathML or ODF file.
   (lambda (l)
     (cl-assert (eq (org-element-type l) 'link))
     (org-export-inline-image-p l (plist-get info :odt-inline-formula-rules)))))


;; To transclude a subdocument in to the currently exported document,
;; use either
;;
;;     #+ATTR_ODT: :transclude t
;;     [[file:subdocument.odt]]
;;
;;     or
;;
;;     #+ATTR_ODT: :style \"OrgTwoColumnSection\"
;;     #+BEGIN_section
;;       #+ATTR_ODT: :transclude t
;;       [[file:subdocument.odt]]
;;     #+END_section
;;
;; Once the document is exported, use LibreOffice's
;; "Menu->Tools->Update->Update All" to do the actual transclusion.
;; Or, equivalently, enable "Optimize Column Width of all Tables" step
;; in `org-odt-transform-processes'.
;;
;; Link transclusion, as seen above, may seem equivalent to
;;
;;     #+INCLUDE: "./subdocument.org"
;;
;; But in the ODT context, it does offer an advantage.
;;
;; You can use link transclusion, to embed a subdocument that has
;; Headlines / Chapters within a Office section.
;;
;;     #+BEGIN_section
;;       #+ATTR_ODT: :transclude t
;;       [[file:subdocument.org]]
;;     #+END_section
;;
;; But there is no way you can achieve the same with
;;
;;     #+BEGIN_section
;;       #+INCLUDE: "./subdocument.org"
;;     #+END_section
;;
;; because the following construct wouldn't parse well in Org.
;;
;;     #+BEGIN_section
;;       * Heading
;;       Body text
;;     #+END_section

(defun org-odt--transclude-link-p (element info)
  ;; A link is transcluded, only if all the following conditions are met:
  ;;   - It is a link to a ODT file.
  ;;   - It is the *sole* content of the containing paragraph.
  ;;   - The containing paragraph is marked with \"#+ATTR_ODT: :transclude t\".

  (org-odt--standalone-link-p
   element info
   nil
   (lambda (link)
     (cl-assert (eq (org-element-type link) 'link))
     (and (org-export-inline-image-p link '(("file" . "\\.\\(odt\\)\\'")))
	  (org-odt--read-attribute (org-export-get-parent link) :transclude)))))

(defun org-odt--standalone-link-p (element _info &optional
					   paragraph-predicate
					   link-predicate)
  "Test if ELEMENT is a standalone link for the purpose ODT export.
INFO is a plist holding contextual information.

Return non-nil, if ELEMENT is of type paragraph satisfying
PARAGRAPH-PREDICATE and its sole content, save for whitespaces,
is a link that satisfies LINK-PREDICATE.

Return non-nil, if ELEMENT is of type link satisfying
LINK-PREDICATE and its containing paragraph satisfies
PARAGRAPH-PREDICATE in addition to having no other content save for
leading and trailing whitespaces.

Return nil, otherwise."
  (let ((p (cl-case (org-element-type element)
	     (paragraph element)
	     (link (and (or (not link-predicate)
			    (funcall link-predicate element))
			(org-export-get-parent element)))
	     (t nil))))
    (when (and p (eq (org-element-type p) 'paragraph))
      (when (or (not paragraph-predicate)
		(funcall paragraph-predicate p))
	(let ((contents (org-element-contents p)))
	  (cl-loop for x in contents
		   with inline-image-count = 0
		   always (cl-case (org-element-type x)
			    (plain-text
			     (not (org-string-nw-p x)))
			    (link
			     (and (or (not link-predicate)
				      (funcall link-predicate x))
				  (= (cl-incf inline-image-count) 1)))
			    (t nil))))))))

(defun org-odt-link--infer-description (destination info)
  ;; DESTINATION is a HEADLINE, a "<<target>>" or an element (like
  ;; paragraph, verse-block etc) to which a "#+NAME: label" can be
  ;; attached.  Note that labels that are attached to captioned
  ;; entities - inline images, math formulae and tables - get resolved
  ;; as part of `org-odt-format-label' and `org-odt--enumerate'.

  ;; Create a cross-reference to DESTINATION but make best-efforts to
  ;; create a *meaningful* description.  Check item numbers, section
  ;; number and section title in that order.

  ;; NOTE: Counterpart of `org-export-get-ordinal'.
  ;; FIXME: Handle footnote-definition footnote-reference?
  (let* ((genealogy (org-element-lineage destination))
	 (data (reverse genealogy))
	 (label (cl-case (org-element-type destination)
		  ((headline target)
		   (org-export-get-reference destination info))
		  (t (org-element-property :name destination)))))
    (or
     ;; Case 1: Does the user want the cross-references to be typeset
     ;; in a custom manner (say for example, generate page numbers
     ;; etc.)?  If yes, emit the required XML tags but with "???" as
     ;; the field value.  This (incorrect) field value can be
     ;; corrected by using an external Office application.  For
     ;; example, in case of LibreOffice, the field values can be
     ;; synchronized by running Tools->Update->Fields/Update All on
     ;; the exported document.
     (org-odt--xref-target :TARGET: "[PLS. UPDATE FIELDS]" label)
     ;; Case 2: Is target an item of a numbered list?  If yes, use the
     ;; item's number as description.  The target need not necessarily
     ;; be part of a proper numbered list, it can also be part of a
     ;; low-level headline that is deemed as a list for purposes of
     ;; export.
     (let* ( ;; Locate top-level list.
	    (top-level-list
	     (cl-loop for x on data
		      when (eq (org-element-type (car x)) 'plain-list)
		      return x))
	    ;; Get list item nos.
	    (item-numbers
	     (cl-loop for (plain-list item . rest) on top-level-list by #'cddr
		      until (not (eq (org-element-type plain-list) 'plain-list))
		      collect (when (eq (org-element-property :type
							      plain-list)
					'ordered)
				(1+ (length (org-export-get-previous-element
					     item info t))))))
	    ;; Locate top-most listified headline.
	    (listified-headlines
	     (cl-loop for x on data
		      when (and (eq (org-element-type (car x)) 'headline)
				(org-export-low-level-p (car x) info))
		      return x))
	    ;; Get listified headline numbers.
	    (listified-headline-nos
	     (cl-loop for el in listified-headlines
		      when (eq (org-element-type el) 'headline)
		      collect (when (org-export-numbered-headline-p el info)
				(1+ (length (org-export-get-previous-element
					     el info t)))))))
       ;; Combine item numbers from both the listified headlines and
       ;; regular list items.

       ;; Case 2.1: Check if all the parents of list item are
       ;; numbered.  If yes, link to the item proper.
       (let ((item-numbers (append listified-headline-nos item-numbers)))
	 (when (and item-numbers (not (memq nil item-numbers)))
	   (format "<text:bookmark-ref text:reference-format=\"number-all-superior\" text:ref-name=\"%s\">%s</text:bookmark-ref>"
		   label
		   (mapconcat (lambda (n) (if (not n) " "
					    (concat (number-to-string n) ".")))
			      item-numbers "")))))
     ;; Case 3: Is the target part of a regular and numbered headline
     ;; in the hierarchy?  If yes, use the chapter/section number as
     ;; description.
     (let ((headline (cl-loop for el in (cons destination genealogy)
			      when (and (eq (org-element-type el) 'headline)
					(not (org-export-low-level-p el info))
					(org-export-numbered-headline-p el info))
			      return el)))
       ;; We found one.
       (when headline
	 (format "<text:bookmark-ref text:reference-format=\"chapter\" text:ref-name=\"%s\">%s</text:bookmark-ref>"
		 label
		 (mapconcat 'number-to-string (org-export-get-headline-number
					       headline info) "."))))
     ;; Case 4: Is the target part of any headline.  If yes, use the
     ;; chapter/section's title description.
     (let ((headline (cl-loop for el in (cons destination genealogy)
			      when (and (eq (org-element-type el) 'headline)
					(not (org-export-low-level-p el info)))
			      return el)))
       ;; We found one.
       (when headline
	 (format "<text:bookmark-ref text:reference-format=\"text\" text:ref-name=\"%s\">%s</text:bookmark-ref>"
		 label
		 (let ((title (org-element-property :title headline)))
		   (org-export-data title info)))))
     ;; Case 5: The target is part of a document that is outside of
     ;; any headline.  Use "???" as description.  (We can use the
     ;; label text itself as the description.  But, philosophically
     ;; speaking, this is in-appropriate.  Targets are just labels and
     ;; must not generate any content text.  So, it makes sense to
     ;; insist that the user provide an explicit description.)
     (format "<text:bookmark-ref text:reference-format=\"number-all-superior\" text:ref-name=\"%s\">%s</text:bookmark-ref>"
	     label "[PLS. UPDATE FIELDS]"))))

(defun org-odt-link (link desc info)
  "Transcode a LINK object from Org to ODT.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((file.org->file.odt?
	  (lambda (raw-path info)
	    (let ((raw-path (cond
			     ;; If file path is absolute, prepend it with protocol
			     ;; component - "file://".
			     ((file-name-absolute-p raw-path)
			      (org-export-file-uri raw-path))
			     ;; Otherwise, use the relative path, but prepend it with "../".
			     (t (concat "../" (file-relative-name raw-path))))))
	      ;; Treat links to `file.org' as links to `file.odt', if
	      ;; needed.  See `org-odt-link-org-files-as-odt'.
	      (cond
	       ((and (plist-get info :odt-link-org-files-as-odt)
		     (string= ".org" (downcase (file-name-extension raw-path "."))))
		(concat (file-name-sans-extension raw-path) ".odt"))
	       (t raw-path)))))
	 (type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (and (not (string= desc "")) desc))
	 (_imagep (org-export-inline-image-p
		   link (plist-get info :odt-inline-image-rules)))
	 (path
	  (cond
	   ((member type '("http" "https" "ftp" "mailto"))
	    (xml-escape-string (url-encode-url (org-link-unescape (concat type ":" raw-path)))))
	   ((string= type "file")
	    (funcall file.org->file.odt? raw-path info))
	   (t raw-path))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'odt))
     ;; Link to a transcluded ODT file.
     ((org-odt--transclude-link-p link info)
      (org-odt-text:section
       link
       (format "\n<text:section-source xlink:href=\"%s\" xlink:type=\"simple\" text:filter-name=\"writer8\"/>"
	       path)
       info))
     ;; Image file.
     ((and (not desc) (org-export-inline-image-p
		       link (plist-get info :odt-inline-image-rules)))
      (org-odt-link--inline-image link info))
     ;; Formula file.
     ((and (not desc) (org-export-inline-image-p
		       link (plist-get info :odt-inline-formula-rules)))
      (org-odt-link--inline-formula link info))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(if (not destination) desc
	  (let ((desc (org-export-data (org-element-contents destination) info))
		(href (org-export-get-reference destination info)))
	    (or
	     ;; Case 1: Honour user's customization.
	     (org-odt--xref-target :TARGET: "[PLS. UPDATE FIELDS]" href)
	     ;; Case 2: Use the text of the radio target.
	     (format
	      "<text:bookmark-ref text:reference-format=\"text\" text:ref-name=\"%s\">%s</text:bookmark-ref>"
	      href desc))))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(pcase (org-element-type destination)
	  ;; Case 0: ID link points to an external file.
	  (`plain-text
	   (format "<text:a xlink:type=\"simple\" xlink:href=\"%s#%s\">%s</text:a>"
		   (funcall file.org->file.odt? destination info)
		   (concat "ID-" path)
		   (or desc (org-odt--encode-plain-text destination))))
	  ;; Case 1: Fuzzy link points nowhere.
	  (`nil
	   (user-error
	    "Link \"%s\" at char position %d-%d points nowhere."
	    (org-element-property :raw-link link)
	    (org-element-property :begin link)
	    (org-element-property :end link)))
	  ;; Case 2: Fuzzy link points to a headline.
	  (`headline
	   ;; If there's a description, create a hyperlink.
	   ;; Otherwise, try to provide a meaningful description.
	   (if (not desc) (org-odt-link--infer-description destination info)
	     (format
	      "<text:a xlink:type=\"simple\" xlink:href=\"#%s\">%s</text:a>"
	      (org-export-get-reference destination info) desc)))
	  ;; Case 3: Fuzzy link points to a target.
	  (`target
	   ;; If there's a description, create a hyperlink.
	   ;; Otherwise, try to provide a meaningful description.
	   (if (not desc) (org-odt-link--infer-description destination info)
	     (format "<text:a xlink:type=\"simple\" xlink:href=\"#%s\">%s</text:a>"
		     (org-export-get-reference destination info)
		     desc)))
	  ;; Case 4: Fuzzy link points to some element (e.g., an
	  ;; inline image, a math formula or a table).
	  (_
	   (let ((label-reference
		  (ignore-errors (org-odt-format-label
				  destination info 'reference))))
	     (cond ((not label-reference)
		    (org-odt-link--infer-description destination info))
		   ;; LINK has no description.  Create
		   ;; a cross-reference showing entity's sequence
		   ;; number.
		   ((not desc) label-reference)
		   ;; LINK has description.  Insert a hyperlink with
		   ;; user-provided description.
		   (t (format "<text:a xlink:type=\"simple\" xlink:href=\"#%s\">%s</text:a>"
			      (org-export-get-reference destination info)
			      desc))))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let* ((line-no (format "%d" (org-export-resolve-coderef path info)))
	     (href (concat "coderef-" path)))
	(format
	 (org-export-get-coderef-format path desc)
	 (format
	  "<text:bookmark-ref text:reference-format=\"number\" text:ref-name=\"%s\">%s</text:bookmark-ref>"
	  href line-no))))
     ;; External link with a description part.
     ((and path desc)
      (let ((link-contents (org-element-contents link)))
	;; Check if description is a link to an inline image.
	(if (and (not (cdr link-contents))
		 (let ((desc-element (car link-contents)))
		   (and (eq (org-element-type desc-element) 'link)
			(org-export-inline-image-p
			 desc-element (plist-get info :odt-inline-image-rules)))))
	    ;; Format link as a clickable image.
	    (format "\n<draw:a xlink:type=\"simple\" xlink:href=\"%s\">\n%s\n</draw:a>"
		    path desc)
	  ;; Otherwise, format it as a regular link.
	  (format "<text:a xlink:type=\"simple\" xlink:href=\"%s\">%s</text:a>"
		  path desc))))
     ;; External link without a description part.
     (path
      (format "<text:a xlink:type=\"simple\" xlink:href=\"%s\">%s</text:a>"
	      path path))
     ;; No path, only description.  Try to do something useful.
     (t (format "<text:span text:style-name=\"%s\">%s</text:span>"
		"Emphasis" desc)))))


;;;; Node Property

(defun org-odt-node-property (node-property _contents info)
  "Transcode a NODE-PROPERTY element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let* ((key (org-element-property :key node-property))
	 (value (or (org-element-property :value node-property) "")))
    (format "<text:p text:style-name=\"%s\">%s</text:p>"
	    (if (org-export-last-sibling-p node-property info)
		"OrgPropertiesBlockLastLine"
	      "OrgPropertiesBlock")
	    (format "%s: <text:tab/>%s"
		    (format "<text:span text:style-name=\"OrgPropertyName\">%s</text:span>"
			    (org-odt--encode-plain-text key))
		    (format "<text:span text:style-name=\"OrgPropertyValue\">%s</text:span>"
			    (org-odt--encode-plain-text value))))))

;;;; Paragraph

;; You can customize paragraphs - standalone one and those occurring
;; within lists - using `:style' and `:p-style' attributes.  Try out
;; the following example and see for yourself what you can achieve.
;;
;; #+ATTR_ODT: :style "OrgBulletedList" :p-style "Text_20_body_20_bold"
;; 1. N1
;;    1. N11
;;    2. N12
;; 2. N2
;;    #+ATTR_ODT: :style "OrgNumberedList" :p-style "Preformatted_20_Text"
;;    * B21
;;
;;    * B22
;;      - B221
;;
;;        First paragraph.
;;
;;        #+ATTR_ODT: :style "OrgBibliographyList" :p-style "Text_20_body"
;;        1. one
;;        2. two
;;        3. three
;;
;;        #+ATTR_ODT: :style "Text_20_body_20_indent"
;;        Second paragraph.
;;
;;      - B222
;;    * B23
;; 3. N3

(defun org-odt--get-derived-paragraph-style (paragraph info parent-style)
  (let* ((props (nconc (list :parent-style-name parent-style)
		       (org-odt--read-attribute paragraph))))
    (if (not (or (plist-get props :page-style)
		 (plist-get props :page-break)
		 (plist-get props :page-number)))
	parent-style
      (let* ((style-name (format "Org%s" (org-odt--name-object info 'paragraph))))
	(plist-put info :odt-automatic-styles
		   (concat (plist-get info :odt-automatic-styles)
			   (let* ((master-page-name (plist-get props :page-style))
				  (pagebreak-after-p (string= (plist-get props :page-break) "after"))
				  (page-number (plist-get props :page-number)))
			     (format org-odt-page-break-style-format
				     style-name
				     (plist-get props :parent-style-name)
				     (or master-page-name "")
				     (concat
				      " style:writing-mode=\"page\""
				      (if pagebreak-after-p " fo:break-after=\"page\""
					" fo:break-before=\"page\"")
				      (when (numberp page-number)
					(unless master-page-name
					  (user-error "You have specified `:page-number', but not `:page-style'."))
					(format " style:page-number=\"%d\"" page-number)))))))

	style-name))))

(defun org-odt-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to ODT.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (setq contents (org-trim contents))
  (if (org-odt--transclude-link-p paragraph info) contents
    (let* ((parent (org-export-get-parent paragraph))
	   (_parent-type (org-element-type parent))
	   (genealogy (cons paragraph (org-element-lineage paragraph)))
	   (data (reverse genealogy))
	   (style
	    ;; Traverse the parse-tree from root element to this
	    ;; paragraph.  Use the following rule at each element to
	    ;; calculate the paragraph style applicable at that element.

	    ;; Case 1: If an element specifies an EXPLICIT STYLE of it's
	    ;; own via the #+ATTR_ODT line, use it.  PARAGRAPH and
	    ;; SPECIAL-BLOCK use the `:style' attribute for this
	    ;; purpose, while TABLE and PLAIN-LIST uses `:p-style'
	    ;; attribute.

	    ;; Case 2: If an element does not have an explicit style but
	    ;; has an IMPLICIT, PRE-CONFIGURE STYLE of it's own, use it.
	    ;; For example, paragraphs within a FOOTNOTE-DEFINITON,
	    ;; CENTER-BLOCK or QUOTE-BLOCK get pre-configured styles
	    ;; like "Footnote", "OrgCenter" or "Quotations" resply.

	    ;; Case 3: If an element specifies neither an IMPLICIT style
	    ;; or an EXPLICIT style, use the style from it's parent.
	    ;; For example, a paragraph within a TABLE and PLAIN-LIST
	    ;; (that doesn't specify a `:p-style' of it's own) inherit
	    ;; it's style from the it's parent.

	    ;; Case 4: If an element has no parent (i.e., root node),
	    ;; use the fallback style "Text_20_body".
	    (cl-loop for el in data
		     ;; Fallback style.
		     with style = (or
				   ;; Style set in `org-odt-link--inline-image'
				   ;; or `org-odt-link--inline-formula'.
				   (get-text-property 0 :p-style contents)
				   "Text_20_body")
		     with within-note-definition-p = nil do
		     (setq style
			   (or
			    ;; Case 1: Does this node IMPLICITLY or
			    ;; EXPLICITLY specify a style?  Use it.
			    (cl-case (org-element-type el)
			      (verse-block
			       (or (org-odt--read-attribute el :style)
				   "OrgVerse"))
			      (center-block
			       (or (org-odt--read-attribute el :style)
				   (cl-case within-note-definition-p
				     (footnote "OrgFootnoteCenter")
				     (endnote "OrgEndnoteCenter")
				     (t "OrgCenter"))))
			      (footnote-definition
			       (setq within-note-definition-p
				     (if (org-odt--endnote-p el info) 'endnote 'footnote))
			       (or (org-odt--read-attribute el :style)
				   (cl-case within-note-definition-p
				     (footnote "Footnote")
				     (endnote "Endnote")
				     (t (error "This shouldn't happen")))))
			      (paragraph
			       (or
				;; Case 1: Some paragraphs are "created"
				;; not by the user but by the
				;; pre-processing stage.  They use the
				;; `:style' property of the element rather
				;; than the style property from the
				;; attribute line.  See
				;; `org-odt--translate-description-lists/latex',
				;; `org-odt--translate-description-lists/html'
				;; `org-odt--translate-latex-fragments'.
				(org-element-property :style el)
				(org-odt--read-attribute el :style)))
			      (plain-list
			       ;; NOTE: ITEMs cannot have #+ATTR_ODT
			       ;; attached to them.  See
			       ;;
			       ;; http://lists.gnu.org/archive/html/emacs-orgmode/2013-08/msg00586.html
			       (org-odt--read-attribute el :p-style))
			      (quote-block
			       (or (org-odt--read-attribute el :style)
				   (cl-case within-note-definition-p
				     (footnote "OrgFootnoteQuotations")
				     (endnote "OrgEndnoteQuotations")
				     (t "Quotations"))))
			      (special-block
			       (let ((type (downcase (org-element-property :type el))))
				 (cond
				  ;; Case 1: Handle SPECIAL-BLOCKs that are
				  ;; well-known (and treated specially) by
				  ;; the ODT exporter.
				  ((string= type "textbox")
				   (org-odt--read-attribute el :p-style))
				  ((string= type "customshape")
				   (org-odt--read-attribute el :p-style))
				  ((string= type "section")
				   (org-odt--read-attribute el :p-style))
				  ;; Case 2: Handle user-specified
				  ;; SPECIAL-BLOCKs not known to the
				  ;; exporter.
				  (t (org-odt--read-attribute el :style)))))
			      ;; (table-cell
			      ;;  ;; A table cell can have paragraphs, only if
			      ;;  ;; it is part of a list table.
			      ;;  (org-odt--table-cell-get-paragraph-style el info))
			      )
			    ;; Case 2: Element doesn't specify a style of
			    ;; it's own.  Use the parent style.
			    style))
		     finally return style))
	   (index-entries (or (org-odt--format-index-entries paragraph info) "")))
      ;; If this paragraph is a leading paragraph in an item and the
      ;; item has a checkbox, splice the checkbox and paragraph contents
      ;; together.
      (when (and (eq (org-element-type parent) 'item)
		 (eq paragraph (car (org-element-contents parent))))
	(setq contents (concat (org-odt--checkbox parent) contents)))
      (cond
       ((org-odt--standalone-link-p
	 paragraph info nil
	 ;; Link should point to a MathML or ODF file.
	 (lambda (l)
	   (cl-assert (eq (org-element-type l) 'link))
	   (org-export-inline-image-p l (plist-get info :odt-inline-formula-rules))))
	contents)
       ;; Is this paragraph part of a paragraph block?
       ((and (eq (org-element-type parent) 'special-block)
	     (string= "paragraph" (downcase (org-element-property :type parent))))
	;; Yes.  If the paragraph is the last paragraph in the block,
	;; return it's contents, otherwise append a space to it.
	(if (eq paragraph (car (last (org-element-contents parent)))) contents
	  (concat contents " ")))
       (t
	(format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		(org-odt--get-derived-paragraph-style paragraph info style)
		(concat index-entries contents)))))))


;;;; Plain List

(defun org-odt-plain-list (plain-list contents _info)
  "Transcode a PLAIN-LIST element from Org to ODT.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (format "\n<text:list text:style-name=\"%s\" %s>\n%s</text:list>"
	  ;; Choose style based on list type.
	  (cl-case (org-element-property :type plain-list)
	    (ordered (or (org-odt--read-attribute  plain-list :style)
			 "OrgNumberedList"))
	    (unordered (or (org-odt--read-attribute  plain-list :style)
			   "OrgBulletedList"))
	    ;; FIXME: Define and handle `:style' attributes for
	    ;; description lists.
	    (descriptive-1 "OrgDescriptionList")
	    (descriptive-2 "OrgDescriptionList"))
	  ;; If top-level list, re-start numbering.  Otherwise,
	  ;; continue numbering.
	  (format "text:continue-numbering=\"%s\""
		  (let* ((parent (org-export-get-parent plain-list)))
		    (if (and parent (eq (org-element-type parent) 'item))
			"true" "false")))
	  contents))

;;;; Plain Text

(defun org-odt--encode-tabs-and-spaces (line)
  (replace-regexp-in-string
   "\\([\t]\\|\\([ ]+\\)\\)"
   (lambda (s)
     (cond
      ((string= s "\t") "<text:tab/>")
      (t (let ((n (length s)))
	   (cond
	    ((= n 1) " ")
	    ((> n 1) (concat " " (format "<text:s text:c=\"%d\"/>" (1- n))))
	    (t ""))))))
   line))

(defun org-odt--encode-plain-text (text &optional no-whitespace-filling)
  (dolist (pair '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;")))
    (setq text (replace-regexp-in-string (car pair) (cdr pair) text t t)))
  (if no-whitespace-filling text
    (org-odt--encode-tabs-and-spaces text)))

(defun org-odt-plain-text (text info)
  "Transcode a TEXT string from Org to ODT.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (let ((output text))
    ;; Protect &, < and >.
    (setq output (org-odt--encode-plain-text output t))
    ;; Handle smart quotes.  Be sure to provide original string since
    ;; OUTPUT may have been modified.
    (when (plist-get info :with-smart-quotes)
      (setq output (org-export-activate-smart-quotes output :utf-8 info text)))
    ;; Convert special strings.
    (when (plist-get info :with-special-strings)
      (dolist (pair org-odt-special-string-regexps)
	(setq output (replace-regexp-in-string (car pair) (cdr pair) output t nil))))
    ;; Handle break preservation if required.
    (when (plist-get info :preserve-breaks)
      (setq output (replace-regexp-in-string
		    "\\(\\\\\\\\\\)?[ \t]*\n" "<text:line-break/>" output t)))
    ;; Return value.
    (concat (org-odt--pop-index-entries info)
	    output)))


;;;; Planning

(defun org-odt-planning (planning contents info)
  "Transcode a PLANNING element from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
	  "OrgPlanning"
	  (concat
	   (let ((closed (org-element-property :closed planning)))
	     (when closed
	       (concat
		(format "<text:span text:style-name=\"%s\">%s</text:span>"
			"OrgClosedKeyword" org-closed-string)
		(org-odt-timestamp closed contents info))))
	   (let ((deadline (org-element-property :deadline planning)))
	     (when deadline
	       (concat
		(format "<text:span text:style-name=\"%s\">%s</text:span>"
			"OrgDeadlineKeyword" org-deadline-string)
		(org-odt-timestamp deadline contents info))))
	   (let ((scheduled (org-element-property :scheduled planning)))
	     (when scheduled
	       (concat
		(format "<text:span text:style-name=\"%s\">%s</text:span>"
			"OrgScheduledKeyword" org-deadline-string)
		(org-odt-timestamp scheduled contents info)))))))


;;;; Property Drawer

(defun org-odt-property-drawer (_property-drawer contents _info)
  "Transcode a PROPERTY-DRAWER element from Org to ODT.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  contents)


;;;; Quote Block

(defun org-odt-quote-block (_quote-block contents _info)
  "Transcode a QUOTE-BLOCK element from Org to ODT.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  contents)


;;;; Section

(defun org-odt-text:section (element contents info &optional indentation-level)
  (format "\n<text:section %s text:style-name=\"%s\">\n%s\n</text:section>"
	  (format "text:name=\"%s\"" (org-odt--name-object info 'section))
	  (cl-case (org-element-type element)
	    (link
	     (or (let ((grandparent (org-export-get-parent (org-export-get-parent element))))
		   (when (and (eq (org-element-type grandparent) 'special-block)
			      (string= (org-element-property :type grandparent) "section"))
		     (org-odt--read-attribute grandparent :style)))
		 "OrgSection"))
	    (special-block
	     (or (org-odt--read-attribute element :style) "OrgSection"))
	    (table (format "OrgIndentedSection-Level-%d" indentation-level))
	    (t "OrgSection"))
	  contents))

(defun org-odt-section (section contents info)
  "Transcode a SECTION element from Org to ODT.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let* ((parent-headline (org-export-get-parent-headline section))
	 (index-plist (let ((index-alias (org-export-get-node-property :INDEX parent-headline t)))
			(org-odt--index-alias->plist index-alias info))))
    (concat contents
	    (or (when index-plist
		  (org-odt--format-index index-plist info))
		""))))

;;;; Radio Target

(defun org-odt-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to ODT.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (org-odt--target text (org-export-get-reference radio-target info)))


;;;; Special Block

(defun org-odt--compose-image-and-text-into-a-draw:frame (file-path text attributes info)
  (pcase-let* ((`(,path ,widths ,heights)
		(org-odt--do-image-size file-path attributes info))
	       (width (plist-get attributes :width))
	       (height (plist-get attributes :height))
	       (style (or (plist-get attributes :style)
			  "OrgTextBoxFrame"))
	       (extra (plist-get attributes :extra))
	       (anchor (plist-get attributes :anchor)))
    (when path
      ;; A textbox can specify a fill image using the `:image'
      ;; attribute.

      ;; #+ATTR_ODT: :image "images/org-mode-unicorn.png"
      ;; #+begin_textbox
      ;;     Org Mode Unicorn
      ;; #+end_textbox

      ;; You can use the above construct if you want the text within
      ;; the textbox to sit on top of the image. In a sense, this is
      ;; an alternative way to get a "captioned" figure.  Note that
      ;; unlike the standard captioned figure, the figure created
      ;; with this constrcut doesn't get enumerated or enter the
      ;; table of figures.
      (let* ((internal-path (org-odt--copy-image-file
			     info path (format "Images/%04d.%s"
					       (org-odt--count-object info :images)
					       (file-name-extension path))))
	     (draw-name (format "image-%s" (file-name-sans-extension (file-name-nondirectory internal-path))))
	     (parent-style style))
	(setq width (car widths)
	      height (car heights)
	      style (format "Org%s" (org-odt--name-object info 'graphic)))
	(plist-put info :odt-extra-styles
		   (concat (plist-get info :odt-extra-styles)
			   (format
			    "
<draw:fill-image draw:name=\"%s\" xlink:actuate=\"onLoad\" xlink:href=\"%s\" xlink:show=\"embed\" xlink:type=\"simple\" />"
			    draw-name
			    internal-path)))
	(plist-put info :odt-automatic-styles
		   (concat (plist-get info :odt-automatic-styles)
			   (format org-odt-graphic-style-format
				   style
				   parent-style
				   draw-name)))))
    (org-odt--textbox text :width width :height height
                      :style style :extra extra :anchor anchor)))

(defun org-odt--compose-image-and-text-into-a-draw:custom-shape (file-path text attributes info)
  (pcase-let* ((`(,path ,widths ,heights)
		(org-odt--do-image-size file-path attributes info))
	       (style (format "Org%s" (org-odt--name-object info 'graphic)))
	       ;; (extra (plist-get attributes :extra))
	       (anchor (or (plist-get attributes :anchor) "as-char"))
	       (width nil)
	       (height nil))
    (cond
     ((not path)
      (setq width (plist-get attributes :width))
      (setq height (plist-get attributes :height))
      (plist-put info :odt-automatic-styles
		 (concat (plist-get info :odt-automatic-styles)
			 (org-odt--graphic-style :style style
						 :height height
						 :width width))))
     (path
      (setq width (car widths)
	    height (car heights))
      (let* ((internal-path (org-odt--copy-image-file info
						      path
						      (format "Images/%04d.%s"
							      (org-odt--count-object info :images)
							      (file-name-extension path))))
	     (draw-name (format "image-%s" (file-name-sans-extension (file-name-nondirectory internal-path)))))
	(plist-put info :odt-extra-styles
		   (concat (plist-get info :odt-extra-styles)
			   (format
			    "
<draw:fill-image draw:name=\"%s\" xlink:actuate=\"onLoad\" xlink:href=\"%s\" xlink:show=\"embed\" xlink:type=\"simple\" />"
			    draw-name
			    internal-path)))
	(plist-put info :odt-automatic-styles
		   (concat (plist-get info :odt-automatic-styles)
			   (org-odt--graphic-style :style style :image draw-name :height height :width width))))))
    (org-odt--draw:custom-shape :text (or text "")
				:width width :height height :style style
				;; :extra extra
				:anchor-type anchor
				:id (plist-get attributes :name)
				:other-id (plist-get attributes :other-id)
				:shape (or (plist-get attributes :shape) "rectangle"))))

(defun org-odt-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to ODT.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (org-element-property :type special-block))
	(attributes (org-export-read-attribute :attr_odt special-block)))
    (cond
     ;; Paragraph.

     ;; A paragraph block is a special block, where the enclosed
     ;; paragraphs gets collapsed in to a single paragraph.
     ;;
     ;; For example, a block like this
     ;;
     ;; #+begin_paragraph
     ;;     I have apples, oranges and
     ;;
     ;;     bananas.
     ;; #+end_paragraph
     ;;
     ;; is typeset as if it were just a single paragraph like this.
     ;;
     ;;    I have apples, oranges and bananas.
     ;;
     ;; Paragraph blocks are most useful for creating multiple
     ;; *captioned* images in a single paragraph.
     ;;
     ;; For example, to construct two side-by-side images (that are
     ;; also captioned), do
     ;;
     ;; #+ATTR_ODT: :style "OrgCenter"
     ;; #+begin_paragraph
     ;;     #+ATTR_ODT: :width 5 :anchor "as-char"
     ;;     #+CAPTION: First Figure
     ;;     [[./org-mode-unicorn.png]]
     ;;
     ;;     #+ATTR_ODT: :width 5 :anchor "as-char"
     ;;     #+CAPTION: Second Figure
     ;;     [[./org-mode-unicorn.png]]
     ;; #+end_paragraph
     ;;
     ((string= type "paragraph")
      ;; Enclose the contens in a paragraph and return it.
      (org-odt-paragraph special-block contents info))
     ((string= type "customshape")
      ;; WARNING: `customshape' blocks are EXPERIMENTAL and pre-ALPHA
      ;; quality.  Don't use it in production.  See
      ;; `org-odt--draw:custom-shape' for sample usage and other details.

      (let* ((file-path (org-odt--read-attribute special-block :image)))
	(org-odt-paragraph special-block
			   (org-odt--compose-image-and-text-into-a-draw:custom-shape
			    file-path contents
			    (org-combine-plists ; FIXME
			     (org-odt--read-attribute special-block)
			     (list :name (org-element-property :name special-block)))
			    info)
			   info)))
     ;; Annotation.
     ((string= type "annotation")
      (let* ((author (or (plist-get attributes :author)
			 (let ((author (plist-get info :author)))
			   (and author (org-export-data author info)))))
	     (date (or (plist-get attributes :date)
		       ;; FIXME: Is `car' right thing to do below?
		       (car (plist-get info :date)))))
	(format "\n<text:p>%s</text:p>"
		(format "<office:annotation>\n%s\n</office:annotation>"
			(concat
			 (and author
			      (format "<dc:creator>%s</dc:creator>" author))
			 (and date
			      (format "<dc:date>%s</dc:date>"
				      (org-odt--format-timestamp date nil 'iso-date)))
			 contents)))))
     ;; Section.
     ((string= type "section")
      (cond
       ((let ((c (org-element-contents special-block)))
	  (and (null (cdr c)) (org-odt--transclude-link-p (car c) info)))
	;;  This section encloses a transcluded link.  `org-odt-link'
	;;  took care of all the formalities.  Nothing more to do.
	contents)
       (t (org-odt-text:section special-block contents info))))
     ;; Textbox.
     ((string= type "textbox")
      ;; Textboxes an be used for centering tables etc horizontally
      ;; and vertically within a page.

      ;; In the example below, a landscape and centered table is
      ;; created in the middle of what is essentially a portrait
      ;; document.

      ;; Leading text.
      ;;
      ;; #+ATTR_ODT: :style "OrgPageBreakLandscape"
      ;; #+PAGEBREAK:
      ;;
      ;; #+ATTR_ODT: :width 5 :style "OrgPageImage" :anchor "page"
      ;; #+BEGIN_TEXTBOX
      ;; | a          | b          |
      ;; | e          | f          |
      ;; #+END_TEXTBOX
      ;;
      ;; #+ATTR_ODT: :style "OrgPageBreakDefault"
      ;; #+PAGEBREAK:
      ;;
      ;; Trailing text.
      ;;
      (let* ((file-path (org-odt--read-attribute special-block :image)))

	;; A textbox can specify a fill image using the `:image'
	;; attribute.

	;; #+ATTR_ODT: :image "images/org-mode-unicorn.png"
	;; #+begin_textbox
	;;     Org Mode Unicorn
	;; #+end_textbox

	;; You can use the above construct if you want the text within
	;; the textbox to sit on top of the image. In a sense, this is
	;; an alternative way to get a "captioned" figure.  Note that
	;; unlike the standard captioned figure, the figure created
	;; with this constrcut doesn't get enumerated or enter the
	;; table of figures.
	(org-odt-paragraph special-block
			   (org-odt--compose-image-and-text-into-a-draw:frame
			    file-path contents (org-odt--read-attribute special-block) info)
			   info)))
     (t contents))))


;;;; Src Block

(with-no-warnings
  (with-eval-after-load 'hfy-cmap
    (advice-add 'hfy-fallback-color-values :override
		(defun org-odt--hfy-fallback-color-values (color-string)
		  "Workaround a bug in `hfy-fallback-color-values'."
		  ;;Make the color association case-insensitive.
		  (cdr (assoc-string color-string
				     (or hfy-rgb-txt-color-map
					 hfy-fallback-color-map)
				     t))))))

(defvar org-odt-handled-face-attributes
  '(:background                         ;; :box :extend
    :family :foreground                 ;; :foundry
    :height                             ;; :inherit :inverse-video
    :overline :slant                    ;; :stipple
    :strike-through :underline :weight  ;; :width
    ))

(defun org-odt--face-attributes (face &rest props)
  (cl-loop for prop in (or
			;; When props is non-nil, return values of only
			;; specified face attributes.
			props
			;; When props is nil, return values of all face
			;; attributes.
			(sort (mapcar #'car face-attribute-name-alist)
			      (lambda (s1 s2)
				(string< (symbol-name s1) (symbol-name s2)))))
	   collect (cons prop (face-attribute face prop nil t))))

(defun org-odt--face->style:text-properties (face &rest props)
  (cl-loop with attributes = (apply #'org-odt--face-attributes face
				    (or props org-odt-handled-face-attributes))
	   ;; See `hfy-face-to-style-i'
	   with handlers = '(
			     (:background fo:background-color hfy-bgcol)
			     (:family fo:font-family hfy-family)
			     (:foreground fo:color hfy-color)
			     (:height fo:font-size hfy-size)
			     (:inherit nil (lambda (val)
					     (when val
					       `((style:parent-style-name
						  . ,(concat "OrgSrc"
							     (mapconcat
							      'capitalize (split-string
									   (hfy-face-or-def-to-name val) "-")
							      "")))))))
			     (:overline nil (lambda (_val)
					      '((style:text-overline-style . "solid")
						(style:text-overline-width . "auto")
						(style:text-overline-color . "font-color"))))
			     (:slant fo:font-style hfy-slant)
			     (:strike-through nil (lambda (_val)
						    `((style:text-line-through-style . "solid")
						      (style:text-line-through-type . "single"))))
			     (:underline nil (lambda (_val)
					       '((style:text-underline-style . "solid")
						 (style:text-underline-width . "auto")
						 (style:text-underline-color . "font-color"))))
			     (:weight fo:font-weight hfy-weight)
			     (:invisible nil (lambda (_val)
					       `((text:display . "none")))))
	   for (prop . val) in attributes
	   for (odt-attribute fn) = (assoc-default prop handlers)
	   for value = (unless (eq val 'unspecified)
			 (when (and val (functionp fn))
			   (cond
			    ((null odt-attribute)
			     (funcall fn val))
			    (t (let ((v (funcall fn val)))
				 (when (cdar v)
				   (list (cons odt-attribute (cdar v)))))))))
	   when value
	   append value))

(defun org-odt-hfy-face-to-css (fn)
  "Create custom style for face FN.
When FN is the default face, use its foreground and background
properties to create \"OrgSrcBlock\" paragraph style.  Otherwise
use its color attribute to create a character style whose name
is obtained from FN.  Currently all attributes of FN other than
color are ignored.

The style name for a face FN is derived using the following
operations on the face name in that order - de-dash, CamelCase
and prefix with \"OrgSrc\".  For example,
`font-lock-function-name-face' is associated with
\"OrgSrcFontLockFunctionNameFace\"."
  (let* ((style-name (concat "OrgSrc"
			     (mapconcat
			      'capitalize (split-string
					   (hfy-face-or-def-to-name fn) "-")
			      "")))
	 ;; (css-list (hfy-face-to-style fn))
	 ;; (color-val (cdr (assoc "color" _css-list)))
	 ;; (background-color-val (cdr (assoc "background" _css-list)))
	 )
    (cons style-name
	  (when org-odt-create-custom-styles-for-srcblocks
	    (cond
	     ((eq fn 'default)
	      (format org-odt-src-block-style-format
		      (cdar (org-odt--face->style:text-properties 'default :background))
		      (org-odt--lisp-to-xml
		       `(style:text-properties
			 ,(org-odt--face->style:text-properties 'default :foreground)))))
	     (t
	      (org-odt--lisp-to-xml
	       `(style:style
		 ((style:name . ,style-name)
		  (style:family . "text")
		  ,@(org-odt--face->style:text-properties fn :inherit))
		 (style:text-properties
		  ,(org-odt--face->style:text-properties fn))))))))))

(defun org-odt-htmlfontify-string (line)
  (let* ((hfy-html-quote-regex "\\([<\"&> 	]\\)")
	 (hfy-html-quote-map '(("\"" "&quot;")
			       ("<" "&lt;")
			       ("&" "&amp;")
			       (">" "&gt;")
			       (" " "<text:s/>")
			       ("	" "<text:tab/>")))
	 (hfy-face-to-css 'org-odt-hfy-face-to-css)
	 (hfy-optimizations-1 (copy-sequence hfy-optimizations))
	 (hfy-optimizations (cl-pushnew 'body-text-only hfy-optimizations-1))
	 (hfy-begin-span-handler
	  (lambda (style _text-block _text-id _text-begins-block-p)
	    (insert (format "<text:span text:style-name=\"%s\">" style))))
	 (hfy-end-span-handler (lambda nil (insert "</text:span>"))))
    (with-no-warnings (htmlfontify-string line))))

(defun org-odt-do-format-code
    (code info &optional lang refs retain-labels number-lines num-start)
  (let* ((code-length (length (org-split-string code "\n")))
	 (lang-mode (let* ((lang (or (assoc-default lang org-src-lang-modes) lang))
			   (mode (and lang (intern (format "%s-mode" lang)))))
		      (when (and (functionp mode)
				 (plist-get info :odt-fontify-srcblocks)
				 (require 'htmlfontify nil t)
				 (fboundp 'htmlfontify-string))
			mode)))
	 (i 0))
    (cl-reduce
     (lambda (code f) (funcall f code))
     (list
      ;; Fontify code, if language is specified.
      (lambda (code)
	(or (when lang-mode
	      (with-temp-buffer
		(insert code)
		(funcall lang-mode)
		(org-font-lock-ensure)
		(buffer-string)))
	    code))
      ;; Transcode code in OpenDocument format.
      (lambda (code)
	(org-export-format-code
	 code
	 (lambda (line _line-num ref)
	   (cl-reduce
	    (lambda (line f) (funcall f line))
	    (list
	     ;; Transcode a line, taking in to account any fontification.
	     (lambda (line)
	       (if lang-mode
		   (let ((hfy-user-sheet-assoc (plist-get info :odt-hfy-user-sheet-assoc)))
		     (prog1 (org-odt-htmlfontify-string line)
		       (plist-put info :odt-hfy-user-sheet-assoc hfy-user-sheet-assoc)))
		 (org-odt--encode-plain-text line)))
	     ;; Append a label to the line, if the line has a label
	     ;; and the user wants to retain it.
	     (lambda (line)
	       (concat line
		       (and ref retain-labels (format "<text:tab/>(%s)" ref))))
	     ;; Add a bookmark around the line, if the line has a label.
	     (lambda (line)
	       (or (when ref (org-odt--target line (concat "coderef-" ref))) line))
	     ;; Typeset each line as a paragraph.  The last line has
	     ;; it's own style.
	     (lambda (line)
	       (format "<text:p text:style-name=\"%s\">%s</text:p>"
		       (concat (if lang-mode "OrgSrcBlock"
				 "OrgFixedWidthBlock")
			       (and (= (cl-incf i) code-length) "LastLine"))
		       line)))
	    :initial-value line))
	 nil refs))
      ;; Listify the lines of code, if code is numbered.
      (lambda (code)
	(or (when number-lines
	      (let ((continued-p (eq (car number-lines) 'continued)))
		(format
    		 "\n<text:list text:style-name=\"OrgSrcBlockNumberedLine\"%s>\n%s\n</text:list>"
    		 (format " text:continue-numbering=\"%s\""
    			 (or (when (and continued-p (zerop num-start)) "true") "false"))
    		 (let* ((locs (split-string code "\n"))
    			(first-line (car locs))
    			(rest (cdr locs)))
    		   (concat
    		    (format "\n<text:list-item%s>%s\n</text:list-item>"
    			    (or (when (zerop num-start) "")
    				(format " text:start-value=\"%d\"" (1+ num-start)))
    			    first-line)
    		    (cl-loop for loc in rest concat
    			     (format "\n<text:list-item>%s\n</text:list-item>" loc)))))))
	    code)))
     :initial-value code)))

(defun org-odt-format-code (element info)
  (let* ((lang (org-element-property :language element))
	 ;; Extract code and references.
	 (code-info (org-export-unravel-code element))
	 (code (car code-info))
	 (refs (cdr code-info))
	 ;; Does the src block contain labels?
	 (retain-labels (org-element-property :retain-labels element))
	 ;; Does it have line numbers?
	 (number-lines (org-element-property :number-lines element))
	 ;; What number it starts from?
	 (num-start (org-export-get-loc element info)))
    (org-odt-do-format-code code info lang refs retain-labels
			    number-lines num-start)))

(defun org-odt-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to ODT.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information.

The following `nxml-mode' src blocks are treated specially.

    #+ATTR_ODT: :target \"extra_styles\"
    #+begin_src nxml
      ...
    #+end_src

    #+ATTR_ODT: :target \"extra_automatic_styles\"
    #+begin_src nxml
      ...
    #+end_src

    #+ATTR_ODT: :target \"master_styles\"
    #+begin_src nxml
      ...
    #+end_src

    #+ATTR_ODT: :target \"automatic_styles\"
    #+begin_src nxml
      ...
    #+end_src

The content of these blocks are assumed to specify user-specified
styles.  You can use these `nxml-mode' blocks as convenient
alternative to specifying user styles using the following style
keywords

       - #+odt_extra_styles:
       - #+odt_extra_automatic_styles:
       - #+odt_master_styles:
       - #+odt_automatic_styles:

Use of `nxml-mode' blocks for specifying custom styles has many
advantages compared to specifying styles using inbuffer keywords.
They XML styles are syntx highlighted, easy to comprehend and
edit.  More importantly you can exploit full set of babel
features to build source blocks brick-by-brick.  For example, the
Org snippet below uses noweb expansion to modify the default page
header

    #+NAME: header
    #+begin_src nxml :exports none
    <text:p text:style-name=\"OrgCenter\">
      <text:a xlink:href=\"https://orgmode.org\"
              xlink:type=\"simple\">Emacs Orgmode</text:a>
    </text:p>
    #+end_src

    #+ATTR_ODT: :target \"master_styles\"
    #+begin_src nxml  :noweb yes
    <style:master-page style:name=\"Standard\"
                       style:page-layout-name=\"Mpm1\">
      <style:header>
        <<header>>
      </style:header>
      <style:footer>
        <text:p text:style-name=\"MP1\">
          <text:page-number text:select-page=\"current\"></text:page-number>
        </text:p>
      </style:footer>
    </style:master-page>
    #+end_src

    First page.

    #+ATTR_ODT: :page-break t
    Second page.

    #+ATTR_ODT: :page-break t
    Third page.

.          
"
  (let* ((lang (org-element-property :language src-block))
	 (style-key
	  (when (string= (downcase lang) "nxml")
	    (let ((style-spec-p
		   (car (member
			 (org-odt--read-attribute src-block :target)
			 '("extra_styles"
			   "extra_automatic_styles"
			   "master_styles"
			   "automatic_styles")))))
	      (when style-spec-p
		(intern (format ":odt-%s" (replace-regexp-in-string "_" "-" style-spec-p t t))))))))
    (cond
     (style-key
      (plist-put info style-key
		 (concat (plist-get info style-key)
			 (org-element-property :value src-block)))
      "")
     (t
      (let* ((_lang (org-element-property :language src-block))
	     (captions-plist (org-odt-format-label src-block info 'definition))
	     (caption (plist-get captions-plist :caption))
	     (caption-position (plist-get captions-plist :caption-position))
	     (text (let ((--src-block (org-odt-format-code src-block info)))
		     ;; Is `:textbox' property non-nil?
		     (if (not (org-odt--read-attribute src-block :textbox)) --src-block
		       ;; Yes.  Enclose it in a Text Box.
		       (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
			       "Text_20_body"
			       (apply 'org-odt--textbox --src-block nil))))))
	(cl-case caption-position
	  (above (concat caption text))
	  (t (concat text caption))))))))

;;;; Statistics Cookie

(defun org-odt-statistics-cookie (statistics-cookie _contents _info)
  "Transcode a STATISTICS-COOKIE object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((cookie-value (org-element-property :value statistics-cookie)))
    (format "<text:span text:style-name=\"%s\">%s</text:span>"
	    "OrgCode" cookie-value)))


;;;; Strike-Through

(defun org-odt-strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH from Org to ODT.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "Strikethrough" contents))


;;;; Subscript

(defun org-odt-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object from Org to ODT.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "OrgSubscript" contents))


;;;; Superscript

(defun org-odt-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to ODT.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "OrgSuperscript" contents))


;;;; Table Cell

(defun org-odt-table-row-number (table-row info)
  "Return TABLE-ROW number.
INFO is a plist used as a communication channel.  Return value is
zero-indexed and ignores separators.  The function returns nil
for special rows and separators.

This is based on `org-export-table-row-number', with suitable
modifications to account for nested tables."
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((cache (or (plist-get info :table-row-number-cache)
		      (let ((table (make-hash-table :test #'eq)))
			(plist-put info :table-row-number-cache table)
			table)))
	   (cached (gethash table-row cache 'no-cache)))
      (if (not (eq cached 'no-cache)) cached
	;; First time a row is queried, populate cache with all the
	;; rows from the table.
	(let ((number -1))
	  (org-element-map (org-export-get-parent-table table-row) 'table-row
	    (lambda (row)
	      (when (eq (org-element-property :type row) 'standard)
		(puthash row (cl-incf number) cache)))
	    info nil 'table-cell))
	(gethash table-row cache)))))

(defun org-odt-table-dimensions (table info)
  "Return TABLE dimensions.

INFO is a plist used as a communication channel.

Return value is a CONS like (ROWS . COLUMNS) where
ROWS (resp. COLUMNS) is the number of exportable
rows (resp. columns).

This is based on `org-export-table-dimensions', with suitable
modifications to account for nested tables."
  (let (first-row (columns 0) (rows 0))
    ;; Set number of rows, and extract first one.
    (org-element-map table 'table-row
      (lambda (row)
	(when (eq (org-element-property :type row) 'standard)
	  (cl-incf rows)
	  (unless first-row (setq first-row row))))
      info nil 'table-row)
    ;; Set number of columns.
    (org-element-map first-row 'table-cell
      (lambda (_) (cl-incf columns)) info nil 'table-cell)
    ;; Return value.
    (cons rows columns)))

(defun org-odt-table-cell-address (table-cell info)
  "Return address of a regular TABLE-CELL object.

TABLE-CELL is the cell considered.  INFO is a plist used as
a communication channel.

Address is a CONS cell (ROW . COLUMN), where ROW and COLUMN are
zero-based index.  Only exportable cells are considered.  The
function returns nil for other cells.

This is based on `org-export-table-cell-address', with suitable
modifications to account for nested tables."
  (let* ((table-row (org-export-get-parent table-cell))
	 (row-number (org-odt-table-row-number table-row info)))
    (when row-number
      (cons row-number
	    (let ((col-count 0))
	      (org-element-map table-row 'table-cell
		(lambda (cell)
		  (if (eq cell table-cell) col-count (cl-incf col-count) nil))
		info 'first-match 'table-cell))))))

(defun org-odt-table-style-spec (element info)
  (let* ((table (if (eq (org-element-type element) 'table) element
		  (org-export-get-parent-table element)))
	 (table-style (org-odt--read-attribute table :style)))
    (assoc table-style (plist-get info :odt-table-styles))))

(defun org-odt--table-cell-get-group-address (table-cell info)
  (let* ((table (org-export-get-parent-table table-cell))
	 (table-cell-address (org-odt-table-cell-address table-cell info))
	 (rowgroupns (org-odt--table-get-rowgroup-numbers table info))
	 (colgroupns (org-odt--table-get-colgroup-numbers table info))
	 (r (car table-cell-address)) (c (cdr table-cell-address)))
    (cons (1- (nth r rowgroupns))
	  (1- (nth c colgroupns)))))

(defvar org-odt-table-cell-variant-calculator 'usecolrowgroupsp)

(defun org-odt--table-cell-get-cell-variant (table-cell info &optional usecolrowgroupsp)
  (let* ((table (org-export-get-parent-table table-cell))
	 (table-cell-address (cond
			      (usecolrowgroupsp
			       (org-odt--table-cell-get-group-address table-cell info))
			      (t
			       (org-odt-table-cell-address table-cell info))))
	 (r (car table-cell-address)) (c (cdr table-cell-address))
	 (style-spec (org-odt-table-style-spec table-cell info))
	 (table-dimensions (cond
			    (usecolrowgroupsp
			     (org-odt--table-get-group-dimensions table info))
			    (t (org-odt-table-dimensions table info))))
	 (rmax (car table-dimensions)) (cmax (cdr table-dimensions))
	 (cell-style-selectors (if style-spec (nth 2 style-spec) nil)))
    (cond
     ((and (cdr (assq 'use-first-column-styles cell-style-selectors))
	   (= c 0)
	   (> cmax 1))
      "FirstColumn")
     ((and (cdr (assq 'use-last-column-styles cell-style-selectors))
	   (= (1+ c) cmax)
	   (> cmax 1))
      "LastColumn")
     ((and (cdr (assq 'use-first-row-styles cell-style-selectors))
	   (= r 0)
	   (> rmax 1))
      "FirstRow")
     ((and (cdr (assq 'use-last-row-styles cell-style-selectors))
	   (= (1+ r) rmax)
	   (> rmax 1))
      "LastRow")
     ((and (cdr (assq 'use-banding-rows-styles cell-style-selectors))
	   (= (% r 2) 1))
      "EvenRow")
     ((and (cdr (assq 'use-banding-rows-styles cell-style-selectors))
	   (= (% r 2) 0))
      "OddRow")
     ((and (cdr (assq 'use-banding-columns-styles cell-style-selectors))
	   (= (% c 2) 1))
      "EvenColumn")
     ((and (cdr (assq 'use-banding-columns-styles cell-style-selectors))
	   (= (% c 2) 0))
      "OddColumn")
     (t ""))))

(defun org-odt--table-cell-get-cell-style (table-cell info)
  (let* ((table-cell-address (org-odt-table-cell-address table-cell info))
	 (table-cell-valign (nth (cdr table-cell-address)
				 (plist-get (org-odt--table-col-cookies
					     (org-export-get-parent-table table-cell) info)
					    :valigns)))
	 (table-cell-borders (org-odt--table-cell-get-cell-border table-cell info))
	 (style-spec (org-odt-table-style-spec table-cell info))
	 (template-name (if style-spec (nth 1 style-spec) "Org")))
    ;; LibreOffice - particularly the Writer - honors neither table
    ;; templates nor custom table-cell styles.  Inorder to retain
    ;; inter-operability with LibreOffice, only automatic styles are
    ;; used for styling of table-cells.  The current implementation is
    ;; congruent with ODF-1.2 specification and hence is
    ;; future-compatible.

    ;; Additional Note: LibreOffice's AutoFormat facility for tables -
    ;; which recognizes as many as 16 different cell types - is much
    ;; richer. Unfortunately it is NOT amenable to easy configuration
    ;; by hand.

    ;; BASE-STYLE-NAME		⇒ TABLE-TEMPLAT-NAME x SUFFIX1 x VARIANT x SUFFIX2
    ;; - TABLE-TEMPLAT-NAME	⇒ ["Org" "Custom"]
    ;; - SUFFIX1		⇒ ["Table"]
    ;; - VARIANT		⇒ ["" "FirstRow" "FirstColumn" "LastRow" "LastColumn"]
    ;; - SUFFIX2		⇒ ["Cell"]
    ;;
    ;; TABLE-CELL-STYLE-NAME	⇒    BASE-STYLE-NAME ⨯ VALIGN x BORDER-STYLES
    ;; - VALIGN			⇒ ["" "Top" "Middle" "Bottom"]
    ;; - BORDER-STYLES		⇒ ["T" "B" "L" "R"] | [""]
    ;;

    (concat
     template-name
     "Table"
     (org-odt--table-cell-get-cell-variant table-cell info
                                           ;; 'usecolrowgroupsp
                                           org-odt-table-cell-variant-calculator)
     "Cell"
     (when table-cell-valign (capitalize (format "%s" table-cell-valign)))
     (when (memq 'border (assoc-default template-name org-odt-table-template-props))
       (mapconcat #'identity
		  (list
		   (when (memq 'above table-cell-borders) "T")
		   (when (memq 'below table-cell-borders) "B")
		   (when (memq 'left table-cell-borders) "L")
		   (when (memq 'right table-cell-borders) "R"))
		  "")))))

(defun org-odt--table-cell-widths (table info)
  (let* ((num-columns (cdr (org-odt-table-dimensions table info)))
	 (widths
	  (cond
	   ;; Case 1: Widths comes from `:widths'.
	   ((org-odt--read-attribute table :widths)
	    (mapcar (lambda (n) (* 1.0 (string-to-number n)))
		    (let ((widths (org-odt--read-attribute table :widths)))
		      (when (stringp widths)
			(split-string widths "\\(?:,[[:space:]]*\\)" t "\\(?:[[:space:]]+\\)")))))
	   ;; Case 2: Widths come from `:col-cookies'.
	   ((org-odt--read-attribute table :col-cookies)
	    (plist-get (org-odt--table-col-cookies table info) :widths))
	   ;; Case 3: Widths may be specified in column cookies.  But
	   ;; those widths aren't what we are looking for here.
	   ;; Widths specified in column cookies of a table are meant
	   ;; for comfortable viewing/editing of long tables within
	   ;; Emacs by shrinking one or more columns to a manageable
	   ;; width.  These widths are thus functionally different
	   ;; from column widths that one needs in exported documents.
	   ;; However, the old exporter, specifically the exporter
	   ;; prior to introduction of `:widths' and `:col-cookies'
	   ;; ODT attributes, overloaded widths _within_ the special
	   ;; row of a table to also mean widths of columns in
	   ;; exported document.  This was a convenient hack then.
	   ;; Continue to honour the old behaviour for backward
	   ;; compatibility, even though it is discouraged.
	   (t
	    (plist-get (org-odt--table-col-cookies table info) :widths)))))
    (setq widths (mapcar (lambda (w)
			   (if (or (null w) (zerop w)) 1 w))
			 (or widths (make-list num-columns 1))))

    (unless (= (length widths) num-columns)
      (user-error "You haven't specified widths of all columns: widths is %d, columns is %d"
                  (length widths) num-columns))

    (let* ((cum-width (apply #'+ widths))
	   (normalized-cum-width 1000))
      (cl-loop for width in widths
	       collect (/ (* normalized-cum-width width) cum-width)))))

(defun org-odt--table-col-cookies (table _info)
  (let* ((cookies
	  ;; Cookies come from ...
	  (or
	   ;; ... either the `:col-cookies' attribute
	   (let ((c (org-odt--read-attribute table :col-cookies)))
	     (when c
	       (setq c (replace-regexp-in-string "\\(?:[[:space:]]*\\)" "" c))
	       (when (and (string-prefix-p "|" c)
			  (string-suffix-p "|" c))
		 (split-string (substring c 1 -1) "|"))))
	   ;; ... or right from within the table.
	   (cl-loop for table-row in (org-element-contents table)
		    for table-cells = (org-element-contents table-row)
		    for contents = (cl-loop for table-cell in table-cells
					    collect (org-element-contents table-cell))
		    when (and (org-export-table-row-is-special-p table-row 'ignore)
			      (cl-every (lambda (c)
					  (and (null (cdr c))
					       (stringp (car c))
					       (string-match-p "\\`<[lrc]?\\([0-9]+\\)>\\'" (car c))))
					contents)
			      (mapcar (lambda (s)
					(substring-no-properties (car s)))
				      contents))
		    do (cl-return it)))))
    (cl-loop for (halign valign width) in
	     (cl-loop for s in cookies
		      collect (when (string-match "<?\\([lrc]\\)?\\([tmb]\\)?\\([0-9]+\\)?>?" s)
				(let* ((halign (match-string 1 s))
				       (valign (match-string 2 s))
				       (width (match-string 3 s)))
				  (list (assoc-default (when halign (string-to-char halign))
						       '((?l . left)
							 (?c . center)
							 (?r . right)))
					(assoc-default (when valign (string-to-char valign))
						       '((?t . top)
							 (?m . middle)
							 (?b . bottom)))
					(or (when width (string-to-number width)))))))
	     collecting halign into haligns
	     collecting valign into valigns
	     collecting width into widths
	     finally (return (list :haligns haligns :valigns valigns :widths widths)))))

(defun org-odt--table-cell-get-paragraph-style (table-cell info)
  "Get paragraph style for TABLE-CELL.
INFO is a plist holding contextual information.

Style names used for paragraphs in a table is a concatenation of
three components: BASE-STYLE CELL-TYPE CELL-ALIGNMENT.  These are
chosen as below:

   BASE-STYLE:     The base paragraph style, as specified by the user
		   with \"#+ATTR_ODT: :p-style ...\".  Or
		   \"OrgTable\", if none.

   CELL-TYPE:      One of \"Heading\" or \"Contents\", according
		   as the table cell is part of a header column /
		   header row or otherwise.

   CELL-ALIGNMENT: One of \"Left\", \"Right\" or \"Center\",
		   based on the column alignment.

By default, a table cell uses the base style \"OrgTable\".  So it
can have one of the following 8 styles:

  - OrgTableContents
  - OrgTableContentsLeft
  - OrgTableContentsRight
  - OrgTableContentsCenter
  - OrgTableHeading
  - OrgTableHeadingLeft
  - OrgTableHeadingRight
  - OrgTableHeadingCenter

The above styles are already defined for you in the default
styles file.

However, if you use a custom paragraph style, say \"MyOrgTable\",
for a table with

    #+ATTR_ODT: :p-style \"MyOrgTable\"
    |  <r> | <c>  | <l>  |
    | aaaa | bbbb | cccc |
    |------+------+------|
    | dddd | eeee | ffff |
    | gggg | hhhh | iiii |

then a table cell can have one of the following 8 styles:

  - MyOrgTableContents
  - MyOrgTableContentsLeft
  - MyOrgTableContentsRight
  - MyOrgTableContentsCenter
  - MyOrgTableHeading
  - MyOrgTableHeadingLeft
  - MyOrgTableHeadingRight
  - MyOrgTableHeadingCenter

You need to define the styles \"MyOrgTableContents\" and
\"MyOrgTableHeading\" as part of your styles file.  Rest of the 6
styles will be defined *automatically* for you."
  (let* ((table-cell-address (org-odt-table-cell-address table-cell info))
	 (c (cdr table-cell-address))
	 (style-spec (org-odt-table-style-spec table-cell info))
	 (template-name (if style-spec (nth 1 style-spec) "Org"))
	 (table (org-export-get-parent-table table-cell))
	 (p-style
	  (or (org-odt--read-attribute table :p-style)
	      (concat template-name "Table"))))
    ;; BASE-STYLE		⇒ TABLE-TEMPLAT-NAME x SUFFIX1
    ;; - TABLE-TEMPLAT-NAME	⇒ ["Org" "Custom"]
    ;; - SUFFIX1		⇒ "Table"
    ;;
    ;; TABLE-PARAGRAPH-STYLE	⇒ BASE-STYLE ⨯ VARIANT ⨯ SUFFIX2 ⨯ HALIGN
    ;; - VARIANT		⇒ ["" "FirstRow" "FirstColumn" "LastRow" "LastColumn"]
    ;; - SUFFIX2		⇒ ["Contents"]
    ;; - HALIGN			⇒ ["Left" "Center" "Right"]
    (concat
     p-style
     (org-odt--table-cell-get-cell-variant table-cell info
                                           ;; 'usecolrowgroupsp
                                           org-odt-table-cell-variant-calculator)
     "Contents"
     (capitalize
      (format "%s" (or
		    (cond
		     ;; Case 1: NOT a list table or a table with a
		     ;; paragraph-like content. That is, it is a
		     ;; regular Org table, with no `:col-cookies' line.
		     ((and
		       (not (or (org-odt--read-attribute table :list-table)
				(org-odt--table-type table info)))

		       (not (org-odt--read-attribute table :col-cookies)))
		      ;; Fall back to regular handling where the
		      ;; alignment may be _inferred_ based on
		      ;; heuristics.
		      (org-export-table-cell-alignment table-cell info))
		     ;; Case 2: This is a special table--a list table
		     ;; or a transcluded table.  Don't use
		     ;; `org-export-table-cell-alignment' to avoid
		     ;; running in to
		     ;; https://github.com/kjambunathan/org-mode-ox-odt/issues/25.
		     ;; Instead, go with what the user has explicitly
		     ;; indicated.
		     (t
		      (nth c (plist-get (org-odt--table-col-cookies table info)
					:haligns))))
		    ""))))))


(defun org-odt--table-read-cell-span-attribute (table)
  (unless (org-odt--read-attribute table :suppress-spans)
    (mapconcat #'identity
	       (org-odt--read-attribute table :span 'collect)
	       " ")))

(defun org-odt--table-read-cell-border-attribute  (table)
  (when (org-odt--read-attribute table :suppress-spans)
    (mapconcat #'identity
	       (org-odt--read-attribute table :span 'collect)
	       " ")))

(defun org-odt--table-get-cell-spans (table info)
  "Non-nil when TABLE has spanned row or columns.
INFO is a plist used as a communication channel.

A TABLE has spanned row or columns when it has a `:span'
attribute.  You can attach a `:span' attribute to a table as
follows:

    #+ATTR_ODT: :span \"@R1C1{ROWSPAN1:COLSPAN1} @R2C2{ROWSPAN2:COLSPAN2} ...\"
    | Some | table |
    | ...  | ...   |
    |      |       |

For example, an `org' table like the one below

    #+ATTR_ODT: :span \"@1$1{3:1} @1$2{1:8} @2$2{1:2} @2$4{1:2} @2$6{1:2} @2$8{1:2}\"
    |--------+-------+-----+-----+-----+-----+-----+-----+-----|
    | Region | Sales |     |     |     |     |     |     |     |
    |--------+-------+-----+-----+-----+-----+-----+-----+-----|
    |        | Q1    |     |  Q2 |     |  Q3 |     |  Q4 |     |
    |--------+-------+-----+-----+-----+-----+-----+-----+-----|
    |        | foo   | bar | foo | bar | foo | bar | foo | bar |
    |--------+-------+-----+-----+-----+-----+-----+-----+-----|
    | North  | 350   |  46 | 253 |  34 | 234 |  42 | 382 |  68 |
    | South  | 462   |  84 | 511 |  78 | 435 |  45 | 534 |  89 |
    |--------+-------+-----+-----+-----+-----+-----+-----+-----|

is equivalent to the following `table.el'-table with row and
column spans

    +--------+-------------------------------------------------+
    | Region |                      Sales                      |
    |        +-------------+-----------+-----------+-----------+
    |        | Q1          |    Q2     |    Q3     |    Q4     |
    |        +-------+-----+-----+-----+-----+-----+-----+-----+
    |        | foo   | bar | foo | bar | foo | bar | foo | bar |
    +--------+-------+-----+-----+-----+-----+-----+-----+-----+
    | North  | 350   |  46 | 253 |  34 | 234 |  42 | 382 |  68 |
    +--------+-------+-----+-----+-----+-----+-----+-----+-----+
    | South  | 462   |  84 | 511 |  78 | 435 |  45 | 534 |  89 |
    +--------+-------+-----+-----+-----+-----+-----+-----+-----+
."
  (let* ((cache (or (plist-get info :table-cell-widths-cache)
		    (let ((table (make-hash-table :test #'eq)))
		      (plist-put info :table-cell-widths-cache table)
		      table)))
	 (cached (gethash table cache 'no-cache)))
    (if (not (eq cached 'no-cache)) cached
      (puthash table
	       (let* ((span (org-odt--table-read-cell-span-attribute table)))
		 (cl-loop for spec in (when span (split-string span " "))
			  when (string-match "@\\([[:digit:]]+\\)\\$\\([[:digit:]]+\\){\\([[:digit:]]+\\):\\([[:digit:]]+\\)}" spec)
			  append
			  (pcase-let ((`(,pivot-r ,pivot-c ,rowspan ,colspan)
				       (mapcar (lambda (i)
						 (string-to-number (match-string i spec)))
					       (number-sequence 1 4))))
			    (cons (cons (cons pivot-r pivot-c) (cons rowspan colspan))
				  (cdr (apply #'append
					      (cl-loop for r in (number-sequence pivot-r (1- (+ pivot-r rowspan))) collect
						       (cl-loop for c in (number-sequence pivot-c (1- (+ pivot-c colspan)))
								collect (cons (cons r c) 'covered)))))))))
	       cache))))

(defun org-odt--table-cell-get-cell-span (table-cell info)
  (let* ((table (org-export-get-parent-table table-cell))
	 (address (org-odt-table-cell-address table-cell info))
	 (cell-spans (org-odt--table-get-cell-spans table info)))
    (cdr (assoc (cons (1+ (car address)) (1+ (cdr address)))
		cell-spans))))

(defun org-odt--table-get-cell-borders (table info)
  "Non-nil when TABLE has custom cell borders.
INFO is a plist used as a communication channel.

A TABLE has custom cell borders when it has a `:span' attribute,
together with a non-nil `:suppress-spans' attribute.

You can attach a `:span' attribute to a table as follows:

    #+ATTR_ODT: :span \"@R1C1{ROWSPAN1:COLSPAN1} @R2C2{ROWSPAN2:COLSPAN2} ...\"
    | Some | table |
    | ...  | ...   |
    |      |       |

For example, an `org' table like the one below

    #+ATTR_ODT: :suppress-spans t
    #+ATTR_ODT: :span \"@1$1{3:1} @1$2{1:8}\"
    #+ATTR_ODT: :span \"@2$2{1:2} @2$4{1:2} @2$6{1:2} @2$8{1:2}\"
    |--------+-------+-----+-----+-----+-----+-----+-----+-----|
    | Region | Sales |     |     |     |     |     |     |     |
    |--------+-------+-----+-----+-----+-----+-----+-----+-----|
    |        | Q1    |     |  Q2 |     |  Q3 |     |  Q4 |     |
    |--------+-------+-----+-----+-----+-----+-----+-----+-----|
    |        | foo   | bar | foo | bar | foo | bar | foo | bar |
    |--------+-------+-----+-----+-----+-----+-----+-----+-----|
    | North  | 350   |  46 | 253 |  34 | 234 |  42 | 382 |  68 |
    | South  | 462   |  84 | 511 |  78 | 435 |  45 | 534 |  89 |
    |--------+-------+-----+-----+-----+-----+-----+-----+-----|

is equivalent to the following `table.el'-table in a visual
sense.  The resemblance is visual only, and not geometric because
it doesn't generate spanned cells.  

    +---------+-------------------------------------------------+
    |Region   | Sales                                           |
    |         +-------------+-----------+-----------+-----------+
    |         | Q1          | Q2        | Q3        | Q4        |
    |         +-------+-----+-----+-----+-----+-----+-----+-----+
    |         | foo   | bar | foo | bar | foo | bar | foo | bar |
    +---------+-------+-----+-----+-----+-----+-----+-----+-----+
    | North   | 350   | 46  | 253 | 34  | 234 | 42  | 382 | 68  |
    +---------+-------+-----+-----+-----+-----+-----+-----+-----+
    | South   | 462   | 84  | 511 | 78  | 435 | 45  | 534 | 89  |
    +---------+-------+-----+-----+-----+-----+-----+-----+-----+

The example `org' and `table.el' tables above, use empty cells to
illustrate the grid structure.

In order to study the effect of `:suppress-spans t' attribute,
fill the empty cells of the example `org' table above, and export
with and without `:suppress-spans' attribute.  Examine the cell
boundaries of resulting table in `LibreOffice'.

        *Aside*: You can examine the table cell
        boundaries--irrespective of their borders--in either of
        the following ways

            - In `LibreOffice', `Menu bar -> View -> Table Boundaries'

            - Attach `#+ATTR_ODT: :style \"GriddedTable\"' to an `org' table.

You will notice that

    - when spans are supressed, the `org' text in all the cells carry over
      to ODT output.  The ODT table will have Row x Column number of cells.
      
    - when spans are allowed, the `org' text in spanned cells, (save for the
      pivot cell @R$C) are stripped from the ODT output.  In this case, the
      ODT table will have less than Row x Column number of cells.

In other words,

    - Suppress spans when you want to control the grid structure of the
      table.
      
    - Allow spans when you want the text in the pivot cell @R$C to flow over
      seamlessly to the neighbouring cells and occupy the whole of ROWSPAN x
      COLSPAN number of neighbouring cells. 

Here is another real-world example where `:suppress-spans t'
could be helpful.

    #+ATTR_ODT: :rel-width 50
    #+ATTR_ODT: :col-cookies \"| c | c | c |\"
    #+ATTR_ODT: :suppress-spans t
    #+ATTR_ODT: :span \"@2$1{2:1} @2$2{2:1} @2$3{4:1}\"
    #+ATTR_ODT: :span \"@4$1{2:1} @4$2{2:1}\"
    | $p$ | $q$ | $p \rightarrow q$ |
    |-----+-----+-------------------|
    |   0 |   0 |                 1 |
    |   0 |   1 |                 1 |
    |-----+-----+-------------------|
    |   1 |   0 |                 0 |
    |   1 |   1 |                 1 |
    |-----+-----+-------------------|

For more information, on generating with spanned cells see
`org-odt--table-get-cell-spans'."
  (let* ((cache (or (plist-get info :table-cell-borders-cache)
		    (let ((table (make-hash-table :test #'eq)))
		      (plist-put info :table-cell-borders-cache table)
		      table)))
	 (cached (gethash table cache 'no-cache)))
    (if (not (eq cached 'no-cache)) cached
      (puthash table
	       (let* ((span (org-odt--table-read-cell-border-attribute table)))
		 (cl-loop for spec in (when span (split-string span " "))
			  when (string-match "@\\([[:digit:]]+\\)\\$\\([[:digit:]]+\\){\\([[:digit:]]+\\):\\([[:digit:]]+\\)}" spec)
			  append
			  (pcase-let ((`(,pivot-r ,pivot-c ,rowspan ,colspan)
				       (mapcar (lambda (i)
						 (string-to-number (match-string i spec)))
					       (number-sequence 1 4))))
			    (cl-loop with first-r = pivot-r
				     with last-r = (1- (+ pivot-r rowspan))
				     with first-c = pivot-c
				     with last-c = (1- (+ pivot-c colspan))
				     for r in (number-sequence first-r last-r) append
				     (cl-loop for c in (number-sequence first-c last-c)
					      collect (cons (cons r c)
							    (list
							     (when (= r first-r)
							       'above)
							     (when (= r last-r)
							       'below)
							     (when (= c first-c)
							       'left)
							     (when (= c last-c)
							       'right))))))))
	       cache))))

(defun org-odt--table-cell-get-cell-border (table-cell info)
  (let* ((table (org-export-get-parent-table table-cell))
	 (address (org-odt-table-cell-address table-cell info))
	 (cell-borders (org-odt--table-get-cell-borders table info)))
    (cond
     ((org-odt--table-read-cell-border-attribute table)
      (or (assoc-default (cons (1+ (car address)) (1+ (cdr address)))
			 cell-borders)
	  '(left right above below)))
     (t
      (org-export-table-cell-borders table-cell info)))))

(declare-function org-ods-table-cell "ox-ods" (table-cell contents info))

(defun org-odt-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((cell-span (org-odt--table-cell-get-cell-span table-cell info)))
    (unless contents (setq contents ""))
    (cond
     ((eq cell-span 'covered) "\n<table:covered-table-cell/>")
     (t
      (concat
       (let* ((table-cell-contents (org-element-contents table-cell))
	      (table-cell-contents-is-an-element-p (eq (org-element-class (car table-cell-contents)) 'element))
	      (style-name (org-odt--table-cell-get-cell-style table-cell info))
	      (attributes
	       (or (when (consp cell-span)
		     (pcase-let ((`(,rowspan . ,colspan) cell-span))
		       (concat
			(unless (= 1 rowspan)
			  (format " table:number-rows-spanned=\"%d\""
				  rowspan))
			(unless (= 1 colspan)
			  (format " table:number-columns-spanned=\"%d\""
				  colspan)))))
		   ""))
	      (ods-plist (when (eq org-export-current-backend 'ods)
			   (let ((f 'org-ods-table-cell))
			     (funcall f table-cell contents info)))))
	 (cond
	  (table-cell-contents-is-an-element-p
	   (format "\n<table:table-cell table:style-name=\"%s\" %s>\n%s\n</table:table-cell>"
		   style-name
		   attributes
		   contents))
	  (t
	   (format "\n<table:table-cell table:style-name=\"%s\" %s>\n%s\n</table:table-cell>"
		   (concat style-name
			   ;; In case of ODS backend, suffix data type to style name.
			   (or (when-let* ((data-type (plist-get ods-plist :data-type)))
				 (capitalize (format "%s" data-type)))
			       ""))
		   (concat attributes
			   ;; In case of ODS backend, add data type attributes.
			   (or (plist-get ods-plist :attributes) ""))
		   (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
			   (org-odt--table-cell-get-paragraph-style table-cell info)
			   ;; In case of ODS backend, use the contents provided by it.
			   (or (plist-get ods-plist :contents)
			       contents))))))
       "\n")))))

(defun org-odt--table-type (element info)
  (let* ((table (if (eq (org-element-type element) 'table) element
		  (org-export-get-parent-table element))))
    (let* ((cache (or (plist-get info :table-type-cache)
		      (let ((table (make-hash-table :test #'eq)))
			(plist-put info :table-type-cache table)
			table)))
	   (cached (gethash table cache 'no-cache)))
      (if (not (eq cached 'no-cache)) cached
	(let ((table-type
	       (cl-block outer
		 (cl-loop for table-row in (org-element-contents table)
			  when (and (eq (org-element-property :type table-row) 'standard)
				    (not (org-export-table-row-is-special-p table-row info)))
			  do (cl-loop for table-cell in (org-element-contents table-row)
				      for cell-type = (eq (org-element-class (car (org-element-contents table-cell)))
							  'element)
				      when cell-type do (cl-return-from outer 'special))))))
	  (puthash table table-type cache)
	  table-type)))))


;;;; Table Row

(defun org-odt-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to ODT.
CONTENTS is the contents of the row.  INFO is a plist used as a
communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((rowgroup-tags
	    (if (and (= 1 (org-export-table-row-group table-row info))
		     (org-export-table-has-header-p
		      (org-export-get-parent-table table-row) info))
		;; If the row belongs to the first rowgroup and the
		;; table has more than one row groups, then this row
		;; belongs to the header row group.
		'("\n<table:table-header-rows>" . "\n</table:table-header-rows>")
	      ;; Otherwise, it belongs to non-header row group.
	      '("\n<table:table-rows>" . "\n</table:table-rows>"))))
      (concat
       ;; Does this row begin a rowgroup?
       (when (org-export-table-row-starts-rowgroup-p table-row info)
	 (car rowgroup-tags))
       ;; Actual table row
       (let* ((custom-table-style (nth 1 (org-odt-table-style-spec table-row info)))
	      (table-style (or custom-table-style "OrgTable"))
	      (row-style (format "%sRow" table-style)))
	 (format "\n<table:table-row table:style-name=\"%s\">\n%s\n</table:table-row>"
		 row-style contents))
       ;; Does this row end a rowgroup?
       (when (org-export-table-row-ends-rowgroup-p table-row info)
	 (cdr rowgroup-tags))))))


;;;; Table

(defun org-odt--table-get-group-dimensions (table info)
  (cons (car (last (org-odt--table-get-rowgroup-numbers table info)))
	(car (last (org-odt--table-get-colgroup-numbers table info)))))

(defun org-odt--table-get-rowgroup-numbers (table info)
  (let* ((data-row-p (lambda (table-row)
		       (and (eq (org-element-property :type table-row) 'standard)
			    (not (org-export-table-row-is-special-p table-row 'ignore))))))
    (cl-loop for table-row in (org-element-contents table) counting t into r
	     when (funcall data-row-p table-row)
	     collect (org-export-table-row-group table-row info))))

(defun org-odt--table-get-colgroup-numbers (table info)
  (let* ((data-row-p (lambda (table-row)
		       (when
			   (and (eq (org-element-property :type table-row) 'standard)
				(not (org-export-table-row-is-special-p table-row 'ignore)))
			 table-row)))
	 (first-data-row (cl-loop for table-row in (org-element-contents table)
				  thereis (funcall data-row-p table-row)))
	 (ns (let ((n 0))
	       (org-element-map first-data-row 'table-cell
		 (lambda (table-cell)
		   (if (memq 'left (org-export-table-cell-borders table-cell info))
		       (cl-incf n)
		     n))
	      info nil 'table-cell))))
    (if (zerop (car ns)) (mapcar #'1+ ns) ns)))

(defun org-odt--table (table contents info)
  "Transcode a TABLE element from Org to ODT.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (cl-case (org-element-property :type table)
    ;; Case 1: table.el doesn't support export to OD format.  Strip
    ;; such tables from export.
    (table.el
     (prog1 nil
       (message
	(concat
	 "(ox-odt): Found table.el-type table in the source Org file."
	 "  table.el doesn't support export to ODT format."
	 "  Stripping the table from export."))))
    ;; Case 2: Native Org tables.
    (otherwise
     (let* ((captions-plist (org-odt-format-label table info 'definition))
	    (caption (plist-get captions-plist :caption))
            (short-caption (plist-get captions-plist :short-caption))
	    (caption-position (plist-get captions-plist :caption-position))
	    (custom-table-style (nth 1 (org-odt-table-style-spec table info)))
	    (table-column-specs
	     (lambda (table info)
	       (let* ((table-style (or custom-table-style "OrgTable"))
		      (column-style (format "%sColumn" table-style))
		      (widths (org-odt--table-cell-widths table info)))
		 (cond
		  (widths
		   (mapconcat
		    (lambda (width)
		      (let ((derived-column-style (org-odt--name-object info 'table-column column-style)))
			(plist-put info :odt-automatic-styles
				   (concat (plist-get info :odt-automatic-styles)
					   (format
					    "
					   <style:style style:name=\"%s\" style:family=\"table-column\" style:parent-style-name=\"%s\">
					        <style:table-column-properties style:rel-column-width=\"%d*\"/>
					   </style:style>"
					    derived-column-style column-style width)))
			(format "\n<table:table-column table:style-name=\"%s\"/>"
				derived-column-style)))
		    widths "\n"))
		  (t
		   (format "\n<table:table-column table:style-name=\"%s\" table:number-columns-repeated=\"%d\"/>"
			   column-style
			   (cdr (org-odt-table-dimensions table info))))))))
	    (text (concat
		   ;; begin table.
		   (format
		    "\n<table:table table:style-name=\"%s\"%s>"
		    (let* ((base-style-name (or custom-table-style "OrgTable"))
			   (props (org-odt--read-attribute table))
			   (rel-width (let ((value (plist-get props :rel-width)))
					(if (numberp value) value 96)))
			   (pagebreak-type (let ((page-break (plist-get props :page-break)))
					     (cond
					      ((null page-break)
					       nil)
					      ((string= (plist-get props :page-break) "after")
					       'after)
					      (t 'before))))
			   (master-page-name (let ((page-style (plist-get props :page-style)))
					       ;; When setting up a page break after a table,
					       ;; LibreOffice v7.2.1.2 (Community edition) UI
					       ;; doesn't enable either the page style or page
					       ;; number.  This seems to be a bug or a lack of
					       ;; feature.  For now, mimic the UI behaviour.
					       (unless (eq pagebreak-type 'after)
						 page-style)))
			   (page-number (let ((n (plist-get props :page-number)))
					  ;; Suppress page-number when there is no explicit
					  ;; master page style specified.
					  (when (and (numberp n) master-page-name)
					    n)))
			   style-name)
		      (if (not props) base-style-name
			(setq style-name (format "Org%s" (org-odt--name-object info 'table)))
			(plist-put info :odt-automatic-styles
				   (concat (plist-get info :odt-automatic-styles)
					   (format org-odt-table-style-format
						   style-name
						   ;; style:style attributes
						   (concat
						    (format " style:parent-style-name=\"%s\""
							    base-style-name)
						    (or
						     (cl-case pagebreak-type
						       (before
							(if master-page-name
							    (format " style:master-page-name=\"%s\""
								    master-page-name)
							  ""))
						       (after
							(format " style:master-page-name=\"%s\""
								(or master-page-name "")))
						       (otherwise))
						     ""))
						   ;; style:table-properties
						   (concat
						    (format " style:rel-width=\"%.2f%%\" " rel-width)
						    (cl-case pagebreak-type
						      (before
						       (if (org-string-nw-p master-page-name)
							   (concat
							    " fo:break-before=\"auto\" fo:break-after=\"auto\""
							    (format
							     " style:page-number=\"%s\""
							     (if page-number (number-to-string page-number)
							       "auto")))
							 " fo:break-before=\"page\""))
						      (after
						       (format
							" style:page-number=\"%s\" fo:break-after=\"page\""
							(if page-number (number-to-string page-number)
							  "auto")))
						      (otherwise
						       "")))))))
		      style-name)
		    (concat (when short-caption
			      (format " table:name=\"%s\"" short-caption))))
		   ;; column specification.
		   (funcall table-column-specs table info)
		   ;; actual contents.
		   "\n" contents
		   ;; end table.
		   "</table:table>")))
       (cl-case caption-position
	 (above (concat caption text))
	 (t (concat text caption)))))))

(defun org-odt-table (table contents info)
  "Transcode a TABLE element from Org to ODT.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information.

Use `org-odt--table' to typeset the table.  Handle details
pertaining to indentation here."
  (let* ((--element-preceded-by-table-p
	  (lambda (element info)
	    (cl-loop for el in (org-export-get-previous-element element info t)
		     thereis (eq (org-element-type el) 'table))))
	 (--walk-list-genealogy-and-collect-tags
	  (lambda (table info)
	    (let* ((genealogy (org-element-lineage table))
		   (list-genealogy
		    (when (eq (org-element-type (car genealogy)) 'item)
		      (cl-loop for el in genealogy
			       when (memq (org-element-type el)
					  '(item plain-list))
			       collect el)))
		   (llh-genealogy
		    (apply 'nconc
			   (cl-loop for el in genealogy
				    when (and (eq (org-element-type el) 'headline)
					      (org-export-low-level-p el info))
				    collect
				    (list el
					  (assq 'headline
						(org-element-contents
						 (org-export-get-parent el)))))))
		   parent-list)
	      (nconc
	       ;; Handle list genealogy.
	       (cl-loop for el in list-genealogy collect
			(cl-case (org-element-type el)
			  (plain-list
			   (setq parent-list el)
			   (cons "</text:list>"
				 (format "\n<text:list text:style-name=\"%s\" %s>"
					 (cl-case (org-element-property :type el)
					   (ordered "OrgNumberedList")
					   (unordered "OrgBulletedList")
					   (descriptive-1 "OrgDescriptionList")
					   (descriptive-2 "OrgDescriptionList"))
					 "text:continue-numbering=\"true\"")))
			  (item
			   (cond
			    ((not parent-list)
			     (if (funcall --element-preceded-by-table-p table info)
				 '("</text:list-header>" . "<text:list-header>")
			       '("</text:list-item>" . "<text:list-header>")))
			    ((funcall --element-preceded-by-table-p
				      parent-list info)
			     '("</text:list-header>" . "<text:list-header>"))
			    (t '("</text:list-item>" . "<text:list-item>"))))))
	       ;; Handle low-level headlines.
	       (cl-loop for el in llh-genealogy
			with step = 'item collect
			(cl-case step
			  (plain-list
			   (setq step 'item) ; Flip-flop
			   (setq parent-list el)
			   (cons "</text:list>"
				 (format "\n<text:list text:style-name=\"%s\" %s>"
					 (if (org-export-numbered-headline-p
					      el info)
					     "OrgNumberedList"
					   "OrgBulletedList")
					 "text:continue-numbering=\"true\"")))
			  (item
			   (setq step 'plain-list) ; Flip-flop
			   (cond
			    ((not parent-list)
			     (if (funcall --element-preceded-by-table-p table info)
				 '("</text:list-header>" . "<text:list-header>")
			       '("</text:list-item>" . "<text:list-header>")))
			    ((let ((section? (org-export-get-previous-element
					      parent-list info)))
			       (and section?
				    (eq (org-element-type section?) 'section)
				    (assq 'table (org-element-contents section?))))
			     '("</text:list-header>" . "<text:list-header>"))
			    (t
			     '("</text:list-item>" . "<text:list-item>"))))))))))
	 (close-open-tags (funcall --walk-list-genealogy-and-collect-tags
				   table info)))
    ;; OpenDocument schema does not permit table to occur within a
    ;; list item.

    ;; One solution - the easiest and lightweight, in terms of
    ;; implementation - is to put the table in an indented text box
    ;; and make the text box part of the list-item.  Unfortunately if
    ;; the table is big and spans multiple pages, the text box could
    ;; overflow.  In this case, the following attribute will come
    ;; handy.

    ;; ,---- From OpenDocument-v1.1.pdf
    ;; | 15.27.28 Overflow behavior
    ;; |
    ;; | For text boxes contained within text document, the
    ;; | style:overflow-behavior property specifies the behavior of text
    ;; | boxes where the containing text does not fit into the text
    ;; | box.
    ;; |
    ;; | If the attribute's value is clip, the text that does not fit
    ;; | into the text box is not displayed.
    ;; |
    ;; | If the attribute value is auto-create-new-frame, a new frame
    ;; | will be created on the next page, with the same position and
    ;; | dimensions of the original frame.
    ;; |
    ;; | If the style:overflow-behavior property's value is
    ;; | auto-create-new-frame and the text box has a minimum width or
    ;; | height specified, then the text box will grow until the page
    ;; | bounds are reached before a new frame is created.
    ;; `----

    ;; Unfortunately, LibreOffice-3.4.6 doesn't honor
    ;; auto-create-new-frame property and always resorts to clipping
    ;; the text box.  This results in table being truncated.

    ;; So we solve the problem the hard (and fun) way using list
    ;; continuations.

    ;; The problem only becomes more interesting if you take in to
    ;; account the following facts:
    ;;
    ;; - Description lists are simulated as plain lists.
    ;; - Low-level headlines can be listified.
    ;; - In Org mode, a table can occur not only as a regular list
    ;;   item, but also within description lists and low-level
    ;;   headlines.

    ;; See `org-odt--translate-description-lists' and
    ;; `org-odt-translate-low-level-headlines' for how this is
    ;; tackled.

    (concat "\n"
	    ;; Discontinue the list.
	    (mapconcat 'car close-open-tags "\n")
	    ;; Put the table in an indented section.
	    (let* ((contents-1 (org-odt--table table contents info))
		   (level (/ (length (mapcar 'car close-open-tags)) 2)))
	      (when contents-1
		(if (zerop level) contents-1
		  (org-odt-text:section table contents-1 info level))))
	    ;; Continue the list.
	    (mapconcat 'cdr (nreverse close-open-tags) "\n"))))


;;;; Target

(defun org-odt-target (target _contents info)
  "Transcode a TARGET object from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-odt--target "" (org-export-get-reference target info)))


;;;; Timestamp

(defun org-odt-timestamp (timestamp _contents info)
  "Transcode a TIMESTAMP object from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((_raw-value (org-element-property :raw-value timestamp))
	 (type (org-element-property :type timestamp)))
    (if (not (plist-get info :odt-use-date-fields))
	(let ((value (org-odt-plain-text
		      (org-timestamp-translate timestamp) info)))
	  (cl-case (org-element-property :type timestamp)
	    ((active active-range)
	     (format "<text:span text:style-name=\"%s\">%s</text:span>"
		     "OrgActiveTimestamp" value))
	    ((inactive inactive-range)
	     (format "<text:span text:style-name=\"%s\">%s</text:span>"
		     "OrgInactiveTimestamp" value))
	    (otherwise value)))
      (cl-case type
	(active
	 (format "<text:span text:style-name=\"%s\">%s</text:span>"
		 "OrgActiveTimestamp"
		 (format "%s" (org-odt--format-timestamp timestamp))))
	(inactive
	 (format "<text:span text:style-name=\"%s\">%s</text:span>"
		 "OrgInactiveTimestamp"
		 (format "%s" (org-odt--format-timestamp timestamp))))
	(active-range
	 (format "<text:span text:style-name=\"%s\">%s</text:span>"
		 "OrgActiveTimestamp"
		 (format "&lt;%s&gt;&#x2013;&lt;%s&gt;"
			 (org-odt--format-timestamp timestamp)
			 (org-odt--format-timestamp timestamp 'end))))
	(inactive-range
	 (format "<text:span text:style-name=\"%s\">%s</text:span>"
		 "OrgInactiveTimestamp"
		 (format "[%s]&#x2013;[%s]"
			 (org-odt--format-timestamp timestamp)
			 (org-odt--format-timestamp timestamp 'end))))
	(otherwise
	 (format "<text:span text:style-name=\"%s\">%s</text:span>"
		 "OrgDiaryTimestamp"
		 (org-odt-plain-text (org-timestamp-translate timestamp)
				     info)))))))


;;;; Underline

(defun org-odt-underline (_underline contents _info)
  "Transcode UNDERLINE from Org to ODT.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "Underline" contents))


;;;; Verbatim

(defun org-odt-verbatim (verbatim _contents _info)
  "Transcode a VERBATIM object from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "OrgCode" (org-odt--encode-plain-text
		     (org-element-property :value verbatim))))


;;;; Verse Block

(defun org-odt-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to ODT.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  ;; Add line breaks to each line of verse.
  (setq contents (replace-regexp-in-string
		  "\\(<text:line-break/>\\)?[ \t]*\n"
		  "<text:line-break/>" contents))
  ;; Replace tabs and spaces.
  (setq contents (org-odt--encode-tabs-and-spaces contents))
  ;; Surround it in a verse environment.
  (org-odt-paragraph verse-block contents info))



;;; Filters

;;;; Clickable images

(defun org-odt--translate-clickable-images (data _backend info)
  ;; Clickable images are links whose description is an inline image.
  ;; For example,
  ;;
  ;; [[https://orgmode.org][file:./org-mode-unicorn.png]]
  ;;
  ;; The Org parser does not recognize links within link description
  ;; and any link within link description appears as textual content.
  ;; This function peeks inside textual link description for inline
  ;; images, and converts those in to equivalent parse tree.
  (org-element-map data 'link
    (lambda (l)
      (let* ((link-desc
	      (org-element-interpret-data (org-element-contents l)))
	     (inner-link
	      (with-temp-buffer
		;; Grok link description
		(save-excursion
		  (insert link-desc))
		(let* ((link? (org-element-link-parser)))
		  (when (and
			 ;; Is it is a link?
			 link?
			 ;; Is there anything else riding behind it?
			 (= (org-element-property :end link?) (point-max)))
		    ;; Is it an inline image?
		    (org-odt--standalone-link-p
		     link? info nil
		     (lambda (l)
		       (org-export-inline-image-p
			l (plist-get info :odt-inline-image-rules))))
		    link?)))))
	(when inner-link
	  ;; Replace textual contents with it's parsed equivalent.
	  (org-element-set-contents l inner-link))))
    info nil nil t)
  data)

;;;; LaTeX fragments

;;;;; Starmath-specific

(defun org-odt--extract-starmath-from-latex-frag (&optional latex-frag)
  (let* ((latex-frag (or latex-frag
			 (buffer-substring-no-properties (region-beginning) (region-end))))
	 (latex-frag (org-trim latex-frag)))
    (cond
     ((and (string-prefix-p "$$" latex-frag)
	   (string-suffix-p "$$" latex-frag))
      (substring latex-frag 2 -2))
     ((and (string-prefix-p "$" latex-frag)
	   (string-suffix-p "$" latex-frag))
      (substring latex-frag 1 -1))
     ((and (string-prefix-p "\\[" latex-frag)
	   (string-suffix-p "\\]" latex-frag))
      (substring latex-frag 2 -2))
     ((and (string-prefix-p "\\(" latex-frag)
	   (string-suffix-p "\\)" latex-frag))
      (substring latex-frag 2 -2))
     ((string-match (rx-to-string '(seq "\\begin{" (group (one-or-more (any "0-9A-Za-z" "*\\"))) "}")) latex-frag)
      (let* ((prefix (match-string 1 latex-frag))
	     (preamble (format "\\begin{%s}" prefix))
	     (postamble (format "\\end{%s}" prefix)))
	(when (string-suffix-p postamble latex-frag)
	  (substring latex-frag (length preamble) (- (length postamble))))))
     (t (error "Couldn't match latex-frag: %S" latex-frag)))))

(defun org-create-math-formula-from-starmath (latex-frag &optional mathml-file)
  "Convert LATEX-FRAG to MathML and store it in MATHML-FILE.

LATEX-FRAG here is in fact _not_ a latex fragment as such, but a
startmath formula delimited using .  If the conversion is
successful, return the portion between \"<math...> </math>\"
elements otherwise return nil.  When MATHML-FILE is specified,
write the results in to that file.  When invoked as an
interactive command, prompt for LATEX-FRAG, with initial value
set to the current active region and echo the results for user
inspection."
  (interactive (list (let ((frag (when (org-region-active-p)
				   (buffer-substring-no-properties
				    (region-beginning) (region-end)))))
		       (read-string "LaTeX Fragment: " frag nil frag))))
  (let* ((starmath (org-odt--extract-starmath-from-latex-frag latex-frag))
	 (_ (message "\n->%s<-\n\n-->%s<--\n" latex-frag starmath))
	 (mathml
	  (format
	   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<math xmlns=\"http://www.w3.org/1998/Math/MathML\" display=\"block\">
  <semantics>
    <mi/>
    <annotation encoding=\"StarMath 5.0\">%s</annotation>
 </semantics>
</math>
"
	   (org-odt--encode-plain-text starmath t))))
    (when mathml-file
      (message "Wrote %s" mathml-file)
      (write-region mathml nil mathml-file))
    mathml))


;;;;; Translate LaTeX / Starmath fragments to mathml, image etc

(defun org-odt--translate-latex-fragments (tree _backend info)
  (let* ((starmathp (string= (plist-get info :odt-math-syntax) "starmath"))
	 (processing-type (if starmathp 'mathml
			    (plist-get info :with-latex)))
	 (count 0))
    ;; Normalize processing-type to one of dvipng, mathml or verbatim.
    ;; If the desired converter is not available, force verbatim
    ;; processing.
    (cl-case processing-type
      ((t mathml)
       (cond
	(starmathp
	 (message "Assuming LaTeX fragments  are in starmath format."))
	(t
	 (if (and (fboundp 'org-format-latex-mathml-available-p)
		  (org-format-latex-mathml-available-p))
	     (setq processing-type 'mathml)
	   (message "LaTeX to MathML converter not available.")
	   (setq processing-type 'verbatim)))))
      ((dvipng imagemagick)
       (unless (and (org-check-external-command "latex" "" t)
		    (org-check-external-command
		     (if (eq processing-type 'dvipng) "dvipng" "convert") "" t))
	 (message "LaTeX to PNG converter not available.")
	 (setq processing-type 'verbatim)))
      (otherwise
       (message "Unknown LaTeX option.  Forcing verbatim.")
       (setq processing-type 'verbatim)))

    ;; Store normalized value for later use.
    (when (plist-get info :with-latex)
      (plist-put info :with-latex processing-type))
    (message "Formatting LaTeX using %s" processing-type)

    ;; Convert `latex-fragment's and `latex-environment's.
    (when (memq processing-type '(mathml dvipng imagemagick))
      (org-element-map tree '(latex-fragment latex-environment)
	(lambda (latex-*)
	  (cl-incf count)
	  (let* ((latex-frag (org-element-property :value latex-*))
		 (input-file (plist-get info :input-file))
		 (cache-dir (file-name-directory input-file))
		 (cache-subdir (concat
				(cl-case processing-type
				  ((dvipng imagemagick) "ltxpng/")
				  (mathml
				   (if starmathp "ltxstarmath/" "ltxmathml/")))
				(file-name-sans-extension
				 (file-name-nondirectory input-file))))
		 (display-msg
		  (cl-case processing-type
		    ((dvipng imagemagick)
		     (format "Creating LaTeX Image %d..." count))
		    (mathml
		     (format "Creating MathML snippet from %s fragment %d..."
			     (if starmathp "Starmath" "LaTeX")
			     count))))
		 ;; Get an Org-style link to PNG image or the MathML
		 ;; file.
		 (org-link
		  (let ((link
			 (cond
			  (starmathp
			   (unwind-protect
			       (let* ((org-latex-to-mathml-convert-command "emacs"))
				 (advice-add 'org-create-math-formula :override
					     'org-create-math-formula-from-starmath)
				 (with-temp-buffer
				   (insert latex-frag)
				   (org-format-latex cache-subdir nil nil cache-dir
						     nil display-msg nil 'mathml)
				   (buffer-substring-no-properties
				    (point-min) (point-max))))
			     (advice-remove 'org-create-math-formula
					    'org-create-math-formula-from-starmath)))
			  (t
			   (with-temp-buffer
			     (insert latex-frag)
			     (org-format-latex cache-subdir nil nil cache-dir
					       nil display-msg nil processing-type)
			     (buffer-substring-no-properties
			      (point-min) (point-max)))))))
		    (if (string-match-p "file:\\([^]]*\\)" link) link
		      (prog1 nil (message "LaTeX Conversion failed."))))))
	    (when org-link
	      ;; Conversion succeeded.  Parse above Org-style link to a
	      ;; `link' object.
	      (let* ((link (car (org-element-map (with-temp-buffer
						   (org-mode)
						   (insert org-link)
						   (org-element-parse-buffer))
				    'link 'identity))))
		;; Orphan the link.
		(org-element-put-property link :parent nil)
		(let* (
		       (replacement
			(cl-case (org-element-type latex-*)
			  ;; Case 1: LaTeX environment.
			  ;; Mimic a "standalone image or formula" by
			  ;; enclosing the `link' in a `paragraph'.
			  ;; Copy over original attributes, captions to
			  ;; the enclosing paragraph.
			  (latex-environment
			   (org-element-adopt-elements
			       (list 'paragraph
				     (list ;; :style "OrgFormula"
				      :name (org-element-property :name latex-*)
				      :caption (org-element-property :caption latex-*)))
			     link))
			  ;; Case 2: LaTeX fragment.
			  ;; No special action.
			  (latex-fragment link))))
		  ;; Restore the blanks after the initial element or object.
		  (org-element-put-property
		   replacement :post-blank
		   (org-element-property :post-blank latex-*))
		  ;; Note down the object that link replaces.
		  (org-element-put-property replacement :replaces
					    (list (org-element-type latex-*)
						  (list :value latex-frag)))
		  ;; Replace now.
		  (org-element-set-element latex-* replacement))))))
	info nil nil t)))
  tree)


;;;; Description lists

;; This translator is necessary to handle indented tables in a uniform
;; manner.  See comment in `org-odt--table'.

;; Depending on user option `org-odt-description-list-style',
;; description lists can be typeset either as in HTML documents or as
;; in LaTeX documents.

(defun org-odt--translate-description-lists/html (tree _backend info)
  ;; OpenDocument has no notion of a description list.  So simulate it
  ;; using plain lists.  Description lists in the exported document
  ;; are typeset in the same manner as they are in a typical HTML
  ;; document.  See `org-odt--translate-description-lists/latex' for
  ;; yet another way of translation.
  ;;
  ;; Specifically, a description list like this:
  ;;
  ;; 	 ,----
  ;; 	 | - term-1 :: definition-1
  ;; 	 |
  ;; 	 | 	    paragraph-1
  ;; 	 |
  ;; 	 | - term-2 :: definition-2
  ;; 	 |
  ;; 	 | 	    paragraph-2
  ;; 	 `----
  ;;
  ;; gets translated in to the following form:
  ;;
  ;; 	 ,----
  ;; 	 | - term-1
  ;;     |
  ;; 	 |   - definition-1
  ;; 	 |
  ;; 	 |     paragraph-1
  ;; 	 |
  ;; 	 | - term-2
  ;;     |
  ;; 	 |   - definition-2
  ;; 	 |
  ;; 	 |     paragraph-2
  ;; 	 `----
  ;;
  ;; Further effect is achieved by fixing the OD styles as below:
  ;;
  ;; 1. Set the :type property of the simulated lists to
  ;;    `descriptive-1' and `descriptive-2'.  Map these to list-styles
  ;;    that has *no* bullets whatsoever.
  ;;
  ;; 2. The paragraph containing the definition term is styled to be
  ;;    in bold.
  ;;
  (org-element-map tree 'plain-list
    (lambda (el)
      (when (eq (org-element-property :type el) 'descriptive)
	(org-element-set-element
	 el
	 (apply 'org-element-adopt-elements
		(list 'plain-list (list :type 'descriptive-1))
		(mapcar
		 (lambda (item)
		   (org-element-adopt-elements
		       (list 'item (list :checkbox (org-element-property
						    :checkbox item)))
		     (list 'paragraph nil
			   (list 'bold (list :style "OrgDescriptionTerm")
				 (or (org-element-property :tag item) "(no term)")))
		     (org-element-adopt-elements
			 (list 'plain-list (list :type 'descriptive-2))
		       (apply 'org-element-adopt-elements
			      (list 'item nil)
			      (org-element-contents item)))))
		 (org-element-contents el)))))
      nil)
    info)
  tree)

(defun org-odt--translate-description-lists/latex (tree _backend info)
  ;; OpenDocument has no notion of a description list.  So simulate it
  ;; using plain lists.  Description lists in the exported document
  ;; are typeset in the same manner as they are in a typical LaTeX
  ;; style document.  See `org-odt--translate-description-lists/html'
  ;; for yet another way of translation.
  ;;
  ;; Specifically, a description list like this:
  ;;
  ;; 	,----
  ;; 	| - term-1 :: definition-1
  ;; 	|
  ;; 	| 	    paragraph-1
  ;; 	|
  ;; 	| - term-2 :: definition-2
  ;; 	|
  ;; 	| 	    paragraph-2
  ;; 	`----
  ;;
  ;; gets translated in to the following form:
  ;;
  ;; 	 ,----
  ;; 	 | - *term-1* definition-1
  ;; 	 |
  ;; 	 |   - paragraph-1
  ;; 	 |
  ;; 	 | - *term-2* definition-2
  ;; 	 |
  ;; 	 |   - paragraph-2
  ;; 	 `----
  ;;
  ;; Further effect is achieved by fixing the OD styles as below:
  ;;
  ;; 1. Set the :type property of the simulated lists to
  ;;    `descriptive-1' and `descriptive-2'.  Map these to list-styles
  ;;    that has *no* bullets whatsoever.
  ;;
  ;; 2. The paragraph containing the definition term is styled to be
  ;;    use hanging indent.
  ;;
  (org-element-map tree 'plain-list
    (lambda (el)
      (when (eq (org-element-property :type el) 'descriptive)
	(org-element-set-element
	 el
	 (apply 'org-element-adopt-elements
		(list 'plain-list (list :type 'descriptive-1))
		(mapcar
		 (lambda (item)
		   (let* ((item-contents (org-element-contents item))
			  (leading-paragraph (car item-contents))
			  (item-contents (cdr item-contents)))
		     (org-element-adopt-elements
			 (list 'item (list :checkbox (org-element-property :checkbox item)))
		       (apply 'org-element-adopt-elements
			      (list 'paragraph (list :style "OrgDescriptionDefinition"))
			      (list 'bold (list :style "OrgDescriptionTerm" :post-blank 1)
				    (or (org-element-property :tag item) "(no term)"))
			      (org-element-contents leading-paragraph))
		       (org-element-adopt-elements
			   (list 'plain-list (list :type 'descriptive-2))
			 (apply 'org-element-adopt-elements
				(list 'item nil)
				item-contents)))))
		 (org-element-contents el)))))
      nil)
    info)
  tree)

;;;; List tables

;; Lists that are marked with attribute `:list-table' are called as
;; list tables.  They will be rendered as a table within the exported
;; document.

;; Consider an example.  The following list table
;;
;; #+ATTR_ODT: :rel-width 80
;; #+ATTR_ODT: :list-table t
;; -
;;   - Row 1
;;   - 1.1
;;   - 1.2
;;   - 1.3
;; - -----
;;   - Row 2
;;   - 2.1
;;   - 2.2
;;   - 2.3
;;
;; will be exported as though it were an Org table like the one show
;; below.
;;
;; | Row 1 | 1.1 | 1.2 | 1.3 |
;; |-------+-----+-----+-----|
;; | Row 2 | 2.1 | 2.2 | 2.3 |
;;
;; List tables can contain hrule (see example above).  They can also
;; contain table specific attributes.  Except for column alignment
;; (i.e., lrc spec), all other attributes (column sizing and grouping)
;; are honored on export.
;;
;; Specifically, with a list table such as the one below,
;;
;; #+ATTR_ODT: :list-table t
;; - | /    | <    |    > |       |
;; - | <l8> | <r4> | <c2> | <l1>  |
;; - --------
;;   - Row 1
;;   - Row 1.1
;;     - Subitem under 1.1
;;     - Yet another subitem under 1.1
;;   - Row 1.2
;;   - Row 1.3
;; - --------
;;   - Row 2
;;   - Row 2.1

;;     Subtext for 2.1
;;   - Row 2.2
;;   - Row 2.3
;; - --------
;;
;; you could get, the following table, in to the exported document.
;;
;; |-------+-----------------------------------+---------+---------|
;; | Row 1 | - Row 1.1                         | Row 1.2 | Row 1.3 |
;; |       |   - Subitem under 1.1             |         |         |
;; |       |   - Yet another subitem under 1.1 |         |         |
;; |-------+-----------------------------------+---------+---------|
;; | Row 2 | Row 2.1                           | Row 2.2 | Row 2.3 |
;; |       |                                   |         |         |
;; |       | Subtext for 2.1                   |         |         |
;; |-------+-----------------------------------+---------+---------|
;;
;;
;; MOTIVATION: Org mode's tables have limitations: (1) Specifically,
;; if you want a paragraph with copious text in an exported document,
;; all of it needs to be typed up in a single line.  Editing such long
;; lines using the table editor is be a cumbersome task.  (2)
;; Furthermore, if one wants a multi-paragraph text in a table cell is
;; well-nigh impossible.
;;
;; A LIST-TABLE overcomes the above problem.
;;
;; This feature is inspired by following thread:
;; https://lists.gnu.org/archive/html/emacs-orgmode/2011-03/msg01101.html

;; Experience suggests that translation of a list to tables as done by
;; `org-odt--translate-list-tables' works well for both HTML and LaTeX
;; exports.  See

;; 1. https://emacsnotes.wordpress.com/2020/04/26/create-tables-with-paragraph-like-content-in-org-mode-with-the-least-amount-of-hassle/
;; 2. https://github.com/kjambunathan/org-mode-ox-odt/issues/91#issuecomment-877945835

;; If you desire the convenience of list tables in HTML and LaTeX
;; backends, add the following snippet to your init file.
;; `org-generic--translate-list-tables' has an autoload cookie.  i.e.,
;; if you have used `package.el' to download `ox-odt', then Emacs
;; shouldn't have any problem "linking" to
;; `org-generic--translate-list-tables' as defined here.
;;
;; (add-to-list
;;    'org-export-filter-parse-tree-functions
;;    (defun org-generic--translate-list-tables (tree backend info)
;;      (if (memq backend '(latex html) )
;;          (org-odt--translate-list-tables tree backend info)
;;        tree)))

;; If the above snippet doesn't work, add one or both of the following
;; snippets to your init file.

;; ;; Add support for list tables in HTML export
;;
;; (with-eval-after-load 'ox-html
;;   (unless (featurep 'ox-odt)
;;     (require 'ox-odt))
;;   (add-to-list
;;    'org-export-filter-parse-tree-functions
;;    (defun org-html--translate-list-tables (tree backend info)
;;      (if (eq backend 'html)
;;          (org-odt--translate-list-tables tree backend info)
;;        tree))))

;; ;; Add support for list tables in LaTeX export
;;
;; (with-eval-after-load 'ox-latex
;;   (unless (featurep 'ox-odt)
;;     (require 'ox-odt))
;;   (add-to-list
;;    'org-export-filter-parse-tree-functions
;;    (defun org-latex--translate-list-tables (tree backend info)
;;      (if (eq backend 'latex)
;;          (org-odt--translate-list-tables tree backend info)
;;        tree))))

;; Translate lists to tables

;;;###autoload
(defun org-odt--translate-list-tables (tree _backend info)
  (let* ((org-element-all-objects-1 org-element-all-objects)
	 (org-element-all-elements-1 org-element-all-elements)
	 (org-element-greater-elements-1 org-element-greater-elements)
	 (org-element-all-objects (remq 'table-cell org-element-all-objects-1))
	 (org-element-all-elements (cons 'table-cell org-element-all-elements-1))
	 (org-element-greater-elements (append '(table-row table-cell)
					       org-element-greater-elements-1)))
    (org-element-map tree 'plain-list
      (lambda (l1-list)
	(when (org-odt--read-attribute l1-list :list-table)
	  ;; Replace list with table.
	  (org-element-set-element
	   l1-list
	   ;; Build replacement table.
	   (apply 'org-element-adopt-elements
		  (list 'table (list :type 'org
                                     :attr_odt (org-element-property :attr_odt l1-list)
                                     :attr_latex (org-element-property :attr_latex l1-list)
				     :attr_html (org-element-property :attr_html l1-list)
				     :caption (org-element-property :caption l1-list)
				     :name (org-element-property :name l1-list)))
		  (delq nil
			(apply 'append
			       (org-element-map l1-list 'item
				 (lambda (l1-item)
				   (let* ((l1-item-contents (org-element-contents l1-item))
					  l1-item-leading-text l2-list)
				     ;; Remove Level-2 list from the Level-item.  It
				     ;; will be subsequently attached as table-cells.
				     (let ((cur l1-item-contents) prev)
				       (while (and cur (not (eq (org-element-type (car cur))
								'plain-list)))
					 (setq prev cur)
					 (setq cur (cdr cur)))

				       (if (null prev)
					   (setq l2-list (car cur))
					 (setcdr prev nil)
					 (setq l2-list (car cur))
					 (setq l1-item-leading-text l1-item-contents)))

				     (list
				      (when (and l1-item-leading-text
						 (eq (org-element-type (car l1-item-leading-text))
						     'paragraph))
					(let ((leading-text (org-trim
							     (org-element-interpret-data
							      (car l1-item-leading-text)))))
					  (cond
					   ;; Is the leading text of the Level-1 a horizontal rule?
					   ((string-match "\\`[[:space:]]*-\\{5,\\}[[:space:]]*\\'"
							  leading-text)
					    ;; Yes. Splice a rule in to the table.
					    (org-element-adopt-elements
						(list 'table-row (list :type 'rule))))
					   ;; Is the leading text of the Level-1 a special row?
					   ((string-match "\\`|" leading-text)
					    ;; Yes. Splice that special row in to the table.
					    (org-element-map
						(let ((org-element-all-objects org-element-all-objects-1 )
						      (org-element-all-elements org-element-all-elements-1)
						      (org-element-greater-elements org-element-greater-elements-1))
						  (with-temp-buffer
						    (insert leading-text)
						    (org-element-parse-buffer)))
						'table-row
					      (lambda (table-row)
						;; Nuke all attributes other than `:parent'.
						(setcar (cdr table-row) (list :type 'standard))
						(org-element-map table-row 'table-cell
						  (lambda (table-cell)
						    ;; Nuke all attributes other than `:parent'.
						    (setcar (cdr table-cell) (list :parent table-row))
						    table-cell))
						;; Unfortunately,`org-export--prune-tree'runs before
						;; this filter is called. Do what prune would have done
						;; had it run *after* the listified table has got in to tree.
						(when info
						  (plist-put info :ignore-list
							     (cons table-row (plist-get info :ignore-list))))
						table-row)
					      nil 'first-match)))))
				      (when l2-list
					(apply 'org-element-adopt-elements
					       ;; Level-1 items start a table row.
					       (list 'table-row (list :type 'standard))
					       ;; Level-2 items define
					       ;; subsequent table-cells
					       ;; of the row.
					       (org-element-map l2-list 'item
						 (lambda (l2-item)
						   (apply 'org-element-adopt-elements
							  (list 'table-cell nil)
							  (org-element-contents l2-item)))
						 info nil 'item))))))
				 info nil 'item)))))
	  ;; Complain if the listified table is non-homogenous.
	  ;; Note: A list table is homogenous if all it's rows have
	  ;; same number of columns.  Otherwise, it is non-homogenous.
	  (let* ((table-rows (org-element-map l1-list 'table-row
			       (lambda (table-row)
				 (when (eq (org-element-property :type table-row) 'standard)
				   (org-element-contents table-row)))
			       info nil 'table-row))
		 (ncols-per-row (cl-loop for table-row in table-rows
					 collect (length (org-element-contents table-row)))))
	    (unless (apply '= ncols-per-row)
	      (user-error "List table is non-homogenous.  %s"
			  (format "List table has %d rows, with rows having following number of columns: %S"
				  (length table-rows) ncols-per-row)))))
	nil)
      info))
  tree)

;;;; Transcluded tables :: Better List tables

;; When the `org-odt-experimental-features' has
;; `transclude-sole-footnote-references-in-a-table' feature enabled, a
;; table cell whose content is a _sole_ footnote reference will be
;; typeset as if it held the associated footnote definition.  For
;; simplicity, let us call these "Transcluded Tables".
;;
;; Transcluded tables combine the best features of both a canonical
;; Org table, and a List table.  Like list tables, you can use them to
;; produce documents that have paragraph-like content.  But unlike
;; list tables, you can easily "visualise" the final table output.
;; Thus they are pleasant to create and modify.
;;
;; "Transcluded tables" are better understood with an example.
;;
;; Consider a Transcluded table like the one below:
;;
;; #+ATTR_ODT: :widths "2,1,1,8"
;; | /       | <        | >        |         |
;; | Day     | Min Temp | Max Temp | Summary |
;; |---------+----------+----------+---------|
;; | Monday  | 11C      | 22C      | [fn:1]  |
;; |---------+----------+----------+---------|
;; | Tuesday | 9C       | 19C      | [fn:2]  |
;;
;; [fn:1]
;;
;; 1. A clear day with lots of sunshine.
;; 2. Late in the day, a strong breeze will bring down the temperatures.
;;
;; [fn:2]
;;
;; 1. Cloudy with rain, across many northern regions.
;; 2. Clear spells across most of Scotland and Northern Ireland, but
;;    rain reaching the far northwest.
;;
;; When exported it is typeset "as if" it is like the table below:
;;
;; | /       | <        | >        |                                                                       |
;; | Day     | Min Temp | Max Temp | Summary                                                               |
;; |---------+----------+----------+-----------------------------------------------------------------------|
;; | Monday  | 11C      | 22C      | 1. A clear day with lots of sunshine.                                 |
;; |         |          |          | 2. Late in the day, a strong breeze will bring down the temperatures. |
;; |---------+----------+----------+-----------------------------------------------------------------------|
;; | Tuesday | 9C       | 19C      | 1. Cloudy with rain, across many northern regions.                    |
;; |         |          |          | 2. Clear spells across most of Scotland and Northern Ireland, but     |
;; |         |          |          |    rain reaching the far northwest.                                   |
;;
;;
;; In other words, the above transcluded table, is entirely equivalent
;; to the following list table.
;;
;; #+ATTR_ODT: :widths "2,1,1,8"
;; #+ATTR_ODT: :list-table t
;; - | /    | <    | >    |      |
;; -
;;   - Day
;;   - Min Temp
;;   - Max Temp
;;   - Summary
;; - ----------------
;;   - Monday
;;   - 11C
;;   - 22C
;;   -
;;     1. A clear day with lots of sunshine.
;;     2. Late in the day, a strong breeze will bring down the temperatures.
;; - ----------------
;;   - Tuesday
;;   - 9C
;;   - 19C
;;   -
;;     1. Cloudy with rain, across many northern regions.
;;     2. Clear spells across most of Scotland and Northern Ireland, but
;;        rain reaching the far northwest.
;;

(defun org-odt--transclude-sole-footnote-references-in-a-table (tree _backend info)
  (when (memq 'transclude-sole-footnote-references-in-a-table
	      org-odt-experimental-features)
    (let* ((defns (org-element-map tree '(footnote-definition)
		    (lambda (el) (cons (org-element-property :label el) el)) info))
	   (refs (org-element-map tree '(footnote-reference)
		   (lambda (el)
		     (let* ((parent (org-export-get-parent el))
			    (parent-contents (org-element-contents parent)))
		       (when (and (eq (org-element-type parent) 'table-cell)
				  (eq (car parent-contents) el)
				  (not (cdr parent-contents)))
			 el)))
		   info)))
      (dolist (footnote-reference refs)
	(let* ((def (assoc-default (org-element-property :label footnote-reference) defns)))
	  (org-element-set-element
	   footnote-reference (apply #'org-element-adopt-elements
				     (list 'special-block (list :type "transcluded-text"))
				     (org-element-contents def)))))))
  tree)



;;; Interactive functions

;;;; Export to OpenDocument formula

;;;###autoload
(defun org-odt-export-as-odf (latex-frag &optional starmathp odf-file)
  "Export LATEX-FRAG as OpenDocument formula file ODF-FILE.

STARMATHP, a boolean, specifies the syntax of LATEX-FRAG.  If it
is non-nil LATEX-FRAG is assumed to be in Starmath syntax.
Otherwise, it is assumed to be in LaTeX syntax.

Use `org-create-math-formula' to convert LATEX-FRAG first to
MathML.  When invoked as an interactive command, use
`org-latex-regexps' to infer LATEX-FRAG from currently active
region.  If no LaTeX fragments are found, prompt for it.  Push
MathML source to kill ring depending on the value of
`org-export-copy-to-kill-ring'."
  (interactive
   `(,(let (frag)
	(setq frag (and (setq frag (and (region-active-p)
					(buffer-substring (region-beginning)
							  (region-end))))
			(cl-loop for e in org-latex-regexps
				 thereis (when (string-match (nth 1 e) frag)
					   (match-string (nth 2 e) frag)))))
	(read-string "Fragment (LaTeX / Starmath): " frag nil frag))
     ,(yes-or-no-p "Is fragment in Starmath format? ")
     ,(let ((odf-filename (expand-file-name
			   (concat
			    (file-name-sans-extension
			     (or (file-name-nondirectory buffer-file-name)))
			    "." "odf")
			   (file-name-directory buffer-file-name))))
	(read-file-name "ODF filename: " nil odf-filename nil
			(file-name-nondirectory odf-filename)))))
  (let* ((filename (or odf-file
		       (expand-file-name
			(concat
			 (file-name-sans-extension
			  (or (file-name-nondirectory buffer-file-name)))
			 "." "odf")
			(file-name-directory buffer-file-name))))
	 (info (list :odt-manifest-file-entries nil
		     :odt-zip-dir (file-name-as-directory (make-temp-file "odt-" t))
		     :output-file filename)))
    (condition-case-unless-debug err
	(cl-reduce (lambda (target f)
		     (funcall f target nil info))
		   '(;; org-odt--transform-target
		     org-odt--convert)
		   :initial-value
		   (cl-reduce (lambda (contents f)
				(funcall f contents nil info))
			      '(;; org-odt-write-contents-file
				(lambda (contents _backend info)
				  (with-temp-buffer
				    (insert contents)
				    ;; Add MathML to kill ring, if needed.
				    (when (org-export--copy-to-kill-ring-p)
				      (org-kill-new (buffer-string)))

				    (let ((coding-system-for-write 'utf-8))
				      (write-region nil nil (concat (plist-get info :odt-zip-dir) "content.xml")))
				    (org-odt-create-manifest-file-entry info "text/xml" "content.xml")))
				;; org-odt-write-styles-file
				;; org-odt-write-meta-file
				org-odt-write-mimetype-file
				org-odt-write-manifest-file
				;; org-odt-prettify-xml-files-maybe
				org-odt-zip
				org-odt-cleanup-xml-buffers)
			      :initial-value (or (if starmathp
						     (org-create-math-formula-from-starmath latex-frag)
						   (org-create-math-formula latex-frag))
						 (error "No Math formula created"))))
      ((error)
       ;; Cleanup work directory and work files.
       (org-odt-cleanup-xml-buffers nil nil info)
       (error "OpenDocument export failed with error: `%s'"
	      (error-message-string err))))))

;;;###autoload
(defun org-odt-export-as-odf-and-open ()
  "Export LaTeX fragment as OpenDocument formula and immediately open it.
Use `org-odt-export-as-odf' to read LaTeX fragment and OpenDocument
formula file."
  (interactive)
  (org-open-file (call-interactively 'org-odt-export-as-odf) 'system))


;;;; Export to OpenDocument Text

(defun org-odt-export-to-odt-backend
    (backend &optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to BACKEND or a OpenDocument XML file.

BACKEND, a symbol, must either be `odt' (referring to the
OpenDocument backend) or another symbol referring to a registered
back-end that derives from the OpenDocument backend.

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
  (let* ((outfile (org-export-output-file-name
		   (format "%s.%s"
                           ;; ODS backend passes in a `:uniquifier' to
                           ;; differentiate between ODS files created
                           ;; from different tables in the same `org'
                           ;; file.
			   (or (plist-get ext-plist :uniquifier) "")
			   (if body-only "xml" (symbol-name backend)))
		   subtreep)))
    (if (not (file-writable-p outfile)) (error "Output file not writable")
      (let ((ext-plist (org-combine-plists `(:output-file ,outfile) ext-plist)))
	(if async
	    (org-export-async-start
		`(lambda (outfile)
		   (org-export-add-to-stack (expand-file-name outfile) ',backend))
	      `(let* ((output
		       (org-export-as
			',backend ,subtreep ,visible-only ,body-only
			',ext-plist)))
		 (if (not ,body-only)
		     output
		   (with-temp-buffer
		     (insert output)
		     (let ((coding-system-for-write 'utf-8))
		       (write-region nil nil ,outfile)))
		   (when (and (org-export--copy-to-kill-ring-p) (org-string-nw-p output))
		     (org-kill-new output))
		   ,outfile)
		 ,outfile))
	  (let ((output (org-export-as
			 backend subtreep visible-only body-only ext-plist)))
	    (if (not body-only)
		output
	      (with-temp-buffer
		(insert output)
		(let ((coding-system-for-write 'utf-8))
		  (write-region nil nil outfile)))
	      (when (and (org-export--copy-to-kill-ring-p) (org-string-nw-p output))
		(org-kill-new output))
	      outfile)))))))

;;;###autoload
(defun org-odt-export-to-odt
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a ODT or a OpenDocument XML file.

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
  (let* ((backend 'odt))
    (org-odt-export-to-odt-backend backend async subtreep
				   visible-only body-only ext-plist)))


;;;; Export to OpenDocument master

(org-export-define-derived-backend 'odm 'odt
  :menu-entry
  '(?o "Export to ODT"
       ((?m "As ODM file" org-odt-export-to-odm)
	(?M "As ODM file and open"
	    (lambda (a s v b)
	      (if a (org-odt-export-to-odm t s v)
		(org-open-file (org-odt-export-to-odm nil s v) 'system)))))))

;;;###autoload
(defun org-odt-export-to-odm
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a ODM or a OpenDocument XML file.

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
  (let* ((backend 'odm))
    (org-odt-export-to-odt-backend backend async subtreep
				   visible-only body-only ext-plist)))


;;;; Transform the exported OpenDocument file through 3rd-party converters

(defun org-odt-transform (in-file)
  "Transform IN-FILE using `org-odt-transform-processes'.
IN-FILE is an OpenDocument Text document, usually created as part
of `org-odt-export-as-odf'."
  (require 'browse-url)
  (let* ((in-file (expand-file-name in-file)))
    (cl-loop for (purpose cmd . args) in org-odt-transform-processes
	     with err-string
	     with exit-code do
	     (setq cmd (cons cmd
			     (mapcar (lambda (arg)
				       (format-spec arg `((?i . ,in-file)
							  (?I . ,(browse-url-file-url in-file)))))
				     args)))

	     (message "Applying Transformation: %s" purpose)
	     (message "Running %s" (mapconcat #'identity cmd " "))
	     (setq err-string
		   (with-output-to-string
		     (setq exit-code
			   (apply 'call-process (car cmd)
				  nil standard-output nil (cdr cmd)))))
	     (if (zerop exit-code)
		 (message "Created transformed file `%s'" (expand-file-name in-file))
	       (error (format "Command failed with error (%s)"
			      err-string))))
    in-file))

(defun org-odt-export-as-odt-backend
    (backend &optional async subtreep visible-only _body-only ext-plist)
  "Export current buffer as a OpenDocument XML buffer.

BACKEND, a symbol, must either be `odt' (referring to the
OpenDocument backend) or another symbol referring to a registered
back-end that derives from the OpenDocument backend.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

The optional argument BODY-ONLY is ignored.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done to a buffer named \"*Org BACKEND Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (let ((body-only t))
    (org-export-to-buffer backend
	(format "*Org %s Export*" (capitalize (symbol-name backend)))
      async subtreep visible-only body-only ext-plist
      (lambda ()
	(nxml-mode)
	(goto-char (point-min))
	(when (re-search-forward "<!-- BEGIN: OFFICE:TEXT/CONTENTS -->" nil t)
	  (goto-char (match-beginning 0)))))))

;;;###autoload
(defun org-odt-export-as-odt
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a OpenDocument XML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

The optional argument BODY-ONLY is ignored.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done to a buffer named \"*Org ODT Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (let ((backend 'odt))
    (org-odt-export-as-odt-backend backend async subtreep
				   visible-only body-only ext-plist)))

;;;###autoload
(defun org-odt-export-string-as-odt-string
    (string &optional _backend _body-only ext-plist)
  "Transcode STRING into BACKEND code.

BACKEND and BODY-ONLY are ignored.

Optional argument EXT-PLIST, when provided, is a property list
with external parameters overriding Org default settings, but
still inferior to file-local settings.

In addition to the standard export options, this function
recognizes a `:pick' property in EXT-PLIST.  Its value is a
symbol and can be one of the following: `contents' or
`automatic-styles'.  If `:pick' is unspecified, or is not one of
the allowed values, it is forcibly set to `contents'.

Return the picked portion as a string.

Note that the signature of this function matches with
`org-export-string-as'.

Here is a simple snippet that demonstrates the use of this
function to create page headers:

    #+TITLE: Generate Page Header using `org-odt-export-string-as-odt-string' and Org Babel

    #+name: headertext
    #+begin_src org :exports none
    ,#+begin_center
    # Since the content is routed through ~shell~, the line breaks are
    # double-escaped.
    [[https://Orgmode.org][/Emacs Orgmode/]]\\\\\\\\
    ,#+end_center
    #+end_src

    #+name: tostring-headertext
    #+begin_src sh :noweb yes :exports none :results verbatim
    cat <<EOF
    <<headertext>>
    EOF
    #+end_src

    #+name: tonxml-tostring-headertext
    #+begin_src emacs-lisp :exports none :var string=tostring-headertext
    (org-odt-export-string-as-odt-string string)
    #+end_src

    #+attr_odt: :target \"master_styles\"
    #+begin_src nxml  :noweb yes
    <style:master-page style:name=\"Standard\"
                       style:page-layout-name=\"Mpm1\">
      <style:header>
        <<tonxml-tostring-headertext()>>
      </style:header>
      <style:footer>
        <text:p text:style-name=\"MP1\">
          <text:page-number text:select-page=\"current\"></text:page-number>
        </text:p>
      </style:footer>
    </style:master-page>
    #+end_src

    First page.

    #+attr_odt: :page-break t
    Second page."
  (with-temp-buffer
    (insert string)
    (let ((org-inhibit-startup t)) (org-mode))
    (let ((backend 'odt)
	  (subtreep nil)
	  (visible-only nil)
	  (body-only t))
      (let ((output (org-export-as backend subtreep visible-only body-only
				   ext-plist)))
	(with-temp-buffer
	  (insert output)
	  (goto-char (point-min))
	  (let* ((what (or (alist-get (plist-get ext-plist :pick)
				      '((contents . "OFFICE:TEXT/CONTENTS")
					(automatic-styles . "OFFICE:AUTOMATIC-STYLES"))
				      "OFFICE:TEXT/CONTENTS")))
		 (beg (when (re-search-forward (format "<!-- %s: %s -->\n*" "BEGIN" what) nil t)
			(match-end 0)))
		 (end (when (re-search-forward (format "\n*<!-- %s: %s -->" "END" what) nil t)
			(match-beginning 0))))
	    (unless (and beg end)
	      (error "`org-odt-export-string-as-odt-string': This shouldn't happen"))
	    (buffer-substring-no-properties beg end)))))))


;;;; Validate OpenDocument a file

(defun org-odt-validate (in-file &optional abortp)
  "Validate IN-FILE using `org-odt-validate-process'."
  (interactive "fOpenDocument file: ")
  (let* ((cmd org-odt-validate-process)
	 err-string exit-code)
    (setq cmd (mapcar (lambda (arg)
			(format-spec arg `((?i . ,in-file))))
		      cmd))
    (message "Validating file `%s' ..." (expand-file-name in-file))
    ;; Complain if `org-odt-validate-process' is misconfigured.
    (unless (and cmd
		 (executable-find (car cmd))
		 (let* ((jar-file (cadr (member "-jar" cmd))))
		   (or (null jar-file) (file-readable-p jar-file))))
      (error "Validation requested, but no Validator is configured:\n %S" cmd))
    ;; Run the validator
    (message "Running %s" (mapconcat #'identity cmd " "))
    (setq err-string
	  (with-output-to-string
	    (setq exit-code
		  (apply 'call-process (car cmd)
			 nil standard-output nil (cdr cmd)))))
    (cond
     ;; Document is well-formed.
     ((zerop exit-code)
      (message "File `%s' is well-formed" (expand-file-name in-file)))
     ;; Document is not well-formed.  Output any message from the
     ;; validator.
     (t
      (message "%s" (with-temp-buffer
		      (insert err-string)
		      (let ((fill-prefix "\t"))
			(indent-region (point-min) (point-max))
			(buffer-string))))
      (funcall
       ;; Check how the user wants the malformed-ness to be treated.
       (if abortp #'error #'message)
       "File `%s' is malformed" in-file)))
    in-file))


;;;; Convert between OpenDocument and other formats

(defun org-odt-reachable-p (in-fmt out-fmt)
  "Return non-nil if IN-FMT can be converted to OUT-FMT."
  (catch 'done
    (let ((reachable-formats (org-odt-do-reachable-formats in-fmt)))
      (dolist (e reachable-formats)
	(let ((out-fmt-spec (assoc out-fmt (cdr e))))
	  (when out-fmt-spec
	    (throw 'done (cons (car e) out-fmt-spec))))))))

(defun org-odt-do-convert (in-file out-fmt &optional open)
  "Workhorse routine for `org-odt-convert'."
  (require 'browse-url)
  (let* ((in-file (expand-file-name (or in-file buffer-file-name)))
	 (_dummy (or (file-readable-p in-file)
		     (error "Cannot read %s" in-file)))
	 (in-fmt (file-name-extension in-file))
	 (out-fmt (or out-fmt (error "Output format unspecified")))
	 (how (or (org-odt-reachable-p in-fmt out-fmt)
		  (error "Don't know how to convert file `%s' from type `%s' to type `%s'"
			 (file-name-nondirectory in-file) in-fmt out-fmt)))
	 (convert-process (car how))
	 (out-file (concat (file-name-sans-extension in-file) "."
			   (nth 1 (or (cdr how) out-fmt))))
	 (extra-options (or (nth 2 (cdr how)) ""))
	 (out-dir (file-name-directory in-file))
	 (cmd (format-spec convert-process
			   `(;; When a process is executed with
                             ;; `shell-command', and within
                             ;; `with-environment-variables' does it
                             ;; inherit the locale specific environment
                             ;; variables from the new environement?  A
                             ;; little experimentation suggests that the
                             ;; answer is "Yes". If that that case, this
                             ;; setting is merely gives a visual
                             ;; assurance to the user about what is
                             ;; happening.  If "No", it really does the
                             ;; needed work.  In either case, having it
                             ;; is what the user wants.
                             (?l . ,(format "LANG=%s" (getenv "LANG")))
                             (?i . ,(shell-quote-argument in-file))
			     (?I . ,(browse-url-file-url in-file))
			     (?f . ,out-fmt)
			     (?o . ,(shell-quote-argument out-file))
			     (?O . ,(browse-url-file-url out-file))
			     (?d . ,(shell-quote-argument out-dir))
			     (?D . ,(browse-url-file-url out-dir))
			     (?x . ,extra-options)))))
    (when (file-exists-p out-file)
      (delete-file out-file))
    (message "Executing %s" cmd)
    (let ((cmd-output (shell-command-to-string cmd)))
      (message "%s" cmd-output))
    (cond
     ((file-exists-p out-file)
      (message "Exported to %s" out-file)
      (when open
	(message "Opening %s..." out-file)
	(org-open-file out-file 'system))
      out-file)
     (t
      (message "Export to %s failed" out-file)
      nil))))

(defun org-odt-do-reachable-formats (in-fmt)
  "Return verbose info about formats to which IN-FMT can be converted.
Return a list where each element is of the
form (CONVERTER-PROCESS . OUTPUT-FMT-ALIST).  See
`org-odt-convert-processes' for CONVERTER-PROCESS and see
`org-odt-convert-capabilities' for OUTPUT-FMT-ALIST."
  (let* ((converter
	  (and org-odt-convert-process
	       (cadr (assoc-string org-odt-convert-process
				   org-odt-convert-processes t))))
	 (capabilities
	  (and org-odt-convert-process
	       (cadr (assoc-string org-odt-convert-process
				   org-odt-convert-processes t))
	       org-odt-convert-capabilities))
	 reachable-formats)
    (when converter
      (dolist (c capabilities)
	(when (member in-fmt (nth 1 c))
	  (push (cons converter (nth 2 c)) reachable-formats))))
    reachable-formats))

(defun org-odt-reachable-formats (in-fmt)
  "Return list of formats to which IN-FMT can be converted.
The list of the form (OUTPUT-FMT-1 OUTPUT-FMT-2 ...)."
  (let (l)
    (mapc (lambda (e) (add-to-list 'l e))
	  (apply 'append (mapcar
			  (lambda (e) (mapcar 'car (cdr e)))
			  (org-odt-do-reachable-formats in-fmt))))
    l))

(defun org-odt-convert-read-params ()
  "Return IN-FILE and OUT-FMT params for `org-odt-do-convert'.
This is a helper routine for interactive use."
  (let* ((input (if (featurep 'ido) 'ido-completing-read 'completing-read))
	 (in-file (read-file-name "File to be converted: "
				  nil buffer-file-name t))
	 (in-fmt (file-name-extension in-file))
	 (out-fmt-choices (org-odt-reachable-formats in-fmt))
	 (out-fmt
	  (or (and out-fmt-choices
		   (funcall input "Output format: "
			    out-fmt-choices nil nil nil))
	      (error
	       "No known converter or no known output formats for %s files"
	       in-fmt))))
    (list in-file out-fmt)))

;;;###autoload
(defun org-odt-convert (&optional in-file out-fmt open)
  "Convert IN-FILE to format OUT-FMT using a command line converter.
IN-FILE is the file to be converted.  If unspecified, it defaults
to variable `buffer-file-name'.  OUT-FMT is the desired output
format.  Use `org-odt-convert-process' as the converter.
If PREFIX-ARG is non-nil then the newly converted file is opened
using `org-open-file'."
  (interactive
   (append (org-odt-convert-read-params) current-prefix-arg))
  (org-odt-do-convert in-file out-fmt open))


;;;; Completion for `#+LANGUAGE: ' line

(defun org-odt-update-locale ()
  "Implement `completing-read' for LANGUAGE keyword.

Use entries from `org-odt-locales-alist' as candidates.  

Note, that there already exists
`pcomplete/org-mode/file-option/language' for completing LANGUAGE
values.  Here are the differences between this function and the
`pcomplete'-command:

  - This function completes on a bigger list of candidates, and
    offers human-readable description of the locale.  The
    `pcomplete' version completes the sole candidate,
    `org-export-default-language'.

  - This function hooks to `org-tab-first-hook' and is invoked by
    pressing TAB when on LANGUAGE line.  The `pcomplete' version
    uses \"completion at point\" infrastructure and is invoked by
    pressing \\[completion-at-point]."
  (org-with-wide-buffer
   (let* ((element (org-element-at-point))
	  (completions
	   (append
            ;; Alist of (LANG-NAME . LANG-CODE)
	    (cl-delete-duplicates
	     (cl-loop for (locale name _script-type) in org-odt-locales-alist
		      for language = (car (split-string locale "_"))
		      for language-name = (when (string-match "\\(.*?\\) \\((.*?)\\)" name)
					    (match-string 1 name))
		      collect (list language-name language))
	     :test #'equal)
	    ;; Alist of (LOCALE-NAME . LOCALE-CODE)
	    (mapcar (lambda (it) (cdr (reverse it))) org-odt-locales-alist))))
     (when (and (eq (org-element-type element) 'keyword)
		(string= (upcase (org-element-property :key element)) "LANGUAGE")
		(< (point) (org-element-property :end element))
		(>= (point) (org-element-property :begin element)))
       (goto-char (org-element-property :begin element))
       (when (re-search-forward ":" (line-end-position) t)
	 ;; Wipe-off the current entry.
	 (delete-region (point) (line-end-position))
	 (insert " ")
	 ;; Insert the chosen entry.
	 (let ((input (car (assoc-default
			    (org-completing-read "Language/Locale: " completions) completions))))
	   (when input
	     (insert input))))))))

(add-hook 'org-tab-first-hook 'org-odt-update-locale)


;;;; Suggest `:span' attrbutes for a table

(defun org-odt--table-compute-spans (table info)
  (let* ((cell-empty-p (lambda (table-cell)
			 (null (org-element-contents table-cell))))
	 (data-row-p (lambda (table-row)
		       (and (eq (org-element-property :type table-row) 'standard)
			    (not (org-export-table-row-is-special-p table-row 'ignore)))))
	 (get-span-length
	  (lambda (table-cells)
	    (cl-loop for table-cell in table-cells
		     while (funcall cell-empty-p table-cell)
		     counting t into n
		     finally (return n))))
	 (table (cl-loop for table-row in (org-element-contents table)
			 when (funcall data-row-p table-row)
			 collect table-row)))
    (cl-loop for table-row in table counting t into r
	     when (cl-loop for table-cell in (org-element-contents table-row)
			   counting t into c
			   when (and (not (funcall cell-empty-p table-cell))
				     (let* ((rowspan (funcall get-span-length
							      (cl-loop for table-row in (org-export-get-next-element table-row info t)
								       when (funcall data-row-p table-row)
								       collect (nth (1- c) (org-element-contents table-row)))))
					    (colspan (funcall get-span-length (org-export-get-next-element table-cell info t))))
				       (unless (and (zerop rowspan)
						    (zerop colspan))
					 (format "@%d$%d{%s:%s}"
						 r c
						 (number-to-string (1+ rowspan))
						 (number-to-string (1+ colspan))))))
			   collect it)
	     collect it)))

(defun org-odt-table-suggest-spans ()
  "Insert `:span' lines for a table at point.

Use empty cells to infer a spanned cell.  i.e., A table cell is
considered as spanned cell if it has an empty cell immediately
below it or to the right.  For a spanned cell, suggest the
`:span' attribute by counting the number of empty cells
immediately to it's right and immediately right below it.  This
command doesn't alter any prior attribute lines including the
`:span' lines.

Note that the `#+ATTR_ODT: :span ...' lines are merely
suggestions.  You may have to tweak the suggestions a bit to get
the desired typesetting.  This function generates multipe `:span'
lines, with each line containing *all* spanned cells on a unique
row. 

To understand \"You may have to tweak the suggestions a bit ...\"
remark above above, consider the following example.

If you want to produce the following table

    #+begin_example
    +----------+----------+----------+
    |Column 1  |Column 2  |Column 3  |
    +----------+----------+----------+
    |A         |B                    |
    |          +----------+----------+
    |          |C         |D         |
    +----------+----------+----------+
    |E         |F                    |
    +----------+                     |
    |G         |                     |
    +----------+---------------------+
    |H                               |
    +--------------------------------+
    #+end_example

you will start with the following Org table:

    #+ATTR_ODT: :style \"GriddedTable\" 
    |----------+----------+----------|
    | Column 1 | Column 2 | Column 3 |
    |----------+----------+----------|
    | A        | B        |          |
    |          | C        | D        |
    | E        | F        |          |
    | G        |          |          |
    | H        |          |          |
    |----------+----------+----------|

When you invoke `M-x org-odt-table-suggest-spans' on this table,
you will get the following result

    #+ATTR_ODT: :style \"GriddedTable\"
    #+ATTR_ODT: :span \"@1$3{2:1}\"
    #+ATTR_ODT: :span \"@2$1{2:1} @2$2{1:2}\"
    #+ATTR_ODT: :span \"@3$3{4:1}\"
    #+ATTR_ODT: :span \"@4$2{3:2}\"
    #+ATTR_ODT: :span \"@5$1{1:3}\"
    #+ATTR_ODT: :span \"@6$1{1:3}\"
    |----------+----------+----------|
    | Column 1 | Column 2 | Column 3 |
    |----------+----------+----------|
    | A        | B        |          |
    |          | C        | D        |
    | E        | F        |          |
    | G        |          |          |
    | H        |          |          |
    |----------+----------+----------|

If you export this table, you will get a table with col and
rowspans but in a \"wrong\" way. In order to get the desired
spans, you have to do the following \"edits\"

     #+ATTR_ODT: :style \"GriddedTable\"
    -#+ATTR_ODT: :span \"@1$3{2:1}\"
     #+ATTR_ODT: :span \"@2$1{2:1} @2$2{1:2}\"
    -#+ATTR_ODT: :span \"@3$3{4:1}\"
    -#+ATTR_ODT: :span \"@4$2{3:2}\"
    -#+ATTR_ODT: :span \"@5$1{1:3}\"
    +#+ATTR_ODT: :span \"@4$2{2:2}\"
     #+ATTR_ODT: :span \"@6$1{1:3}\"

That is,

    - Ignore `:span'-suggestions for first, third and fifth rows
    - Modify the `:span'-suggestions on fifth row
    - Retain `:span'-suggestions on other rows

and end up with the table like this:

    #+ATTR_ODT: :style \"GriddedTable\"
    #+ATTR_ODT: :span \"@2$1{2:1} @2$2{1:2}\"
    #+ATTR_ODT: :span \"@4$2{2:2}\"
    #+ATTR_ODT: :span \"@6$1{1:3}\"
    |----------+----------+----------|
    | Column 1 | Column 2 | Column 3 |
    |----------+----------+----------|
    | A        | B        |          |
    |          | C        | D        |
    | E        | F        |          |
    | G        |          |          |
    | H        |          |          |
    |----------+----------+----------|

Note that the `:span'-suggestions are split row-wise,
specifically to help with subsequent tweaks."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-with-wide-buffer
     (let* ((table (let* ((el (org-element-at-point)))
		     (when (memq (org-element-type el) '(table-row table))
		       (narrow-to-region (org-table-begin)
					 (org-table-end))
		       (org-element-map (org-element-parse-buffer) 'table #'identity nil t)))))
       (when table
	 (goto-char (org-table-begin))
	 (insert
	  (mapconcat #'identity
		     (cl-loop for row in (org-odt--table-compute-spans table nil)
			      collect (format "#+ATTR_ODT: :span \"%s\""
					      (mapconcat #'identity row " ")))
		     "\n")
	  "\n"))))))


;;;; Insert prettifed XML

(defun org-odt-prettify-xml-buffer (&optional arg)
  "Run HTML Tidy (i.e., `tidy' on debian) on current buffer.

Specfically, it does the following:

  - indent element content
  - add some extra empty lines for readability
  - begin each attribute on a new line
  - sort attributes within an element alphabetically in ascending order
  - write some attributes, specifically style:name, before other
    attributes of an element

When there is no prefix ARG, run `indent-region' on the tidied
output.  With prefix ARG, run only HTML Tidy, and skip
`indent-region'.

This function is used for prettifying XML files when user option
`org-odt-prettify-xml' is non-nil."
  (interactive "P")
  (when (and (called-interactively-p 'any) arg)
    (setq arg t))
  (unless (consp arg)
    (setq arg (assoc-default arg
			     '(
			       ;; Mapping when called as part of export process,
			       ("tidy+indent" . (tidy indent))
			       ("tidy" . (tidy))
			       ("" . nil)
			       ;; Mapping when called interactively.
			       (nil . (tidy indent))
			       (t . (tidy))))))
  ;; Run HTML tidy.
  (when (and (executable-find "tidy")
	     (memq 'tidy arg))
    (let* ((output-file (make-temp-file "odt-tidy-out-"))
	   (error-file (make-temp-file "odt-tidy-err-"))
	   (attributes (mapconcat #'identity
				  '(
				    "style:name"
				    "style:default-outline-level"
				    "style:parent-style-name"
				    "style:next-style-name"
				    "style:page-layout-name"
				    "style:list-style-name"
				    "style:display-name"
				    "style:font-name"
				    "style:font-name-asian"
				    "style:font-name-complex"
				    "text:master-page-name"
				    "text:default-style-name"
				    "text:citation-style-name"
				    "text:citation-body-style-name"
				    "text:level"
				    "text:style-name"
				    "text:name"
				    "style:family"
				    "style:class")
				  ", "))
	   (cmd (list
		 "tidy"					; executable name
		 "--wrap" "10"				; the right margin for line wrapping;
							; set this to a very small value so that each
							; attribute is on a line of it's own
		 "--indent-attributes" "yes"		; begin each attribute on a new line
		 "--indent" "no"			; don't indent element content		 
		 "--literal-attributes" "yes"		; don't normalize attribute values i.e., preserve whitespace
		 "--quiet" "yes"			; report only document warnings and errors
		 "--sort-attributes" "alpha"		; sort attributes within an element alphabetically in ascending order
		 "--tidy-mark" "no"			; don't add a meta element
		 "--vertical-space" "no"		; don't add any extra empty lines
		 "-utf8"				; use UTF-8 for both input and output
		 "-xml"					; the input is well formed XML
		 "--priority-attributes" attributes	; write these attributes before other attributes of an element
		 "--error-file" error-file		; write errors and warnings this file
		 "--output-file" output-file		; write output to this file
		 ))
	   (exitcode (progn
		       (message "Running %s" (mapconcat 'identity cmd " "))
		       (apply #'call-process-region nil nil (car cmd) nil nil (cdr cmd))))
	   (error-string
	    (with-temp-buffer
	      (insert-file-contents error-file)
	      (prog1 (buffer-string)
		(delete-file error-file))))
	   (output-string
	    (with-temp-buffer
	      (insert-file-contents output-file)
	      (prog1 (buffer-string)
		(delete-file output-file)))))
      (cond
       ((member exitcode '(0 1))
	(delete-region (point-min) (point-max))
	(insert output-string))
       (t
	(message "%s failed with error: ->\n%s\n<-"
		 (car cmd) error-string)))))
  ;; Run `indent-region'.
  (when (memq 'indent arg)
    (nxml-mode)
    (indent-region (point-min) (point-max))))

(defun org-odt-yank-styles (&optional yank-method)
  "Yank `current-kill', (presumably )a ODT (styles) XML, at point.

`current-kill' is assumed to be a XML string, most likely an XML
string copied over from one of the component XML files (i.e.,
styles.xml, content.xml, meta.xml or manifest.xml) in an
OpenDocument file produced by LibreOffice UI.

Use this command to yank XML in to either an `org-mode' buffer or
non `org-mode' buffer.  In both the cases, the result will be
prettified with `org-odt-prettify-xml-buffer'.

The behaviour of this command depends on the current mode and the
value of prefix argument.

  1. In a non-`org-mode' buffer, always insert a prettified version of
     `current-kill' devoid of any style decorations whatsoever.

  2. In a `org-mode' buffer, the behaviour depends on the prefix
     argument.

     - no prefix arg :: Insert styles using in buffer keywords.

       You will be prompted for up one of the following styles-related
       keywords

       - #+odt_extra_styles:
       - #+odt_extra_automatic_styles:
       - #+odt_master_styles:
       - #+odt_automatic_styles:

     - single prefix arg :: Insert styles using a `nxml-mode' src-block

       You will be prompted for up one of the following styles-related
       keywords

       - extra_styles
       - extra_automatic_styles
       - master_styles
       - automatic_styles

       Depending on your input, an `nxml-mode' like the one below will
       be inserted

           #+ATTR_ODT: :target \"extra_styles\"
           #+begin_src nxml
             ...
           #+end_src

           #+ATTR_ODT: :target \"extra_automatic_styles\"
           #+begin_src nxml
             ...
           #+end_src

           #+ATTR_ODT: :target \"master_styles\"
           #+begin_src nxml
             ...
           #+end_src

           #+ATTR_ODT: :target \"automatic_styles\"
           #+begin_src nxml
             ...
           #+end_src

     - double prefix arg :: Insert prettified style without any style
       decorations.

Note that LibreOffice can be configured to emit pretty-print
XML (i.e., with indentation and line breaks) by doing the
following

    On the `Tools' - `Options' - `Load/Save' - General tab page
    you can clear the check box Size optimization for ODF format.

or
    Under `Tools' – `Options' – `LibreOffice' – `Advanced' –
    `Expert Configuration' set the property
    `/org.openoffice.Office.Common/Save/Document PrettyPrinting'
    to true.

This command further prettifies the LibreOffice-prettified XML.
See `org-odt-prettify-xml-buffer' for more information."
  (interactive "P")
  (let* ((yank-method-1
	  (cond
	   ;; In a non `org-mode' buffer, ignore the prefix argument
	   ;; and insert style the prettified string devoid of any
	   ;; style decorations.
	   ((not (derived-mode-p 'org-mode)) 'simple)
	   ((called-interactively-p 'any)
	    (assoc-default yank-method
			   '(
			     ;; Without a prefix argument, insert
			     ;; style using keywords
			     (nil . use-keywords)
			     ;; With a single prefix argument, insert
			     ;; style using an `nxml-mode' src block
			     ((4) . use-nxml-src-block)
			     ;; With a double prefix argument, insert
			     ;; the prettifed string devoid of any
			     ;; style decorations.
			     ((16) . simple))))
	   (t (user-error "This function is for interactive use only"))))
	 (text (or (and kill-ring (current-kill 0)) ""))
	 (start (point))
	 (prettified-text
	  ;; Prettify XML
	  (with-temp-buffer
	    (insert text)
	    (org-odt-prettify-xml-buffer)
	    (buffer-substring-no-properties (point-min) (point-max))))
	 (prettified-and-pruned-text
	  (with-temp-buffer
	    (save-excursion
	      ;; Insert pretty XML
	      (insert prettified-text))
	    ;; Prune leading newlines
	    (goto-char (point-min))
	    (kill-region (point-min)
			 (progn (skip-chars-forward "\n")
				(point)))
	    ;; Prune trailing newlines
	    (goto-char (point-max))
	    (kill-region
	     (progn (skip-chars-backward "\n")
		    (point))
	     (point-max))
	    (buffer-substring-no-properties (point-min) (point-max)))))
    (cl-case yank-method-1
      (simple
       (save-excursion (insert prettified-text)))
      ;; Case 2: `use-keywords'
      ;; Insert styles one of the chosen keywords
      ;;
      ;;     #+odt_extra_styles: ....
      ;;     #+odt_extra_automatic_styles: ...
      ;;     #+odt_master_styles: ....
      ;;     #+odt_automatic_styles: ...
      (use-keywords
       ;; Ensure that point is at beginning of line
       (unless (bolp)
	 (user-error "Cannot insert here"))
       (save-excursion
	 (insert prettified-text)
	 (kill-region
	  (save-excursion (skip-chars-backward "\n")
			  (point))
	  (point))
	 (string-rectangle start (line-beginning-position)
			   (completing-read "Prefix: "
					    '("#+odt_extra_styles: "
					      "#+odt_extra_automatic_styles: "
					      "#+odt_master_styles: "
					      "#+odt_automatic_styles: ")))
	 (goto-char (line-end-position))
	 (insert "\n")))
      (use-nxml-src-block
       ;; Ensure that point is at beginning of line
       (unless (bolp)
	 (user-error "Cannot insert here"))
       (let ((p nil))
	 (save-excursion
	   (insert "#+begin_src nxml\n")
	   (setq p (point-marker))
	   (insert prettified-and-pruned-text)
	   (insert "\n#+end_src\n\n"))
	 (insert (format "\n#+ATTR_ODT: :target \"%s\"\n"
			 (completing-read "Target: "
					  '("extra_styles"
					    "extra_automatic_styles"
					    "master_styles"
					    "automatic_styles"))))
	 (goto-char p)))
      (t (user-error "This shouldn't happen")))))

;;; Publishing

(defun org-odt-publish-to-odt (plist filename pub-dir)
  "Publish an org file to ODT.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name.

This function can be used as `:publishing-function' in
`org-publish-project-alist'.

For example, here is a simple publishing project named
\"org-odt\" that publishes a set of Org files in \"~/org\"
directory as `ODT' and `DOCX' files to \"~/public_odt\"
directory.

    (setq org-publish-project-alist
          \\='((\"org-odt\"
             :base-directory \"~/org/\"
             :publishing-function org-odt-publish-to-odt
             :publishing-directory \"~/public_odt\"
             :section-numbers nil
             :with-toc nil
             :odt-app \"docx\"
             :odt-preferred-output-format \"docx\")))

Note the use of ODT-specific options `:odt-app' and
`:odt-preferred-output-format', in the above configuration.

For a list of ODT-specific options you can evaluate the following
form

    (org-export-backend-options (org-export-get-backend \\='odt))
."
  (cl-letf (((symbol-function 'org-export-to-file)
	     (lambda
	       (_backend file &optional async subtreep visible-only body-only ext-plist
			 _post-process)
	       (if (not (file-writable-p file))
		   (error "Output file not writable")
		 (let ((ext-plist (org-combine-plists `(:output-file ,file) ext-plist))
		       auto-mode-alist)
		   (org-odt-export-to-odt async subtreep visible-only
                                          (and nil body-only) ext-plist))))))
    (org-publish-org-to 'odt filename ".odt" plist pub-dir)))

;;; Library Initializations

(cl-loop for (extn . rest) in org-odt-file-extensions-alist do
	 ;; Let Emacs open all OpenDocument files in archive mode
	 (add-to-list 'auto-mode-alist
		      (cons (concat  "\\." extn "\\'") 'archive-mode)))

;;; Org Export Before Processing Hook

;;;; ODT-specific Global Macros

(defun org-odt-export-before-processing-function (backend)
  (when (eq backend 'odt)
    (cl-loop for macro in org-odt-global-macros
	     do (add-to-list 'org-export-global-macros macro t))))

(add-hook 'org-export-before-processing-hook 'org-odt-export-before-processing-function)

(provide 'ox-odt)

;;; ox-odt.el ends here

;; Local Variables:
;; fill-column: 160
;; eval: (menu-bar--toggle-truncate-long-lines)
;; End:
