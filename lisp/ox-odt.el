;;; ox-odt.el --- OpenDocument Text Exporter for Org Mode -*- lexical-binding: t; coding: utf-8-emacs; -*-

;; Copyright (C) 2010-2019 Jambunathan K <kjambunathan at gmail dot com>

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
    (citation . org-odt-citation))
  ;; :export-block "ODT"			; FIXME
  :filters-alist '((:filter-parse-tree
		    . (org-odt--translate-cite-fragments
		       org-odt--collect-cite-keys
		       org-odt--translate-latex-fragments
		       org-odt--translate-description-lists ; Dummy symbol
		       org-odt--translate-list-tables)))
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
    ;; ODT-specific keywords
    ;; Keywords that affect styles.xml
    (:odt-preferred-output-format "ODT_PREFERRED_OUTPUT_FORMAT" nil org-odt-preferred-output-format t)
    (:odt-styles-file "ODT_STYLES_FILE" nil nil t)
    (:odt-extra-images "ODT_EXTRA_IMAGES" nil nil split)
    (:odt-extra-styles "ODT_EXTRA_STYLES" nil nil newline)
    (:odt-extra-automatic-styles "ODT_EXTRA_AUTOMATIC_STYLES" nil nil newline)
    (:odt-master-styles "ODT_MASTER_STYLES" nil nil newline)
    ;; Keywords that affect content.xml
    (:odt-content-template-file "ODT_CONTENT_TEMPLATE_FILE" nil org-odt-content-template-file)
    (:odt-automatic-styles "ODT_AUTOMATIC_STYLES" nil nil newline)
    (:odt-display-outline-level "ODT_DISPLAY_OUTLINE_LEVEL" nil (number-to-string org-odt-display-outline-level))

    ;; Keys associated with Endnotes
    (:odt-endnote-anchor-format nil nil org-odt-endnote-anchor-format t)
    (:odt-endnote-braces nil nil org-odt-endnote-braces t)
    (:odt-endnote-regexp "ODT_ENDNOTE_REGEXP" nil org-odt-endnote-regexp t)
    (:odt-endnote-separator nil nil org-odt-endnote-separator t)

    ;; Keys associated with Footnotes
    (:odt-footnote-anchor-format nil nil org-odt-footnote-anchor-format t)
    (:odt-footnote-braces nil nil org-odt-footnote-braces t)
    (:odt-footnote-separator nil nil org-odt-footnote-separator t)

    ;; Org has no *native* support Bibliographies and Citations .  So,
    ;; strictly speaking, the following "BIB_FILE" keyword is ODT only
    ;; and should be prefixed with "ODT_".  However, since the
    ;; Bibliography file option makes sense for *all* backends,
    ;; skipping the "ODT_" prefix, makes much sense.
    (:bib-file "BIB_FILE" nil nil t)
    ;; Other variables.
    (:odt-fontify-srcblocks nil nil org-odt-fontify-srcblocks)
    (:odt-format-drawer-function nil nil org-odt-format-drawer-function)
    (:odt-format-headline-function nil nil org-odt-format-headline-function)
    (:odt-format-inlinetask-function nil nil org-odt-format-inlinetask-function)
    (:odt-inline-formula-rules nil nil org-odt-inline-formula-rules)
    (:odt-inline-image-rules nil nil org-odt-inline-image-rules)
    (:odt-pixels-per-inch nil nil org-odt-pixels-per-inch)
    (:odt-styles-file nil nil org-odt-styles-file)
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
  '("odt" "odf" "odm")
  "List of OpenDocument file extensions that this backend can generate.")

(defconst org-odt-page-break-style-format "
<style:style style:name=\"%s\" style:family=\"paragraph\" style:parent-style-name=\"%s\" style:master-page-name=\"%s\">
 <style:paragraph-properties %s/>
</style:style>")

(defconst org-odt-table-style-format
  "
<style:style style:name=\"%s\" style:family=\"table\">
  <style:table-properties style:rel-width=\"%d%%\" fo:margin-top=\"0cm\" fo:margin-bottom=\"0.20cm\" table:align=\"center\"/>
</style:style>
"
  "Template for auto-generated Table styles.")

(defvar org-odt-src-block-paragraph-format
  "<style:style style:name=\"OrgSrcBlock\" style:family=\"paragraph\" style:parent-style-name=\"Preformatted_20_Text\">
   <style:paragraph-properties fo:background-color=\"%s\" fo:padding=\"0.049cm\" fo:border=\"0.51pt solid #000000\" style:shadow=\"none\">
    <style:background-image/>
   </style:paragraph-properties>
   <style:text-properties fo:color=\"%s\"/>
  </style:style>"
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
    (:MATH-FORMULA: :variable "Text"         :entity-name "Equation" :caption-style "Figure"    :use-outline-levelp t   :seq-num-format "1")
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

(defvar org-odt-toc-templates
  (cl-loop for (category . category-props) in org-odt-caption-and-numbering-settings
	   when (memq category '(:FIGURE: :LISTING: :TABLE:)) collect
	   (let ((caption-sequence-format "text")
		 (index-title (format "List of %ss" (plist-get category-props :entity-name))))
	     (cons
	      (downcase (concat (plist-get category-props :entity-name) "s"))
	      (format
	       "
   <text:illustration-index text:style-name=\"OrgIndexSection\" text:protected=\"true\" text:name=\"Org Index1\">
    <text:illustration-index-source text:caption-sequence-name=\"%s\" text:caption-sequence-format=\"%s\">
     <text:index-title-template text:style-name=\"Org_20_Index_20_Heading\">%s</text:index-title-template>
     <text:illustration-index-entry-template text:style-name=\"Org_20_Index\">
      <text:index-entry-link-start text:style-name=\"Index_20_Link\"/>
      <text:index-entry-text/>
      <text:index-entry-tab-stop style:type=\"right\" style:leader-char=\".\"/>
      <text:index-entry-page-number/>
      <text:index-entry-link-end/>
     </text:illustration-index-entry-template>
    </text:illustration-index-source>
    <text:index-body>
      <text:index-title text:style-name=\"OrgIndexSection\" text:name=\"Org Index1_Head\">
        <text:p text:style-name=\"Org_20_Index_20_Heading\">%s</text:p>
      </text:index-title>
    </text:index-body>
   </text:illustration-index>
"
	       (plist-get category-props :variable)
	       caption-sequence-format
	       index-title
	       index-title)))))

(defvar hfy-user-sheet-assoc)



;;; User Configuration Variables

(defgroup org-export-odt nil
  "Options for exporting Org mode files to ODT."
  :tag "Org Export ODT"
  :group 'org-export)


;;;; Debugging

(defcustom org-odt-prettify-xml nil
  "Specify whether or not the xml output should be prettified.
When this option is turned on, `indent-region' is run on all
component xml buffers before they are saved.  Turn this off for
regular use.  Turn this on if you need to examine the xml
visually."
  :group 'org-export-odt
  :version "24.1"
  :type 'boolean)


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

(defcustom org-odt-display-outline-level 2
  "Outline levels considered for enumerating captioned entities."
  :group 'org-export-odt
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'integer)

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

  You most definitely would want the exported document go through
  this macro first.  So, keep this macro enabled and have it
  first in the transformer pipeline.


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
     "soffice --headless --convert-to %f%x --outdir %d %i")
    ("unoconv"
     "unoconv -f %f -o %d %i"))
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
implementation filed under `org-odt-get-table-cell-styles'.

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

;;;; Frame

(defun org-odt--frame (text width height style &optional extra
			    anchor-type &rest title-and-desc)
  (let* ((frame-attrs
	  (concat
	   (if width (format " svg:width=\"%0.2fcm\"" width) "")
	   (if height (format " svg:height=\"%0.2fcm\"" height) "")
	   extra
	   (format " text:anchor-type=\"%s\"" (or anchor-type "paragraph")))))
    (format
     "\n<draw:frame draw:style-name=\"%s\"%s>\n%s\n</draw:frame>"
     style frame-attrs
     (concat text
	     (let ((title (car title-and-desc))
		   (desc (cadr title-and-desc)))
	       (concat (when title
			 (format "<svg:title>%s</svg:title>"
				 (org-odt--encode-plain-text title t)))
		       (when desc
			 (format "<svg:desc>%s</svg:desc>"
				 (org-odt--encode-plain-text desc t)))))))))


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

(defun org-odt--read-attribute (element &optional property)
  (let ((attrs (cl-case (org-element-type element)
		 (headline
		  (let* ((value (org-element-property :ATTR_ODT  element)))
		    (when value
		      (ignore-errors (read (format "(%s)" value))))))
		 (t (cl-loop for x on (org-export-read-attribute :attr_odt element) by 'cddr
			     append (list (car x) (read (cadr x))))))))
    (if property (plist-get attrs property) attrs)))


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

(defun org-odt--textbox (text width height style &optional
			      extra anchor-type)
  (org-odt--frame
   (format "\n<draw:text-box %s>%s\n</draw:text-box>"
	   (concat (format " fo:min-height=\"%0.2fcm\"" (or height .2))
		   (and (not width)
			(format " fo:min-width=\"%0.2fcm\"" (or width .2))))
	   text)
   width nil style extra anchor-type))



;;;; Table of Contents

(defun org-odt-begin-toc (index-title max-depth &optional parent-depth)
  (concat
   (let ((scope (if (zerop parent-depth) "document" "chapter")))
     (format "
    <text:table-of-content text:style-name=\"OrgIndexSection\" text:protected=\"true\" text:name=\"Table of Contents\">
     <text:table-of-content-source text:outline-level=\"%d\" text:use-outline-level=\"false\" text:use-index-source-styles=\"true\" text:index-scope=\"%s\">
      <text:index-title-template text:style-name=\"Contents_20_Heading\">%s</text:index-title-template>
" max-depth scope index-title))

   (cl-loop for level from 1 to 10
	    concat
	    (format
	     "
      <text:table-of-content-entry-template text:outline-level=\"%d\" text:style-name=\"Contents_20_%d\">
       <text:index-entry-link-start text:style-name=\"Internet_20_link\"/>
       <text:index-entry-chapter/>
       <text:index-entry-text/>
       <text:index-entry-link-end/>
      </text:table-of-content-entry-template>
" level level))

   (cl-loop for depth from (1+ parent-depth) to max-depth
	    for level from 1
	    concat
	    (format
	     "
    <text:index-source-styles text:outline-level=\"%d\">
      <text:index-source-style text:style-name=\"Heading_20_%d\"/>
    </text:index-source-styles>
"
	     level depth))
   (format  "
     </text:table-of-content-source>

     <text:index-body>
      <text:index-title text:style-name=\"Sect1\" text:name=\"Table of Contents1_Head\">
       <text:p text:style-name=\"Contents_20_Heading\">%s</text:p>
      </text:index-title>
 " index-title)))

(defun org-odt-end-toc ()
  (format "
     </text:index-body>
    </text:table-of-content>
"))

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

(defun org-odt-toc (depth info &optional scope)
  (cl-assert (wholenump depth))
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
  (let* ((title (org-export-translate "Table of Contents" :utf-8 info))
	 (headlines (org-export-collect-headlines
		     info (and (wholenump depth) depth) scope))
	 (parent-level (or (and scope (org-export-get-relative-level
				       (org-export-get-parent-headline scope) info))
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
	(if (> parent-level 1) toc-text
	  (concat (org-odt-begin-toc title depth parent-level) toc-text
		  (org-odt-end-toc)))))))


;;;; Document styles

(defun org-odt--count-object (info object)
  (cl-assert (symbolp object))
  (let* ((counters (plist-get info :odt-object-counters))
	 (seqno (1+ (or (plist-get counters object) 0))))
    (plist-put info :odt-object-counters
	       (plist-put counters object seqno))
    seqno))

(defun org-odt--name-object (info object)
  (cl-assert (and (symbolp object)
		  (not (keywordp object))))
  (format "%s%d"
	  (capitalize (symbol-name object))
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

;;;; Inner Template

(defun org-odt-inner-template (contents info)
  "Return body of document string after ODT conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ( ;; `org-display-custom-times' should be accessed right
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
		  (expand-file-name "OrgOdtContentTemplate.xml" org-odt-styles-dir))
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
      ;;   1. styles specified with "#+ODT_AUTOMATIC_STYLES: ..."
      ;;   2. paragraph styles that request pagebreaks
      ;;   3. table styles that specify `:rel-width'
      (insert (org-element-normalize-string (or (plist-get info :odt-automatic-styles) "")))

      ;; - Dump automatic styles for paragraphs within a table.  See
      ;;   `org-odt-table-cell--get-paragraph-styles'.
      (let* ((p-styles (cl-delete-duplicates
			(org-element-map (plist-get info :parse-tree) 'table
			  (lambda (table)
			    (org-odt--read-attribute table :p-style))
			  info) :test 'string=)))
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
      (when (plist-get info :odt-use-date-fields)
	(insert (org-odt--build-date-styles (car custom-time-fmts)
					    "OrgDate1")
		(org-odt--build-date-styles (cdr custom-time-fmts)
					    "OrgDate2")))
      ;; Update display level.
      ;; - Remove existing sequence decls.  Also position the cursor.
      (goto-char (point-min))
      (when (re-search-forward "<text:sequence-decls" nil t)
	(delete-region (match-beginning 0)
		       (re-search-forward "</text:sequence-decls>" nil nil)))
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
      ;; Position the cursor to document body.
      (goto-char (point-min))
      (re-search-forward "</office:text>" nil nil)
      (goto-char (match-beginning 0))

      ;; Preamble - Title, Author, Date etc.
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
      ;; Table of Contents
      (let* ((with-toc (plist-get info :with-toc))
	     (depth (and with-toc (if (wholenump with-toc)
				      with-toc
				    (plist-get info :headline-levels)))))
	(when depth (insert (or (org-odt-toc depth info) ""))))
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

;;;; Write content.xml

(defun org-odt-write-contents-file (contents _backend info)
  (with-temp-buffer
    (insert contents)
    ;; Write content.xml.
    (let ((coding-system-for-write 'utf-8))
      (write-file
       (concat (plist-get info :odt-zip-dir) "content.xml")))
    ;; Create a manifest entry for content.xml.
    (org-odt-create-manifest-file-entry info "text/xml" "content.xml")))


;;;; Write styles.xml

(defun org-odt-write-styles-file (_contents _backend info)
  (let* ((styles-file
	  (let ((file (plist-get info :odt-styles-file)))
	    (cond
	     ((or (not file) (string= file ""))
	      (expand-file-name "OrgOdtStyles.xml" org-odt-styles-dir))
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

      ;; Write extra styles.
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
      (when org-odt-prettify-xml
	(nxml-mode)
	(indent-region (point-min) (point-max)))
      ;; Write styles.xml
      (let ((coding-system-for-write 'utf-8))
	(write-file (concat (plist-get info :odt-zip-dir) "styles.xml"))))))


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
	   (format "<meta:user-defined meta:name=\"Subtitle\">%s</meta:user-defined>\n" subtitle)))
	"  </office:meta>\n" "</office:document-meta>"))
      ;; Prettify buffer contents, if needed
      (when org-odt-prettify-xml
	(nxml-mode)
	(indent-region (point-min) (point-max)))
      ;; Write meta.xml.
      (let ((coding-system-for-write 'utf-8))
	(write-file (concat (plist-get info :odt-zip-dir) "meta.xml"))))

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
    (when org-odt-prettify-xml
      (nxml-mode)
      (indent-region (point-min) (point-max)))
    ;; Write manifest.xml
    (let ((coding-system-for-write 'utf-8))
      (write-file (concat (plist-get info :odt-zip-dir) "META-INF/manifest.xml")))))

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
	    (write-file file)))))))

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
		 '(org-odt--transform-target
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
  (string-match-p (plist-get info :odt-endnote-regexp)
		  (org-element-property :label footnote-reference)))

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
    (todo todo-type  priority name tags contents)
  "Transcode an INLINETASK element from Org to ODT.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
	  "Standard"
	  (org-odt--textbox
	   (concat
	    (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
		    "OrgInlineTaskHeading"
		    (org-odt-format-headline-default-function
		     todo todo-type  priority name tags))
	    contents)
	   nil nil "OrgInlineTaskFrame" " style:rel-width=\"100%\"")))

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

(defun org-odt-keyword (keyword _contents info)
  "Transcode a KEYWORD element from Org to ODT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "ODT") value)
     ((string= key "INDEX")
      ;; FIXME
      (ignore))
     ((string= key "TOC")
      (let ((value (downcase value)))
	(cond
	 ((string-match "\\<headlines\\>" value)
	  (let ((depth (or (and (string-match "[0-9]+" value)
				(string-to-number (match-string 0 value)))
			   (plist-get info :with-toc)))
		(localp (string-match-p "\\<local\\>" value)))
	    (when (wholenump depth) (org-odt-toc depth info (and localp keyword)))))
	 ((or (string-match-p "\\<tables\\>" value)
	      (string-match-p "\\<figures\\>" value)
	      (string-match-p "\\<listings\\>" value))
	  ;; Return an index template.
	  (let* ((localp (string-match-p "\\<local\\>" value))
		 (template (assoc-default (car (split-string value)) org-odt-toc-templates)))
	    ;; Specify the scope of the index.
	    (if (string-match "<text:illustration-index-source\\(.\\|\n\\)*?>" template)
		(setq  template (replace-match (concat (match-string 1 template)
						       (format " text:index-scope=\"%s\""
							       (if localp "chapter" "document")))
					       t t template 1)))
	    ;; Translate the title.
	    (if (string-match "\\(?:<text:index-title-template.*>\\(?1:\\(?:.\\|\n\\)+?\\)</text:index-title-template>\\)"
			      template)
		(replace-match (org-export-translate (org-trim (match-string 1 template)) :utf-8 info) t t template 1)
	      template))))))
     ;; Handle BIBLIOGRAPHY.  Ignore it.
     ((string= key "BIBLIOGRAPHY")
      ;; Citation Processors (see ox-jabref.el) may handle this by
      ;; registering their own transcoders.
      (ignore))
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
	(org-element-put-property keyword :attr_odt (list ":page-break \"after\"") ))

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
	   (table :TABLE:)
	   (src-block :LISTING:)
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
	  ;; Sneaking in short-caption as name attribute is
	  ;; problematic with LibreOffice > 4.0.4.2.  So ignore
	  ;; short-captions.  See following thread:
	  ;; http://lists.gnu.org/archive/html/emacs-orgmode/2013-12/msg00100.html
	  (ignore
	   (let ((short-caption (or short-caption caption))
		 (backend (org-export-create-backend
			   :parent (org-export-backend-name
				    (plist-get info :back-end))
			   :transcoders
			   (mapcar (lambda (type) (cons type (lambda (_o c _i) c)))
				   org-element-all-objects))))
	     (when short-caption
	       (org-export-data-with-backend short-caption backend info))))))
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
	      (list
	       (let ((caption-text
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
				  (org-export-translate (plist-get category-props :entity-name) :utf-8 info))
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
		 (unless (string= caption-text "")
		   (cl-case (or format-prop :caption-format)
		     (:caption-format
		      (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
			      (plist-get category-props :caption-style)
			      caption-text))
		     (t caption-text))))
	       short-caption
	       (plist-get (assoc-default secondary-category org-odt-caption-and-xref-settings)
			  :caption-position)))
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

(defun org-odt-link--inline-image (element info)
  "Return ODT code for an inline image.
LINK is the link pointing to the inline image.  INFO is a plist
used as a communication channel."
  (cl-assert (eq (org-element-type element) 'link))
  (let* ((src (let* ((type (org-element-property :type element))
		     (raw-path (org-element-property :path element)))
		(cond ((member type '("http" "https"))
		       (concat type ":" raw-path))
		      ((file-name-absolute-p raw-path)
		       (expand-file-name raw-path))
		      (t raw-path))))
	 (src-expanded (if (file-name-absolute-p src) src
			 (expand-file-name src (file-name-directory
						(plist-get info :input-file)))))
	 (href (format
		"\n<draw:image xlink:href=\"%s\" xlink:type=\"simple\" xlink:show=\"embed\" xlink:actuate=\"onLoad\"/>"
		(org-odt--copy-image-file info
					  src-expanded
					  (format "Images/%04d.%s"
						  (org-odt--count-object info :images)
						  (file-name-extension src-expanded)))))
	 ;; Extract attributes from #+ATTR_ODT line.
	 (attr-from (cl-case (org-element-type element)
		      (link (org-export-get-parent-element element))
		      (t element)))
	 ;; Handle `:anchor', `:style' and `:attributes' properties.
	 (user-frame-anchor
	  (car (assoc-string (org-odt--read-attribute attr-from :anchor)
			     '(("as-char") ("paragraph") ("page")) t)))
	 (user-frame-style nil)
	 (user-frame-attrs nil)
	 (user-frame-params
	  (list user-frame-style user-frame-attrs user-frame-anchor))
	 ;; (embed-as (or embed-as user-frame-anchor "paragraph"))
	 ;;
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
	     src-expanded info
	     (funcall --user-dim (org-odt--read-attribute attr-from :width))
	     (funcall --user-dim (org-odt--read-attribute attr-from :height))
	     (org-odt--read-attribute attr-from :scale)
	     nil			; embed-as
	     "paragraph"		; FIXME
	     )))
	 (widths (car size)) (heights (cdr size))
	 (standalone-link-p (org-odt--standalone-link-p element info))
	 (embed-as (if standalone-link-p "paragraph" "as-char"))
	 (captions (org-odt-format-label element info 'definition))
	 (caption (nth 0 captions))
	 (entity (concat (and caption "Captioned") embed-as "Image"))
	 ;; Check if this link was created by LaTeX-to-PNG converter.
	 (replaces (org-element-property
		    :replaces (if (not standalone-link-p) element
				(org-export-get-parent-element element))))
	 ;; If yes, note down the type of the element - LaTeX Fragment
	 ;; or LaTeX environment.  It will go in to frame title.
	 (title (and replaces (capitalize
			       (symbol-name (org-element-type replaces)))))

	 ;; If yes, note down its contents.  It will go in to frame
	 ;; description.  This quite useful for debugging.
	 (desc (and replaces (org-element-property :value replaces))))
    (org-odt--render-image/formula entity href widths heights
				   captions user-frame-params title desc)))


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
	 (embed-as (if standalone-link-p 'paragraph 'character))
	 (captions (org-odt-format-label element info 'definition))
	 ;; Check if this link was created by LaTeX-to-MathML
	 ;; converter.
	 (replaces (org-element-property
		    :replaces (if (not standalone-link-p) element
				(org-export-get-parent-element element))))
	 ;; If yes, note down the type of the element - LaTeX Fragment
	 ;; or LaTeX environment.  It will go in to frame title.
	 (title (and replaces (capitalize
			       (symbol-name (org-element-type replaces)))))

	 ;; If yes, note down its contents.  It will go in to frame
	 ;; description.  This quite useful for debugging.
	 (desc (and replaces (org-element-property :value replaces)))
	 (width nil) (height nil))
    (cond
     ((eq embed-as 'character)
      (org-odt--render-image/formula "InlineFormula" href width height
				     nil nil title desc))
     (t
      (let* ((equation (org-odt--render-image/formula
			"CaptionedDisplayFormula" href width height
			captions nil title desc))
	     (label
	      (car (org-odt-format-label element info 'definition :label-format))))
	(concat equation "<text:tab/>" label))))))

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

(defun org-odt--render-image/formula (cfg-key href widths heights &optional
					      captions user-frame-params
					      &rest title-and-desc)
  (let* ((frame-cfg-alist
	  ;; Each element of this alist is of the form (CFG-HANDLE
	  ;; INNER-FRAME-PARAMS OUTER-FRAME-PARAMS).

	  ;; CFG-HANDLE is the key to the alist.

	  ;; INNER-FRAME-PARAMS and OUTER-FRAME-PARAMS specify the
	  ;; frame params for INNER-FRAME and OUTER-FRAME
	  ;; respectively.  See below.

	  ;; Configurations that are meant to be applied to
	  ;; non-captioned image/formula specifies no
	  ;; OUTER-FRAME-PARAMS.

	  ;; TERMINOLOGY
	  ;; ===========
	  ;; INNER-FRAME :: Frame that directly surrounds an
	  ;;                image/formula.

	  ;; OUTER-FRAME :: Frame that encloses the INNER-FRAME.  This
	  ;;                frame also contains the caption, if any.

	  ;; FRAME-PARAMS :: List of the form (FRAME-STYLE-NAME
	  ;;                 FRAME-ATTRIBUTES FRAME-ANCHOR).  Note
	  ;;                 that these are the last three arguments
	  ;;                 to `org-odt--frame'.

	  ;; Note that an un-captioned image/formula requires just an
	  ;; INNER-FRAME, while a captioned image/formula requires
	  ;; both an INNER and an OUTER-FRAME.
	  '(("As-CharImage" ("OrgInlineImage" nil "as-char"))
	    ("ParagraphImage" ("OrgDisplayImage" nil "paragraph"))
	    ("PageImage" ("OrgPageImage" nil "page"))
	    ("CaptionedAs-CharImage"
	     ("OrgDisplayImage"
	      " style:rel-width=\"100%\" style:rel-height=\"scale\"" "paragraph")
	     ("OrgInlineImage" nil "as-char"))
	    ("CaptionedParagraphImage"
	     ("OrgDisplayImage"
	      " style:rel-width=\"100%\" style:rel-height=\"scale\"" "paragraph")
	     ("OrgImageCaptionFrame" nil "paragraph"))
	    ("CaptionedPageImage"
	     ("OrgDisplayImage"
	      " style:rel-width=\"100%\" style:rel-height=\"scale\"" "paragraph")
	     ("OrgPageImageCaptionFrame" nil "page"))
	    ("InlineFormula" ("OrgInlineFormula" nil "as-char"))
	    ("DisplayFormula" ("OrgDisplayFormula" nil "as-char"))
	    ("CaptionedDisplayFormula"
	     ("OrgCaptionedFormula" nil "as-char")
	     ("OrgFormulaCaptionFrame" nil "paragraph"))))
	 (caption (nth 0 captions)) (short-caption (nth 1 captions))
	 (caption-position (nth 2 captions))
	 ;; Retrieve inner and outer frame params, from configuration.
	 (frame-cfg (assoc-string cfg-key frame-cfg-alist t))
	 (inner (nth 1 frame-cfg))
	 (outer (nth 2 frame-cfg))
	 ;; User-specified frame params (from #+ATTR_ODT spec)
	 (user user-frame-params)
	 (--merge-frame-params (lambda (default user)
				 "Merge default and user frame params."
				 (if (not user) default
				   (cl-assert (= (length default) 3))
				   (cl-assert (= (length user) 3))
				   (cl-loop for u in user
					    for d in default
					    collect (or u d))))))
    (cond
     ;; Case 1: Image/Formula has no caption.
     ;;         There is only one frame, one that surrounds the image
     ;;         or formula.
     ((not caption)
      ;; Merge user frame params with that from configuration.
      (setq inner (funcall --merge-frame-params inner user))
      (apply 'org-odt--frame href (car widths) (car heights)
	     (append inner title-and-desc)))
     ;; Case 2: Image/Formula is captioned or labeled.
     ;;         There are two frames: The inner one surrounds the
     ;;         image or formula.  The outer one contains the
     ;;         caption/sequence number.
     (t
      ;; Merge user frame params with outer frame params.
      (setq outer (funcall --merge-frame-params outer user))
      ;; Short caption, if specified, goes as part of inner frame.
      (setq inner (let ((frame-params (copy-sequence inner)))
		    (when (or (cdr widths) (cdr heights))
		      (setcar (cdr frame-params) nil))
		    (setcar (cdr frame-params)
			    (concat
			     (cadr frame-params)
			     (when short-caption
			       (format " draw:name=\"%s\" " short-caption))))
		    frame-params))
      (let ((text (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
			  "Caption"
			  (apply 'org-odt--frame href (car widths) (car heights)
				 (append inner title-and-desc)))))
	(apply 'org-odt--textbox
	       (cl-case caption-position
		 (above (concat caption text))
		 (t (concat text caption)))
	       (or (cdr widths) (car widths))
	       (or (cdr heights) (car heights))
	       outer))))))

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
  (let* ((type (org-element-property :type link))
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
	    (cond
	     ;; If file path is absolute, prepend it with protocol
	     ;; component - "file://".
	     ((file-name-absolute-p raw-path)
	      (org-export-file-uri raw-path))
	     ;; Otherwise, use the relative path, but prepend it with "../".
	     (t (concat "../" (file-relative-name raw-path) ))))
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
	(cl-case (org-element-type destination)
	  ;; Case 1: Fuzzy link points nowhere.
	  ('nil
	   (user-error
	    "Link \"%s\" at char position %d-%d points nowhere."
	    (org-element-property :raw-link link)
	    (org-element-property :begin link)
	    (org-element-property :end link)))
	  ;; Case 2: Fuzzy link points to a headline.
	  (headline
	   ;; If there's a description, create a hyperlink.
	   ;; Otherwise, try to provide a meaningful description.
	   (if (not desc) (org-odt-link--infer-description destination info)
	     (format
	      "<text:a xlink:type=\"simple\" xlink:href=\"#%s\">%s</text:a>"
	      (org-export-get-reference destination info) desc)))
	  ;; Case 3: Fuzzy link points to a target.
	  (target
	   ;; If there's a description, create a hyperlink.
	   ;; Otherwise, try to provide a meaningful description.
	   (if (not desc) (org-odt-link--infer-description destination info)
	     (format "<text:a xlink:type=\"simple\" xlink:href=\"#%s\">%s</text:a>"
		     (org-export-get-reference destination info)
		     desc)))
	  ;; Case 4: Fuzzy link points to some element (e.g., an
	  ;; inline image, a math formula or a table).
	  (otherwise
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
	    ;; use the fallback style "Standard".
	    (cl-loop for el in data
		     ;; Fallback style.
		     with style = "Standard"
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
				  ((string= type "section")
				   (org-odt--read-attribute el :p-style))
				  ;; Case 2: Handle user-specified
				  ;; SPECIAL-BLOCKs not known to the
				  ;; exporter.
				  (t (org-odt--read-attribute el :style)))))
			      (table-cell
			       ;; A table cell can have paragraphs, only if
			       ;; it is part of a list table.
			       (org-odt-table-cell--get-paragraph-styles el info)))
			    ;; Case 2: Element doesn't specify a style of
			    ;; it's own.  Use the parent style.
			    style))
		     finally return style)))
      ;; If this paragraph is a leading paragraph in an item and the
      ;; item has a checkbox, splice the checkbox and paragraph contents
      ;; together.
      (when (and (eq (org-element-type parent) 'item)
		 (eq paragraph (car (org-element-contents parent))))
	(setq contents (concat (org-odt--checkbox parent) contents)))

      (cond
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
		contents))))))


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
    output))


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

(defun org-odt-section (_section contents _info) ; FIXME
  "Transcode a SECTION element from Org to ODT.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)

;;;; Radio Target

(defun org-odt-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to ODT.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (org-odt--target text (org-export-get-reference radio-target info)))


;;;; Special Block

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
      (let ((width (org-odt--read-attribute special-block :width))
	    (height (org-odt--read-attribute special-block :height))
	    (style (org-odt--read-attribute special-block :style))
	    (extra (org-odt--read-attribute special-block :extra))
	    (anchor (org-odt--read-attribute special-block :anchor)))
	(org-odt-paragraph special-block
			   (org-odt--textbox contents width height
					     style extra anchor)
			   info)))
     (t contents))))


;;;; Src Block

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
  (let* ((css-list (hfy-face-to-style fn))
	 (style-name (concat "OrgSrc"
                             (mapconcat
                              'capitalize (split-string
                                           (hfy-face-or-def-to-name fn) "-")
                              "")))
	 (color-val (cdr (assoc "color" css-list)))
	 (background-color-val (cdr (assoc "background" css-list))))
    (cons style-name
	  (when org-odt-create-custom-styles-for-srcblocks
	    (if (eq fn 'default)
		(format org-odt-src-block-paragraph-format background-color-val color-val)
	      (format
	       "
<style:style style:name=\"%s\" style:family=\"text\">
  <style:text-properties fo:background-color=\"%s\" fo:color=\"%s\" />
 </style:style>" style-name background-color-val color-val))))))

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
contextual information."
  (let* ((_lang (org-element-property :language src-block))
	 (captions (org-odt-format-label src-block info 'definition))
	 (caption (nth 0 captions))
	 (caption-position (nth 2 captions))
	 (text (let ((--src-block (org-odt-format-code src-block info)))
		 ;; Is `:textbox' property non-nil?
		 (if (not (org-odt--read-attribute src-block :textbox)) --src-block
		   ;; Yes.  Enclose it in a Text Box.
		   (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
			   "Text_20_body"
			   (org-odt--textbox --src-block nil nil nil))))))
    (cl-case caption-position
      (above (concat caption text))
      (t (concat text caption)))))


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
  (let* ((table (org-export-get-parent-table element))
	 (table-style (org-odt--read-attribute table :style)))
    (assoc table-style (plist-get info :odt-table-styles))))

(defun org-odt-get-table-cell-styles (table-cell info)
  "Retrieve styles applicable to a table cell.
R and C are (zero-based) row and column numbers of the table
cell.  STYLE-SPEC is an entry in `org-odt-table-styles'
applicable to the current table.  It is nil if the table is not
associated with any style attributes.

Return a cons of (TABLE-CELL-STYLE-NAME . PARAGRAPH-STYLE-NAME).

When STYLE-SPEC is nil, style the table cell the conventional way
- choose cell borders based on row and column groupings and
choose paragraph alignment based on `org-col-cookies' text
property.  See also
`org-odt-get-paragraph-style-cookie-for-table-cell'.

When STYLE-SPEC is non-nil, ignore the above cookie and return
styles congruent with the ODF-1.2 specification."
  (let* ((table-cell-address (org-odt-table-cell-address table-cell info))
	 (r (car table-cell-address)) (c (cdr table-cell-address))
	 (style-spec (org-odt-table-style-spec table-cell info))
	 (table-dimensions (org-export-table-dimensions
			    (org-export-get-parent-table table-cell)
			    info)))
    (when style-spec
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
      (let* ((template-name (nth 1 style-spec))
	     (cell-style-selectors (nth 2 style-spec))
	     (cell-type
	      (cond
	       ((and (cdr (assq 'use-first-column-styles cell-style-selectors))
		     (= c 0)) "FirstColumn")
	       ((and (cdr (assq 'use-last-column-styles cell-style-selectors))
		     (= (1+ c) (cdr table-dimensions)))
		"LastColumn")
	       ((and (cdr (assq 'use-first-row-styles cell-style-selectors))
		     (= r 0)) "FirstRow")
	       ((and (cdr (assq 'use-last-row-styles cell-style-selectors))
		     (= (1+ r) (car table-dimensions)))
		"LastRow")
	       ((and (cdr (assq 'use-banding-rows-styles cell-style-selectors))
		     (= (% r 2) 1)) "EvenRow")
	       ((and (cdr (assq 'use-banding-rows-styles cell-style-selectors))
		     (= (% r 2) 0)) "OddRow")
	       ((and (cdr (assq 'use-banding-columns-styles cell-style-selectors))
		     (= (% c 2) 1)) "EvenColumn")
	       ((and (cdr (assq 'use-banding-columns-styles cell-style-selectors))
		     (= (% c 2) 0)) "OddColumn")
	       (t ""))))
	(concat template-name cell-type)))))

(defun org-odt--table-cell-widths (table info)
  (let* ((user-widths (org-export-read-attribute :attr_odt table :widths))
	 (user-width-p (and user-widths t))
	 (user-widths (and user-width-p (split-string user-widths ","))))
    (org-element-map
	(org-element-map table 'table-row
	  (lambda (row)
	    (unless (eq (org-element-property :type row) 'rule) row))
	  info 'first-match)
	'table-cell
      (lambda (table-cell)
	(or (and user-width-p (string-to-number (or (pop user-widths) "0")))
	    (org-export-table-cell-width table-cell info) 0))
      info nil 'table)))

(defun org-odt-table-cell--get-paragraph-styles (table-cell info)
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
	 (_r (car table-cell-address))
	 (c (cdr table-cell-address))
	 (table-row (org-export-get-parent table-cell))
	 (custom-style-prefix (org-odt-get-table-cell-styles
			       table-cell info))
	 (table (org-export-get-parent-table table-cell))
	 (p-style (or (org-odt--read-attribute table :p-style) "OrgTable")))
    (or
     (and custom-style-prefix (null (org-odt--read-attribute table :p-style))
	  (format "%sContents" custom-style-prefix))
     ;; Style names used for paragraphs in a table is a concatenation
     ;; of BASE-STYLE, CELL-TYPE and CELL-ALIGNMENT
     (concat
      ;; BASE-STYLE: The base paragraph style, as specified by the
      ;; user.  Or "OrgTable", if none.
      p-style
      ;; CELL-TYPE: One of "Heading" or "Contents", according as the
      ;; table cell is part of a header column / header row or
      ;; otherwise.
      (cond
       ((and (= 1 (org-export-table-row-group table-row info))
	     (org-export-table-has-header-p
	      (org-export-get-parent-table table-row) info))
	"Heading")
       ((let* ((table-header-columns
		(let ((cols (org-odt--read-attribute table :header-columns)))
		  (and cols (read cols)))))
	  (<= c (cond ((wholenump table-header-columns)
		       (- table-header-columns 1))
		      (table-header-columns 0)
		      (t -1))))
	"Heading")
       (t "Contents"))
      ;; CELL-ALIGNMENT: One of "Left", "Right" or "Center" based on
      ;; the column alignment.  BUT, ignore column alignment
      ;; attributes on list tables, to work around
      ;; https://github.com/kjambunathan/org-mode-ox-odt/issues/25.
      (unless (org-odt--read-attribute table :list-table)
	(capitalize (symbol-name (org-export-table-cell-alignment
				  table-cell info))))))))

(defun org-odt-table-cell-types (table info)
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
	       (let* ((span (org-odt--read-attribute table :span)))
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

(defun org-odt-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((table-cell-address (org-odt-table-cell-address table-cell info))
	 (table-cell-borders (org-export-table-cell-borders table-cell info))
	 (_r (car table-cell-address))
	 (c (cdr table-cell-address))
	 (horiz-span (nth c (org-odt--table-cell-widths
			     (org-export-get-parent-table table-cell) info)))
	 (table-cell-types (org-odt-table-cell-types (org-export-get-parent-table table-cell) info))
	 (table-cell-type (cdr (assoc (cons (1+ (car table-cell-address)) (1+ (cdr table-cell-address)))
				      table-cell-types)))
	 (custom-style-prefix (org-odt-get-table-cell-styles
			       table-cell info))
	 (cell-style-name
	  (or
	   (and custom-style-prefix (format "%sTableCell"
					    custom-style-prefix))
	   (concat
	    "OrgTblCell"
	    (when (memq 'above table-cell-borders) "T")
	    (when (memq 'below table-cell-borders) "B")
	    (when (memq 'left table-cell-borders) "L")
	    (when (memq 'right table-cell-borders) "R"))))
	 (cell-attributes
	  (cond
	   ;; Table with `:span' cells.
	   ((null table-cell-types)
	    (concat
	     (format " table:style-name=\"%s\"" cell-style-name)
	     (and (> horiz-span 0)
		  (format " table:number-columns-spanned=\"%d\""
			  (1+ horiz-span)))))
	   ;; Table without any `:span' cells.
	   (t
	    (concat
	     (format " table:style-name=\"%s\"" cell-style-name)
	     (when (consp table-cell-type)
	       (pcase-let ((`(,rowspan . ,colspan) table-cell-type))
		 (concat
		  (unless (= 1 rowspan)
		    (format " table:number-rows-spanned=\"%d\""
			    rowspan))
		  (unless (= 1 colspan)
		    (format " table:number-columns-spanned=\"%d\""
			    colspan))))))))))
    (unless contents (setq contents ""))
    (cond
     ;; Table with `:span' cells.
     ((null table-cell-types)
      (concat
       (format "\n<table:table-cell%s>\n%s\n</table:table-cell>"
	       cell-attributes
	       (let ((table-cell-contents (org-element-contents table-cell)))
		 (if (eq (org-element-class (car table-cell-contents)) 'element)
		     contents
		   (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
			   (org-odt-table-cell--get-paragraph-styles table-cell info)
			   contents))))
       (let (s)
	 (dotimes (_i horiz-span s)
	   (setq s (concat s "\n<table:covered-table-cell/>"))))
       "\n"))
     ;; Table without any `:span' cells.
     (t
      (cond
       ((eq table-cell-type 'covered) "\n<table:covered-table-cell/>")
       (t
	(concat
	 (format "\n<table:table-cell%s>\n%s\n</table:table-cell>"
		 cell-attributes
		 (let ((table-cell-contents (org-element-contents table-cell)))
		   (if (eq (org-element-class (car table-cell-contents)) 'element)
		       contents
		     (format "\n<text:p text:style-name=\"%s\">%s</text:p>"
			     (org-odt-table-cell--get-paragraph-styles table-cell info)
			     contents))))
	 "\n")))))))


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
     (let* ((captions (org-odt-format-label table info 'definition))
	    (caption (nth 0 captions)) (short-caption (nth 1 captions))
	    (caption-position (nth 2 captions))
	    (custom-table-style (nth 1 (org-odt-table-style-spec table info)))
	    (table-column-specs
	     (lambda (table info)
	       (let* ((table-style (or custom-table-style "OrgTable"))
		      (column-style (format "%sColumn" table-style)))
		 (mapconcat
		  (lambda (width)
		    (setq width (1+ width))
		    (let ((s (format
			      "\n<table:table-column table:style-name=\"%s\"/>"
			      column-style))
			  out)
		      (dotimes (_i width out) (setq out (concat s out)))))
		  (org-odt--table-cell-widths table info) "\n"))))
	    (text (concat
		   ;; begin table.
		   (format
		    "\n<table:table table:style-name=\"%s\"%s>"
		    (or custom-table-style
			(let* ((props (org-export-read-attribute :attr_odt table))
			       style-name)
			  (when props
			    (setq style-name (format "Org%s" (org-odt--name-object info 'table)))
			    (plist-put info :odt-automatic-styles
				       (concat (plist-get info :odt-automatic-styles)
					       (format org-odt-table-style-format
						       style-name
						       (or (let ((value (plist-get props :rel-width)))
							     (and value (ignore-errors (read value))))
							   96)))))
			  style-name)
			"OrgTable")
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
		 (format "&lt;%s&gt;" (org-odt--format-timestamp timestamp))))
	(inactive
	 (format "<text:span text:style-name=\"%s\">%s</text:span>"
		 "OrgInactiveTimestamp"
		 (format "[%s]" (org-odt--format-timestamp timestamp))))
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

;;;; Citations

(defun org-odt--translate-cite-fragments (tree _backend info)
  "Translate all \\cite{} LaTeX fragments in TREE to `citation-reference'.
Return the translated tree.

`citation-reference' is a non-standard element (stricly speaking,
an object) used internally by the ODT exporter.  See
`org-odt-citation-transcoders'.

Modify INFO plist by appending a `:citations-alist' property.
The value of this property is a list where each element is of the
form (CITE-KEY . CITATION-REFERENCE), with CITATION-REFERENCE
being the first element that references CITE-KEY.  The list is
sorted in reverse order of appearance of CITE-KEYs in the
exported file."
  (org-element-map tree 'latex-fragment
    (lambda (latex-*)
      (let* ((latex-frag (org-element-property :value latex-*)))
	(when (string-match "\\\\cite{\\(.*?\\)}" latex-frag)
	  ;; LaTeX fragment matches \cite{...}
	  (let* ((value (match-string 1 latex-frag))
		 (cite-keys (split-string value ",")))
	    (let* (;; Translate the LaTeX fragment to a new `citation'
		   ;; object.
		   (citation (apply 'org-element-adopt-elements
				    (list 'citation (list :parenthetical nil))
				    (mapcar (lambda (cite-key)
					      (list 'citation-reference
						    (list :key
							  cite-key)))
					    cite-keys))))
	      ;; Yes. Stash a copy of the original LaTeX fragment in
	      ;; to the new `citation' object.
	      (org-element-put-property citation :replaces (copy-sequence latex-*))
	      ;; Does the Org parser support native `citation' and
	      ;; `citation-reference'objects?
	      (if (not (memq 'citation org-element-all-objects))
		  ;; No. Stash the new `citation' object in to the
		  ;; LaTeX fragment.
		  (org-element-put-property latex-* :replaced-by citation)
		;; Replace the original LaTeX fragment with the
		;; `citation' object.
		(org-element-set-element latex-* citation)))))))
    info nil nil '--with-affiliated)
  tree)

(defun org-odt--collect-cite-keys (tree _backend info)
  "Collect cite keys (in reverse order) in to INFO.

Modify INFO plist by appending a `:citations-alist' property.
The value of this property is a list where each element is of the
form (CITE-KEY . CITATION), with CITATION
being the first element that references CITE-KEY.  The list is
sorted in reverse order of appearance of CITE-KEYs in the
exported file."
  (let (citations-alist)
    (org-element-map tree '(citation 'latex-fragment)
      (lambda (el)
	(let* ((citation (cl-case (org-element-type el)
			   (citation el)
			   (latex-fragment (org-element-property :replaced-by el))
			   (t nil)))
	       (cite-keys (org-element-map citation 'citation-reference
			    (lambda (citation-reference)
			      (org-element-property :key citation-reference)))))
	  (dolist (cite-key cite-keys)
	    (setq cite-key (org-trim cite-key))
	    (unless (assoc cite-key citations-alist)
	      (push (cons cite-key citation) citations-alist)))))
      info nil nil '--with-affiliated)
    ;; Modify INFO by side-effects.
    (nconc info (list :citations-alist citations-alist)))
  tree)

;;;; LaTeX fragments

(defun org-odt--translate-latex-fragments (tree _backend info)
  (let ((processing-type (plist-get info :with-latex))
	(count 0))
    ;; Normalize processing-type to one of dvipng, mathml or verbatim.
    ;; If the desired converter is not available, force verbatim
    ;; processing.
    (cl-case processing-type
      ((t mathml)
       (if (and (fboundp 'org-format-latex-mathml-available-p)
		(org-format-latex-mathml-available-p))
	   (setq processing-type 'mathml)
	 (message "LaTeX to MathML converter not available.")
	 (setq processing-type 'verbatim)))
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
				  (mathml "ltxmathml/"))
				(file-name-sans-extension
				 (file-name-nondirectory input-file))))
		 (display-msg
		  (cl-case processing-type
		    ((dvipng imagemagick)
		     (format "Creating LaTeX Image %d..." count))
		    (mathml
		     (format "Creating MathML snippet %d..." count))))
		 ;; Get an Org-style link to PNG image or the MathML
		 ;; file.
		 (org-link
		  (let ((link (with-temp-buffer
				(insert latex-frag)
				(org-format-latex cache-subdir nil nil cache-dir
						  nil display-msg nil
						  processing-type)
				(buffer-substring-no-properties
				 (point-min) (point-max)))))
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
				     (list :style "OrgFormula"
					   :name (org-element-property :name
								       latex-*)
					   :caption (org-element-property :caption
									  latex-*)))
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



;;; Interactive functions

;;;; Export to OpenDocument formula

;;;###autoload
(defun org-odt-export-as-odf (latex-frag &optional odf-file)
  "Export LATEX-FRAG as OpenDocument formula file ODF-FILE.
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
	(read-string "LaTeX Fragment: " frag nil frag))
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
				      (write-file (concat (plist-get info :odt-zip-dir) "content.xml")))
				    (org-odt-create-manifest-file-entry info "text/xml" "content.xml")))
				;; org-odt-write-styles-file
				;; org-odt-write-meta-file
				org-odt-write-mimetype-file
				org-odt-write-manifest-file
				;; org-odt-prettify-xml-files-maybe
				org-odt-zip
				org-odt-cleanup-xml-buffers)
			      :initial-value (or (org-create-math-formula latex-frag)
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
		   (concat "." (if body-only "xml" (symbol-name backend))) subtreep)))
    (if (not (file-writable-p outfile)) (error "Output file not writable")
      (let ((ext-plist (org-combine-plists `(:output-file ,outfile) ext-plist)))
	(if async
            (org-export-async-start
		`(lambda (outfile)
		   (org-export-add-to-stack (expand-file-name outfile) ',backend))
	      `(let ((output
		      (org-export-as
		       ',backend ,subtreep ,visible-only ,body-only
		       ',ext-plist)))
		 (if (not body-only)
		     output
		   (with-temp-buffer
		     (insert output)
		     (let ((coding-system-for-write 'utf-8))
		       (write-file outfile)))
		   (when (and (org-export--copy-to-kill-ring-p) (org-string-nw-p output))
		     (org-kill-new output))
		   outfile)
		 ,outfile))
          (let ((output (org-export-as
			 backend subtreep visible-only body-only ext-plist)))
	    (if (not body-only)
		output
	      (with-temp-buffer
		(insert output)
		(let ((coding-system-for-write 'utf-8))
		  (write-file outfile)))
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
	(when (re-search-forward "<!-- begin -->" nil t)
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
			   `((?i . ,(shell-quote-argument in-file))
			     (?I . ,(browse-url-file-url in-file))
			     (?f . ,out-fmt)
			     (?o . ,out-file)
			     (?O . ,(browse-url-file-url out-file))
			     (?d . , (shell-quote-argument out-dir))
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
	(message "Opening %s..."  out-file)
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

;;; Library Initializations

(cl-loop for (extn . rest) in org-odt-file-extensions-alist do
	 ;; Let Emacs open all OpenDocument files in archive mode
	 (add-to-list 'auto-mode-alist
		      (cons (concat  "\\." extn "\\'") 'archive-mode)))

(provide 'ox-odt)

;;; ox-odt.el ends here
