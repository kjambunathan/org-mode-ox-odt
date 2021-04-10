;;; oc.el --- Org Cite library                  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>

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

;; This library provides tooling to handle citations in Org, e.g,
;; follow, fontify, and export them, respectively called "follow",
;; "activate" and "export" capabilities.  Libraries responsible for
;; providing some, or all, of these capabilities are called "citation
;; processors".

;; Such processors are defined using `org-cite-register-processor'.
;; Using this function, it is possible, in addition to giving it
;; a name, to attach functions associated to capabilities.  As such,
;; a processor handling citation export must set the
;; `:export-citation' property to an appropriate function.  Likewise,
;; "activate" capability require an appropriate `:activate' property,
;; and, unsurprisingly, "follow" capability implies `:follow'
;; property.

;; As a user, the first thing to do is setting a bibliography, either
;; globally with `org-cite-global-bibliography', or locally using one
;; ore more "bibliography" keywords.  Then one can select any
;; registered processor for each capability by providing a processor
;; name to the variables `org-cite-activate-processor' and
;; `org-cite-follow-processor'.

;; The "export" capability is slightly more involved as one need to
;; select the processor providing it, but may also provide a default
;; style for citations and bibliography.  These three parameters, or
;; triplet, can be set in `org-cite-export-processor' variable, or in
;; a document, through the "cite_export" keyword.

;; Eventually, this library provides some tools, mainly targeted at
;; processor implementors.  Most are export-specific and are located
;; in the "Tools only available during export" and "Tools operating on
;; parsed data" sections.

;; The few others can be used directly from an Org buffer, or operate
;; on processors.  See "Generic tools" section.

;;; Code:

(require 'org-compat)
(require 'org-macs)

(declare-function org-collect-keywords "org" (keywords &optional unique directory))

(declare-function org-element-adopt-elements "org-element" (parent &rest children))
(declare-function org-element-citation-parser "org-element" ())
(declare-function org-element-citation-reference-parser "org-element" ())
(declare-function org-element-contents "org-element" (element))
(declare-function org-element-create "org-element" (type &optional props &rest children))
(declare-function org-element-extract-element "org-element" (element))
(declare-function org-element-insert-before "org-element" (element location))
(declare-function org-element-lineage "org-element" (datum &optional types with-self))
(declare-function org-element-map "org-element" (data types fun &optional info first-match no-recursion with-affiliated))
(declare-function org-element-normalize-string "org-element" (s))
(declare-function org-element-parse-secondary-string "org-element" (string restriction &optional parent))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-put-property "org-element" (element property value))
(declare-function org-element-restriction "org-element" (element))
(declare-function org-element-set-element "org-element" (old new))
(declare-function org-element-type "org-element" (element))

(declare-function org-export-get-footnote-definition "org-export" (footnote-reference info))
(declare-function org-export-get-next-element "org-export" (blob info &optional n))
(declare-function org-export-get-previous-element "org-export" (blob info &optional n))
(declare-function org-export-raw-string "org-export" (contents))

(defvar org-element-citation-key-re)
(defvar org-element-citation-prefix-re)


;;; Configuration variables
(defgroup org-cite nil
  "Options concerning citations in Org mode."
  :group 'org
  :tag "Org Cite")

(defcustom org-cite-global-bibliography nil
  "List of bibliography files available in all documents.
File names must be absolute."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type '(choice (const :tag "No global bibliography" nil)
		 (repeat :tag "List of bibliography files"
                         (file :tag "Bibliography")))
  :safe t)

(defcustom org-cite-activate-processor nil
  "Processor used for activating citations, as a symbol."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type '(choice (const :tag "No fontification" nil)
                 (symbol :tag "Citation processor"))
  :safe nil)

(defcustom org-cite-export-processor nil
  "Processor used for exporting citations, as a triplet, or nil.

When nil, citations and bibliography are not exported.

When non-nil, an export processor is a triplet

  (NAME BIBLIOGRAPHY-STYLE CITATION-STYLE)

when NAME is the name of a registered citation processor providing export
functionality, as a symbol.  BIBLIOGRAPHY-STYLE (resp. CITATION-STYLE) is the
desired default style to use when printing a bibliography (resp. exporting a
citation), as a string or nil.

BIBLIOGRAPHY-STYLE is overridden by specifying a style along with the
\"print_bibliography\" keyword.  CITATION-STYLE is overridden by adding a style
to any citation object.  A nil style lets the export processor choose the
default output.  Any style not recognized by the export processor is equivalent
to nil.

The citation export processor can also be set with the CITE_EXPORT keyword.
E.g.,

  #+CITE_EXPORT: default bibliography-style citation-style

or

  #+CITE_EXPORT: default"
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type '(choice (const :tag "No export" nil)
                 (list :tag "Citation processor"
                       (symbol :tag "Processor name")
                       (choice
                        (const :tag "Default bibliography style" nil)
                        (string :tag "Use specific bibliography style"))
                       (choice
                        (const :tag "Default citation style" nil)
                        (string :tag "Use specific citation style"))))
  :safe nil)

(defcustom org-cite-follow-processor nil
  "Processor used for following citations, as a symbol."
  :group 'org-cite
  :package-version '(Org . "9.5")
  :type '(choice (const :tag "No following" nil)
                 (symbol :tag "Citation processor"))
  :safe nil)


;;; Citation processors
(cl-defstruct (org-cite-processor (:constructor org-cite--make-processor)
				  (:copier nil))
  (name nil :read-only t)
  (activate nil :read-only t)
  (export-bibliography nil :read-only t)
  (export-citation nil :read-only t)
  (export-finalizer nil :read-only t)
  (follow nil :read-only t))

(defvar org-cite--processors nil
  "List of registered citation processors.
See `org-cite-register-processor' for more information about
processors.")

(defun org-cite--get-processor (name)
  "Return citation processor named after symbol NAME.
Return nil if no such processor is found."
  (cl-find-if (lambda (p) (eq name (org-cite-processor-name p)))
	      org-cite--processors))

(defun org-cite-register-processor (name &rest body)
  "Mark citation processor NAME as available.

NAME is a symbol.  BODY is a property list, where the following optional keys
can be set:

  `:activate'

    Function activating a citation.  It is called with a single argument: a
    citation object extracted from the current buffer.  It may add text
    properties to the buffer.  If it is not provided, `org-cite-fontify-default'
    is used.

  `:export-bibliography'

    Function rendering a bibliography.  It is called with six arguments: the
    list of citation keys used in the document, as strings, a list of
    bibliography files, the style, as a string or nil, the local properties, as
    a property list, the export back-end, as a symbol, and the communication
    channel, as a property list.

    It is called at each \"print_bibliography\" keyword in the parse tree.
    It may return a string or nil.  When it returns nil, the keyword is ignored.
    Otherwise, the string it returns replaces the keyword in the export output.

  `:export-citation'    (mandatory for \"export\" capability)

    Function rendering citations.  It is called with four arguments: a citation
    object, the style, as a pair, the export back-end, as a symbol, and the
    communication channel, as a property list.

    It is called on each citation object in the parse tree.  It may return
    a string or nil.  When it returns nil, the citation is ignored.  Otherwise,
    the string it returns replaces the citation object in the export output.

  `:export-finalizer'

    Function called at the end of export process.  It must accept six arguments:
    the output, as a string, a list of citation keys used in the document, a
    list of bibliography files, the expected bibliography style, as a string or
    nil, the export back-end, as a symbol, and the communication channel, as a
    property list.

    It must return a string, which will become the final output from the export
    process, barring subsequent modifications from export filters.

  `:follow'

    Function called to follow a citation.  It accepts two arguments, the
    citation or citation reference object at point, and any prefix argument
    received during interactive call of `org-open-at-point'."
  (declare (indent 1))
  (unless (and name (symbolp name))
    (error "Invalid processor name: %S" name))
  (when (org-cite--get-processor name)
    (error "Processor %S already registered" name))
  (push (apply #'org-cite--make-processor :name name body)
	org-cite--processors)
  (message "Citation processor %S registered" name))

(defun org-cite-unregister-processor (name)
  "Unregister citation processor NAME.
NAME is a symbol.  Raise an error if processor is not registered."
  (unless (and name (symbolp name))
    (error "Invalid processor name: %S" name))
  (pcase (org-cite--get-processor name)
    (`nil (error "Processor %S not registered" name))
    (processor
     (setq org-cite--processors (delete processor org-cite--processors))))
  (message "Citation processor %S unregistered" name))

(defun org-cite-processor-has-capability-p (processor capability)
  "Return non-nil if PROCESSOR is able to handle CAPABILITY.
PROCESSOR is the name of a cite processor, as a symbol.  CAPABILITY is
`activate', `export', or `follow'."
  (let ((p (org-cite--get-processor processor)))
    (pcase capability
      ((guard (not p)) nil)             ;undefined processor
      (`activate (functionp (org-cite-processor-activate p)))
      (`export (functionp (org-cite-processor-export-citation p)))
      (`follow (functionp (org-cite-processor-follow p)))
      (other (error "Invalid capability: %S" other)))))


;;; Internal functions
(defun org-cite--set-previous-post-blank (datum blanks info)
  "Set `:post-blank' property from element or object before DATUM to BLANKS.

BLANKS is an integer.  DATUM is an element or object.  INFO is the export
communication channel, as a property list.

Previous element, if any, is modified by side-effect."
  (let ((previous (org-export-get-previous-element datum info)))
    (when previous
      (if (not (eq 'plain-text (org-element-type previous)))
          (org-element-put-property previous :post-blank blanks)
        ;; Remove any blank from string before DATUM so it is exported
        ;; with exactly BLANKS white spaces.
        (org-element-set-element
         previous
         (replace-regexp-in-string
          "[ \t\n]*\\'" (make-string blanks ?\s) previous))))))

(defun org-cite--insert-at-split (s citation n regexp)
  "Split string S and insert CITATION object between the two parts.
S is split at beginning of match group N upon matching REGEXP against it.
This function assumes S precedes CITATION."
  ;; When extracting the citation, remove white spaces before it, but
  ;; preserve those after it.
  (let ((post-blank (org-element-property :post-blank citation)))
    (when (and post-blank (> post-blank 0))
      (org-element-insert-before (make-string post-blank ?\s) citation)))
  (org-element-insert-before
   (org-element-put-property (org-element-extract-element citation)
                             :post-blank 0)
   s)
  (string-match regexp s)
  (let* ((split (match-beginning n))
         (first-part (substring s nil split))
         ;; Remove trailing white spaces as they are before the
         ;; citation.
         (last-part
          (replace-regexp-in-string (rx (1+ (any blank ?\n)) string-end)
                                    ""
                                    (substring s split))))
    (when (org-string-nw-p first-part)
      (org-element-insert-before first-part citation))
    (org-element-set-element s last-part)))

(defun org-cite--move-punct-before (punct citation s info)
  "Move punctuation PUNCT before CITATION object.
String S contains PUNCT.  The function assumes S follows CITATION.
Parse tree is modified by side-effect."
  (if (equal s punct)
      (org-element-extract-element s)   ;it would be empty anyway
    (org-element-set-element s (substring s (length punct))))
  ;; Remove blanks before citation.
  (org-cite--set-previous-post-blank citation 0 info)
  (org-element-insert-before
   ;; Blanks between citation and punct are now before punct and
   ;; citation.
   (concat (make-string (or (org-element-property :post-blank citation) 0) ?\s)
           punct)
   citation))

(defun org-cite--parse-as-plist (s)
  "Parse string S as a property list.
Values are always strings.  Return nil if S is nil."
  (cond
   ((null s) nil)
   ((stringp s)
    (with-temp-buffer
      (save-excursion (insert s))
      (skip-chars-forward " \t")
      (let ((results nil)
            (value-flag nil))
        (while (not (eobp))
          (pcase (char-after)
            (?:
             (push (read (current-buffer)) results)
             (setq value-flag t))
            ((guard (not value-flag))
             (skip-chars-forward "^ \t"))
            (?\"
             (let ((origin (point)))
               (condition-case _
                   (progn
                     (read (current-buffer))
                     (push (buffer-substring (1+ origin) (1- (point))) results))
                 (end-of-file
                  (goto-char origin)
                  (skip-chars-forward "^ \t")
                  (push (buffer-substring origin (point)) results)))
               (setq value-flag nil)))
            (_
             (let ((origin (point)))
               (skip-chars-forward "^ \t")
               (push (buffer-substring origin (point)) results)
               (setq value-flag nil))))
          (skip-chars-forward " \t"))
        (nreverse results))))
   (t (error "Invalid argument type: %S" s))))


;;; Generic tools
(defun org-cite-inside-footnote-p (citation)
  "Return the closest footnote containing CITATION object.
Return nil if there is no footnote containing CITATION."
  (org-element-lineage citation '(footnote-definition footnote-reference)))

(defun org-cite-list-bibliography-files ()
  "List all bibliography files defined in the buffer."
  (delete-dups
   (append (mapcar (lambda (value)
		     (pcase value
		       (`(,f . ,d)
                        (expand-file-name (org-strip-quotes f) d))))
		   (pcase (org-collect-keywords
                           '("BIBLIOGRAPHY") nil '("BIBLIOGRAPHY"))
		     (`(("BIBLIOGRAPHY" . ,pairs)) pairs)))
	   org-cite-global-bibliography)))

(defun org-cite-get-references (citation)
  "Return citations references contained in CITATION object.
Assume CITATION object comes from either a full parse tree, e.g., during export,
or from the current buffer."
  (or (org-element-contents citation)
      (org-with-point-at (org-element-property :contents-begin citation)
        (narrow-to-region (point) (org-element-property :contents-end citation))
        (let ((references nil))
          (while (not (eobp))
            (let ((reference (org-element-citation-reference-parser)))
              (goto-char (org-element-property :end reference))
              (push reference references)))
          (nreverse references)))))

(defun org-cite-key-boundaries (reference)
  "Return citation REFERENCE's key boundaries as buffer positions.
The function returns a pair (START . END) where START and END denote positions
in the current buffer.  Positions include leading \"@\" character."
  (org-with-point-at (org-element-property :begin reference)
    (let ((end (org-element-property :end reference)))
      (re-search-forward org-element-citation-key-re end t)
      (cons (match-beginning 0) (match-end 0)))))


;;; Tools only available during export
(defun org-cite-citation-style (citation info)
  "Return citation style used for CITATION object.

Style is a pair (NAME . VARIANT) where NAME and VARIANT are strings or nil.
A nil NAME means the default style for the current processor should be used.

INFO is a plist used as a communication channel."
  (let* ((separate
          (lambda (s)
            (cond
             ((null s) (cons nil nil))
             ((not (string-match "/" s)) (cons s nil))
             (t (cons (substring s nil (match-beginning 0))
                      (org-string-nw-p (substring s (match-end 0))))))))
         (local (funcall separate (org-element-property :style citation)))
         (global
          (funcall separate (pcase (plist-get info :cite-export)
                              (`(,_ ,_ ,style) style)
                              (_ nil)))))
    (cond
     ((org-string-nw-p (car local))
      (cons (org-not-nil (car local)) (cdr local)))
     (t
      (cons (org-not-nil (car global))
            (or (cdr local) (cdr global)))))))

(defun org-cite-bibliography-style (info)
  "Return expected bibliography style.
INFO is a plist used as a communication channel."
  (pcase (plist-get info :cite-export)
    (`(,_ ,style ,_) style)
    (_ nil)))

(defun org-cite-bibliography-properties (keyword)
  "Return properties associated to \"print_bibliography\" KEYWORD object.
Return value is a property list."
  (org-cite--parse-as-plist (org-element-property :value keyword)))

(defun org-cite-list-citations (info)
  "List citations in the exported document.
Citations are ordered by appearance in the document, when following footnotes.
INFO is the export communication channel, as a property list."
  (or (plist-get info :citations)
      (letrec ((cites nil)
               (search-cites
                (lambda (data)
                  (org-element-map data '(citation footnote-reference)
                    (lambda (datum)
                      (pcase (org-element-type datum)
                        (`citation (push datum cites))
		        ;; Do not force entering inline definitions, since
		        ;; `org-element-map' is going to enter it anyway.
                        ((guard (eq 'inline (org-element-property :type datum))))
                        (_
                         (funcall search-cites
                                  (org-export-get-footnote-definition datum info)))))
                    info nil 'footnote-definition t))))
        (funcall search-cites (plist-get info :parse-tree))
        (let ((result (nreverse cites)))
          (plist-put info :citations result)
          result))))

(defun org-cite-list-keys (info)
  "List citation keys in the exported document.
Keys are ordered by first appearance in the document, when following footnotes.
Duplicate keys are removed.  INFO is the export communication channel, as a
property list."
  (delete-dups
   (org-element-map (org-cite-list-citations info) 'citation-reference
     (lambda (r) (org-element-property :key r))
     info)))

(defun org-cite-key-number (key info &optional predicate)
  "Return number associated to string KEY.

INFO is the export communication channel, as a property list.

Optional argument PREDICATE is called with two keys, and returns non-nil
if the first reference should sort before the second.  When nil, references
are sorted in order cited."
  (let* ((keys (org-cite-list-keys info))
         (sorted-keys (if (functionp predicate)
                          (sort keys predicate)
                        keys))
         (position (cl-position key sorted-keys :test #'string-equal)))
    (and (integerp position)
         (1+ position))))

(defun org-cite-wrap-citation (citation info)
  "Wrap an anonymous inline footnote around CITATION object in the parse tree.

INFO is the export state, as a property list.

White space before the citation, if any, are removed.  The parse tree is
modified by side-effect."
  (let ((footnote
         (list 'footnote-reference
               (list :label nil
                     :type 'inline
                     :contents-begin (org-element-property :begin citation)
                     :contents-end (org-element-property :end citation)
                     :post-blank (org-element-property :post-blank citation)))))
    ;; Remove any white space before citation.
    (org-cite--set-previous-post-blank citation 0 info)
    ;; Footnote swallows citation.
    (org-element-insert-before footnote citation)
    (org-element-adopt-elements footnote
      (org-element-extract-element citation))))

(defun org-cite-adjust-punctuation (citation rule info &optional punct add-space)
  "Adjust punctuation around CITATION object.

When CITATION follows a quotation, or when there is punctuation next to it,
the function tries to normalize the location of punctuation and citation
according to some RULE.

RULE is a triplet of symbols (PUNCTUATION CITE ORDER):

  PUNCTUATION is the desired location of the punctuation with regards to the
  quotation, if any.  It may be `inside', `outside', or`static'.  When set to
  `static', the punctuation is not moved.

  CITE is the desired location of the citation with regards to the quotation
  mark, if any.  It may be `inside', `outside', or `same'.  When set to `same',
  the citation is moved on the same side as the punctuation, but does not move
  if there is punctuation on both sides or on none.

  ORDER is the relative position of the citation with regards to the closest
  punctuation.  It may be `after' or `before'.

For example, when changing from author-date to note style,

  (inside outside after) corresponds to American typography;
  (static outside after) corresponds to German typography;
  (static same before) corresponds to French typography.

INFO is the export state, as a property list.

Optional argument PUNCT is a list of punctuation marks to be considered.
When nil, it includes the following: \".\" \",\" \";\" \":\" \"!\" and \"?\".

When optional argument ADD-SPACE is non-nil, add a space before citation.  This
is useful, for example, when switching from note to author-year style.

Parse tree is modified by side-effect.

Note: if you are calling both `org-cite-adjust-punctuation' and
`org-cite-wrap-citation' on the same object, call `org-cite-adjust-punctuation'
first."
  (pcase-let* ((punct-re (regexp-opt (or punct '("." "," ";" ":" "!" "?"))))
               (previous-punct-re
                (rx (opt (group (0+ (any blank ?\n)) (regexp punct-re)))
                    (opt (0+ (any blank ?\n)) (group ?\"))
                    (0+ (any blank ?\n))
                    string-end))
               (next-punct-re
                (rx string-start
                    (group (0+ (any blank ?\n)) (regexp punct-re))))
               (next (org-export-get-next-element citation info))
               (final-punct
                (and (stringp next)
                     (string-match next-punct-re next)
                     (match-string 1 next)))
               (previous
                ;; Find the closest terminal object.  Consider
                ;; citation, subscript and superscript objects as
                ;; terminal.
                (org-last
                 (org-element-map (org-export-get-previous-element citation info)
                     '(citation code entity export-snippet footnote-reference
                                line-break latex-fragment link plain-text
                                radio-target statistics-cookie timestamp
                                verbatim)
                   #'identity info nil '(citation subscript superscript))))
               (`(,punct . ,quote)
                (and (stringp previous)
                     (string-match previous-punct-re previous)
                     (cons (match-string 1 previous)
                           (match-string 2 previous)))))
    ;; Bail you when there is no quote and either no punctuation, or
    ;; punctuation on both sides.
    (when (or quote (org-xor punct final-punct))
      ;; Phase 1: handle punctuation rule.
      (pcase rule
        ((guard (not quote)) nil)
        ;; Static punctuation does not move.
        (`(static . ,_) nil)
        ;; Move punctuation inside.
        (`(inside . ,_)
         ;; This only makes sense if there is a quotation before the
         ;; citation that does not end with some punctuation.
         (when (and (not punct) final-punct)
           ;; Quote guarantees there is a string object before
           ;; citation.  Likewise, any final punctuation guarantees
           ;; there is a string object following citation.
           (let ((new-prev
                  (replace-regexp-in-string
                   previous-punct-re
                   (concat final-punct "\"") previous nil nil 2))
                 (new-next
                  (replace-regexp-in-string
                   (rx string-start (literal final-punct))
                   "" next)))
             (org-element-set-element previous new-prev)
             (org-element-set-element next new-next)
             (setq previous new-prev)
             (setq next new-next)
             (setq punct final-punct)
             (setq final-punct nil))))
        ;; Move punctuation outside.
        (`(outside . ,_)
         ;; This is only meaningful if there is some inner punctuation
         ;; and no final punctuation already.
         (when (and punct (not final-punct))
           ;; Inner punctuation guarantees there is text object before
           ;; the citation.  However, there is no information about
           ;; the object following citation, if any.  Therefore, we
           ;; handle all the possible cases (string, other type, or
           ;; none).
           (let ((new-prev
                  (replace-regexp-in-string
                   previous-punct-re "" previous nil nil 1))
                 (new-next (if (stringp next) (concat punct next) punct)))
             (org-element-set-element previous new-prev)
             (cond
              ((stringp next)
               (org-element-set-element next new-next))
              (next
               (org-element-insert-before new-next next))
              (t
               (org-element-adopt-elements
                   (org-element-property :parent citation)
                 new-next)))
             (setq previous new-prev)
             (setq next new-next)
             (setq final-punct punct)
             (setq punct nil))))
        (_
         (error "Invalid punctuation rule: %S" rule))))
    ;; Phase 2: move citation to its appropriate location.
    ;;
    ;; First transform relative citation location into a definitive
    ;; location, according to the surrounding punctuation.
    (pcase rule
      (`(,punctuation same ,order)
       (setf rule
             (list punctuation
                   (cond
                    ;; When there is punctuation on both sides, the
                    ;; citation is necessarily on the outside.
                    ((and punct final-punct) 'outside)
                    (punct 'inside)
                    (final-punct 'outside)
                    ;; No punctuation: bail out on next step.
                    (t nil))
                   order))))
    (pcase rule
      (`(,_ nil ,_) nil)
      (`(,_ inside after)
       ;; Citation has to be moved after punct, if there is
       ;; a quotation mark, or after final punctuation.
       (cond
        (quote
         (org-cite--insert-at-split previous citation 2 previous-punct-re))
        (final-punct
         (org-cite--move-punct-before final-punct citation next info))
        ;; There is only punct, and we're already after it.
        (t nil)))
      (`(,_ inside before)
       ;; Citation is already behind final-punct, so only consider
       ;; other locations.
       (when (or punct quote)
         (org-cite--insert-at-split previous citation 0 previous-punct-re)))
      (`(,_ outside after)
       ;; Citation is already after any punct or quote.  It can only
       ;; move past final punctuation, if there is one.
       (when final-punct
         (org-cite--move-punct-before final-punct citation next info)))
      (`(,_ outside before)
       ;; The only non-trivial case is when citation follows punct
       ;; without a quote.
       (when (and punct (not quote))
         (org-cite--insert-at-split previous citation 0 previous-punct-re)))
      (_
       (error "Invalid punctuation rule: %S" rule))))
  (when add-space (org-cite--set-previous-post-blank citation 1 info)))


;;; Tools operating on parsed data
(defun org-cite-make-paragraph (&rest data)
  "Return a paragraph element containing DATA.
DATA are strings, objects or secondary strings."
  (apply #'org-element-create 'paragraph nil (apply #'org-cite-concat data)))

(defun org-cite-emphasize (type &rest data)
  "Apply emphasis TYPE on DATA.
TYPE is a symbol among `bold', `italic', `strike-through' and `underline'.
DATA are strings, objects or secondary strings.  Return an object of type TYPE."
  (declare (indent 1))
  (unless (memq type '(bold italic strike-through underline))
    (error "Wrong emphasis type: %S" type))
  (apply #'org-element-create type nil (apply #'org-cite-concat data)))

(defun org-cite-concat (&rest data)
  "Concatenate all the DATA arguments and make the result a secondary string.
Each argument may be a string, an object, or a secondary string."
  (let ((results nil))
    (dolist (datum (reverse data))
      (pcase datum
        (`nil nil)
        ;; Element or object.
        ((pred org-element-type) (push datum results))
        ;; Secondary string.
        ((pred consp) (setq results (append datum results)))
        (_
         (signal
          'wrong-type-argument
          (list (format "Argument is not a string or a secondary string: %S"
                        datum))))))
    results))

(defun org-cite-mapconcat (function data separator)
  "Apply FUNCTION to each element of DATA, and return a secondary string.

In between each pair of results, stick SEPARATOR, which may be a string,
an object, or a secondary string.  FUNCTION must be a function of one argument,
and must return either a string, an object, or a secondary string."
  (and data
       (let ((result (list (funcall function (car data)))))
         (dolist (datum (cdr data))
           (setq result
                 (org-cite-concat result separator (funcall function datum))))
         result)))


;;; Internal interface with fontification (activate capability)
(defun org-cite-fontify-default (datum)
  "Fontify DATUM with `org-cite' and `org-cite-key' face.
DATUM is a citation object, or a citation reference.  In any case, apply
`org-cite' face on the whole citation, and `org-cite-key' face on each key."
  (let* ((cite (if (eq 'citation-reference (org-element-type datum))
                   (org-element-property :parent datum)
                 datum))
         (beg (org-element-property :begin cite))
         (end (org-with-point-at (org-element-property :end cite)
                (skip-chars-backward " \t")
                (point))))
    (add-text-properties beg end '(font-lock-multiline t))
    (add-face-text-property beg end 'org-cite)
    (dolist (reference (org-cite-get-references cite))
      (let ((boundaries (org-cite-key-boundaries reference)))
        (add-face-text-property (car boundaries) (cdr boundaries)
                                'org-cite-key)))))

(defun org-cite-activate (limit)
  "Activate citations from up to LIMIT buffer position.
Each citation encountered is activated using the appropriate function
from the processor set in `org-cite-activate-processor'."
  (let ((name org-cite-activate-processor))
    (let ((activate
           (or (and name
                    (org-cite-processor-has-capability-p name 'activate)
                    (org-cite-processor-activate (org-cite--get-processor name)))
               #'org-cite-fontify-default)))
      (while (re-search-forward org-element-citation-prefix-re limit t)
        (let ((cite (org-with-point-at (match-beginning 0)
                      (org-element-citation-parser))))
          (when cite (save-excursion (funcall activate cite))))))))


;;; Internal interface with Org Export library (export capability)
(defun org-cite-store-bibliography (info)
  "Store bibliography in the communication channel.

Bibliography is stored as a list of absolute file names in the `:bibliography'
property.

INFO is the communication channel, as a plist.  It is modified by side-effect."
  (plist-put info :bibliography (org-cite-list-bibliography-files)))

(defun org-cite-store-export-processor (info)
  "Store export processor in the `:cite-export' property during export.

Export processor is stored as a triplet, or nil.

When non-nil, it is defined as (NAME BIBLIOGRAPHY-STYLE CITATION-STYLE) where
NAME is a symbol, whereas BIBLIOGRAPHY-STYLE and CITATION-STYLE are strings,
or nil.

INFO is the communication channel, as a plist.  It is modified by side-effect."
  (let ((processor
         (pcase (plist-get info :cite-export)
           ((or "" `nil) nil)
           ((and (pred stringp) s)
            (let ((triplet (org-split-string s)))
              (when (< 3 (length triplet))
                (user-error "Invalid processor triplet: %S" s))
              (list (intern (car triplet))
                    (org-not-nil (nth 1 triplet))
                    (org-not-nil (nth 2 triplet)))))
           ((and `(,(pred symbolp)
                   ,(or (pred stringp) `nil)
                   ,(or (pred stringp) `nil))
                 p)
            p)
           (other
            (error "Invalid cite export processor definition: %S" other)))))
    (when processor
      (let ((name (car processor)))
        (cond
         ((not (org-cite--get-processor name))
          (user-error "Unknown processor %S" name))
         ((not (org-cite-processor-has-capability-p  name 'export))
          (user-error "Processor %S is unable to handle citation export" name)))))
    (plist-put info :cite-export processor)))

(defun org-cite-export-citation (citation _ info)
  "Export CITATION object according to INFO property list.
This function delegates the export of the current citation to the
selected citation processor."
  (pcase (plist-get info :cite-export)
    (`nil nil)
    (`(,p ,_ ,_)
     (funcall (org-cite-processor-export-citation (org-cite--get-processor p))
	      citation
              (org-cite-citation-style citation info)
              (plist-get info :back-end)
              info))
    (other (error "Invalid `:cite-export' value: %S" other))))

(defun org-cite-export-bibliography (keyword _ info)
  "Return bibliography associated to \"print_bibliography\" KEYWORD.
BACKEND is the export back-end, as a symbol.  INFO is a plist
used as a communication channel."
  (pcase (plist-get info :cite-export)
    (`nil nil)
    (`(,p ,_ ,_)
     (let ((export-bibilography
            (org-cite-processor-export-bibliography
             (org-cite--get-processor p))))
       (when export-bibilography
         (funcall export-bibilography
	          (org-cite-list-keys info)
                  (plist-get info :bibliography)
                  (org-cite-bibliography-style info)
                  (org-cite-bibliography-properties keyword)
                  (plist-get info :back-end)
                  info))))
    (other (error "Invalid `:cite-export' value: %S" other))))

(defun org-cite-process-citations (info)
  "Replace all citations in the parse tree.
INFO is the communication channel, as a plist.  Parse tree is modified
by side-effect."
  (dolist (cite (org-cite-list-citations info))
    (let ((replacement (org-cite-export-citation cite nil info))
          (blanks (or (org-element-property :post-blank cite) 0)))
      (pcase replacement
        ((pred stringp)
         ;; Handle `:post-blank' before replacing value.
         (let ((output (concat (org-trim replacement)
                               (make-string blanks ?\s))))
           ;; Don't use `org-element-set-element' here, because this
           ;; function modifies objects by side-effect.  However, we
           ;; still want to preserve citations located in `:citations'
           ;; property.
           (org-element-insert-before (org-export-raw-string output) cite)))
        (`nil
         ;; Before removing the citation, transfer its :post-blank
         ;; property to the object before, if any.
         (org-cite--set-previous-post-blank cite blanks info))
        (_
         (error "Invalid return value from citation export processor: %S"
                replacement)))
      (org-element-extract-element cite))))

(defun org-cite-process-bibliography (info)
  "Replace all \"print_bibliography\" keywords in the parse tree.

INFO is the communication channel, as a plist.  Parse tree is modified
by side effect."
  (org-element-map (plist-get info :parse-tree) 'keyword
    (lambda (keyword)
      (when (equal "PRINT_BIBLIOGRAPHY" (org-element-property :key keyword))
        (let ((replacement (org-cite-export-bibliography keyword nil info))
              (blanks (or (org-element-property :post-blank keyword) 0)))
          (pcase replacement
            ((pred stringp)
             ;; Handle `:post-blank' before replacing value.
             (let ((output (concat (org-element-normalize-string replacement)
                                   (make-string blanks ?\n))))
               (org-element-set-element keyword (org-export-raw-string output))))
            (`nil
             ;; Before removing the citation, transfer its
             ;; `:post-blank' property to the element before, if
             ;; any.
             (org-cite--set-previous-post-blank keyword blanks info)
             (org-element-extract-element keyword))
            (_
             (error "Invalid return value from citation export processor: %S"
                    replacement))))))
    info))

(defun org-cite-finalize-export (output info)
  "Finalizer for export process.
OUTPUT is the full output of the export process.  INFO is the communication
channel, as a property list."
  (pcase (plist-get info :cite-export)
    (`nil output)
    (`(,p ,_ ,_)
     (let ((finalizer
            (org-cite-processor-export-finalizer (org-cite--get-processor p))))
       (if (not finalizer)
           output
         (funcall finalizer
                  output
                  (org-cite-list-keys info)
                  (plist-get info :bibliography)
                  (org-cite-bibliography-style info)
                  (plist-get info :back-end)
                  info))))
    (other (error "Invalid `:cite-export' value: %S" other))))


;;; Internal interface with `org-open-at-point' (follow capability)
(defun org-cite-follow (datum arg)
  "Follow citation or citation-reference DATUM.
Following is done according to the processor set in `org-cite-follow-processor'.
ARG is the prefix argument received when calling `org-open-at-point', or nil."
  (let ((name org-cite-follow-processor))
    (cond
     ((null name)
      (user-error "No processor set to follow citations"))
     ((not (org-cite--get-processor name))
      (user-error "Unknown processor %S" name))
     ((not (org-cite-processor-has-capability-p name 'follow))
      (user-error "Processor %S cannot follow citations" name))
     (t
      (let ((follow (org-cite-processor-follow (org-cite--get-processor name))))
        (funcall follow datum arg))))))

(provide 'org-cite)
(provide 'oc)
;;; oc.el ends here
