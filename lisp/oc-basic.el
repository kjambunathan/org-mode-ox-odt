;;; oc-basic.el --- Default back-end for citations  -*- lexical-binding: t; -*-

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

;; The `basic' citation processor provides "activate", "follow" and
;; "export" capabilities.

;; More specifically, "activate" capability re-use default
;; fontification, but highlights with `error' face any unknown
;; citation key according to the bibliography defined in the document.

;; On a citation key, "follow" capability moves point to the
;; corresponding entry in the current bibliography.  Elsewhere on the
;; citation, it asks the user to follow any of the keys cited there,
;; with completion.

;; "export" capability supports the following styles for citations:
;;   - author
;;   - nocite
;;   - note
;;   - numeric
;;   - text
;;   - year
;;   - author-year (default)
;; It also supports the following styles for bibliography:
;;   - plain
;;   - numeric
;;   - author-year (default)

;; It assumes bibliography files are in BibTex format.

;; Disclaimer: this citation processor is meant to be a proof of
;; concept, and possibly a fall-back mechanism when nothing else is
;; available.  It is too limited for any serious use case.

;; With that out of the way, if you still want to use this processor,
;; you may set `org-cite-activate-processor',
;; `org-cite-follow-processor', or `org-cite-export-processor' to
;; `basic'.

;;; Code:

(require 'bibtex)
(require 'oc)

(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))

(declare-function org-export-data "org-export" (data info))
(declare-function org-export-derived-backend-p "org-export" (backend &rest backends))
(declare-function org-export-raw-string "org-export" (contents))


;;; Constants
(defconst org-cite-basic-sorting-field "author"
  "Field used to sort bibliography items, or nil.")

(defconst org-cite-basic-author-year-separator ", "
  "Separator string used to separate cites in an author-year configuration.")


;;; Internal variables
(defvar org-cite-basic--cache nil
  "Cache for parsed BiBTeX files.
This is an association list following the pattern:

  (KEY . ENTRIES)

where KEY is a cons cell (FILE . HASH), with FILE being the
absolute file name of the bibliography file, and HASH a hash of
its contents.")


;;; Internal functions
(defun org-cite-basic--parse-bibtex-file (file)
  "Parse BibTex FILE.
Store parsed entries in `org-cite-basic--cache', which see."
  (let* ((buffer (find-file-noselect file t))
         (key (cons file (buffer-hash buffer))))
    (or (cdr (assoc key org-cite-basic--cache))
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (let ((results nil)
                  (bibtex-sort-ignore-string-entries t))
              (bibtex-map-entries
               (lambda (&rest _)
                 ;; Normalize entries: field names are downcased, and
                 ;; consecutive white spaces are removed from values.
                 (push (mapcar (pcase-lambda (`(,field . ,value))
                                 (cons (downcase field)
                                       (replace-regexp-in-string "[ \t\n]+" " " value)))
                               (bibtex-parse-entry t))
                       results)))
              (push (cons key results)
                    org-cite-basic--cache)
              results))))))

(defun org-cite-basic--file-keys (bibliography)
  "List associations between bibliography files and citation keys
BIBLIOGRAPHY is the bibliography used for the current document,
as returned by `org-cite-basic--parse-bibliography'."
  (mapcar (lambda (pair)
            (pcase pair
              (`(,file . ,entries)
               (cons file
                     (mapcar (lambda (entry) (cdr (assoc "=key=" entry)))
                             entries)))))
          bibliography))

(defun org-cite-basic--parse-bibliography (&optional info)
  "List all entries available in the buffer.

Each association follows the pattern

  (FILE ((FIELD . TEXT) ...) ...)

where FILE is the absolute file name of the BibTeX file, FIELD
are field values associated to entries, are can also be the
special strings \"=type=\" and \"=key=\".  TEXT is a string or
nil."
  (if (plist-member info :cite-default/bibliography)
      (plist-get info :cite-default/bibliography)
    (let ((results nil))
      (dolist (file (org-cite-list-bibliography-files))
        (when (file-readable-p file)
	  (push (cons file (org-cite-basic--parse-bibtex-file file))
	        results)))
      (when info (plist-put info :cite-default/bibliography results))
      results)))

(defun org-cite-basic--format-author-year (citation format-cite format-ref info)
  "Format CITATION object according to author-year format.

FORMAT-CITE is a function of three arguments: the global prefix, the
contents, and the global suffix.  All arguments can be strings or secondary
strings.

FORMAT-REF is a function of four arguments: the reference prefix, as a string or
secondary string, the author, the year, and the reference suffix, as a string or
secondary string.

INFO is the export state, as a property list."
  (org-export-data
   (funcall format-cite
            (org-element-property :prefix citation)
            (org-cite-mapconcat
             (lambda (ref)
               (let ((k (org-element-property :key ref))
                     (prefix (org-element-property :prefix ref))
                     (suffix (let ((s (org-element-property :suffix ref)))
                               (and s (org-cite-concat "," s)))))
                 (funcall format-ref
                          prefix
                          (org-cite-basic--get-field "author" k info)
                          (org-cite-basic--get-field "year" k info)
                          suffix)))
             (org-cite-get-references citation)
             org-cite-basic-author-year-separator)
            (org-element-property :suffix citation))
   info))

(defun org-cite-basic--field-less-p (field info)
  "Return a sort predicate comparing FIELD values for two citation keys.
INFO is the export state, as a property list."
  (and field
       (lambda (a b)
         (org-string-collate-lessp
          (org-cite-basic--get-field field a info 'raw)
          (org-cite-basic--get-field field b info 'raw)
          nil t))))

(defun org-cite-basic--key-number (key info)
  "Return number associated to cited KEY.
INFO is the export state, as a property list."
  (let ((predicate
         (org-cite-basic--field-less-p org-cite-basic-sorting-field info)))
    (org-cite-key-number key info predicate)))

(defun org-cite-basic--citation-numbers (citation info)
  "Return numbers associated to references in CITATION object.
INFO is the export state as a property list."
  (let* ((numbers
          (sort (mapcar
                 (lambda (r)
                   (org-cite-basic--key-number
                    (org-element-property :key r) info))
                 (org-cite-get-references citation))
                #'<))
         (last (car numbers))
         (result (list (number-to-string (pop numbers)))))
    ;; Use compact number references, i.e., "1, 2, 3" becomes "1-3".
    (while numbers
      (let ((current (pop numbers))
            (next (car numbers)))
        (cond
         ((and next
               (= current (1+ last))
               (= current (1- next)))
          (unless (equal "-" (car result))
            (push "-" result)))
         ((equal "-" (car result))
          (push (number-to-string current) result))
         (t
          (push (format ", %d" current) result)))
        (setq last current)))
    (apply #'concat (nreverse result))))

(defun org-cite-basic--all-keys ()
  "List all keys available in current bibliography."
  (let ((bibliography (org-cite-basic--parse-bibliography)))
    (apply #'append
           (mapcar #'cdr (org-cite-basic--file-keys bibliography)))))

(defun org-cite-basic--sort-keys (keys info)
  "Sort KEYS by author name.
INFO is the export communication channel, as a property list."
  (let ((predicate (org-cite-basic--field-less-p org-cite-basic-sorting-field info)))
    (when predicate (sort keys predicate))))

(defun org-cite-basic--get-entry (key info)
  "Return BibTeX entry for KEY, as an association list.
INFO is the export state, as a property list."
  (catch :found
    (pcase-dolist (`(,_ . ,entries) (org-cite-basic--parse-bibliography info))
      (dolist (entry entries)
        (let ((k (cdr (assoc "=key=" entry))))
          (when (equal key k)
            (throw :found entry)))))
    nil))

(defun org-cite-basic--get-field (field entry-or-key info &optional raw)
  "Return FIELD value for ENTRY-OR-KEY, or nil.

FIELD is a string.  ENTRY-OR-KEY is either an association list, as returned by
`org-cite-basic--get-entry', or a string representing a citation key.
INFO is the export state, as a property list.

Return value may be nil or a string.  If current export back-end is derived
from `latex', return a raw string instead, unless optional argument RAW is
non-nil."
  (let ((value
         (cdr (assoc
               (downcase field)
               (pcase entry-or-key
                 ((pred stringp)
                  (org-cite-basic--get-entry entry-or-key info))
                 ((pred consp)
                  entry-or-key)
                 (_
                  (error "Wrong value for ENTRY-OR-KEY: %S" entry-or-key)))))))
    (if (and value
             (not raw)
             (org-export-derived-backend-p (plist-get info :back-end) 'latex))
        (org-export-raw-string value)
      value)))

(defun org-cite-basic--print-entry (entry style info)
  "Format ENTRY according to STYLE string.
ENTRY is an alist, as returned by `org-cite-basic--get-entry'.  INFO is the
export state, as a property list."
  (let ((author (org-cite-basic--get-field "author" entry info))
        (title (org-cite-emphasize 'italic
                 (org-cite-basic--get-field "title" entry info)))
        (year (cdr (assoc "year" entry)))
        (from
         (or (org-cite-basic--get-field "publisher" entry info)
             (org-cite-basic--get-field "journal" entry info)
             (org-cite-basic--get-field "institution" entry info)
             (org-cite-basic--get-field "school" entry info))))
    (pcase style
      ("plain"
       (org-cite-concat
        author ". " title (and from (list ", " from)) ", " year "."))
      ("numeric"
       (let ((n (org-cite-basic--key-number (cdr (assoc "=key=" entry)) info)))
         (org-cite-concat
          (format "[%d] " n) author ", "
          title
          (and from (list ", " from)) ", "
          year ".")))
      ;; Default to author-year
      (_
       (org-cite-concat
        author " (" year "). " title (and from (list ", " from)) ".")))))


;;; "Activate" capability
(defun org-cite-basic-activate (citation)
  "Fontify CITATION object.
Fontify whole citation with `org-cite' face.  Fontify key with `error' face if
when it does not belong to known keys.  Otherwise, use `org-cite-key' face."
  (let ((beg (org-element-property :begin citation))
        (end (org-with-point-at (org-element-property :end citation)
               (skip-chars-backward " \t")
               (point)))
        (keys (org-cite-basic--all-keys)))
    (add-text-properties beg end '(font-lock-multiline t))
    (add-face-text-property beg end 'org-cite)
    (dolist (reference (org-cite-get-references citation))
      (let ((boundaries (org-cite-key-boundaries reference)))
        (add-face-text-property
         (car boundaries) (cdr boundaries)
         (if (member (org-element-property :key reference) keys)
             'org-cite-key
           'error))))))


;;; "Export" capability
(defun org-cite-basic-export-citation (citation style _ info)
  "Export CITATION object.
STYLE is the expected citation style, as a pair of strings or nil.  INFO is the
export communication channel, as a property list."
  (pcase style
    (`("author" . ,_)
     (org-export-data
      (mapconcat
       (lambda (r)
         (let ((key (org-element-property :key r)))
           (org-cite-basic--get-field "author" key info)))
       (org-cite-get-references citation)
       org-cite-basic-author-year-separator)
      info))
    (`("nocite" . ,_) nil)
    (`("note" . ,_)
     (unless (org-cite-inside-footnote-p citation)
       (org-cite-wrap-citation citation info))
     ;; Use "text" style in footnote.
     (capitalize
      (org-cite-basic--format-author-year
       citation
       (lambda (p c s) (org-cite-concat p c s))
       (lambda (p a y s) (org-cite-concat p a " (" y s ")"))
       info)))
    (`("numeric" . ,_)
     ;; When using this style on citations with multiple references,
     ;; use global affixes and ignore local ones.
     (let* ((references (org-cite-get-references citation))
            (prefix (or (org-element-property :prefix citation)
                        (and (= 1 (length references))
                             (org-element-property :prefix (car references)))))
            (suffix (let ((s (or (org-element-property :suffix citation)
                                 (and (= 1 (length references))
                                      (org-element-property :suffix (car references))))))
                      (and s (org-cite-concat ", " s)))))
       (org-export-data
        (org-cite-concat
         "[" prefix (org-cite-basic--citation-numbers citation info) suffix "]")
        info)))
    (`("text" . ,_)
     (org-cite-basic--format-author-year
      citation
      (lambda (p c s) (org-cite-concat p c s))
      (lambda (p a y s) (org-cite-concat p a " (" y s ")"))
      info))
    (`("year" . ,_)
     (format "(%s)"
             (mapconcat
              (lambda (r)
                (org-cite-basic--get-field
                 "year" (org-element-property :key r) info))
              (org-cite-get-references citation)
              org-cite-basic-author-year-separator)))
    ;; Default format is author-year.
    (_
     (org-cite-basic--format-author-year
      citation
      (lambda (p c s) (org-cite-concat "(" p c s ")"))
      (lambda (p a y s) (org-cite-concat p a ", " y s))
      info))))

(defun org-cite-basic-export-bibliography (keys _files style _props backend info)
  "Generate bibliography.
KEYS is the list of cited keys, as strings.  STYLE is the expected bibliography
style, as a string.  BACKEND is the export back-end, as a symbol. INFO is the
export state, as a property list."
  (mapconcat
   (lambda (k)
     (let ((entry (org-cite-basic--get-entry k info)))
       (org-export-data
        (org-cite-make-paragraph
         (and (org-export-derived-backend-p backend 'latex)
              (org-export-raw-string "\\noindent\n"))
         (org-cite-basic--print-entry entry style info))
        info)))
   (org-cite-basic--sort-keys keys info)
   "\n"))


;;; "Follow" capability
(defun org-cite-basic-goto (datum _)
  "Follow citation or citation reference DATUM.
When DATUM is a citation reference, open bibliography entry referencing
the citation key.  Otherwise, select which key to follow among all keys
present in the citation."
  (let* ((key (if (eq 'citation-reference (org-element-type datum))
                  (org-element-property :key datum)
                (pcase (org-cite-get-references datum)
                  (`(,reference) (org-element-property :key reference))
                  (all
                   (or (completing-read "Select citation key: "
                                        (mapcar (lambda (r) (org-element-property :key r))
                                                all)
                                        nil t)
                       (user-error "Aborted"))))))
         (bibliography (org-cite-basic--parse-bibliography))
         (file (catch :found
                 (pcase-dolist (`(,file . ,keys)
                                (org-cite-basic--file-keys bibliography))
                   (when (member key keys)
                     (throw :found file))))))
    (unless file (user-error "Cannot find citation key: %S" key))
    (org-open-file file '(4))
    (bibtex-search-entry key)))


;;; Register processor
(org-cite-register-processor 'basic
  :activate #'org-cite-basic-activate
  :export-citation #'org-cite-basic-export-citation
  :export-bibliography #'org-cite-basic-export-bibliography
  :follow #'org-cite-basic-goto)

(provide 'org-cite-basic)
(provide 'oc-basic)
;;; oc-default.el ends here
