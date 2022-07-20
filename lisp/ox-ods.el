;;; ox-ods.el --- OpenDocument Spreadsheet Document for Org Mode -*- lexical-binding: t; coding: utf-8-emacs; -*-

;; Copyright (C) 2022 Jambunathan K <kjambunathan at gmail dot com>

;; Author: Jambunathan K <kjambunathan at gmail dot com>
;; Maintainer: Jambunathan K <kjambunathan at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: https://github.com/kjambunathan/org-mode-ox-odt

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

(require 'dash)
(require 'rx)
(require 'ox)

(defun org-ods-table-cell-element-to-lisp (table-cell)
  (substring-no-properties
   (org-element-interpret-data
    (org-element-contents table-cell))))

(defun org-ods-table-row-element-to-lisp (table-row)
  (org-element-map table-row 'table-cell
    #'org-ods-table-cell-element-to-lisp))

(defun org-ods-table-row-elements-to-lisp (table-rows)
  (mapcar #'org-ods-table-row-element-to-lisp table-rows))

(defun org-ods--do-on-list-of-list-of-table-cell-elements
    (list-of-list-of-table-cells f)
  (cl-loop for list-of-table-cells in list-of-list-of-table-cells
	   collect (mapcar f list-of-table-cells)))

(defun org-ods-list-of-list-of-table-cell-elements-to-lisp
    (list-of-list-of-table-cells)
  (org-ods--do-on-list-of-list-of-table-cell-elements list-of-list-of-table-cells
						      #'org-ods-table-cell-element-to-lisp))

(defun org-ods-table-element-to-lisp-table (table)
  (org-ods-table-row-elements-to-lisp
   (org-element-contents table)))

(defun org-ods-lisp-table-to-org-table (lisp-table)
  (with-temp-buffer
    (org-mode)
    (save-excursion
      (insert
       (mapconcat
	#'identity
	(cl-loop for row in lisp-table collect
		 (if (null row) "|-"
		   (apply #'concat
			  (cl-loop for cell in row collect
				   (format "| %s" (org-trim
						   (replace-regexp-in-string
						    "[\r\n]+" " " (substring-no-properties cell) t t)))))))
	"\n")))
    ;; (inspect)
    (org-table-align)
    ;; (inspect)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-ods-table-element-print (table)
  (org-ods-lisp-table-to-org-table
   (org-ods-table-element-to-lisp-table table)))

(defun org-ods-table-row-elements-print (table-rows)
  (org-ods-lisp-table-to-org-table
   (org-ods-table-row-elements-to-lisp table-rows)))

;; (org-ods-table-element-print t1)



;;; Hrules

(defun org-ods-table-row-is-rule-p (table-row)
  (eq (org-element-property :type table-row) 'rule))

(defun org-ods-get-table-rules (table)
  (->> (org-element-contents table)
       (-split-when #'org-ods-table-row-is-rule-p)))

;;; Special Rows

(defun org-ods-table-row-is-special-p (table-row)
  (org-export-table-row-is-special-p table-row nil))

(defun org-ods-get-special-table-rows (table)
  (org-element-map table 'table-row
    (lambda (table-row)
      (when (org-ods-table-row-is-special-p table-row)
	table-row))))

(defun org-ods-get-table-row-type (table-row)
  (when (org-ods-table-row-is-special-p table-row)
    (let* ((first-table-cell (org-element-map table-row
				 'table-cell #'identity nil t))
	   (c (org-element-contents first-table-cell)))
      (org-trim (substring-no-properties (car c))))))

(defun org-ods-get-special-table-rows-of-type (table tag)
  (cl-loop for table-row in (org-ods-get-special-table-rows table)
	   for type = (org-ods-get-table-row-type table-row)
	   when (and (stringp type) (string= type tag))
	   collect table-row))

;;; Data Rows

(defun org-ods-table-row-is-data-row-p (table-row &optional hrule-is-data-p)
  (when (and (not (org-export-table-row-is-special-p table-row nil))
	     (or hrule-is-data-p
                 (not (eq (org-element-property :type table-row) 'rule))))
    table-row))

(defun org-ods-get-previous-data-row (table-row)
  (cl-loop for table-row in (reverse (org-export-get-previous-element table-row nil t))
	   when (org-ods-table-row-is-data-row-p table-row)
	   return table-row))

(defun org-ods-get-previous-data-rows (table-row)
  (cl-loop for table-row in (reverse (org-export-get-previous-element table-row nil t))
	   when (org-ods-table-row-is-data-row-p table-row)
	   collect table-row))

(defun org-ods-get-next-data-row (table-row)
  (cl-some
   (lambda (table-row)
     (when (and (not (org-export-table-row-is-special-p table-row nil))
		(not (eq (org-element-property :type table-row) 'rule)))
       table-row))
   (org-export-get-next-element table-row nil t)))

(defun org-ods-get-table-data-rows (table &optional hrule-is-data-p)
  (org-element-map table 'table-row
    (lambda (table-row)
      (when (org-ods-table-row-is-data-row-p table-row hrule-is-data-p)
	table-row))))

;;; Prune Table

(defun org-ods-prune-table (table &optional prune-special-column-p hrule-is-data-p)
  (let* ((data-rows (org-ods-get-table-data-rows table hrule-is-data-p))
	 (has-special-column-p (org-export-table-has-special-column-p table))
	 (f (if (and prune-special-column-p has-special-column-p) #'cdr #'identity)))
    (cl-loop for data-row in data-rows
	     collect (funcall f (org-element-contents data-row)))))

(defun org-ods-get-first-formula-row-num (table)
  (let* ((first-formula-row (cl-some #'org-ods-table-row-is-data-row-p
				     (nth 1 (org-ods-get-table-rules table))))
	 (data-rows (org-ods-get-table-data-rows table)))
    ;; first-formula-row
    (length (memq first-formula-row (reverse data-rows)))))

(defun org-ods-rowgroup->rownum-alist (table)
  (cl-loop with data-rows = (org-ods-get-table-data-rows table)
	   with rowgroups = (org-ods-get-table-rules table)
	   with data-rowgroups = (cdr rowgroups)
	   for rowgroup in data-rowgroups
	   counting rowgroup into rowgroup-number
	   collect (cons rowgroup-number
			 (let* ((first-formula-row
				 (cl-loop for table-row in rowgroup
					  when (org-ods-table-row-is-data-row-p table-row)
					  return table-row)))
			   (length (memq first-formula-row (reverse data-rows)))))))

;;; Cell Address

(defun org-ods-get-table-cell-address (table-cell)
  ;; This is 1-based address
  (let ((table (org-export-get-parent-table table-cell)))
    (cl-loop with rows = (org-ods-prune-table table)
	     for row in rows
	     counting row into r
	     for match = (memq table-cell (reverse row))
	     when match
	     return (cons r (length match)))))

(defun org-ods-table-cell-cell-address-alist (table)
  (let ((pruned-table (org-ods-prune-table table)))
    (cl-loop for data-row in pruned-table
	     counting data-row into r
	     append (cl-loop for data-cell in data-row
			     counting data-cell into c
			     collect (cons data-cell (cons r c))))))

;;; Numeric Cell address -> ODS style address

(defun org-ods-encode-digit (digit)
  ;; digit is 1-based
  (let ((digit (cond
		((stringp digit)
		 (string-to-number digit))
		((numberp digit)
		 digit))))
    (when (or (> digit 26) (< digit 1))
      (error "Invalid digit %s" digit))
    (format "%c" (+ ?A (1- digit)))))

(defun org-ods-convert-n-to-base-26 (n)
  (let* ((base 26)
	 (q n)
	 (digits nil))
    (while (not (zerop q))
      (setq q (/ n base))
      (push (- n (* q base)) digits)
      (setq n q))
    digits))

(defun org-ods-encode-cell-address (address)
  (pcase-let ((`(,r . ,c) address))
    (format "%s%s%s%s"
	    "" (org-ods-encode-digit c) "" r)))

(defun org-ods-encode-cell-range (range)
  (pcase-let* ((`(,first . ,second) range))
    (cond
     (second
      (format "%s:%s"
	      (org-ods-encode-cell-address first)
	      (org-ods-encode-cell-address second)))
     ((null second)
      (org-ods-encode-cell-address first))
     (t (error "FIXME")))))

(defun org-ods-cell-address->text (table cell-address)
  (org-ods-table-cell-element-to-lisp
   (car (rassoc cell-address (org-ods-table-cell-cell-address-alist table)))))

;; Named Columns, Fields, and Parameters

;; (info "(org) Advanced features")
;;
;; ‘!’
;;      The fields in this line define names for the columns, so that you
;;      may refer to a column as ‘$Tot’ instead of ‘$6’.
;;
;; ‘^’
;;      This row defines names for the fields _above_ the row.  With such a
;;      definition, any formula in the table may use ‘$m1’ to refer to the
;;      value ‘10’.  Also, if you assign a formula to a names field, it is
;;      stored as ‘$name = ...’.
;;
;; ‘_’
;;      Similar to ‘^’, but defines names for the fields in the row
;;      _below_.
;;
;; ‘$’
;;      Fields in this row can define _parameters_ for formulas.  For
;;      example, if a field in a ‘$’ row contains ‘max=50’, then formulas
;;      in this table can refer to the value 50 using ‘$max’.  Parameters
;;      work exactly like constants, only that they can be defined on a
;;      per-table basis.
;;
;; ‘#’
;;      Fields in this row are automatically recalculated when pressing
;;      ‘<TAB>’ or ‘<RET>’ or ‘S-<TAB>’ in this row.  Also, this row is
;;      selected for a global recalculation with ‘C-u C-c *’.  Unmarked
;;      lines are left alone by this command.
;;
;; ‘*’
;;      Selects this line for global recalculation with ‘C-u C-c *’, but
;;      not for automatic recalculation.  Use this when automatic
;;      recalculation slows down editing too much.
;;
;; ‘/’
;;      Do not export this line.  Useful for lines that contain the
;;      narrowing ‘<N>’ markers or column group markers.
;;

(defun org-ods-get-named-columns (table)
  (cl-loop for row in (org-ods-table-row-elements-to-lisp
		       (org-ods-get-special-table-rows-of-type table "!"))
	   for row-sans-first-cell = (cdr row)
	   append (cl-loop for cell in row-sans-first-cell
			   for n in (number-sequence 1 (length row-sans-first-cell))
			   when (org-string-nw-p cell)
			   collect (cons cell n))))

(defun org-ods-get-named-fields (table)
  (cl-loop with rows = (org-ods-get-special-table-rows-of-type table "^")
	   for row in rows
	   append (cl-loop with row-content = (org-ods-table-row-element-to-lisp row)
			   with previous-data-row = (org-ods-get-previous-data-row row)
			   for cell in (cdr row-content)
			   for prev-row-table-cell in (cdr (org-element-contents previous-data-row))
			   when (org-string-nw-p cell)
			   collect (cons cell (org-ods-get-table-cell-address prev-row-table-cell)))))

(defun org-ods-get-named-values (table)
  (cl-loop for row in (org-ods-table-row-elements-to-lisp (org-ods-get-special-table-rows-of-type table "$"))
	   append (cl-loop for cell in (cdr row)
			   when (and (org-string-nw-p cell)
				     (string-match (rx-to-string
						    '(and bos (group (one-or-more (not "=")))
							  "=" (group (one-or-more any))))
						   cell))
			   collect (cons (org-trim (match-string 1 cell))
					 (org-trim (match-string 2 cell))))))

(defun org-ods-get-fullvar-table (table)
  (append
   ;; :colvar-table
   (mapcar (lambda (pair)
	     (pcase-let ((`(,cname . ,c) pair))
	       (cons (format "$%s" cname)
		     (format "$%s" c))))
	   (org-ods-get-named-columns table))
   ;; :fieldvar-table
   (mapcar (lambda (pair)
	     (pcase-let ((`(,name . ,cell-address) pair))
	       (cons (format "$%s" name)
		     (org-ods-cell-address->text table cell-address))))
	   (org-ods-get-named-fields table))
   ;; :var-table
   (mapcar (lambda (pair)
	     (pcase-let ((`(,name . ,value) pair))
	       (cons (format "$%s" name)
		     value)))
	   (org-ods-get-named-values table))))

;; TBLFM

(defun org-ods-table->tblfms (table)
  (let* ((tblfmlines (org-element-property :tblfm table))
	 (fullvar-table (org-ods-get-fullvar-table table)))
    (cl-loop for tblfmline in tblfmlines append
	     (cl-loop for fm in (split-string tblfmline "::" t " +") collect
		      (pcase-let* ((`(,lhs ,rhs-and-env) (split-string fm "=" t " +"))
				   (`(,rhs ,env) (split-string rhs-and-env ";")))
			(list :fm fm
			      :vartable fullvar-table
			      :lhs lhs
			      :rhs rhs
			      :env env
			      :rhs1 (org-ods-substitute-vars-in-expression fullvar-table rhs)))))))

(defun org-ods-substitute-vars-in-expression (fullvar-table expression)
  (cl-loop for (name . value) in fullvar-table
	   with exp = expression
	   do (setq exp (replace-regexp-in-string name value exp t t))
	   finally return exp))

(defvar org-ods-calc-f->ods-f-alist
  '(("vsum" . "SUM")))

(defun org-ods-tokenize-tblfms (tblfms)
  (cl-loop with n = 0
	   for tblfm in tblfms
	   for terms = '()
	   for rhs1 = (plist-get tblfm :rhs1) collect
	   ;; Actual tokenization
	   (rx-let ((ABS-N (one-or-more digit))
		    (SIGN (any "-+"))
		    (REL-N (and SIGN ABS-N))
		    (r-part (and "@" (or REL-N
					 (and (one-or-more "I") (optional REL-N))
					 (and (and SIGN (one-or-more "I")) (optional REL-N))
					 ABS-N
					 (one-or-more (or "<" ">")))))
		    (c-part (and "$" (or REL-N
					 ABS-N
					 (one-or-more (or "<" ">")))))
		    (field (or (and r-part (optional c-part))
			       c-part))
		    (field-range (and field (optional (and ".." field)))))
	     (with-temp-buffer
	       (insert rhs1)
	       (let* ((term)
		      (field nil))
		 (goto-char (point-min))
		 (while (and (not (eobp))
			     (re-search-forward (rx field-range) nil t))
		   ;; (inspect "SEARCHED")
		   (setq field (match-string 0))
		   (cond
		    ((org-string-nw-p field)
		     (replace-match (setq term (format "ODS%d" n)))
		     ;; (inspect "REPLACED")
		     (push (cons term field) terms)
		     (cl-incf n))
		    (t (forward-char 1))))
		 ;; (inspect "DONE")
		 (append tblfm
			 (list :rhs2
			       (buffer-substring-no-properties (point-min) (point-max))
			       :terms terms
			       :lhs-parsed (org-ods-parse-field (plist-get tblfm :lhs))
			       :rhs-terms-parsed (cl-loop for (name . field-range) in terms
							  collect (cons name (org-ods-parse-field-range field-range))))))))))

(defun org-ods-table-dimensions (table)
  (cdar (last (org-ods-table-cell-cell-address-alist table))))

(defun org-tblfm->cell-and-ods-formula (table tblfm)
  (let* ((has-special-column-p (org-export-table-has-special-column-p table))
	 (cell-addresses-to (org-ods-table-dimensions table))
	 (lhs-parsed (plist-get tblfm :lhs-parsed))
	 (rhs-terms-parsed (plist-get tblfm :rhs-terms-parsed))
	 (cell-addresses-from (cons
			       (org-ods-get-first-formula-row-num table)
			       (if has-special-column-p 2 1)))
	 (lhs-cell-addresses (org-ods-get-cell-addresess-matching-field
			      cell-addresses-from cell-addresses-to lhs-parsed)))
    (cl-loop for cell-address in lhs-cell-addresses collect
	     (list
	      ;; cell-address
	      cell-address
	      ;; lhs
	      ;; (org-ods-encode-cell-address cell-address)
	      ;; terms
	      (let ((terms (cl-loop for (ODSn first . second) in rhs-terms-parsed
				    ;; do (message "first: %S second: %S" first second)
				    collect
				    (cons ODSn
					  (org-ods-encode-cell-range
					   (cons (org-ods-apply-field-spec-to-cell-address
						  cell-address (org-ods-derelativize-field table first))
						 (org-ods-apply-field-spec-to-cell-address
						  cell-address (org-ods-derelativize-field table second 'secondp))))))))
		(format "=%s" (org-ods-substitute-vars-in-expression (append org-ods-calc-f->ods-f-alist
                                                                             terms)
                                                                     (plist-get tblfm :rhs2)))
                ;; (format "%s=%s" (org-ods-encode-cell-address (pcase-let ((`(,r . ,c) cell-address))
		;; 					       (cons r (+ c
		;; 							  (if has-special-column-p -1 0)))))
		;; 	(org-ods-substitute-vars-in-expression terms (plist-get tblfm :rhs2)))
                )))))



(defun org-ods-insert-ods-formula (table)
  ;; Modifies table by side-effects
  (cl-loop for tblfm in (org-ods-tokenize-tblfms (org-ods-table->tblfms table))
	   ;; do (message "\n\n%S" (plist-get tblfm :fm))
	   for final = (cons tblfm (org-tblfm->cell-and-ods-formula table tblfm))
	   do (cl-loop for (cell-address formula) in (cdr final)
		       ;; do (message "cell-address: %S" cell-address)
		       for table-cell = (car (rassoc cell-address (org-ods-table-cell-cell-address-alist table)))
		       do (setcar (last table-cell) formula))))

(defun org-ods-table->ods-table (table)
  (org-ods-insert-ods-formula table)
  ;; (org-ods-prune-table t)
  (org-ods-lisp-table-to-org-table
   (org-ods-table-row-elements-to-lisp (org-ods-prune-table table 'prune-special-column-p
                                                            'hrule-is-data-p))))



;;; Field and Field Ranges

(defun org-ods-parse-field (field)
  (rx-let ((ABS-N (one-or-more digit))
	   (SIGN (any "-+"))
	   (REL-N (and SIGN ABS-N))
	   (FIELD
	    (and (group-n 1
		   (optional
		    (and "@" (or (group-n 2 (group-n 3 REL-N))
				 (group-n 4 (group-n 5 (one-or-more "I")) (optional (group-n 21 REL-N)))
				 (group-n 24 (group-n 25 (and (group-n 26 SIGN) (one-or-more "I")))
					  (optional (group-n 27 REL-N)))
				 (group-n 6 ABS-N)
				 (group-n 7 (one-or-more (or (group-n 8 "<") (group-n 9 ">"))))))))
		 (group-n 11
		   (optional (and "$" (or (group-n 12 (group-n 13 REL-N))
					  (group-n 16 ABS-N)
					  (group-n 17 (one-or-more (or (group-n 18 "<") (group-n 19 ">")))))))))))
    (let* ((n->props
	    '(
	      (1 :row-full identity)
	      (2 :row-num (lambda (x) 'self))
	      (3 :row-offset string-to-number)
	      ;;
	      (5 :row-num (lambda (x) (list 'hline (length x))))
	      (21 :row-offset string-to-number)
	      ;;
	      (26 :row-num (lambda (x) 'self))
	      (25 :row-offset-hline (lambda (x)
				      (list 'hline (read
						    (let* ((i (string-match "I" x)))
						      (format "%s%s"
							      (substring x 0 i)
							      (length (substring x i))))))))
	      (27 :row-offset string-to-number)
	      ;;
	      (6 :row-num string-to-number)
	      ;;
	      (7 :row-offset (lambda (x) (1- (length x))))
	      (8 :row-num (lambda (x) 'first))
	      (9 :row-num (lambda (x) 'last))
	      ;;
	      (11 :col-full identity)
	      ;;
	      (12 :col-num (lambda (x) 'self))
	      (13 :col-offset string-to-number)
	      ;;
	      (16 :col-num string-to-number)
	      ;;
	      (17 :col-offset (lambda (x) (1- (length x))))
	      (18 :col-num (lambda (x) 'first))
	      (19 :col-num (lambda (x) 'last)))))
      (when (string-match (rx FIELD) field)
	(cl-loop for (n prop fn) in n->props
		 for m = (match-string n field)
		 when (org-string-nw-p m)
		 append (list prop (funcall (or fn #'identity) m)))))))

(defun org-ods-parse-field-range (field-range)
  (let* ((sep-regexp (rx "..")))
    (cond
     ((string-match-p sep-regexp field-range)
      (pcase-let* ((`(,first ,second) (split-string field-range sep-regexp t)))
	(cons (org-ods-parse-field first)
	      (org-ods-parse-field second))))
     (t
      (cons (org-ods-parse-field field-range) nil)))))



(defvar org-ods--test-refs
  '(
    ;; Simple
    "@2$3" ;; 2nd row, 3rd column (same as "C2")
    "$5"   ;; column 5 in the current row (same as "E&")
    "@2"   ;; current column, row 2
    "@-1$-3" ;; field one row up, three columns to the left
    "@-I$2"  ;; field just under hline above current row, column 2
    "@>$5"   ;; field in the last row, in column 5
    ;; Ranges
    "$1..$3" ;; first three fields in the current row
    ;; "$P..$Q"  ;; range, using column names (see *note Advanced features::)
    "$<<<..$>>" ;; start in third column, continue to the last but one
    "@2$1..@4$3" ;; nine fields between these two fields (same as "A2..C4")
    "@-1$-2..@-1" ;; 3 fields in the row above, starting from 2 columns on the left
    "@I..@II" ;; between first and second hline, short for "@I..@II"
    ))



;; (mapcar #'org-ods-parse-field-range org-ods--test-refs)

(defun org-ods-derelativize-field (table field &optional secondp)
  ;; Modifies field by side-effect.  Also returns it.
  (pcase-let ((`(,rmin . ,cmin) '(1 . 1))
	      (`(,rmax . ,cmax) (org-ods-table-dimensions table)))
    (cons
     ;; Row part
     (pcase (plist-get field :row-num)
       ('first
	(let ((base-r rmin)
	      (offset (or (plist-get field :row-offset) 0)))
	  (plist-put field :row-offset nil)
	  (plist-put field :row-num (+ base-r offset))))
       ('last
	(let ((base-r rmax)
	      (offset (or (plist-get field :row-offset) 0)))
	  (plist-put field :row-offset nil)
	  (plist-put field :row-num (- base-r offset))))
       (`(hline ,n)
	(let ((base-r (let ((r (assoc-default n (org-ods-rowgroup->rownum-alist table))))
			(if secondp (1- r) r)))
	      (offset (or (plist-get field :row-offset) 0)))
	  (plist-put field :row-offset nil)
	  (plist-put field :row-num (+ base-r offset)))))
     ;; Row part
     (pcase (plist-get field :col-num)
       ('first
	(let ((base-c cmin)
	      (offset (or (plist-get field :col-offset) 0)))
	  (plist-put field :col-offset nil)
	  (plist-put field :col-num (+ base-c offset))))
       ('last
	(let ((base-c cmax)
	      (offset (or (plist-get field :col-offset) 0)))
	  (plist-put field :col-offset nil)
	  (plist-put field :col-num (- base-c offset)))))))
  field)

(defun org-ods-apply-field-spec-to-cell-address (cell-address field)
  (when field
    (pcase-let ((`(,r . ,c) cell-address))
      (cons
       ;; Row part
       (pcase (plist-get field :row-num)
	 ('self (+ r (plist-get field :row-offset)))
	 ((and (pred numberp) n) n)
	 ('() r))
       ;; Col part
       (pcase (plist-get field :col-num)
	 ('self
	  (+ c (plist-get field :col-offset)))
	 ((and (pred numberp) n) n)
	 ('() c))))))

(defun org-ods-get-cell-addresess-matching-field (address-min address-max field)
  (when field
    (pcase-let ((`(,rmin ,cmin) address-min)
		(`(,rmax ,cmax) address-max))
      (cl-loop for r in (pcase (plist-get field :row-num)
			  ('()
			   (number-sequence rmin rmax))
			  ((and (pred numberp) n)
			   (list n))
			  (_ (error "FIXME?")))
	       append (cl-loop for c in (pcase (plist-get field :col-num)
					  ('()
					   (number-sequence cmin cmax))
					  ((and (pred numberp) n)
					   (list n))
					  (_ (error "FIXME?")))
			       collect (cons r c))))))



(defun org-ods-table-as (s as)
  (with-temp-buffer
    (insert s)
    (org-mode)
    (org-table-align)
    (cl-case as
      (lisp (org-table-to-lisp))
      (string
       (buffer-substring-no-properties (point-min) (point-max)))
      (data
       (org-element-map (org-element-parse-buffer)
	   'table #'identity nil t)))))

(defun org-ods-table-at-point (as)
  (org-with-wide-buffer
   (when-let* ((el (when (org-at-table-p)
		     (goto-char (org-table-begin))
		     (org-element-at-point)))
	       (begin (org-element-property :begin el))
	       (end (org-element-property :end el))
	       (contents-begin (org-element-property :contents-begin el))
	       (contents-end (org-element-property :contents-end el))
	       (preamble (buffer-substring-no-properties begin contents-begin))
	       (postamble (buffer-substring-no-properties end contents-end))
	       (s (buffer-substring-no-properties contents-begin contents-end))
	       (data (org-element-map
			 (progn
			   (narrow-to-region begin end)
			   (org-element-parse-buffer))
			 'table #'identity nil t)))
     (cl-case as
       (data data)
       (full-data
	(list data preamble postamble))
       (t (org-ods-table-as s as))))))

;; =SUM($A4:$B4)
;; "=IF(F4>$K$27,(F4-$K$27)*$AE$3)+MIN($K$27,F4)*$G$3+MIN($K$27,G4)*$H$3+IF(H4>$K$27,(H4-$K$27)*$AE$3)+MIN($K$27,H4)*$I$3+I4*$J$3"

;;;###autoload
(defun org-ods-convert (&optional org-table out-file out-fmt open)
  (interactive
   (when-let*
       ((el (let ((el (org-ods-table-at-point 'data)))
	      (unless el (user-error "No table at point"))
              el))
	(s (org-ods-table-at-point 'string))
	;; (lisp-table (progn
	;;       	(unless (org-at-table-p)
	;;       	  (user-error "No table at point"))
	;;       	(org-odt-table-at-point 'lisp)))
	(default-output-file-name (file-relative-name (file-name-sans-extension (buffer-file-name))))
	(in-fmt "csv")
	(out-fmt-choices (org-odt-reachable-formats in-fmt))
	(out-fmt
	 (or (and out-fmt-choices
		  (funcall (if (featurep 'ido) 'ido-completing-read 'completing-read)
			   "Output format: "
			   out-fmt-choices nil nil nil))
	     (error
	      "No known converter or no known output formats for %s files"
	      in-fmt)))
	(out-file (read-file-name "File to output: "
				  nil	; dir
				  nil	; default-file-name
				  nil	; mustmatch
				  (concat default-output-file-name
					  (let* ((uniquifier (or (org-element-property :name el)
								 (org-export-data-with-backend
								  (org-export-get-caption el) 'plain-text nil))))
					    (when (org-string-nw-p uniquifier)
					      (concat "#" uniquifier)))
					  "." out-fmt) ; initial
				  ;; (lambda (output-file-name)
				  ;;   (when (file-exists-p output-file-name)
				  ;;     (yes-or-no-p (format "Overwrite %s?" output-file-name))))
				  )))
     (list s out-file out-fmt current-prefix-arg)))
  (when-let*
      ((org-table org-table)
       (tmp-in-csv-file (make-temp-file "org-ods-" nil ".csv" (orgtbl-to-csv
							       (org-ods-table-as org-table 'lisp)
							       nil)))
       (tmp-out-ods-file (cond
			  ((string= out-fmt "csv") tmp-in-csv-file)
			  (t (org-odt-do-convert tmp-in-csv-file out-fmt (not 'open))))))
    (when tmp-out-ods-file
      (message "Exported to %s" tmp-out-ods-file)
      (message "Copying %s to %s" tmp-out-ods-file out-file)
      (copy-file tmp-out-ods-file out-file t))
    (when open
      (message "Opening %s..." out-file)
      (org-open-file out-file 'system))))

(defun org-ods-translate (&optional full-table-data)
  (interactive (list (org-ods-table-at-point 'full-data)))
  (unless full-table-data
    (user-error "Not at a Table"))
  (pcase-let ((`(,table-element ,preamble ,_postamble) full-table-data))
    (goto-char (org-element-property :end table-element))
    (save-excursion
      (insert (concat
               "\n\n"
               preamble
	       (org-ods-table->ods-table table-element)
	       ;; postamble
	       "\n\n")))
    (re-search-forward org-table-any-line-regexp)))

;;; Unit Testing

(eval-and-compile
  (defun s->table (s)
    (org-element-map
	(with-temp-buffer
	  (insert s)
	  (org-element-parse-buffer))
	'table #'identity nil t)))



(defvar org-ods--test-t1
  (s->table "
| Student  | Mark1 | Mark2 | Mark3 | Result | Result-simple | ResultSkaled |
|----------+-------+-------+-------+--------+---------------+--------------|
| Student1 |     2 |     3 |     4 |      9 |             9 |          900 |
| Student2 |     4 |     2 |     1 |      7 |             7 |          700 |
#+TBLFM: $5=vsum($2..$4)::$6=$2+$3+$4::$7=vsum($2..$4)*100
"))

;; (org-ods-table->ods-table org-ods--test-t1)



(defvar org-ods--test-t2
  (s->table "
| Account      |     |
|--------------+-----|
| Account1     | 200 |
| Account1     | 300 |
|--------------+-----|
| All accounts | 500 |
#+TBLFM: @4$2=vsum(@I..@II)
"))

;; (org-ods-table->ods-table org-ods--test-t2)



(defvar org-ods--test-t3
  (s->table "
| First |   |     |
|-------+---+-----|
|     3 | 6 |   9 |
|     1 | 3 |   4 |
|     1 | 2 |   3 |
|     8 | 9 |  17 |
|   100 | 1 | 101 |
#+TBLFM: $3=vsum($1..$2);f-2::@2$3=vsum(@2$1..@2$2)
"))

;; (org-ods-table->ods-table org-ods--test-t3)



(defvar org-ods--test-t4
  (s->table "
     |---+---------+--------+--------+--------+-------+------|
     |   | Student | Prob 1 | Prob 2 | Prob 3 | Total | Note |
     |---+---------+--------+--------+--------+-------+------|
     | ! |         |     P1 |     P2 |     P3 |   Tot |      |
     | # | Maximum |     10 |     15 |     25 |    50 | 10.0 |
     | ^ |         |     m1 |     m2 |     m3 |    mt |      |
     |---+---------+--------+--------+--------+-------+------|
     | # | Peter   |     10 |      8 |     23 |    41 |  8.2 |
     | # | Sam     |      2 |      4 |      3 |     9 |  1.8 |
     |---+---------+--------+--------+--------+-------+------|
     |   | Average |        |        |        |  25.0 |      |
     | ^ |         |        |        |        |    at |      |
     | $ | max=50  |        |        |        |       |      |
     |---+---------+--------+--------+--------+-------+------|
     #+TBLFM: $6=vsum($P1..$P3)::$7=10*$Tot/$max;%.1f::$at=vmean(@-II..@-I);%.1f
"))

;; (org-ods-table->ods-table org-ods--test-t4)

;; | C | 1       |      2 |      3 |      4 |     5 |    6 | 7 |
;; |---+---------+--------+--------+--------+-------+------+---|
;; |   | Student | Prob 1 | Prob 2 | Prob 3 | Total | Note | 1 |
;; |---+---------+--------+--------+--------+-------+------+---|
;; | ! |         |     P1 |     P2 |     P3 |   Tot |      |   |
;; | # | Maximum |     10 |     15 |     25 |    50 | 10.0 | 2 |
;; | ^ |         |     m1 |     m2 |     m3 |    mt |      |   |
;; |---+---------+--------+--------+--------+-------+------+---|
;; | # | Peter   |     10 |      8 |     23 |    41 |  8.2 | 3 |
;; | # | Sam     |      2 |      4 |      3 |     9 |  1.8 | 4 |
;; |---+---------+--------+--------+--------+-------+------+---|
;; |   | Average |        |        |        |  25.0 |      | 5 |
;; | ^ |         |        |        |        |    at |      |   |
;; | $ | max=50  |        |        |        |       |      | 6 |
;; |---+---------+--------+--------+--------+-------+------+---|

(defvar org-ods--test-t1/vars
  (list
   :named-columns (org-ods-get-named-columns org-ods--test-t1)
   :named-fields (org-ods-get-named-fields org-ods--test-t1)
   :named-values (org-ods-get-named-values org-ods--test-t1)
   :colvar-table (mapcar (lambda (pair)
			   (pcase-let ((`(,cname . ,c) pair))
			     (cons (format "$%s" cname)
				   (format "$%s" c))))
			 (org-ods-get-named-columns org-ods--test-t1))
   :fieldvar-table (mapcar (lambda (pair)
			     (pcase-let ((`(,name . ,cell-address) pair))
			       (cons (format "$%s" name)
				     (org-ods-cell-address->text org-ods--test-t1 cell-address))))
			   (org-ods-get-named-fields org-ods--test-t1))
   :var-table (mapcar (lambda (pair)
			(pcase-let ((`(,name . ,value) pair))
			  (cons (format "$%s" name)
				value)))
		      (org-ods-get-named-values org-ods--test-t1))
   :fullvar-table (org-ods-get-fullvar-table org-ods--test-t1)
   :tblfms (org-ods-tokenize-tblfms
	    (org-ods-table->tblfms
	     org-ods--test-t1))))

(provide 'ox-ods)
