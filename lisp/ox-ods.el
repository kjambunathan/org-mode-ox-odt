;;; ox-ods.el --- OpenDocument Spreadsheet Document for Org Mode -*- lexical-binding: t; coding: utf-8-emacs; -*-

;; Copyright (C) 2022-23 Jambunathan K <kjambunathan at gmail dot com>

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

(require 'rx)
(require 'ox-odt)
(require 'peg)

(defvar org-ods-debug nil)

(defvar org-ods-debug-buffer
  "*Org Ods Debug*")

(defun org-ods-message (lisp-object &optional heading level)
  (when org-ods-debug
    (with-current-buffer (get-buffer-create org-ods-debug-buffer)
      (goto-char (point-max))
      (when heading
	(unless level
	  (setq level 1))
	(insert "\n"
		(string-join (cons ";;" (make-list level ";"))) " " (upcase heading) "\n"))
      (insert (or (unless heading "\n") "")
	      (pp-to-string lisp-object)))))

(defun org-ods-debug--op-on-element (op el)
  (cl-labels ((org-ods-debug--object-signature (el)
	        (pcase (org-element-type el)
		  (`table
		   (cond
		    ((org-element-property :name el)
		     (format "NAME `%s'"
			     (org-element-property :name el)))
		    (t
		     (let* ((table-row (org-element-map el 'table-row
				         #'org-ods-table-row-is-data-row-p nil t)))
		       (or (when table-row
			     (format "ROW `%s'"
				     (org-ods-debug--object-signature table-row)))
			   "")))))
		  (_
		   (format "`%s'"
			   (org-trim
			    (substring-no-properties
			     (org-element-interpret-data el))))))))
    (format "%s %s WITH SIGNATURE: %s"
	    (upcase op)
	    (org-element-type el)
	    (org-ods-debug--object-signature el))))

(defun org-ods-table-cell-element-to-lisp (table-cell)
  (substring-no-properties
   (org-element-interpret-data
    (org-element-contents table-cell))))

(defvar org-ods-print-hrule nil)

(defun org-ods-table-row-element-to-lisp (table-row)
  (if (and org-ods-print-hrule (org-ods-table-row-is-rule-p table-row)) 'hrule
    (org-element-map table-row 'table-cell
      #'org-ods-table-cell-element-to-lisp)))

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

(defvar org-ods-cell-mapper)

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
				   (format "| %s"
					   (let ((cell-contents (org-trim
								 (replace-regexp-in-string
								  "[\r\n]+" " " (substring-no-properties cell) t t))))
					     (cond
					      ((and (boundp 'org-ods-cell-mapper)
						    (functionp org-ods-cell-mapper))
					       (funcall org-ods-cell-mapper cell-contents))
					      (t cell-contents))))))))
	"\n")))
    ;; (inspect)
    (org-table-align)
    ;; (inspect)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-ods-table-element-print (table)
  (org-ods-lisp-table-to-org-table
   (org-ods-table-element-to-lisp-table table)))

(defun org-ods-table-row-elements-print (table-rows)
  (if (null table-rows) ""
    (org-ods-lisp-table-to-org-table
     (org-ods-table-row-elements-to-lisp table-rows))))

;;; Hrules

(defun org-ods-table-row-is-rule-p (table-row)
  (eq (org-element-property :type table-row) 'rule))

(defun org-ods-split-into-rowgroups (table)
  ;; Not yet
  (let* ((hrule '())
	 (nonhrule '())
	 (result '()))
    (dolist (table-row (org-element-contents table) result)
      (cond
       ((org-ods-table-row-is-rule-p table-row)
	(when (or hrule nonhrule)
	  (push (cons hrule (reverse nonhrule)) result))
	(setq hrule table-row)
	(setq nonhrule nil))
       (t (push table-row nonhrule))))
    (when (or hrule nonhrule)
      (push (cons hrule (reverse nonhrule)) result))
    (reverse result)))

(defun org-ods-locate-rowgroup (table table-row)
  ;; Not yet
  (let ((rowgroups (org-ods-split-into-rowgroups table)))
    (cl-loop for (rowgroup . rest) on rowgroups by #'cdr
	     for nonhrules = (cdr rowgroup)
	     while (not (memq table-row nonhrules))
	     collecting rowgroup into before
	     finally (return (list rowgroup (reverse before)
				   rest)))))

;;; Special Rows

(defun org-ods-table-row-is-special-p (table-row)
  (org-export-table-row-is-special-p table-row nil))

(defun org-ods-get-table-row-type (table-row)
  (when (org-ods-table-row-is-special-p table-row)
    (let* ((first-table-cell (org-element-map table-row
				 'table-cell #'identity nil t))
	   (c (org-element-contents first-table-cell)))
      (org-trim (substring-no-properties (car c))))))

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

(defun org-ods-glimpse-table (&optional table-el-or-tinfo quickp)
  (when-let* ((tinfo1 (let* ((table-el))
			(cond
			 ;; Input is null
			 ((null table-el-or-tinfo)
			  ;; Get the table at point
			  (setq table-el (car (org-ods-table-at-point 'full-data)))
			  ;; Convert it in to tinfo
			  (when table-el
			    (org-ods-table->table-info table-el)))
			 ;; Input is table element;
			 ((eq 'table (org-element-type table-el-or-tinfo))
			  ;; Convert it into tinfo
			  (org-ods-table->table-info table-el-or-tinfo))
			 ;; Input is tinfo
			 (t
			  table-el-or-tinfo))))
	      (tinfo (append (list :pretty-table (plist-get tinfo1 :table)) tinfo1))
	      (org-ods-print-hrule t))
    (thread-last
      '(:pretty-table
	:dimensions
	:has-special-column
	:cell-address-adjust
	;; Row
	:rgn->rg
	:rgn->r/org
	:data-rows
	:data-rowgroups
	:rowgroups
	:special-rows
	;; Other Row
	:param-row
	:colname-row
	;; Field Names
	:field-name-above-row
	:field-name-below-row
	:nonhrules
	:add-on-table
	:add-on-table-sans-special-markers
	:table)
      ;; tinfo
      (seq-map
       (lambda (k)
	 (let ((v (map-elt tinfo k)))
	   (pcase k
	     (`:pretty-table (list :pretty-table
				   (let* ((pp-table
					   (lambda (table)
					     (concat "\n" (substring-no-properties
                                                           (org-element-interpret-data table))))))
				     (funcall pp-table v))))
	     (`:dimensions (list :dimensions v))
	     (`:has-special-column (list :has-special-column v))
	     (`:cell-address-adjust (list :cell-address-adjust v))
	     (`:rgn->rg (list :rgn->rg
			      (thread-last v
					   (seq-map
					    (pcase-lambda (`(,n . ,row-groups))
					      (cons n (org-ods-table-row-elements-to-lisp row-groups)))))))
	     (`:rgn->r/org (list :rgn->r/org v))
	     (`:data-rows (list :data-rows (org-ods-table-row-elements-to-lisp (plist-get tinfo :data-rows))))
	     (`:data-rowgroups (list :data-rowgroups
				     (thread-last v
						  (seq-map
						   (lambda (it)
						     (org-ods-table-row-elements-to-lisp it))))))
	     (`:rowgroups (list :rowgroups
				(seq-map (lambda (it) (org-ods-table-row-elements-to-lisp it)) (plist-get tinfo :rowgroups))))
	     (`:special-rows (list :special-rows v))
	     (`:param-row (list :param-row v))
	     (`:colname-row (list :colname-row v))
	     (`:field-name-above-row (list :field-name-above-row v))
	     (`:field-name-below-row (list :field-name-below-row v))
	     (`:nonhrules (list :nonhrules
				(thread-last v
					     (seq-map #'org-ods-table-row-element-to-lisp))))
	     (`:add-on-table (list :add-on-table
                                   (thread-last v
                                                org-element-contents
                                                org-ods-table-row-elements-to-lisp)))
	     (`:add-on-table-sans-special-markers
	      (list :add-on-table-sans-special-markers
                    (thread-last v
                                 org-element-contents
                                 org-ods-table-row-elements-to-lisp)))
	     (`:table (list :table (if quickp 'elided v)))))))
      (seq-filter #'identity))))

;;; Prune Table

;; Named Columns, Fields, and Parameters

;; (info "(org) Advanced features")
;;
;;
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

(defun org-ods-table->table-info (table)
  (org-ods-message (list 'org-ods-table->table-info))
  ;; Not yet
  (cl-loop with predicate = #'org-ods-table-row-is-data-row-p
	   ;; with rowgroup-number = -1
	   ;; Skip past the initial hrules, and position the cursor on
	   ;; first non-hrule row.
	   ;; with table-rows = (cl-loop for table-rows on (org-element-contents table)
	   ;;      		      for table-row = (car table-rows)
	   ;;      		      until (not (org-ods-table-row-is-rule-p table-row))
	   ;;      		      finally return table-rows)
	   with table-rows = (org-element-contents table)
	   for table-row in table-rows
	   for special-row-type = (org-ods-get-table-row-type table-row)
	   ;; A hrule starts a new rowgroup
	   if (org-ods-table-row-is-rule-p table-row)
               collecting (setq rowgroup (list table-row)) into rowgroups
               and counting rowgroup into rowgroup-number
               and collecting (cons rowgroup-number rowgroup) into rgn->rg
           else
           	collecting table-row into nonhrules
                and counting table-row into r/org
               ;; ‘!’
               ;;      The fields in this line define names for the columns, so that you
               ;;      may refer to a column as ‘$Tot’ instead of ‘$6’.
               and if (and special-row-type (string= "!" special-row-type))
                   collecting table-row into colname-row
                   and collecting table-row into special-rows
               ;; ‘^’
               ;;      This row defines names for the fields _above_ the row.  With such a
               ;;      definition, any formula in the table may use ‘$m1’ to refer to the
               ;;      value ‘10’.  Also, if you assign a formula to a names field, it is
               ;;      stored as ‘$name = ...’.
               else if (and special-row-type (string= "^" special-row-type))
                   collecting table-row into field-name-above-row
                   and collecting table-row into special-rows
               ;; ‘_’
               ;;      Similar to ‘^’, but defines names for the fields in the row
               ;;      _below_.
               ;;
               else if (and special-row-type (string= "_" special-row-type))
                   collecting table-row into field-name-below-row
                   and collecting table-row into special-rows
               ;; ‘$’
               ;;      Fields in this row can define _parameters_ for formulas.  For
               ;;      example, if a field in a ‘$’ row contains ‘max=50’, then formulas
               ;;      in this table can refer to the value 50 using ‘$max’.  Parameters
               ;;      work exactly like constants, only that they can be defined on a
               ;;      per-table basis.
               else if (and special-row-type (string= "$" special-row-type))
                   collecting table-row into param-row
                   and collecting table-row into special-rows
               else
               ;; A datarow gets in to the current rowgroup
                   when (funcall predicate table-row)
                       when (not rowgroup)	    ; A data row always go in to a
                                                    ; rowgroup; create one if
                                                    ; there is not already one
                           collecting (setq rowgroup (list nil)) into rowgroups
                       end
                       and collecting (cons rowgroup-number r/org) into rgn->r/org
                       and collecting table-row into rowgroup
                       and collecting table-row into data-rows
                   end
               end
               finally return
               (let* ((tinfo (list
			      :special-rows special-rows
			      :colname-row colname-row
			      :field-name-above-row field-name-above-row
			      :field-name-below-row field-name-below-row
			      :param-row param-row
			      :data-rows data-rows
			      :rowgroups rowgroups
			      :data-rowgroups (mapcar #'cdr rowgroups)
			      :rgn->r/org rgn->r/org
			      :rgn->rg rgn->rg
			      :cell-address-adjust (if (org-export-table-has-special-column-p table)
						       '(0 . -1)
						     '(0 . 0))
			      :has-special-column (org-export-table-has-special-column-p table)
			      :add-on-table (append (list (nth 0 table)
							  (nth 1 table))
						    (append
						     ;; special-rows
						     colname-row
						     field-name-above-row
						     field-name-below-row
						     param-row))
			      :add-on-table-sans-special-markers
			      (append (list (nth 0 table)
					    (nth 1 table))
				      (append
				       (cl-loop for (type . (props . contents)) in special-rows
						collect (append (list type props) (cdr contents)))))
			      :nonhrules nonhrules
			      :dimensions (cons r/org
						(length (org-element-contents (car nonhrules))))
			      :table table)))
		 (prog1 tinfo
		   (plist-put (cadr table) :tinfo tinfo)))))

;;; Cell Address

(defun org-ods-get-table-row->row-number/org (table-row)
  (unless (org-ods-table-row-is-rule-p table-row)
    (let* ((table (org-element-property :parent table-row))
	   (table-rows (org-element-contents table)))
      (cl-loop for row in table-rows
	       if (org-ods-table-row-is-rule-p row)
	       do (ignore)
	       else if (not (eq row table-row))
	       counting row into r/org
	       else return (1+ r/org)
	       finally (error "Shouldn't come here")))))

(defun org-ods-get-table-cell->cell-address/org (table-cell)
  ;; This is 1-based address
  (when-let* ((table-row (org-element-property :parent table-cell))
	      (table-rows (org-element-contents table-row))
	      (r/org (org-ods-get-table-row->row-number/org table-row))
	      (c/org (length (memq table-cell (reverse table-rows)))))
    (cons r/org c/org)))

;;; Numeric Cell address -> ODS style address

(defconst org-ods-colnames
  (let* ((cartesian-product
	  (lambda (list-of-lists)
	    (seq-reduce (lambda (A B)
			  (cond
			   ((null A)
			    (seq-map
			     (lambda (it)
			       (list it))
			     B))
			   (t (seq-mapcat
			       (lambda (a)
				 (seq-map (lambda (b)
					    (append a (list b)))
					  B))
			       A))))
			list-of-lists nil)))
	 (n 2))
    (thread-last n
		 (number-sequence 1)
		 (seq-map
		  (lambda (it)
		    (thread-last '(?A ?Z)
				 (apply #'number-sequence)
				 (seq-map
				  (lambda (it)
				    (format "%c" it)))
				 (make-list it)
				 (funcall cartesian-product)
				 (seq-map
				  (lambda (it)
				    (string-join it ""))))))
		 (apply #'append)
                 (pcase--flip seq-into 'vector))))

(defconst org-ods-max-columns
  (length org-ods-colnames))

(defun org-ods-encode-digit (n)
  ;; n is 1-based
  (let ((n (cond
	    ((stringp n)
	     (string-to-number n))
	    ((numberp n)
	     n))))
    (cl-assert (> n 0))
    (when (or (> n org-ods-max-columns))
      (error (format (concat "Refusing to encode column number `%s'.  "
			     "ODS exporter can handle column numbers only up to `%s'")
		     n org-ods-max-columns)))
    (aref org-ods-colnames (- n 1))))

(defun org-ods-convert-n-to-base-26 (n)
  (let* ((base 26)
	 (q n)
	 (digits nil))
    (while (not (zerop q))
      (setq q (/ n base))
      (push (- n (* q base)) digits)
      (setq n q))
    digits))

(defun org-ods-encode-cell-address (address adjust)
  (pcase-let* ((`(,org-r . ,org-c) address)
	       (`(,deltar . ,deltac) adjust)
	       (r (+ org-r deltar))
	       (c (+ org-c deltac)))
    (format "%s%s%s%s"
	    "" (org-ods-encode-digit c) "" r)))

(defvar org-ods-encode-cell-range-function
  'org-ods-encode-cell-range-for-text)

(defun org-ods-encode-cell-range (range adjust)
  (funcall org-ods-encode-cell-range-function range adjust))

(defun org-ods-encode-cell-range-for-text (range adjust)
  (pcase-let* ((`(,first . ,second) range))
    (cond
     (second
      (format "%s:%s"
	      (org-ods-encode-cell-address first adjust)
	      (org-ods-encode-cell-address second adjust)))
     ((null second)
      (org-ods-encode-cell-address first adjust))
     (t (error "FIXME")))))

(defun org-ods-encode-cell-range-for-ods (range adjust)
  (pcase-let* ((`(,first . ,second) range))
    (cond
     (second
      (format "[.%s:.%s]"
	      (org-ods-encode-cell-address first adjust)
	      (org-ods-encode-cell-address second adjust)))
     ((null second)
      (format "[.%s]"
	      (org-ods-encode-cell-address first adjust)))
     (t (error "FIXME")))))

;; Named Columns, Fields, and Parameters

(defun org-ods-name->column-locator (tinfo)
  (cl-loop for row in (org-ods-table-row-elements-to-lisp
		       (plist-get tinfo :colname-row))
	   for row-sans-first-cell = (cdr row)
	   append (cl-loop for cell in row-sans-first-cell
			   for n in (number-sequence 2 (length row-sans-first-cell))
			   when (org-string-nw-p cell)
			   collect (cons (format "$%s" cell)
					 (format "$%s" n)))))

(defun org-ods-name->field-locator (tinfo)
  (cl-loop with rows = (append (plist-get tinfo :field-name-above-row)
			       (plist-get tinfo :field-name-below-row))
	   for row in rows
	   ;; do (print (org-ods-table-row-element-to-lisp row))
	   for row-content = (org-ods-table-row-element-to-lisp row)
	   for previous-data-row = (org-ods-get-previous-data-row row)
	   ;; do (print (org-ods-table-row-element-to-lisp previous-data-row))
	   append (cl-loop for cell in (cdr row-content)
			   for prev-row-table-cell in (cdr (org-element-contents previous-data-row))
			   when (org-string-nw-p cell)
			   collect (let* ((cell-address (org-ods-get-table-cell->cell-address/org prev-row-table-cell)))
				     (cons cell
					   ;; :address/org cell-address
					   ;; :address/ods (org-ods-encode-cell-address
					   ;;      	 cell-address '(0 . 0))
					   ;; :locator (format "@%s$%s" (car cell-address)
					   ;;                  (cdr cell-address))
					   (format "@%s$%s" (car cell-address)
						   (cdr cell-address)))))))

(defun org-ods-name->value (tinfo)
  (cl-loop for (_name . rest) in (org-ods-do-name->value tinfo)
	   collect (cons (plist-get rest :occurs-as)
			 (plist-get rest :replace-with))))

(defun org-ods-do-name->value (tinfo)
  (cl-loop for row-text in (org-ods-table-row-elements-to-lisp
			    (plist-get tinfo :param-row))
	   for row in (plist-get tinfo :param-row)
	   append (cl-loop for cell-text in (cdr row-text)
			   for cell in (cdr (org-element-contents row))
			   when (and (org-string-nw-p cell-text)
				     (string-match (rx-to-string
						    '(and bos (group (one-or-more (not "=")))
							  "=" (group (one-or-more any))))
						   cell-text))
			   collect (let* ((name (org-trim (match-string 1 cell-text)))
					  (value (org-trim (match-string 2 cell-text)))
					  (defined-at (org-ods-get-table-cell->cell-address/org cell)))
				     (list name
					   :defined-at defined-at
					   :value@defined-at value
					   :occurs-as (format "$%s" name)
					   :replace-with (format "@%s$%s"
								 (car defined-at)
								 (cdr defined-at)))))))

(defun org-ods-special-cell-address/org->value (tinfo)
  (org-ods-message (list 'org-ods-special-cell-address/org->value))
  (cl-loop for (_name . rest) in (org-ods-do-name->value tinfo)
	   collect (cons (plist-get rest :defined-at)
			 (plist-get rest :value@defined-at))))

(defun org-ods-get-fullvar-table (tinfo)
  (append
   (org-ods-name->column-locator tinfo)
   (org-ods-name->field-locator tinfo)
   (org-ods-name->value tinfo)))

;; TBLFM

(defvar org-ods-formula-arg-separator ";")

(defun org-ods-table->tblfms (tinfo)
  (let* ((table (plist-get tinfo :table))
	 (tblfmlines (org-element-property :tblfm table))
	 (fullvar-table (org-ods-get-fullvar-table tinfo)))
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
  (org-ods-message (list 'org-ods-substitute-vars-in-expression
			 :fullvar-table fullvar-table
			 :expression expression))
  (cl-loop for (name . value) in fullvar-table
	   with exp = expression
	   do (setq exp (replace-regexp-in-string (rx-to-string name) value exp t t))
	   finally return exp))

(defvar org-ods-calc-f->ods-f-alist
  '(("$#" . "COLUMN()")
    ("@#" . "ROW()")
    ("vmax" . "MAX")
    ("vmean" . "AVG")
    ("vmin" . "MIN")
    ("vsum" . "SUM")))

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
			       :lhs-parsed (org-ods-parse-field-range (plist-get tblfm :lhs))
			       :rhs-terms-parsed (cl-loop for (name . field-range) in terms
							  collect (cons name (org-ods-parse-field-range field-range))))))))))

(defun org-ods->derelativize-field-range (tinfo field-range)
  ;; (org-ods-message (list 'org-ods->derelativize-field-range
  ;;       		 :field-range field-range
  ;;       		 ;; :tinfo
  ;;       		 ;; (org-ods-glimpse-table tinfo t)
  ;;       		 ))
  (pcase-let ((`(,first . ,second) field-range))
    (cons (org-ods-derelativize-field tinfo first)
	  (org-ods-derelativize-field tinfo second 'secondp))))

(defun org-tblfm->cell-and-ods-formula (tinfo tblfm)
  (org-ods-message (list 'org-tblfm->cell-and-ods-formula
			 :table (org-ods-glimpse-table (plist-get tinfo :table) 'quick)
			 :tblfm tblfm))
  (let* ((lhs-parsed (plist-get tblfm :lhs-parsed))
	 (rhs-terms-parsed (plist-get tblfm :rhs-terms-parsed))
	 (lhs-cell-addresses
	  (org-ods-table-field-range->formula-cell-addresses
	   tinfo (org-ods->derelativize-field-range tinfo lhs-parsed))))
    (cl-loop do (org-ods-message (list 'org-tblfm->cell-and-ods-formula
				       :lhs-cell-addresses lhs-cell-addresses))
	     for cell-address in lhs-cell-addresses collect
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
						  cell-address (org-ods-derelativize-field tinfo first))
						 (org-ods-apply-field-spec-to-cell-address
						  cell-address (org-ods-derelativize-field tinfo second 'secondp)))
					   (plist-get tinfo :cell-address-adjust))))))
		(format "=%s" (org-ods-substitute-vars-in-expression (append org-ods-calc-f->ods-f-alist
									     terms)
								     (plist-get tblfm :rhs2)))
		;; (format "%s=%s" (org-ods-encode-cell-address (pcase-let ((`(,r . ,c) cell-address))
		;; 					       (cons r (+ c
		;; 							  (if has-special-column-p -1 0)))))
		;; 	(org-ods-substitute-vars-in-expression terms (plist-get tblfm :rhs2)))
		)))))

(defun org-ods-cell-address/org->table-cell (tinfo cell-address/org)
  (pcase-let* ((table-rows (plist-get tinfo :nonhrules))
	       (`(,r . ,c) cell-address/org)
	       (table-row (nth (1- r) table-rows))
	       (table-cells (org-element-contents table-row)))
    (nth (1- c) table-cells)))

(defun org-ods-clear-special-rows (tinfo)
  (org-ods-message (list 'org-ods-clear-special-rows))
  (cl-loop for table-row in (plist-get tinfo :special-rows)
	   do (cl-loop for table-cell in (org-element-contents table-row)
		       do (message "contents: %S" (org-ods-table-cell-element-to-lisp table-cell))
		       ;; do (pause (org-ods-table-cell-element-to-lisp table-cell))
		       do (setcdr (cdr table-cell) (list "")))))

(defun org-ods-remove-special-column (tinfo)
  (when (plist-get tinfo :has-special-column)
    (cl-loop with table = (plist-get tinfo :table)
	     for table-row in (org-element-contents table)
	     for contents = (org-element-contents table-row)
	     do (setcdr (cdr table-row) (cdr contents)))))

(defun org-ods-cell-address/org->value (tinfo cell-address/org value)
  (cl-assert (stringp value))
  (pcase-let* ((table-cell (org-ods-cell-address/org->table-cell tinfo cell-address/org)))
    (message "setting table-cell: %S" table-cell)
    ;; (org-ods-message (list 'org-ods-cell-address/org->value
    ;;                        :cell-address/org cell-address/org
    ;;                        :value value
    ;;                        :table-cell (substring-no-properties (org-element-interpret-data table-cell))))
    ;; (debug)
    (setcdr (cdr table-cell) (list value))))

(defun org-ods-insert-ods-formula (tinfo)
  ;; (org-ods-message (list 'org-ods-insert-ods-formula
  ;;       		 :tinfo (org-ods-glimpse-table tinfo t)))
  ;; Modifies table by side-effects
  (cl-loop for tblfm in (org-ods-tokenize-tblfms (org-ods-table->tblfms tinfo))
	   for final = (cons tblfm (org-tblfm->cell-and-ods-formula tinfo tblfm))
	   do (org-ods-message (list 'org-ods-insert-ods-formula
				     :final final))
	   do (cl-loop for (cell-address formula) in (cdr final)
		       do (org-ods-message (list 'org-ods-insert-ods-formula
						 :cell-address cell-address
						 :formula formula))
		       ;; for table-cell = (assoc-default cell-address
		       ;;                                 ;; (plist-get tinfo :cell-address->table-cell)
		       ;;                                 )
		       for table-cell = (org-ods-cell-address/org->table-cell tinfo cell-address)
		       do (unless table-cell
			    (error (concat (format "Table cell at address %S is empty." cell-address)
					   "Create that row / column by re-executing the TBLFM")))
		       (plist-put (cadr table-cell) :ods-formula
			          (org-ods-substitute-vars-in-expression
			           (list (cons "," org-ods-formula-arg-separator))
			           formula))
		       (setcdr (cdr table-cell) (list formula)))))

(defun org-ods-table->ods-table (table)
  (when-let* ((debug-buf (get-buffer "*Org Ods Debug*")))
    (with-current-buffer debug-buf
      (erase-buffer)))
  (let* ((tinfo (org-ods-table->table-info table))
	 (replacement-values (org-ods-special-cell-address/org->value tinfo)))
    (org-ods-message (list 'org-ods-table->ods-table
			   :tinfo (org-ods-glimpse-table tinfo t)))
    (org-ods-insert-ods-formula tinfo)
    (org-ods-clear-special-rows tinfo)
    (cl-loop for (cell-address/org . value) in replacement-values
	     do (org-ods-cell-address/org->value tinfo cell-address/org value))
    (org-ods-remove-special-column tinfo))
  (org-ods-lisp-table-to-org-table (org-ods-table-element-to-lisp-table table)))

;;; Field and Field Ranges

(defun org-ods-do-parse-field-range ()
  (with-peg-rules
      (
       (ABS-N (substring (+ [digit]))
	      `(it -- (string-to-number it)))
       (SIGN (substring (or "-" "+"))
	     `(it -- (string-to-number (concat it "1"))))
       (REL-N (and SIGN ABS-N
		   `(sign n -- (* sign n))))
       (OPTIONAL-REL-N (or (and REL-N
				`(it -- it))
			   (and (substring "")
				`(_it -- nil))))
       (Is (substring (+ "I"))
	   `(it -- (length it)))
       (LEFT-ARROWS (substring (+ "<")))
       (RIGHT-ARROWS (substring (+ ">")))
       (ROW-PART (and "@"
		      (or
		       (and REL-N
			    `(it -- (list :row-num 'self :row-offset it)))
		       (and ABS-N
			    `(it
			      -- (list :row-num it)))
		       (and SIGN Is OPTIONAL-REL-N
			    `(sign num-is rel-n-maybe -- (org-combine-plists
							  (list :row-num 'self
								:row-offset-hline
								(list 'hline (* sign num-is)))
							  (when rel-n-maybe
							    (list :row-offset rel-n-maybe)))))
		       (and Is OPTIONAL-REL-N
			    `(num-is rel-n
				     -- (org-combine-plists
					 (list :row-num (list 'hline num-is))
					 (when rel-n
					   (list :row-offset rel-n)))))
		       (and LEFT-ARROWS
			    `(it -- (list :row-num 'first
					  :row-offset (1- (length it)))))
		       (and RIGHT-ARROWS
			    `(it -- (list :row-num 'last
					  :row-offset (1- (length it))))))))
       (COL-PART
	(and "$" (or (and REL-N
			  `(it -- (list :col-num 'self :col-offset it)))
		     (and ABS-N
			  `(it
			    -- (list :col-num it)))
		     (and LEFT-ARROWS
			  `(it -- (list :col-num 'first
					:col-offset (1- (length it)))))
		     (and RIGHT-ARROWS
			  `(it -- (list :col-num 'last
					:col-offset (1- (length it))))))))
       (OPTIONAL-ROW-PART (or (and ROW-PART
				   `(it -- it))
			      (and (substring "")
				   `(_it -- nil))))
       (OPTIONAL-COL-PART (or (and COL-PART
				   `(it -- it))
			      (and (substring "")
				   `(_it -- nil))))
       (FIELD (or (and ROW-PART OPTIONAL-COL-PART
		       `(r-part col-part -- (org-combine-plists
					     r-part col-part)))
		  (and COL-PART
		       `(col-part -- col-part))))
       (START-FIELD FIELD)
       (END-FIELD FIELD)
       (OPTIONAL-END-FIELD
	(and (or (and ".." END-FIELD)
		 (and (substring "")
		      `(_it -- nil)))))
       (FIELD-RANGE (and START-FIELD OPTIONAL-END-FIELD
			 `(start end -- (cons start end))))
       (FIELD-RANGE-STRICT (and bob FIELD-RANGE eob
				`(it -- it))))
    (condition-case err
	(car (peg-parse FIELD-RANGE-STRICT))
      (peg-search-failed
       (error "org-ods-parse-field-range: `%s' does NOT parse as a FIELD or a FIELD-RANGE => (%S)"
	      (buffer-substring-no-properties (point-min) (point-max))
	      err)))))

(defun org-ods-parse-field-range (string)
  (with-temp-buffer
    (save-excursion
      (insert string))
    (org-ods-do-parse-field-range)))

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

(defvar org-ods--test-refs-parsed
  ;; (thread-last org-ods--test-refs
  ;;              (seq-map
  ;;       	(lambda (it)
  ;;       	  (cons it (org-ods-parse-field-range it)))))
  '(("@2$3" (:row-num 2 :col-num 3))
    ("$5" (:col-num 5))
    ("@2" (:row-num 2))
    ("@-1$-3" (:row-num self :row-offset -1 :col-num self :col-offset -3))
    ("@-I$2" (:row-num self :row-offset-hline (hline -1) :col-num 2))
    ("@>$5" (:row-num last :row-offset 0 :col-num 5))
    ("$1..$3" (:col-num 1) :col-num 3)
    ("$<<<..$>>" (:col-num first :col-offset 2) :col-num last :col-offset
     1)
    ("@2$1..@4$3" (:row-num 2 :col-num 1) :row-num 4 :col-num 3)
    ("@-1$-2..@-1"
     (:row-num self :row-offset -1 :col-num self :col-offset -2) :row-num
     self :row-offset -1)
    ("@I..@II" (:row-num (hline 1)) :row-num (hline 2))))

(defun org-ods-derelativize-field (tinfo field &optional secondp)
  ;; Modifies field by side-effect.  Also returns it.
  (pcase-let ((`(,rmin . ,cmin) '(1 . 1))
	      (`(,rmax . ,cmax) (plist-get tinfo :dimensions)))
    ;; (org-ods-message (list 'org-ods-derelativize-field-in
    ;;     		   :field field
    ;;     		   :secondp secondp
    ;;     		   :mins (cons rmin cmin)
    ;;     		   :maxs (cons rmax cmax)
    ;;     		   :rgn->r/org (plist-get tinfo :rgn->r/org)
    ;;     		   ;; :tinfo
    ;;     		   ;; tinfo
    ;;     		   ;; :tinfo
    ;;     		   ;; (org-ods-glimpse-table-at-point tinfo)
    ;;     		   ))
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
	(let ((base-r
               ;; When hline is the first in the field range, then
               ;; look for the first row in its rowgroup.
               ;;
               ;; When hline is the second in the field range, then
               ;; look for the last row in previous rowgroup.
	       (let* ((this-rgn n)
                      (previous-rgn (- n 1))
                      (which-rgn (if secondp  previous-rgn this-rgn))
		      (~r/orgs
		       (thread-last (plist-get tinfo :rgn->r/org)
				    (seq-keep
				     (pcase-lambda (`(,~rgn . ,~r/org))
				       (when (= ~rgn which-rgn)
					 ~r/org))))
		       ;; (assoc-default n (plist-get tinfo :rgn->r/org))
		       ))
		 (if secondp (car (last ~r/orgs)) (car ~r/orgs))))
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
  ;; (org-ods-message (list 'org-ods-derelativize-field-out
  ;;       		 :field field))
  field)

(defun org-ods-apply-field-spec-to-cell-address (cell-address field)
  (when field
    (pcase-let ((`(,r . ,c) cell-address))
      (cons
       ;; Row part
       (pcase (plist-get field :row-num)
	 ('self
	  ;; (:row-full "@-II" :row-num self :row-offset-hline (hline -2))
	  (pcase (plist-get field :row-offset-hline)
	    (`(hline ,(and (pred numberp) n))
	     n
	     (error "Not supported yet.  File a feature request."))
	    ('()
	     (+ r (plist-get field :row-offset)))))
	 ((and (pred numberp) n) n)
	 ('() r))
       ;; Col part
       (pcase (plist-get field :col-num)
	 ('self
	  (+ c (plist-get field :col-offset)))
	 ((and (pred numberp) n) n)
	 ('() c))))))

(defun org-ods-get-cell-addresess-matching-field (address-min address-max field)
  (org-ods-message (list 'org-ods-get-cell-addresess-matching-field
			 :address-min address-min
			 :address-max address-max
			 :field field))
  (unless field
    (error "This shouldn't happen? field: %S" field))
  (when field
    (pcase-let ((`(,rmin . ,cmin) address-min)
		(`(,rmax . ,cmax) address-max))
      (cl-loop for r in (pcase (plist-get field :row-num)
			  ('()
			   (number-sequence rmin rmax))
			  ((and (pred numberp) n)
			   (list n))
			  (_ (error "FIXME :row-num")))
	       append (cl-loop for c in (pcase (plist-get field :col-num)
					  ('()
					   (number-sequence cmin cmax))
					  ((and (pred numberp) n)
					   (list n))
					  (_ (error "FIXME :col-num")))
			       collect (cons r c))))))

(defun org-ods-get-cell-addresses-in-rectangle (rect)
  (org-ods-message (list 'org-ods-get-cell-addresses-in-rectangle
			 :rect rect))
  (pcase-let* ((`((,x1 . ,y1) . (,x2 . ,y2)) rect))
    (cl-loop for r in (number-sequence x1 x2)
	     append (cl-loop for c in (number-sequence y1 y2)
			     collect (cons r c)))))

(defun org-ods-tinfo->rectangle (tinfo)
  (pcase-let* ((`(,rmin . ,cmin) '(1 . 1))
	       (`(,rmax . ,cmax) (plist-get tinfo :dimensions)))
    (cons (cons rmin cmin) (cons rmax cmax))))

(defun org-ods-first-formula-row (tinfo)
  (org-ods-get-table-row->row-number/org
   (cl-loop with leaders = '()
	    with rowgroups = (plist-get tinfo :rowgroups)
	    for (_hrule-maybe . rest) in rowgroups
	    when rest
                counting rest into count
                and do (push (car rest) leaders)
	    end
	    and when (= count 2) return (car leaders)
	    finally return (car leaders))))

(defun org-ods-tinfo->formula-rectangle (tinfo)
  (pcase-let* ((table (plist-get tinfo :table))
	       (has-special-column-p (org-export-table-has-special-column-p table))
	       (`(,rmin . ,cmin) (cons
				  (org-ods-first-formula-row tinfo)
				  (if has-special-column-p 2 1)))
	       (`(,rmax . ,cmax) (plist-get tinfo :dimensions)))
    (cons (cons rmin cmin) (cons rmax cmax))))

(defun org-ods-get-common-rectangle (rect1 rect2)
  (org-ods-message (list 'org-ods-get-common-rectangle
			 :rect1 rect1
			 :rect2 rect2))
  (pcase-let* ((`((,r1x1 . ,r1y1) . (,r1x2 . ,r1y2)) rect1)
	       (`((,r2x1 . ,r2y1) . (,r2x2 . ,r2y2)) rect2)
	       (r3x1 (max r1x1 r2x1))
	       (r3x2 (min r1x2 r2x2))
	       (r3y1 (max r1y1 r2y1))
	       (r3y2 (min r1y2 r2y2))
	       (rect3 (cons (cons r3x1 r3y1)
			    (cons r3x2 r3y2))))
    (org-ods-message (list 'org-ods-get-common-rectangle
			   :rect3 rect3))
    rect3))

(defun org-ods-do-table-solitary-field->rectangle (formula-rect field)
  (org-ods-message (list 'org-ods-do-table-solitary-field->rectangle
			 :formula-rect formula-rect
			 :field field))
  (pcase-let* ((`((,rmin . ,cmin) . (,rmax . ,cmax)) formula-rect)
	       (cell-address (cons (plist-get field :row-num)
				   (plist-get field :col-num)))
	       (field-rect
		(pcase cell-address
		  (
		   ;; Both r and c are specified
		   `(,(and (pred numberp) r) . ,(and (pred numberp) c))
		   (cons (cons r c) (cons r c)))
		  ;; Only r is specified; c is wildcard.
		  (`(,(and (pred numberp) r) . nil)
		   (cons (cons r cmin) (cons r cmax)))
		  ;; Only c is specified; r is wildcard.
		  (`(nil . ,(and (pred numberp) c))
		   (cons (cons rmin c) (cons rmax c)))
		  ;; Both r and c are wildcards
		  (_
		   (error "org-ods-do-table-solitary-field->rectangle. This shouldn't happen")))))
    (org-ods-get-common-rectangle formula-rect field-rect)))

(defun org-ods-do-table-field-range->rectangle (formula-rect field &optional secondp)
  (org-ods-message (list 'org-ods-do-table-field-range->rectangle
			 :formula-rect formula-rect
			 :field field
			 :secondp secondp))
  (pcase-let* ((`((,rmin . ,cmin) . (,rmax . ,cmax)) formula-rect)
	       (cell-address (cons (plist-get field :row-num)
				   (plist-get field :col-num)))
	       (field-rect
		(cond
		 ((null secondp)
		  (pcase cell-address
		    (
		     ;; Both r and c are specified
		     `(,(and (pred numberp) r) . ,(and (pred numberp) c))
		     (cons (cons r c) (cons rmax cmax)))
		    ;; Only r is specified; c is wildcard.
		    (`(,(and (pred numberp) r) . nil)
		     (cons (cons r cmin) (cons rmax cmax)))
		    ;; Only c is specified; r is wildcard.
		    (`(nil . ,(and (pred numberp) c))
		     (cons (cons rmin c) (cons rmax cmax)))
		    ;; Both r and c are wildcards
		    (_
		     (error "org-ods-table-field->rectangle: This shouldn't happen"))))
		 (secondp
		  (pcase cell-address
		    (
		     ;; Both r and c are specified
		     `(,(and (pred numberp) r) . ,(and (pred numberp) c))
		     (cons (cons rmin cmin) (cons r c)))
		    ;; Only r is specified; c is wildcard.
		    (`(,(and (pred numberp) r) . nil)
		     (cons (cons rmin cmin) (cons r cmax)))
		    ;; Only c is specified; r is wildcard.
		    (`(nil . ,(and (pred numberp) c))
		     (cons (cons rmin cmin) (cons rmax c)))
		    ;; Both r and c are wildcards
		    (_
		     (error "org-ods-table-field->rectangle: This shouldn't happen")))))))
    (org-ods-get-common-rectangle formula-rect field-rect)))

(defun org-ods-table-field-range->formula-cell-addresses (tinfo field-range)
  (pcase-let* ((formula-rect (org-ods-tinfo->formula-rectangle tinfo))
	       (_dummy (org-ods-message (list 'org-ods-table-field-range->formula-cell-addresses
					      :formula-rect formula-rect
					      :field-range field-range)))
	       (`(,field-from . ,field-to) field-range)
	       (field-rect
		(cond
		 (field-to
		  (let* ((rect-from (org-ods-do-table-field-range->rectangle formula-rect field-from))
			 (rect-to (org-ods-do-table-field-range->rectangle formula-rect field-to 'second)))
		    (org-ods-get-common-rectangle rect-from rect-to)))
		 (t (org-ods-do-table-solitary-field->rectangle formula-rect field-from)))))
    (org-ods-get-cell-addresses-in-rectangle field-rect)))

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

(defmacro org-ods--with-environment (env &rest body)
  (declare (indent 1))
  `(let ((env (progn ,env)))
     (cond
      (env
       (let* ((process-environment (copy-sequence process-environment)))
	 (dolist (elem env)
	   (when elem
	     (setenv (car elem) (cadr elem))))
	 ,@body))
      (t ,@body))))

;;;###autoload
(defun org-ods-convert (&optional org-table out-file out-fmt open env)
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
     (list s out-file out-fmt current-prefix-arg
	   (ignore-errors (car (alist-get 'TABLE_CONVERTER_ENV (org-export--list-bound-variables)))))))
  (when-let*
      ((org-table org-table)
       (tmp-in-csv-file (make-temp-file "org-ods-" nil ".csv" (orgtbl-to-csv
							       (org-ods-table-as org-table 'lisp)
							       nil)))
       (tmp-out-ods-file (cond
			  ((string= out-fmt "csv") tmp-in-csv-file)
			  (t
			   (org-ods--with-environment env
			     (org-odt-do-convert tmp-in-csv-file out-fmt (not 'open)))))))
    (when tmp-out-ods-file
      (message "Exported to %s" tmp-out-ods-file)
      (message "Copying %s to %s" tmp-out-ods-file out-file)
      (copy-file tmp-out-ods-file out-file t))
    (when open
      (message "Opening %s..." out-file)
      (org-open-file out-file 'system))))

;;;###autoload
(defun org-ods-import-spreadsheet-file (&optional spreadsheet-file file-format)
  (interactive
   (list (thread-last (read-file-name "ODS-like filename: " nil nil
				      t nil
				      (lambda (file-name)
					(thread-last "Spreadsheet"
						     (map-elt org-odt-convert-capabilities)
						     car
						     (pcase--flip seq-difference '("csv"))
						     ;; ("ods" "ots" "xls" "xlsx")
						     (member (file-name-extension file-name)))))
		      expand-file-name)
	 (let* ((choices '(("Data only" . data-only)
			   ("Data and Spreadsheet Formula" . data+formula)))
		(choice (completing-read "Options for Importing: " choices nil t)))
	   (map-elt choices choice))))
  (let* ((out-file-extension (format (pcase file-format
				       (`data-only ".tsv")
				       (`data+formula ".formula.tsv"))))
	 (org-odt-transform-processes
	  `(("Convert ODS file to TSV file" "libreofficedev24.2" "--norestore" "--invisible" "--headless"
	     ,(format "macro:///OrgMode.Utilities.%s(%%I)"
		      (pcase file-format
			(`data-only "ODSExportToTSV")
			(`data+formula "ODSExportToFormulaTSV")))))))
    (org-odt-transform spreadsheet-file)
    (thread-last spreadsheet-file
		 file-name-directory
		 (funcall (lambda (dir)
			    (directory-files dir t
					     ;; Name of the exported file =
					     ;;     Name of the ODS file + "--"
					     ;;     + Name of the Sheet + ".tsv"
					     (rx-to-string `(and ,(file-name-nondirectory spreadsheet-file)
								 "--" (zero-or-more any)
								 ,out-file-extension
								 eos)))))
		 (pcase--flip sort 'string>)
		 (seq-do
		  (lambda (it)
		    (when-let* (((file-readable-p it))
				((string-match (rx-to-string `(and bos
								   ,spreadsheet-file "--"
								   (group-n 1 (zero-or-more any))
								   ,out-file-extension
								   eos))
					       it))
				(sheet-name (match-string 1 it)))
		      ;; When the command is invoked on an `org-mode'
		      ;; buffer, import the TSV file at point.  Set
		      ;; `+NAME: ' attribute of the imported table to
		      ;; the source sheet name.
		      (when (derived-mode-p 'org-mode)
			(org-table-import it '(16))
			(unless (eq 'table (org-element-type (org-element-context)))
			  (goto-char (org-table-begin)))
			(save-excursion
			  (insert "\n\n" (format "#+name: %s" sheet-name) "\n")))))))))

(defvar org-ods-cell-mapper
  (defun org-ods-replace-org-ts-with-locale-ts-in-string (string)
    (message "LANGUAGE is %S" current-locale-environment)
    (with-locale-environment (getenv "LANG")
      (message "LANGUAGE is %S" current-locale-environment)
      (org-quote-csv-field
       (replace-regexp-in-string
	org-ts-regexp-both
	(lambda (time-string)
	  (format-time-string
	   (let ((hours? (string-match-p "[0-9]+:[0-9]+" time-string)))
	     (funcall (if hours? #'cdr #'car) '("%x" . "%X")))
	   (apply #'encode-time (save-match-data (org-parse-time-string time-string)))))
	string)))))

;;;###autoload
(defun org-ods-translate (&optional full-table-data)
  (interactive (list (org-ods-table-at-point 'full-data)))
  (unless full-table-data
    (user-error "Not at a Table"))
  (let* ((converter-env (car (alist-get 'TABLE_CONVERTER_ENV (org-export--list-bound-variables)))))
    (message "CONVERTER_ENV: %S" converter-env)
    (org-ods--with-environment converter-env
      (pcase-let ((`(,table-element ,preamble ,_postamble) full-table-data))
	(goto-char (org-element-property :end table-element))
	(save-excursion
	  (insert (concat
		   "\n\n"
		   preamble
		   (org-ods-table->ods-table table-element)
		   ;; postamble
		   "\n\n")))
	(re-search-forward org-table-any-line-regexp)))))

;;; Unit Testing

(eval-and-compile
  (defun s->table (s)
    (org-element-map
	(with-temp-buffer
	  (insert s)
	  (org-element-parse-buffer))
	'table #'identity nil t)))

;;; Dynamic Block

(defun org-ods-collect-tables ()
  (org-with-wide-buffer
   (let* ((data (org-element-parse-buffer))
	  (tables (org-element-map data 'table
		    (lambda (table)
		      (when-let ((label (org-element-property :name table)))
			(cons label table)))))
	  (p (point)))
     (sort tables
	   (lambda (a b)
	     (< (min (abs (- p (org-element-property :begin (cdr a))))
		     (abs (- p (org-element-property :end (cdr a)))))
		(min (abs (- p (org-element-property :begin (cdr b))))
		     (abs (- p (org-element-property :end (cdr b)))))))))))

;;;###autoload
(defun org-ods-insert-dblock ()
  "Create a dynamic block for capturing the ODS view of a table."
  (interactive)
  (let ((label (completing-read
		"Enter Table's NAME: "
		(mapcar #'car (org-ods-collect-tables)) nil t)))
    (org-create-dblock
     (list :name "ods-table" :label label)))
  (org-update-dblock))

(org-dynamic-block-define "ods-table" #'org-ods-insert-dblock)

;;;###autoload
(defun org-dblock-write:ods-table (args)
  (apply #'org-ods-do-write-ods-table args))

(cl-defun org-ods-do-write-ods-table (&key label &allow-other-keys)
  "Fetch table named LABEL, and run `org-ods-translate' on it."
  (if-let* ((table (assoc-default label (org-ods-collect-tables))))
      (insert (org-ods-table->ods-table table))
    (user-error "No Table with NAME `%s'" (or label ""))))

;;; ODS Export backend

;;;; User Configuration Options

(defgroup org-export-ods nil
  "Options for exporting Org mode files to ODT."
  :tag "Org Export to ODS"
  :group 'org-export)

(defcustom org-ods-content-template-file nil
  "Template file for \"content.xml\".
The exporter embeds the exported content just before
\"</office:text>\" element.

If unspecified, the file named \"OrgOdtContentTemplate.xml\"
under `org-odt-styles-dir' is used."
  :type '(choice (const nil)
		 (file))
  :group 'org-export-ods)

(defcustom org-ods-styles-file nil
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
  :group 'org-export-ods
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

(defcustom org-ods-automatic-styles nil
  "Extra styles, an XML string.
The styles specified here are prepended to in-buffer styles
specified with the following keywords

    #+ODS_AUTOMATIC_STYLES: ...

       and

    #+ATTR_ODT: :target \"extra_styles\"
    #+begin_src nxml
    ...
    #+end_src
."
  :group 'org-export-ods
  :type
  '(choice
    (const :tag "None" nil)
    (string :tag "XML string")))

(defcustom org-ods-preferred-output-format nil
  "Automatically post-process to this format after exporting to \"ods\".

Command `org-ods-export-to-ods' exports first to \"ods\" format
and then uses `org-ods-convert-process' to convert the
resulting document to this format.  During customization of this
variable, the list of valid values are populated based on
`org-odt-convert-capabilities'.

You can set this option on per-file basis using file local
values.  See Info node `(emacs) File Variables'."
  :group 'org-export-ods
  :type '(choice :convert-widget
		 (lambda (w)
		   (apply 'widget-convert (widget-type w)
			  (eval (car (widget-get w :args)))))
		 `((const :tag "None" nil)
		   ,@(mapcar (lambda (c)
			       `(const :tag ,c ,c))
			     (org-odt-reachable-formats "odt")))))

(defcustom org-ods-convert-process "LibreOffice"
  "Use this converter to convert from \"ods\" format to other formats.
During customization, the list of converter names are populated
from `org-odt-convert-processes'."
  :group 'org-export-ods
  :type '(choice :convert-widget
		 (lambda (w)
		   (apply 'widget-convert (widget-type w)
			  (eval (car (widget-get w :args)))))
		 `((const :tag "None" nil)
		   ,@(mapcar (lambda (c)
			       `(const :tag ,(car c) ,(car c)))
			     org-odt-convert-processes))))

;;;###autoload
(put 'org-ods-preferred-output-format 'safe-local-variable 'stringp)

;;;; Filters

(defun org-ods--translate-tblfms-to-ods-formulae (data _backend info)
  (let ((pp-table
	 (lambda (table)
	   (concat "\n" (substring-no-properties
			 (org-element-interpret-data table))))))
    (org-element-map data 'table
      (lambda (table)
	(let* ((table-in (funcall pp-table table)))
	  (org-ods-message (list 'org-ods--translate-tblfms-to-ods-formulae)
			   (org-ods-debug--op-on-element "TRANSLATING" table))
	  (let* ((tinfo (org-ods-table->table-info table))
		 (replacement-values (org-ods-special-cell-address/org->value tinfo)))
	    (org-ods-message (list 'org-ods--translate-tblfms-to-ods-formulae
				   :tinfo (org-ods-glimpse-table tinfo t)
				   :replacement-values replacement-values))
	    (org-ods-insert-ods-formula tinfo)
	    (org-ods-clear-special-rows tinfo)
	    (cl-loop for (cell-address/org . value) in replacement-values
		     do (org-ods-cell-address/org->value tinfo cell-address/org value))
	    (org-ods-remove-special-column tinfo))
	  ;; Side-effects above has modified the table.  Log what the
	  ;; table looks after translation.
	  (let ((table-out (funcall pp-table
				    (org-ods-lisp-table-to-org-table
				     (org-ods-table-element-to-lisp-table table)))))
	    (org-ods-message (list 'org-ods--translate-tblfms-to-ods-formulae
				   :org-table table-in
				   :translated-table table-out)))))
      info nil nil t))
  (org-ods-message (list 'org-ods--translate-tblfms-to-ods-formulae
			 :odt-automatic-styles (plist-get info :odt-automatic-styles)
			 ;; :data data
			 ))
  data)

;;;; Transcoder

(defun org-ods-table (table contents info)
  "Return body of document string after ODT conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((table-contents (org-odt-table table contents info)))
    (org-ods-message (list 'org-ods-table)
		     (org-ods-debug--op-on-element "TRANSCODING" table))
    (prog1 table-contents
      (plist-put info :ods-tables
		 (concat (plist-get info :ods-tables)
			 table-contents)))))

(defvar org-ods-data-types
  '(formula date float))

(defun org-ods-table-cell (table-cell _contents info)
  ;; (org-ods-message (list 'org-ods-table-cell
  ;;       		 :ods-formula (org-element-property :ods-formula table-cell)
  ;;       		 ;; :el-contents (substring-no-properties (org-element-interpret-data table-cell))
  ;;       		 :el-properties ;; (cadr table-cell)
  ;;       		 )
  ;;       	   (org-ods-debug--op-on-element "TRANSCODING" table-cell))
  (let* ((el-contents (org-element-contents table-cell))
	 (content (car el-contents))
	 (rest (cdr el-contents)))
    (when (and (null rest) content)
      (cond
       ;; Formula
       ((stringp (org-element-property :ods-formula table-cell))
	(list :data-type 'formula
	      :attributes
              (format "table:formula=\"of:%s\""
                      (org-element-property :ods-formula table-cell))
	      :contents
	      ""))
       ;; Timestamp
       ((and (consp content)
	     (eq (org-element-type content) 'timestamp)
             (org-odt-timestamp-is-plain-p content))
	(when (plist-get (plist-get info :odt-timestamp-options) :emit-date-as-date-object)
          (list :data-type (if (org-timestamp-has-time-p content)
                               "OrgDateAndTime"
                             "OrgDate")
	        :attributes
	        (format "office:date-value=\"%s\" office:value-type=\"date\""
		        (org-odt--format-a-time-in-timestamp content 'iso))
	        :contents
	        "")))
       ;; Float
       ((stringp content)
	(when-let* ((number (odt-string-to-number content 'trim)))
	  (list :data-type 'float
		:attributes
		(format "office:value=\"%s\" office:value-type=\"float\""
			(number-to-string number))
		:contents
		"")))))))

;;;; ODS Command

(declare-function
 org-odt-export-to-odt-backend "ox-odt"
 (backend &optional async subtreep visible-only body-only ext-plist))

;;;###autoload
(defun org-ods-export-to-ods
    (&optional async subtreep visible-only body-only ext-plist)
  "Export table at point to a a OpenDocument Spreadsheet file.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

All other arguments--SUBTREEP, VISIBLE-ONLY and BODY-ONLY
arguments--are ignored.

The function returns a file name in any one of the BACKEND
format, `org-ods-preferred-output-format'."
  (interactive)
  (let* ((backend 'ods)
	 (org-ods-encode-cell-range-function 'org-ods-encode-cell-range-for-ods)
	 (org-ods-cell-mapper nil)
	 (org-odt-convert-process org-ods-convert-process))
    ;; Clear Debug Log
    (when-let* ((debug-buf (get-buffer "*Org Ods Debug*")))
      (with-current-buffer debug-buf
	(erase-buffer)))
    (prog1
	;; Export
	(org-odt-export-to-odt-backend backend async subtreep
				       visible-only body-only ext-plist)
      ;; Prettify Debug Log
      (when-let ((debug-buffer (get-buffer org-ods-debug-buffer)))
	(with-current-buffer debug-buffer
	  (emacs-lisp-mode)
	  (goto-char (point-min))
	  (save-excursion
	    (while (re-search-forward (rx "\\n") nil t)
	      (replace-match "\n" t t))))))))

;;;; Define Back-End

(org-export-define-derived-backend 'ods 'odt
  :translate-alist
  '((table . org-ods-table))
  :menu-entry
  '(?o "Export to ODT"
       ((?s "As ODS file" org-ods-export-to-ods)
        (?S "As ODS file and open"
            (lambda (a s v b)
              (if a (org-ods-export-to-ods t s v b)
                (org-open-file (org-ods-export-to-ods a s v b) 'system))))
        ;; (?x "As XML buffer" org-odt-export-as-odt)
        ))
  :options-alist
  '((:odt-preferred-output-format "ODS_PREFERRED_OUTPUT_FORMAT" nil org-ods-preferred-output-format t)
    (:odt-automatic-styles "ODS_AUTOMATIC_STYLES" nil org-ods-automatic-styles newline)
    (:odt-content-template-file "ODS_CONTENT_TEMPLATE_FILE" nil org-ods-content-template-file))
  :filters-alist '((:filter-parse-tree
                    . (org-ods--translate-tblfms-to-ods-formulae))))

(provide 'ox-ods)

;; Local Variables:
;; eval: (auto-fill-mode -1)
;; End:
