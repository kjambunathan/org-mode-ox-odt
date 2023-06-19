;;;; odt --- Odt -*- lexical-binding: t; coding: utf-8-emacs; -*-

;; Copyright (C) 2022  Jambuanthan K

;; Author: Jambunathan K <kjambunathan@gmail.com>
;; Version:
;; Homepage: https://github.com/kjambunathan/dotemacs
;; Keywords:
;; Package-Requires: ((emacs "24"))

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

;;; Code:

(require 'dom)

;;;; XML String <-> DOM

(defun odt-xml-string-to-dom (xml-string)
  (when xml-string
    (with-temp-buffer
      (insert xml-string)
      (libxml-parse-xml-region (point-min) (point-max)))))

(defun odt-dom-to-xml-string (dom &optional depth prettify)
  (let* ((newline (if prettify "\n" ""))
	 (print-attributes
	  (lambda (attributes)
	    (mapconcat #'identity (cl-loop for (attribute . value) in attributes collect
					   (format "%s=\"%s\"" attribute value))
		       " "))))
    (setq depth (or depth 0))
    (cond
     ((null dom)
      "")
     ((stringp dom)
      dom)
     ((symbolp (car dom))
      (let* ((name (car dom))
	     (attributes (cadr dom))
	     (contents (cddr dom)))
	(let ((prefix (if prettify (make-string depth ? ) "")))
	  (cond
	   ((null contents)
	    (format "%s%s<%s %s/>"
		    newline
		    prefix name (funcall print-attributes attributes)))
	   ((eq 'comment name)
	    (format "%s%s<!-- %s -->"
		    newline
		    prefix
		    (if (stringp contents) contents
		      ;; (print-element contents (1+ depth))
		      (odt-dom-to-xml-string contents (1+ depth) prettify))))
	   (t
	    (format "%s%s<%s %s>%s%s%s</%s>"
		    newline
		    prefix
		    name
		    (funcall print-attributes attributes)
		    (if (stringp contents) contents
		      ;; (print-element contents (1+ depth))
		      (odt-dom-to-xml-string contents (1+ depth) prettify))
		    newline
		    prefix
		    name))))))
     (t
      (mapconcat #'identity
		 (cl-loop for el in dom collect
			  ;; (print-element el (1+ depth))
			  (odt-dom-to-xml-string el (1+ depth) prettify))
		 "")))))

;;;; XML Buffer/Region -> DOM

(defun odt-current-buffer-or-region-to-dom ()
  (let* ((beg (if (use-region-p)
		  (region-beginning)
		(point-min)))
	 (end (if (use-region-p)
		  (region-end)
		(point-max))))
    (libxml-parse-xml-region beg end)))

(defun odt-file-to-dom (file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (odt-current-buffer-or-region-to-dom)))

;;; DOM

;;;; DOM: Bare Essentials

(defun odt-dom-type (node)
  (when-let ((first (car-safe node))
	     ((symbolp first)))
    first))

(defalias 'odt-dom-node-p
  'odt-dom-type)

(defun odt-dom-properties (node)
  (when (odt-dom-node-p node)
    (cadr node)))

(defun odt-dom-property (node property)
  (cdr (assq property
	     (odt-dom-properties node))))

(defun odt-dom-contents (node)
  (when (odt-dom-node-p node)
    (cddr node)))

;;;; DOM: Query or Transform

(defun odt-dom-do-map (f composef dom)
  (when dom
    (cond
     ((consp dom)
      (funcall composef dom
	       (cl-loop for n in (odt-dom-contents dom)
			for val = (odt-dom-do-map f composef n)
			when val
			append val)))
     (t
      (funcall f dom)))))

(defun odt-dom-map (f dom)
  (cl-assert (odt-dom-node-p dom))
  (odt-dom-do-map f
		  (lambda (dom results)
		    (when (odt-dom-node-p dom)
		      (let ((val (funcall f dom)))
			(if val (append (list val)
					results)
			  results))))
		  dom))

;;;; DOM: Retrieve Nodes of specific type

(defun odt-dom:type->nodes (type dom)
  (odt-dom-map (lambda (node)
		 (when (eq type (odt-dom-type node))
		   node))
	       dom))

;;;; DOM: Retrieve a unique Node

(defun odt-dom:type->node (type dom)
  (let ((nodes (odt-dom:type->nodes type dom)))
    (when (cdr nodes)
      (error "Multiple nodes of type `%s' in DOM.  Refusing to return a unique node" type))
    (car nodes)))

;;; ODT DOM

;;;; File <-> DOM

(defun odt-dom:file->dom (file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (goto-char (point-min))
    (when (re-search-forward
	   (rx-to-string `(and "<"
			       (group
				(or ,@(mapcar #'symbol-name
					      '(office:document
						office:document-styles
						office:document-content
						office:document-meta))))
			       (group (or ""
					  (and space (one-or-more (not ">")))))
			       ">"))
	   nil 'noerror)
      (let* ((tag (intern (match-string 1)))
	     (attrs (prog1 (match-string 2)
		      (delete-region (match-beginning 2) (match-end 2))))
	     (dom (progn
		    (odt-current-buffer-or-region-to-dom)))
	     (subdom (car (odt-dom:type->nodes tag dom))))
	(prog1 subdom
	  (prog1 subdom
	    (setcar (cdr subdom)
		    (cl-loop for attr-and-value in (split-string attrs)
			     collect (pcase-let ((`(,attr ,value)
						  (split-string attr-and-value "=")))
				       (cons (intern attr) (when value
							     (read value))))))))))))

(defun odt-dom:dom->file (file-name prettifyp dom)
  (let ((coding-system-for-write 'utf-8))
    (write-region (concat "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
			  (odt-dom-to-xml-string dom nil prettifyp))
		  nil file-name)))

;;; Styles

;;;; DOM -> File

(defun odt-stylesdom:dom->file (file-name prettifyp dom)
  (cl-assert (memq (odt-dom-type dom) '(office:document-styles office:document-content)))
  (odt-dom:dom->file file-name prettifyp dom))

;;;; Get Style Nodes

(defun odt-stylesdom:dom->style-nodes (dom)
  (odt-dom-map
   (lambda (node)
     (when-let* ((style-name (odt-dom-property node 'style:name)))
       node))
   dom))

(defun odt-stylesdom:dom->style-names (dom)
  (odt-dom-map
   (lambda (node)
     (when-let* ((style-name (odt-dom-property node 'style:name)))
       style-name))
   dom))

;;;; Compare two Style nodes

(defun odt-stylesdom:style-signature (node)
  (cl-assert (odt-dom-property node 'style:name))
  (list (odt-dom-property node 'style:name)
	(odt-dom-property node 'style:family)
	(odt-dom-type node)))

(defun odt-stylesdom:styles= (node1 node2)
  (equal (odt-stylesdom:style-signature node1)
	 (odt-stylesdom:style-signature node2)))

;;;; Diff two Style Trees

(defun odt-stylesdom:trim-dom1 (dom1 dom2)
  ;; DOM1 <- DOM1 - DOM2;  MINUS here is Set Difference
  ;; DOM2 <- DOM2       ;  DOM2 is left untouched

  ;; Modification of DOM1 happens by side-effects. This function
  ;; doesn't return anything, and so it is assumed that the root node
  ;; of DOM1 stays intact;

  ;; Conceptually DOM1 is the low-priority "base" styles, and
  ;; DOM2 is the high-priority overlay.
  ;;
  ;; At the end of this call DOM1 and DOM2 are disjoint, and share no
  ;; styles between them.  Together they define the "effective" style.
  (when (and dom2 dom1)
    (cl-loop with styles2 = (odt-stylesdom:dom->style-nodes dom2)
	     with styles1 = (odt-stylesdom:dom->style-nodes dom1)
	     for style2 in styles2
	     for shared-style1 = (cl-some
				  (lambda (style1)
				    (when (odt-stylesdom:styles= style2 style1)
				      style1))
				  styles1)
	     when shared-style1
	     do (dom-remove-node dom1 shared-style1))))

;;;; Merge two Trees

(defun odt-stylesdom:dom->add-nodes-to (to nodes dom)
  (cl-assert dom)
  (prog1 dom
    (cl-loop with type = to
	     with edited-node = (odt-dom:type->node type dom)
	     for node in nodes
	     do (dom-append-child edited-node node))))

;;;; WIP-1

(defun odt-dom-values->parentvalues (&optional values value->V value->parentvalues V->values &rest env)
  (let* (G Vs sortedVs value)
    (while (setq value (pop values))
      (let* ((V (funcall value->V value)))
        (unless (member V (map-keys G))
          (let* ((parentvalues (funcall value->parentvalues value))
                 (parentVs (seq-map value->V parentvalues)))
            (setq values (append parentvalues values))
            (setq Vs (delete-dups (append parentVs Vs)))
            (push (cons V parentVs) G)))))
    G
    (while G
      (let* ((partition (seq-group-by (lambda (it)
                                        (null (null (cdr it))))
                                      G))
             (orphanedVs (map-keys (alist-get nil partition))))
        (setq G (alist-get t partition))
        (seq-map
         (lambda (it)
           (when (cdr it)
             (setcdr it (seq-difference (cdr it) sortedVs))))
         G)
        (setq sortedVs (append sortedVs orphanedVs))))
    sortedVs
    (seq-map (lambda (id)
               (apply V->values id env))
             sortedVs)))

;;;; WIP-2

(defvar let-dom-separator "%")

(defun let-dom--data->special-vars (data)
  (delete-dups
   (cond
    ((symbolp data)
     (let ((name (symbol-name data)))
       (cond
        ((string-match (rx-to-string `(and bos ,let-dom-separator)) name)
         (list data)))))
    ((vectorp data)
     (apply #'nconc (mapcar #'let-dom--data->special-vars data)))
    ((not (consp data)) nil)
    ((eq (car data) 'let-dom)
     (let-dom--data->special-vars (cadr data)))
    (t (append (let-dom--data->special-vars (car data))
               (let-dom--data->special-vars (cdr data)))))))

(defun let-dom--sym->valueform (sym expr)
  (let* ((this sym))
    (pcase this
      ('_self
       expr)
      ('_tag
       `(odt-dom-type ,expr))
      ('_properties
       `(odt-dom-properties ,expr))
      ((or '_children '_contents)
       `(odt-dom-contents ,expr))
      ('_text
       `(odt-dom-contents ,expr))
      ('_all
       `(odt-dom-map (lambda (dom)
                       (when (odt-dom-node-p dom)
                         dom))
                     ,expr))
      (_
       (cond
        ((string-prefix-p "//" (symbol-name this))
         `(odt-dom-map (lambda (dom)
                         (when (eq (odt-dom-type dom) ',(intern (substring (symbol-name this) 2)))
                           dom))
                       ,expr))
        ((string-prefix-p "/" (symbol-name this))
         `(odt-dom-map-nodes (let-dom ,expr
                                      %_children)
                             (eq %_tag ',(intern (substring (symbol-name this) 1)))))
        (t
         `(odt-dom-property ,expr ',sym)))))))

(defmacro let-dom-1 (dom &rest body)
  (declare (indent 1) (debug t))
  (cl-labels ((dottedsym->valueform (dottedsym-or-symnames expr)
                (message "%S" `(dottedsym->valueform :dottedsym ,dottedsym-or-symnames :expr ,expr))
                (let ((symnames (if (symbolp dottedsym-or-symnames)
                                    (cdr (split-string (symbol-name dottedsym-or-symnames) let-dom-separator))
                                  dottedsym-or-symnames)))
                  (cond
                   ((and (consp symnames)
                         (cdr symnames))
                    (let-dom--sym->valueform (intern (car (last symnames)))
                                             (dottedsym->valueform (butlast symnames) expr)))
                   ((and (consp symnames)
                         (null (cdr symnames)))
                    (let-dom--sym->valueform (intern (car symnames)) expr))
                   (t
                    (let-dom--sym->valueform (intern symnames) expr))))))
    (let* ((var (gensym "dom")))
      `(let ((,var ,dom))
         (let ,(seq-map
                (lambda (dottedsym)
                  (list dottedsym
                        (dottedsym->valueform dottedsym var)))
                (let-dom--data->special-vars body))
           ,@body)))))

(defmacro let-dom-2 (dom &rest body)
  (declare (indent 1) (debug t))
  (cl-labels ((dottedsym->parentdottedsyms (dottedsym)
                (let ((components (cdr (split-string (symbol-name dottedsym) let-dom-separator))))
                  (when (cdr components)
                    (list (intern (concat let-dom-separator (string-join (butlast components) let-dom-separator)))))))
              (symnames->valueform (dottedsym expr)
                (let ((symnames (cdr (split-string (symbol-name dottedsym) let-dom-separator))))
                  (cond
                   ((and (consp symnames)
                         (cdr symnames))
                    (let* ((prev (intern (string-join (butlast symnames) let-dom-separator)))
                           (@prev (intern (format "%s%s" let-dom-separator prev))))
                      (let-dom--sym->valueform (intern (car (last symnames))) @prev)))
                   ((and (consp symnames)
                         (null (cdr symnames)))
                    (let-dom--sym->valueform (intern (car symnames)) expr))
                   (t
                    (let-dom--sym->valueform (intern symnames) expr)))))
              (dottedsym->dottedsym-and-valueform (dottedsym expr)
                (list dottedsym
                      (symnames->valueform dottedsym expr))))
    (let ((var (gensym "dom")))
      `(let ((,var ,dom))
         (let* ,(odt-dom-values->parentvalues (let-dom--data->special-vars body)
                                              #'identity
                                              #'dottedsym->parentdottedsyms
                                              #'dottedsym->dottedsym-and-valueform
                                              var)

           ,@body)))))

(defalias 'let-dom 'let-dom-1)

(defmacro odt-dom-%-predicate-function (%-predicate &optional %-result)
  `(lambda (dom)
     (let-dom dom
              (when (progn ,%-predicate)
                (or ,%-result dom)))))

(defmacro odt-dom-map* (%-predicate %-result dom)
  `(odt-dom-map
    (odt-dom-%-predicate-function ,%-predicate ,%-result)
    ,dom))

(defmacro odt-dom-map-nodes (nodes %-predicate &optional %-result)
  `(seq-keep
    (odt-dom-%-predicate-function ,%-predicate ,%-result)
    ,nodes))

(defmacro odt-dom--step (dom expander predicate &optional result)
  (let* ((--dom-- (gensym "dom")))
    `(let ((,--dom-- ,dom))
       (odt-dom-map-nodes
        (let-dom ,--dom--
                 ,expander)
        ,predicate
        (let-dom ,--dom--
                 ,result)))))

(provide 'odt)
;;; odt.el ends here
