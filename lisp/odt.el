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
  (cddr node))

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

(provide 'odt)
;;; odt.el ends here
