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

;;; DOM Conversions

(defun odt-dom:xml-string->dom (xml-string &optional remove-xmlns-attributes)
  (with-temp-buffer
    (insert (or xml-string ""))
    (when remove-xmlns-attributes
      (goto-char (point-min))
      (when (re-search-forward (rx "<office:document-styles") nil t)
	(delete-region (match-beginning 0)
		       (save-excursion
			 (goto-char (match-beginning 0))
			 (xmltok-forward)
			 (point)))
	(insert "<office:document-styles>"))
      (goto-char (point-min))
      (when (re-search-forward (rx "<office:document-content") nil t)
	(delete-region (match-beginning 0)
		       (save-excursion
			 (goto-char (match-beginning 0))
			 (xmltok-forward)
			 (point)))
	(insert "<office:document-content>")))
    (org-odt--xml-to-lisp (buffer-substring-no-properties (point-min) (point-max)))))

(defun odt-dom:dom->xml-string (prettifyp dom)
  (with-temp-buffer
    (insert (org-odt--lisp-to-xml dom nil prettifyp))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun odt-dom:file->dom (file-name &optional remove-xmlns-attributes)
  (odt-dom:xml-string->dom
   (with-temp-buffer
     (insert-file-contents file-name)
     (buffer-substring-no-properties (point-min) (point-max)))
   remove-xmlns-attributes))

(defun odt-dom:dom->file (file-name prettifyp dom)
  (with-temp-buffer
    (insert (org-odt--lisp-to-xml dom nil prettifyp))
    ;; (inspect "Tmp Styles file: 1")
    (write-region nil nil (or file-name
			      (make-temp-file "odt-rewritten-styles-" nil ".xml")))))

;;; DOM

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

(defun odt-dom:type->nodes (type dom)
  (odt-dom-map (lambda (node)
		 (when (eq type (odt-dom-type node))
		   node))
	       dom))

;;; Styles

;;;; Styles.xml <-> DOM

(defun odt-stylesdom:get-attrs (file-name type)
  (with-current-buffer (find-file-noselect file-name)
    (goto-char (point-min))
    (when (re-search-forward (rx-to-string `(and (group ,(format "<%s" type)
							(one-or-more space))))
			     nil t)
      (let ((attr-start (match-end 1)))
	(when (re-search-forward ">" nil t)
	  (let* ((attrs (buffer-substring-no-properties attr-start (1- (match-end 0))))
		 (attr-list (split-string attrs)))
	    (cl-loop for attr-and-value in attr-list
		     collect (pcase-let ((`(,attr ,value)
					  (split-string attr-and-value "=")))
			       (cons (intern attr) (read value))))))))))

(defun odt-stylesdom:file->dom (styles-file)
  (let* ((dom (odt-dom:file->dom styles-file 'remove-xmlns-attributes))
	 (attrs (odt-stylesdom:get-attrs styles-file 'office:document-styles))
	 (document-styles-node (car (odt-dom:type->nodes 'office:document-styles dom))))
    (prog1 document-styles-node
      (setcar (cdr document-styles-node)
	      attrs))))

(defun odt-stylesdom:dom->file (file-name prettifyp dom)
  (cl-assert (eq 'office:document-styles (odt-dom-type dom)))
  (let ((coding-system-for-write 'utf-8))
    (write-region (concat "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
			  (org-odt--lisp-to-xml dom nil prettifyp))
		  nil file-name
		  (not 'append)
		  (not 'visit)
		  (not 'lockname)
		  'mustbenew)))

(defun odt-stylesdom:dom->style-nodes (dom)
  (odt-dom-map
   (lambda (node)
     (when-let* ((style-name (odt-dom-property node 'style:name)))
       node))
   dom))

(defun odt-stylesdom:style-signature (node)
  (cl-assert (odt-dom-property node 'style:name))
  (list (odt-dom-type node)
	(odt-dom-property node 'style:name)
	(odt-dom-property node 'style:family)))

(defun odt-stylesdom:styles= (node1 node2)
  (let* ((result (equal (odt-stylesdom:style-signature node1)
			(odt-stylesdom:style-signature node2))))
    result))

(defun odt-stylesdom:trim-dom1 (dom1 dom2 &optional rewrite-dom2)
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
	     do (when rewrite-dom2
		  ;; Overwrite style2 with replacement
		  (setcar style2 (car shared-style1))
		  (setcar (cdr style2) (cadr shared-style1))
		  (setcdr (cdr style2) (cddr shared-style1)))
	     (dom-remove-node dom1 shared-style1))))

(provide 'odt)
;;; odt.el ends here
