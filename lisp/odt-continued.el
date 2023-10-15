;;;; odt-continued --- Odt Continued -*- lexical-binding: t; coding: utf-8-emacs; -*-

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

(require 'help-fns)
(require 'ox-odt)
(require 'rnc-mode)

;;; DOM

(defun odt-list:group-by-car (alist)
  (cl-loop for (key . value) in alist
	   for group = (assoc key groups)
	   for el = (or group (list key value))
	   unless (member value (cdr el))
	   do (setcdr el (cons value (cdr el)))
	   unless group
	   collect el into groups
	   finally return (cl-loop for (key . values) in groups
				   collect (cons key (odt-list:sort< values)))))

(defun odt-list:sort< (list &optional f)
  (let* ((f (or f #'identity))
	 (an-item (funcall f (car list)))
	 (comparator (cond ((numberp an-item) '<)
			   (t 'string<)))
	 (valuemapper (lambda (x)
			(let ((y (funcall f x)))
			  (cond
			   ((or (stringp y)
				(numberp y))
			    y)
			   (t (format "%S" y)))))))
    (sort list
	  (lambda (a b)
	    (funcall comparator
		     (funcall valuemapper a)
		     (funcall valuemapper b))))))

(defun odt-dom-following-siblings (node dom)
  (let* ((parent (dom-parent dom node))
	 (contents (dom-children parent)))
    (cdr (memq node contents))))

(defun odt-dom-preceding-siblings (node dom)
  (let* ((parent (dom-parent dom node))
	 (contents (dom-children parent)))
    (cdr (memq node (reverse contents)))))

(defun odt-dom-has-siblings-p (node dom)
  (or (odt-dom-following-siblings node dom)
      (odt-dom-preceding-siblings node dom)))

(defun odt-dom-is-sole-child (node dom)
  (not (odt-dom-has-siblings-p node dom)))

(defun odt-dom-is-first-child (node dom)
  (let* ((parent (dom-parent dom node))
	 (contents (dom-children parent)))
    (eq node (car contents))))

(defun odt-dom-is-second-child (node dom)
  (let* ((parent (dom-parent dom node))
	 (contents (dom-children parent)))
    (eq node (nth 1 contents))))

(defun odt-dom-has-only-one-child-p (_rng node)
  (= 1 (length (dom-children node))))

(defun odt-dom-search (dom predicate)
  (let ((matches (cl-loop for child in (dom-children dom)
			  for matches = (and (not (stringp child))
					     (odt-dom-search child predicate))
			  when matches
			  append matches)))
    (if (funcall predicate dom)
	(cons (funcall predicate dom) matches)
      matches)))

(defun odt-dom:remove-nodes-satisfying (f dom)
  (prog1 dom
    (cl-loop with nodes = (odt-dom-map
			   (lambda (node)
			     (funcall f node))
			   dom)
	     for node in nodes
	     do (dom-remove-node dom node))))

(defun odt-dom:remove-nodes (nodes dom)
  (prog1 dom
    (cl-loop for node in nodes
	     do (dom-remove-node dom node))))

;;; Tag

(defun odt-tag:tag->shorttag (tag)
  (let ((tag-name (format "%s" tag)))
    (intern
     (let ((i (string-match ":" tag-name)))
       (cond
	(i (substring tag-name (1+ i)))
	(t tag-name))))))

(defun odt-tag:tag->tag-and-shorttag (tags)
  (let* ((tag-and-shorttag
	  (cl-loop for tag in tags
		   collect (cons tag (odt-tag:tag->shorttag tag)))))
    (prog1 tag-and-shorttag
      (cl-assert
       (cl-loop for (tag . shorttag) in tag-and-shorttag
		collecting tag into fulls
		collecting shorttag into suffixes
		finally return (=
				(length fulls)
				(length
				 (delete-dups suffixes))))))))

;;; PP

(defun odt-pp (form &optional elp)
  (let* ((print-circle nil))
    (cond
     (elp
      (let* (;; (pp-use-max-width nil)
	     ;; (pp-max-width 72)
	     )
	(pp-emacs-lisp-code form)))
     (t
      (insert (pp-to-string form))))))

;;; RNG

(defun odt-rngdom:data-types (dom)
  (odt-dom-search dom
		  (lambda (n)
		    (when (and (eq 'define (dom-tag n))
			       (eq 'data (dom-tag (car (dom-children n)))))
		      (list
		       (dom-attr n 'name)
		       (apply #'append
			      (list :type (dom-attr (car (dom-children n)) 'type))
			      (odt-dom-search (car (dom-children n))
					      (lambda (n)
						(when (eq (dom-tag n) 'param)
						  (list (intern (format ":%s" (dom-attr n 'name)))
							(car (dom-children n))))))))))))

(defun odt-rngdom:define-data-node-p (node)
  (when (and (eq 'define (odt-dom-type node))
	     (eq 'data (odt-dom-type (car (odt-dom-contents node)))))
    node))

(defun odt-rngdom:remove-define-data (dom)
  (prog1 dom
    (cl-loop with nodes = (odt-dom-map
			   (lambda (node)
			     (odt-rngdom:define-data-node-p node))
			   dom)
	     for node in nodes
	     do (dom-remove-node dom node))))

(defun odt-rngdom:collect-data-types (dom)
  (odt-dom-map
   (lambda (node)
     (when (odt-rngdom:define-data-node-p node)
       (odt-dom-property node 'name)))
   dom))

(defun odt-rngdom:name->attribute-node (name dom)
  (odt-dom-search dom
		  (lambda (n)
		    (when (and (eq 'attribute (dom-tag n))
			       (string= name (dom-attr n 'name)))
		      n))))

(defun odt-rngdom:name->ref-node (name dom)
  (odt-dom-search dom
		  (lambda (n)
		    (when (and (eq 'ref (dom-tag n))
			       (string= name (dom-attr n 'name)))
		      n))))

(defun odt-rngdom:name->element-node (name dom)
  (odt-dom-search dom
		  (lambda (n)
		    (when (and (eq 'element (dom-tag n))
			       (string= name (dom-attr n 'name)))
		      n))))

(defun odt-rngdom:name->define-node (name dom)
  (let ((result (car (odt-dom-search dom
				     (lambda (n)
				       (when (and (eq 'define (dom-tag n))
						  (string= name (dom-attr n 'name)))
					 n))))))
    result))

(defun odt-rngdom:collect-name-of-ref-nodes (node)
  (delete-dups
   (cl-loop with nodes = (odt-dom:type->nodes 'ref node)
	    for node in nodes
	    collect (dom-attr node 'name))))

(defun odt-rngdom:ref-name->ref-names (ref-name-or-ref-names dom)
  (let* ((done '())
	 (pending (delete-dups
		   (if (consp ref-name-or-ref-names)
		       ref-name-or-ref-names
		     (list ref-name-or-ref-names))))
	 (this nil)
	 (final '()))
    (while (progn
	     (cl-assert (= (length pending)
			   (length (delete-dups pending))))
	     (cl-assert (null (cl-intersection pending done)))
	     (setq this (pop pending)))
      (cl-assert (null (member this done)))
      (let* ((to-collect (odt-rngdom:name->define-node this dom))
	     (more-to-collect (odt-rngdom:collect-name-of-ref-nodes
			       (odt-rngdom:name->define-node this dom))))
	(when to-collect
	  (push to-collect final))
	(push this done)
	(setq pending
	      (cl-set-difference (cl-union more-to-collect
					   pending
					   :test 'equal)
				 done :test 'equal)))
      (message "In odt-rngdom:ref-name->ref-names: %d names pending" (length pending)))
    final))

(defun odt-rngdom:remove-nodes-of-type (type dom)
  (prog1 dom
    (cl-loop with nodes = (odt-dom-map
			   (lambda (node)
			     (when (eq type (odt-dom-type node))
			       node))
			   dom)
	     for node in nodes
	     do (dom-remove-node dom node))))

(defun odt-rngdom:remove-description (dom)
  (odt-rngdom:remove-nodes-of-type 'description dom)
  ;; (prog1 dom
  ;;   (cl-loop with descs = (odt-dom-map
  ;;       		   (lambda (node)
  ;;       		     (when (eq (odt-dom-type node) 'description)
  ;;       		       node))
  ;;       		   dom)
  ;;            for desc in descs
  ;;            do (dom-remove-node dom desc)))
  )

(defun odt-rngdom:is-implicitly-parenthesized (dom node)
  (let* ((parent (dom-parent dom node))
	 (_contents (dom-children parent)))
    (or (and (memq (dom-tag parent) '(define start))
	     (odt-dom-is-sole-child node dom))
	;; Element has a name attribute, and this
	;; element fully expands to element's content
	(and (memq (dom-tag parent) '(element attribute))
	     (dom-attr parent 'name)
	     (odt-dom-is-sole-child node dom))
	;; Element doesn't have a name attribute, and this element
	;; provides the name
	(and (memq (dom-tag parent) '(element attribute))
	     (null (dom-attr parent 'name))
	     (odt-dom-is-first-child node dom))
	;; Element doesn't have a name attribute, and this element
	;; fully expands to the element type
	(and (memq (dom-tag parent) '(element attribute))
	     (null (dom-attr parent 'name))
	     (odt-dom-is-second-child node dom)
	     (null (odt-dom-following-siblings node dom))))))

(defun odt-rngdom:surrounds-element-attribute-or-ref (_dom node)
  (and (= 1 (length (dom-children node)))
       (memq (dom-tag (car (dom-children node))) '(ref element attribute data))))

(defun odt-rngdom:is-explicitly-parenthesized (_dom _node contents)
  (and (string-match-p (rx (and bos (zero-or-more (or space "\n")) (or "(" "{")))
		       contents)
       (string-match-p (rx (and (or ")" "}") eos))
		       contents)))

(defun odt-rngdom:do-rng->rnc (info node)
  ;; See (find-file (expand-file-name "schema/relaxng.rnc" data-directory))
  ;; https://relaxng.org/ns/structure/1.0
  ;; http://relaxng.org/relaxng.rng
  ;; ../testing/examples/odtxml/relaxng.rng
  ;; This specification describes a compact, non-XML syntax for [RELAX NG]
  ;; - (https://www.oasis-open.org/committees/relax-ng/compact-20021121.html#spec)
  ;; RELAX NG Compact Syntax Tutorial
  ;; - https://relaxng.org/compact-tutorial.html
  (let* ((dom (plist-get info :dom))
	 (use-newline (plist-get info :use-newline))
	 (newline (if use-newline "\n" " ")))
    (cond
     ((stringp node)
      node)
     ((eq 'anyName (odt-dom-type node))
      (format "%s"
	      " * "
	      ;; (cl-assert (null (dom-children node))
	      ;;   	 t
	      ;;   	 "type: %s" (odt-dom-type node))
	      ))
     ((eq 'attribute (odt-dom-type node))
      (let ((name (dom-attr node 'name)))
	(cond
	 (name
	  (format "attribute %s { %s }"
		  name
		  (prog1 (odt-rngdom:do-rng->rnc info (car (dom-children node)))
		    (cl-assert (= 1 (length (dom-children node)))
			       t
			       "type: %s" (odt-dom-type node)))))
	 (t
	  (format "attribute %s%s"
		  (odt-rngdom:do-rng->rnc info (car (dom-children node)))
		  (prog1 (format " { %s }"
				 (cond
				  ((cadr (dom-children node))
				   (odt-rngdom:do-rng->rnc info (cadr (dom-children node))))
				  (t "text")))
		    (cl-assert (or (null (cadr (dom-children node)))
				   (and (memq (dom-tag (cadr (dom-children node))) '(ref text))
					(= 2 (length (dom-children node)))))
			       t
			       "type: %s" (odt-dom-type node))))))))
     ((eq 'choice (odt-dom-type node))
      (format
       (if (odt-rngdom:is-implicitly-parenthesized dom node)
	   "%s"
	 "(%s)")
       (mapconcat (lambda (n)
		    (odt-rngdom:do-rng->rnc info n))
		  (dom-children node)
		  ;; (format "%s| " )
		  (format "%s| " newline)
		  ;; "\n| "
		  )))
     ((eq 'comment (odt-dom-type node))
      (format "%s%s%s%s%s"
	      newline
	      newline
	      (with-temp-buffer
		(rnc-mode)
		(insert (odt-rngdom:do-rng->rnc info (dom-children node)))
		(comment-region (point-min) (point-max))
		(buffer-substring-no-properties (point-min) (point-max)))
	      newline
	      newline))
     ((eq 'data (odt-dom-type node))
      (format "xsd:%s%s"
	      (dom-attr node 'type)
	      (or (when (dom-children node)
		    (format " {%s%s%s}"
			    newline
			    (odt-rngdom:do-rng->rnc info (dom-children node))
			    newline))
		  "")))
     ((eq 'define (odt-dom-type node))
      (format "\n\n%s =\n%s"
	      (dom-attr node 'name)
	      (mapconcat (lambda (n)
			   (odt-rngdom:do-rng->rnc info n))
			 (dom-children node)
			 (format ",%s" newline))))
     ((memq (odt-dom-type node) '(description dc:description))
      "")
     ((eq 'element (odt-dom-type node))
      (let ((name (dom-attr node 'name)))
	(cond
	 (name
	  (format "element %s { %s }"
		  (dom-attr node 'name)
		  (mapconcat (lambda (n)
			       (odt-rngdom:do-rng->rnc info n))
			     (dom-children node)
			     (format ",%s " newline))))
	 (t
	  (format "element %s%s"
		  (odt-rngdom:do-rng->rnc info (car (dom-children node)))
		  (format " {%s %s %s}"
			  newline
			  (or (when (cdr (dom-children node))
				(mapconcat (lambda (n)
					     (odt-rngdom:do-rng->rnc info n))
					   (cdr (dom-children node))
					   (format ",%s" newline)))
			      "")
			  newline))))))
     ((eq 'empty (odt-dom-type node))
      (format "%s"
	      (odt-dom-type node)))
     ((eq 'grammar (odt-dom-type node))
      (odt-rngdom:do-rng->rnc info (dom-children node)))
     ((eq 'group (odt-dom-type node))
      (format (if (or (odt-rngdom:is-implicitly-parenthesized dom node)
		      (odt-rngdom:surrounds-element-attribute-or-ref dom node))
		  "%s"
		"(%s)")
	      (mapconcat (lambda (n)
			   (odt-rngdom:do-rng->rnc info n))
			 (dom-children node)
			 (format ",%s" newline))))
     ((eq 'interleave (odt-dom-type node))
      (format (if (odt-rngdom:is-implicitly-parenthesized dom node)
		  "%s"
		"(%s)")
	      (mapconcat (lambda (n)
			   (odt-rngdom:do-rng->rnc info n))
			 (dom-children node)
			 (format "%s& " newline))))
     ((eq 'list (odt-dom-type node))
      (format "list { %s }"
	      (mapconcat (lambda (n)
			   (odt-rngdom:do-rng->rnc info n))
			 (dom-children node)
			 (format ",%s" newline))))
     ((eq 'oneOrMore (odt-dom-type node))
      (let ((contents (mapconcat (lambda (n)
				   (odt-rngdom:do-rng->rnc info n))
				 (dom-children node)
				 (format ",%s" newline))))
	(format (if (and (odt-dom-has-only-one-child-p dom node)
			 (or (odt-rngdom:is-implicitly-parenthesized dom node)
			     (odt-rngdom:surrounds-element-attribute-or-ref dom node)
			     (odt-rngdom:is-explicitly-parenthesized dom node contents)))
		    (format "%s%s+%s" newline contents newline)
		  (format "%s(%s%s%s)+%s"
			  newline newline
			  contents
			  newline newline)))))
     ((eq 'optional (odt-dom-type node))
      (let ((contents (mapconcat (lambda (n)
				   (odt-rngdom:do-rng->rnc info n))
				 (dom-children node)
				 (format ",%s" newline))))
	(format (if (or (odt-rngdom:surrounds-element-attribute-or-ref dom node)
			(odt-rngdom:is-explicitly-parenthesized dom node contents))
		    "%s?"
		  "(%s)?")
		contents)))
     ((eq 'param (odt-dom-type node))
      (format "%s  %s = \"%s\""
	      newline
	      (dom-attr node 'name)
	      (odt-rngdom:do-rng->rnc info (dom-children node))))
     ((eq 'ref (odt-dom-type node))
      (let ((name (dom-attr node 'name)))
	(cond
	 ((string= "string" name)
	  "\\string")
	 (t name))))
     ((eq 'value (odt-dom-type node))
      (format "\"%s\""
	      (prog1 (car (dom-children node))
		(cl-assert (= 1 (length (dom-children node)))
			   t
			   "type: %s" (odt-dom-type node)))))
     ((eq 'top (odt-dom-type node))
      (odt-rngdom:do-rng->rnc info (dom-children node)))
     ((eq 'zeroOrMore (odt-dom-type node))
      (let ((contents (mapconcat (lambda (n)
				   (odt-rngdom:do-rng->rnc info n))
				 (dom-children node)
				 (format ",%s" newline))))
	(format (if (or (odt-rngdom:surrounds-element-attribute-or-ref dom node)
			(odt-rngdom:is-explicitly-parenthesized dom node contents))
		    "%s*"
		  "(%s)*")
		(prog1 contents))))
     ((eq 'start (odt-dom-type node))
      (format "\n\n%s =\n%s"
	      (dom-tag node)
	      (mapconcat (lambda (n)
			   (odt-rngdom:do-rng->rnc info n))
			 (dom-children node)
			 (format ",%s" newline))))
     ((eq 'text (odt-dom-type node))
      (prog1 (format "%s"
		     (odt-dom-type node))
	(cl-assert (null (dom-children node)))))
     ((eq 'mixed (odt-dom-type node))
      (format "%s { %s }"
	      (odt-dom-type node)
	      (prog1 (odt-rngdom:do-rng->rnc info (car (dom-children node)))
		(cl-assert (= 1 (length (dom-children node)))
			   t
			   "type: %s" (odt-dom-type node)))))
     ((eq 'name (odt-dom-type node))
      (format "%s"
	      (prog1 (odt-rngdom:do-rng->rnc info (car (dom-children node)))
		(cl-assert (= 1 (length (dom-children node)))
			   t
			   "type: %s" (odt-dom-type node)))))
     ((odt-dom-type node)
      (error "You aren't handling %S" (odt-dom-type node)))
     (t
      (mapconcat (lambda (n)
		   (odt-rngdom:do-rng->rnc info n))
		 node
		 "")))))

(defun odt-rngdom:rng->rnc (&optional rnc-file-name rng-file-name-or-rng-nodes)
  (interactive)
  (let* ((rng-file-name-or-rng-nodes
	  (or rng-file-name-or-rng-nodes
	      (expand-file-name "odf1.2/OpenDocument-v1.2-os-schema.rng" org-odt-schema-dir)))
	 (rng-nodes (when (consp rng-file-name-or-rng-nodes)
		      rng-file-name-or-rng-nodes))
	 (rng-file-name
	  (when (stringp rng-file-name-or-rng-nodes)
	    rng-file-name-or-rng-nodes))
	 (rng-nodes (or rng-nodes
			(list (odt-file-to-dom rng-file-name (not 'strip-comment-nodes-p)))))
	 (info (list :use-newline nil
		     :dom rng-nodes))
	 (rnc-file-dir
	  (concat (file-name-parent-directory org-odt-lib-dir)
		  "testing/examples/odtxml/"))
	 (rnc-file-name (cond
			 (rnc-file-name
			  rnc-file-name)
			 (rng-file-name
			  (expand-file-name
			   (format "my-%s.rnc" (file-name-sans-extension (file-name-nondirectory rng-file-name)))
			   rnc-file-dir))
			 (t
			  (expand-file-name
			   (format "my-%s.rnc" "tmp")
			   rnc-file-dir)))))
    (when (file-exists-p rnc-file-name)
      (delete-file rnc-file-name))
    (with-current-buffer (find-file-noselect rnc-file-name t)
      (erase-buffer)
      (pop-to-buffer (current-buffer))
      (cl-loop for n1 in rng-nodes
	       for n = (odt-rngdom:remove-description n1)
	       do (insert "\n\n" (odt-rngdom:do-rng->rnc info n)))
      (rnc-mode)
      (indent-region (point-min) (point-max))
      ;; (my-before-save-hook)
      ;; (call-interactively 'normalize-buffer)
      (save-buffer 0)
      (goto-char (point-min)))))

(defun odt-rngdom:normalize-space ()
  (interactive)
  (cl-loop for (regex . repl) in `((,(rx (and (group (or "{" "(")) (one-or-more space))) . "\\1")
				   (,(rx (and (one-or-more space) (group (or "}" ")" ",")))) . "\\1"))
	   do (goto-char (point-min))
	   (while (re-search-forward regex nil t)
	     ;; (pause "test")
	     (replace-match repl t nil))))

(defalias 'odt-rngdom:normalize-definition
  (kmacro "
C-u C-s ^ [ ^ SPC ] . * = <return>
C-r C-q C-j <return> C-f
C-SPC
C-s = <return> C-u C-s ^ [ ^ SPC ] . * = <return>
C-r C-q C-j <return> C-f
C-x n n
<escape> <
C-M-% C-q C-j <return> SPC <return> !
<escape> <
C-M-% [ SPC C-q TAB C-e + <return> SPC <return> !
<escape> <
M-x o d t - r n g d o m : n o r m a l i z e - s p a c e <return>
<escape> > C-q C-j C-q C-j
C-x n w
C-x C-s
"))

(defun odt-rngdom:normalize-rnc-buffer ()
  (interactive)
  (while (not (eobp))
    (odt-rngdom:normalize-definition)))

;;;; RNG Variables

(defvar odt-rngdom:odf-v1.2-os-schema
  (odt-file-to-dom
   (expand-file-name "odf1.2/OpenDocument-v1.2-os-schema.rng" org-odt-schema-dir)
   (not 'strip-comment-nodes-p)))

(defvar odt-rngdom:odf-v1.2-os-schema-data-types
  (odt-rngdom:collect-data-types odt-rngdom:odf-v1.2-os-schema))

;;;; RNG Tests

(odt-rngdom:ref-name->ref-names "style-graphic-properties-content-strict"
				odt-rngdom:odf-v1.2-os-schema)

;; (odt-rngdom:rng->rnc)

;; (odt-rngdom:rng->rnc
;;  (concat (file-name-parent-directory org-odt-lib-dir)
;; 	 "testing/examples/odtxml/broken.rnc")
;;  (mapcar #'odt-rngdom:name->define-node
;; 	 '(
;; 	   "mathMarkup"
;; 	   "string"
;; 	   "anyAttListOrElements"
;; 	   "anyElements")))

;; (odt-rngdom:name->define-node "style-graphic-properties-content-strict")

;; (odt-rngdom:ref-name->ref-names "style-graphic-properties-content-strict")

;; (setq target
;;       (odt-rngdom:ref-name->ref-names
;;        (odt-rngdom:do-collect-names-of-ref-nodes
;; 	(odt-rngdom:name->element-node "style:graphic-properties"))))

;; (odt-rngdom:data-types odt-rngdom:odf-v1.2-os-schema)

;;; Styles

(defun odt-stylesdom:style-name->nodes (dom style-name)
  (odt-dom-map
   (lambda (node)
     (when-let (((odt-dom-node-p node))
		((string= style-name
			  (or (odt-dom-property node 'style:name) ""))))
       node))
   dom))

;; (mapcar #'odt-stylesdom:style-signature
;;         (odt-stylesdom:style-name->nodes
;;          (odt-dom:file->dom (buffer-file-name))
;;          "OrgOutline"))

;; ((text:outline-style "OrgOutline" nil)
;;  (text:list-style "OrgOutline" nil))

(defun odt-stylesdom:style-signature->node (dom style-name)
  (odt-dom-map
   (lambda (node)
     (when-let (((odt-dom-node-p node))
		((string= style-name
			  (or (odt-dom-property node 'style:name) ""))))
       node))
   dom))

(defun odt-stylesdom:style-name->node (dom style-name)
  (let* ((nodes (odt-stylesdom:style-name->nodes dom style-name)))
    (cond ((cdr nodes)
	   (error "Multiple definitions for style-name"))
	  (t (car nodes)))))

(defun odt-stylesdom:get-style-names (dom)
  "Return list of ((ELEMENT . STYLE-FAMILY) . STYLE-NAME). "
  (odt-dom-map
   (lambda (node)
     (when-let ((style-name (odt-dom-property node 'style:name)))
       (cons (if-let ((family (odt-dom-property node 'style:family)))
		 (cons (dom-tag node)
		       (intern family))
	       (dom-tag node))
	     style-name)))
   dom))

(defun odt-dom:dom->prop-and-value->nodes (attribute-tag value dom)
  "Return list of ((ELEMENT . STYLE-FAMILY) . STYLE-NAME). "
  (odt-dom-map
   (lambda (node)
     (when-let* ((v (odt-dom-property node attribute-tag)))
       (when (string= v value)
	 node)))
   dom))

;; (odt-dom:collect-nodes-with-property 'style:name org-odt-styles-dom)

;; (odt-stylesdom:get-char-style-definitions org-odt-styles-dom)

(defun odt-stylesdom:dom->style-names (dom)
  (odt-list:sort<
   (odt-dom-map
    (lambda (node)
      (when-let* ((style-name (odt-dom-property node 'style:name)))
	style-name))
    dom)))

(defun odt-stylesdom:remove-style-names (style-names dom)
  (odt-dom:remove-nodes-satisfying
   (lambda (node)
     (when-let* ((style-name (odt-dom-property node 'style:name))
		 ((member style-name style-names)))
       node))
   dom))

(defun odt-stylesdom:get-char-style-definitions (dom)
  (odt-dom-map
   (lambda (node)
     (when-let ((style-name (odt-dom-property node 'style:name))
		(style-family (odt-dom-property node 'style:family))
		((string= style-family "text")))
       style-name))
   dom))

(defun odt-stylesdom:types->nodes (types dom)
  (unless (consp types)
    (setq types (list types)))
  (odt-dom-map
   (lambda (node)
     (when (and (memq (odt-dom-type node) types))
       node))
   dom))

(defun odt-stylesdom:attribute->parent-nodes (attr dom)
  (odt-dom-map
   (lambda (node)
     (when (and (odt-dom-type node)
		(odt-dom-property node attr))
       node))
   dom))

(defun odt-stylesdom:attribute-tag-values-alist (type dom)
  (odt-list:sort<
   (cl-loop for (attr . values) in
	    (odt-list:group-by-car
	     (apply #'append
		    (odt-dom-map
		     (lambda (node)
		       (when (eq (odt-dom-type node) type)
			 (odt-dom-properties node)))
		     dom)))
	    collect (cons attr
			  (odt-list:sort< values)))
   'car))

(defun odt-stylesdom:type->attribute-tags (type dom)
  (odt-list:sort<
   (delete-dups
    (apply #'append
	   (odt-dom-map
	    (lambda (node)
	      (when (eq (odt-dom-type node) type)
		(mapcar #'odt-dom-type (odt-dom-properties node))))
	    dom)))))

(defun odt-stylesdom:type->parent-tags (type dom)
  (delete-dups
   (odt-dom-map
    (lambda (node)
      (when-let ((ntype (odt-dom-node-p node)))
	(cl-loop for x in (odt-dom-contents node)
		 when (eq (odt-dom-type x) type)
		 return ntype)))
    dom)))

(defun odt-stylesdom:type->child-tags (type dom)
  (odt-list:sort<
   (delete-dups (apply #'append
		       (odt-dom-map
			(lambda (node)
			  (when (and (eq (odt-dom-type node) type)
				     ;; (string= (odt-dom-property node 'family) "paragraph")
				     )
			    (cl-loop for it in (odt-dom-contents node)
				     collect (if (atom it) 'LITERAL (car it)))))
			dom)))))

(defun odt-stylesdom:inspect-type (type dom)
  "TYPE is an ELEMENT tag."
  ;; FIXME: Allow inspecting of TYPE, where TYPE is an ATTRIBUTE tag
  (list
   :parent-tags (odt-stylesdom:type->parent-tags type dom)
   :attributes (odt-stylesdom:type->attribute-tags type dom)
   :child-tags
   (odt-stylesdom:type->child-tags type dom)))

;;;; Styles Variables

(defvar org-odt-styles-dom
  (odt-dom:file->dom
   (expand-file-name "OrgOdtStyles.xml" org-odt-styles-dir)
   (not 'strip-comment-nodes-p)))

;;;; Styles Test

(odt-tag:tag->tag-and-shorttag
 (plist-get (odt-stylesdom:inspect-type 'style:style
					org-odt-styles-dom)
	    :attributes))

;; Get a overview of all styles defined in a styles.xml
(odt-list:sort<
 (odt-list:group-by-car
  (odt-stylesdom:get-style-names org-odt-styles-dom))
 #'car)

(odt-stylesdom:get-char-style-definitions org-odt-styles-dom)

(odt-stylesdom:attribute->parent-nodes 'style:name org-odt-styles-dom)

(odt-stylesdom:style-name->node org-odt-styles-dom "Text_20_body")
(odt-stylesdom:style-name->node org-odt-styles-dom "A4PortraitLayout")

(odt-stylesdom:types->nodes 'style:style org-odt-styles-dom)

(odt-stylesdom:attribute-tag-values-alist 'style:paragraph-properties org-odt-styles-dom)

(odt-stylesdom:type->attribute-tags 'style:paragraph-properties org-odt-styles-dom)
(odt-stylesdom:type->attribute-tags 'style:style org-odt-styles-dom)

(odt-stylesdom:type->child-tags 'style:graphic-properties org-odt-styles-dom)

;;; Contents

(defun odt-contentsdom:do-node->string (node contents)
  (cl-assert (stringp contents))
  (message "\n\n\n[%s] %S" (odt-dom-node-p node) contents)
  (pcase (odt-dom-node-p node)
    (`text:p
     (format "\n\n#+ATTR_ODT: :style \"%s\"\n%s"
	     (odt-dom-property node 'text:style-name)
	     contents))
    (`text:span
     (format "*%s*"
	     contents))
    (_
     (message "Not handling %s" (odt-dom-node-p node))
     contents)))

(defun odt-contentsdom:dom->string (dom)
  (when dom
    (cond
     ((stringp dom)
      dom)
     ((null dom)
      "")
     (t
      (odt-contentsdom:do-node->string dom
				       (when (consp dom)
					 (mapconcat 'odt-contentsdom:dom->string
						    (odt-dom-contents dom)
						    "")))))))

(defun odt-contentsdom:contents.xml->org-file (&optional contents-file-name)
  (setq contents-file-name (or contents-file-name
			       (concat (file-name-parent-directory org-odt-lib-dir)
				       "testing/examples/odtxml/gnulinuxmagazine/gnulinuxmagazine/content.xml")))
  (with-current-buffer (get-buffer-create "*org*")
    (org-mode)
    (erase-buffer)
    (pop-to-buffer (current-buffer))
    (call-interactively 'set-mark-command)
    (insert (odt-contentsdom:dom->string
             (odt-dom:file->dom contents-file-name (not 'strip-comment-nodes-p))))
    (call-interactively 'org-fill-paragraph)
    (call-interactively 'set-mark-command)))

(defun odt-contentsdom:dom->text:style-names (dom)
  (odt-dom-map
   (lambda (node)
     (when-let ((style-name (odt-dom-property node 'text:style-name)))
       style-name))
   dom))

;;;; Contents Constants

(defvar org-odt-content-template-dom
  (odt-dom:file->dom
   (expand-file-name "OrgOdtContentTemplate.xml" org-odt-styles-dir)
   (not 'strip-comment-nodes-p)))

;;;; Contents Test

(odt-list:sort<
 (delete-dups
  (odt-contentsdom:dom->text:style-names
   (odt-dom:file->dom
    (concat (file-name-parent-directory org-odt-lib-dir)
	    "testing/examples/odtxml/gnulinuxmagazine/gnulinuxmagazine/content.xml")
    (not 'strip-comment-nodes-p)))))

;;; Elfile

(defun odt-elfile:erase-code (out-file)
  (cond
   ((not (file-exists-p out-file))
    (write-region "" nil out-file
		  (not 'append) (not 'visit) (not 'lockname)
		  'mustbenew))
   (t
    (with-current-buffer (find-file-noselect out-file t)
      (let ((code-start (progn
			  (goto-char (point-min))
			  (if (re-search-forward (rx (and bol ";;; Code:\n\n")) nil 'noerror)
			      (point)
			    (point-min))))
	    (code-end (progn
			(goto-char (point-max))
			(if (re-search-backward (rx-to-string `(and "\n\n" ,(format "%S" '(provide 'odt-structs))))
						nil 'noerror)
			    (point)
			  (point-max)))))
	(org-with-wide-buffer
	 (delete-region code-start code-end)
	 (goto-char code-start)))))))

(defun odt-elfile:add-tag-and-forms (el-file tag-and-forms op)
  (with-current-buffer (find-file-noselect el-file t)
    (let* ((pos (pcase op
		  (`append
		   (goto-char (point-max))
		   (if (re-search-backward
			(rx-to-string
			 `(and "\n\n" ,(format "%S"
					       `(provide ',(intern
							    (file-name-sans-extension
							     (file-name-nondirectory el-file)))))))
			nil 'noerror)
		       (point)
		     (point-max)))
		  (`prepend
		   (goto-char (point-min))
		   (if (re-search-forward (rx (and bol ";;; Code:\n\n")) nil 'noerror)
		       (point)
		     (point-min))))))
      (org-with-wide-buffer
       (goto-char pos)
       (pcase-let* ((`(,tag . ,forms) tag-and-forms))
	 (insert "\n\n;;; " (format "%s" (or tag "")) "\n")
	 (cl-loop for form in forms
		  do (insert "\n") (insert (pp-to-string form))))
       (save-buffer 0)))))

;;; Structs

(defun odt-struct:instance->dom (instance)
  (let ((struct-name (aref instance 0)))
    (list struct-name
	  (cl-loop for (slot-name _) in (cl-struct-slot-info struct-name)
		   unless (eq slot-name 'cl-tag-slot)
		   for val = (cl-struct-slot-value struct-name slot-name instance)
		   when val
		   collect (cons slot-name val)))))

(defun odt-struct:attribute->elfile-fragment (attr
					      _default-value
					      default-value-for-sample-usage
					      dom)
  (let* ((print-gensym nil)
	 (out-file (expand-file-name "odt-structs.el" org-odt-lib-dir))
	 (attrs (mapcar #'car (odt-stylesdom:attribute-tag-values-alist attr dom)))
	 (attrs1 (odt-tag:tag->tag-and-shorttag attrs))
	 (struct-type (intern (format "odt-%s-attributes" attr)))
	 (constructor (intern (format "odt-make-%s-attributes" attr)))
	 (constructor-usagevar (intern (format "%s--usage" constructor)))
	 (_constructor-docvar (intern (format "%s-docstring" constructor)))
	 (struct
	  `(cl-defstruct
	       (
		,struct-type
		(:constructor ,constructor
			      (&key
			       ,@(cl-loop for (_full . short) in attrs1
					  ;; collect
					  ;; (list short (assoc-default full default-value))
					  collect short)
			       &aux
			       ,@(cl-loop for (full . short) in attrs1
					  collect (list full short)))))
	     ,@attrs))
	 (example-usage
	  `(,constructor
	    ,@(cl-loop for (attr . value) in default-value-for-sample-usage
		       appending (list (intern (format ":%s" (alist-get attr attrs1)))
				       value))))
	 (csig
	  (progn
	    (unintern constructor obarray)
	    (eval struct)
	    (prog1
		(with-temp-buffer
		  (let ((f constructor))
		    (message "f is %S" f)
		    (help-fns--signature f
					 (documentation f t)
					 (symbol-function f)
					 (symbol-function f) nil))
		  (emacs-lisp-mode)
		  (indent-region (point-min) (point-max))
		  (buffer-substring-no-properties (point-min) (point-max)))))))
    (with-current-buffer (find-file-noselect out-file t)
      (emacs-lisp-mode)
      ;; Insert `cl-defstruct'
      (insert "\n\n")
      (insert (format ";;;; %s" attr))
      (insert "\n\n")
      (odt-pp struct)
      (insert "\n\n")
      ;; Insert example usage.
      (odt-pp `(defvar ,constructor-usagevar ',example-usage))
      (insert "\n\n")
      ;; Augement constructor with `function-documentation'
      (odt-pp `(put ',constructor 'function-documentation))
      ;; Position the docstring where the docstring goes
      (skip-chars-backward "\n")
      (backward-char 1)
      ;; Add double quotes for docstring.  Cursor after opening
      ;; quotes.
      (insert "\n\"")
      (save-excursion
	(insert "\n\""))
      (when csig
	(progn
	  ;; (goto-char (point-min))
	  (insert
	   ;; Open the docstring
	   (format "\nConstructor for objects of type `%s'." struct-type))
	  (insert "\n\n")
	  (insert csig)
	  (widen))
	(progn
	  (insert "\n\nUsage:")
	  (narrow-to-region (point) (point))
	  (let ((start (point)))
	    (insert "\n\n" (pp example-usage))
	    (goto-char (point-min))
	    (when (re-search-forward " " nil t)
	      (while (re-search-forward ":" nil t)
		(save-excursion
		  (goto-char (1- (match-beginning 0)))
		  (newline))))
	    (indent-region start (point))
	    ;; Escape the double quotes around literal strings
	    (replace-regexp-in-region "\"" "\\\\\"" start (point-max)))
	  (goto-char (point-max))
	  (widen)))
      (re-search-forward (rx (and ")")))
      ;; (forward-line 1)
      (save-buffer 0)
      (pop-to-buffer (current-buffer)))))

;;;; Structs Test

;; (require 'odt-structs)

;; (insert
;;  "\n\n"
;;  (odt-dom-to-xml
;;   (odt-struct:instance->dom
;;    (eval odt-make-style:page-layout-properties-attributes--usage))))

;; (odt-struct:attribute->elfile-fragment 'style:style default-value default-value org-odt-styles-dom)

(defun odt-struct:unit-test ()
  (progn
    ;; Erase code
    (odt-elfile:erase-code (expand-file-name "odt-structs.el" org-odt-lib-dir))
    ;; Item 1
    (odt-struct:attribute->elfile-fragment
     'style:style
     nil
     (odt-dom-properties (odt-stylesdom:style-name->node
			  org-odt-styles-dom
			  "Text_20_body"))
     org-odt-styles-dom)
    ;; Item 2
    (let ((props (odt-dom-properties
		  (car (odt-dom-search
			(odt-stylesdom:style-name->node org-odt-styles-dom "A4PortraitLayout")
			(lambda (n)
			  (when (eq (dom-tag n) 'style:page-layout-properties)
			    n)))))))
      (odt-struct:attribute->elfile-fragment
       'style:page-layout-properties
       nil
       props
       org-odt-styles-dom))))

;; (odt-struct:unit-test)

;;; Class

(defclass odt-element nil nil
  "ODT Element.")

(defun odt-class:class-desc->defclass (tag props elements)
  `(defclass ,tag (odt-element)
     ,(append
       (cl-loop for (full . short) in (odt-tag:tag->tag-and-shorttag props)
		collect (list full
			      :initarg (intern (format ":%s" short))
			      :initform nil
			      :type '(or null string)))
       (cl-loop for (full . short) in (odt-tag:tag->tag-and-shorttag elements)
		collect (list full
			      :initarg (intern (format ":%s" short))
			      :initform nil
			      :type '(or null odt-element))))))

(defun odt-class:instance->defclass (&optional instance)
  (let* ((pending-instances (if (odt-dom-type instance)
				(list instance)
			      instance))
	 (done-elements '())
	 (this nil)
	 (final '()))
    (while (setq this (pop pending-instances))
      (unless (memq (dom-tag this) done-elements)
	(push (odt-class:class-desc->defclass (dom-tag this)
					      (mapcar #'car (dom-attributes this))
					      (mapcar #'car (dom-children this)))
	      final)
	(cl-pushnew (odt-dom-type this) done-elements)
	(setq pending-instances (append pending-instances (dom-children instance)))))
    `(progn ,@final)))

(defun odt-class:instance->dom (obj)
  "Return DOM for OBJ."
  (cl-loop with slot-names = (mapcar #'eieio-slot-descriptor-name
				     (eieio-class-slots (eieio-object-class obj))
				     ;; (eieio-object-class obj)
				     )
	   for slot-name in slot-names
	   for val = (slot-value obj slot-name)
	   when val
	   if (stringp val)
	   collecting (cons slot-name val) into attributes
	   else if (child-of-class-p (eieio-object-class val) 'odt-element)
	   collecting (odt-class:instance->dom val) into elements
	   else do (cl-assert nil)
	   end
	   finally return `(,(eieio-object-class obj)
			    ,attributes
			    ,@elements)))

(defun odt-class:type->defclass (type-or-types)
  (let* ((pending-types (delete-dups
			 (cond ((consp type-or-types)
				type-or-types)
			       (t (list type-or-types)))))
	 (done-elements '())
	 (this nil)
	 (final '()))
    (while (setq this (pop pending-types))
      (let ((inspected (odt-stylesdom:inspect-type this org-odt-styles-dom)))
	(unless (memq this done-elements)
	  (push (cons this (odt-class:class-desc->defclass this
							   (plist-get inspected :attributes)
							   (plist-get inspected :child-tags)))
		final)
	  (cl-pushnew this done-elements)
	  (setq pending-types
		(cl-set-difference (cl-union pending-types
					     (plist-get inspected :child-tags)
					     :test 'string=)
				   done-elements
				   :test 'string=)))))
    final))

(defun odt-class:instance->usage (instance)
  (car
   (odt-dom-map
    (lambda (instance)
      `(,(dom-tag instance)
	,@(cl-loop for (full . value) in (odt-dom-properties instance)
		   for field = (odt-tag:tag->shorttag full)
		   for key = (intern (format ":%s" field))
		   appending (list key value))
	,@(cl-loop for child in (dom-children instance)
		   for full = (dom-tag child)
		   for field = (odt-tag:tag->shorttag full)
		   for key = (intern (format ":%s" field))
		   appending `(,key ,(odt-class:instance->usage child)))))
    instance)))

(defun odt-class:type-instance-alist->defclasses (type-instance-alist)
  (let* ((types (mapcar #'car type-instance-alist))
	 (type-defclass-alist (odt-class:type->defclass types)))
    (cl-loop for (type . defclass) in type-defclass-alist
	     for usages = (cl-loop for (type1 . instance) in type-instance-alist
				   when (eq type type1)
				   when instance
				   collect (odt-class:instance->usage instance))
	     collect `(,type ,defclass ,@usages))))

(defun odt-class:type-forms-alist->elfile (type-forms-alist &optional file-name)
  (let ((out-file (or file-name
		      (expand-file-name "odt-classes.el" org-odt-lib-dir))))
    (unless (file-exists-p out-file)
      (write-region "" nil out-file))
    (with-current-buffer (find-file-noselect out-file t)
      (erase-buffer)
      (pop-to-buffer (current-buffer))
      (cl-loop for (type defclass . forms) in type-forms-alist
	       ;; (odt-class:type->defclass type-forms-alist)
	       do (insert (format "\n\n;;;; %s" type))
	       (insert "\n\n") (odt-pp defclass)
	       when forms do
	       (insert (format "\n\n;;;;; `%s' Usage" type))
	       (cl-loop for form in forms
			do (insert "\n\n") (odt-pp form)))
      (save-buffer 0)
      (pop-to-buffer (current-buffer)))))

(defun odt-class:unit-test ()
  (odt-class:type-forms-alist->elfile
   (odt-class:type-instance-alist->defclasses
    `((style:style . ,(odt-stylesdom:style-name->node org-odt-styles-dom "Bold"))
      (style:style . ,(odt-stylesdom:style-name->node org-odt-styles-dom "Text_20_body"))
      (style:page-layout . ,(odt-stylesdom:style-name->node org-odt-styles-dom "A4PortraitLayout")))))
  (odt-elfile:add-tag-and-forms (expand-file-name "odt-classes.el" org-odt-lib-dir)
				'(Code: . ((require 'eieio))) 'prepend)
  (odt-elfile:add-tag-and-forms (expand-file-name "odt-classes.el" org-odt-lib-dir)
				'(Provides . ((provide 'odt-classes))) 'append))

;;;; Class Test

(odt-class:unit-test)

;; ox-odt-xml.el.el ends here

;;;

(defun odt-rngdom-new:is-implicitly-parenthesized (dom node)
  (let* ((parent (dom-parent dom node))
	 (_contents (dom-children parent)))
    (or (and (memq (dom-tag parent) '(define start))
	     (odt-dom-is-sole-child node dom))
	;; Element has a name attribute, and this
	;; element fully expands to element's content
	(and (memq (dom-tag parent) '(element attribute))
	     (dom-attr parent 'name)
	     (odt-dom-is-sole-child node dom))
	;; Element doesn't have a name attribute, and this element
	;; provides the name
	(and (memq (dom-tag parent) '(element attribute))
	     (null (dom-attr parent 'name))
	     (odt-dom-is-first-child node dom))
	;; Element doesn't have a name attribute, and this element
	;; fully expands to the element type
	(and (memq (dom-tag parent) '(element attribute))
	     (null (dom-attr parent 'name))
	     (odt-dom-is-second-child node dom)
	     (null (odt-dom-following-siblings node dom))))))

(defun odt-rngdom-new:surrounds-element-attribute-or-ref (_dom node)
  (and (= 1 (length (dom-children node)))
       (memq (dom-tag (car (dom-children node))) '(ref element attribute data))))

(defun odt-rngdom-new:is-explicitly-parenthesized (_dom _node contents)
  (and (string-match-p (rx (and bos (zero-or-more (or space "\n")) (or "(" "{")))
		       contents)
       (string-match-p (rx (and (or ")" "}") eos))
		       contents)))

(defun odt-rngdom-new:do-rng->rnc (info node)
  (message "node: %S" node)
  (let* ((dom (plist-get info :dom)))
    (cond
     ((stringp node)
      node)
     ((eq 'anyName (odt-dom-type node))
      (format "%s"
	      " * "))
     ((eq 'attribute (odt-dom-type node))
      (let ((name (dom-attr node 'name)))
	(cond
	 (name
	  (list 'attribute
		:name name
		;; :type (odt-rngdom-new:do-rng->rnc info (car (dom-children node)))
		))
	 (t
	  (list 'attribute
		:name (odt-rngdom-new:do-rng->rnc info (car (dom-children node)))
		;; :type (cond
		;;        ((cadr (dom-children node))
		;; 	(odt-rngdom-new:do-rng->rnc info (cadr (dom-children node))))
		;;        (t "text"))
		)))))
     ((eq 'choice (odt-dom-type node))
      (list 'choice (mapcar (lambda (n)
			      (odt-rngdom-new:do-rng->rnc info n))
			    (dom-children node)))
      ;; (format
      ;;  ;; (if (odt-rngdom-new:is-implicitly-parenthesized dom node) "%s" "(%s)")
      ;;  )
      )
     ((eq 'comment (odt-dom-type node)))
     ((eq 'data (odt-dom-type node))
      ;; (list 'data
      ;;       :type (format "xsd:%s" (dom-attr node 'type))
      ;;       :extra ;; (when-let* ((children (dom-children node))
      ;;       ;;             (result (odt-rngdom-new:do-rng->rnc info children)))
      ;;       ;;   (if (null (cdr result))
      ;;       ;;       (car result)
      ;;       ;;     result))
      ;;       (apply #'append (odt-rngdom-new:do-rng->rnc info (dom-children node))))
      )
     ((eq 'define (odt-dom-type node))
      (list 'define
	    :name (dom-attr node 'name)
	    :value (mapcar (lambda (n)
			     (odt-rngdom-new:do-rng->rnc info n))
			   (dom-children node)
			   ;; ,
			   )))
     ((memq (odt-dom-type node) '(description dc:description))
      ;; ""
      )
     ((eq 'element (odt-dom-type node))
      (let ((name (dom-attr node 'name)))
	(cond
	 (name
	  (list 'element
		:name (dom-attr node 'name)
		:type (mapcar (lambda (n)
				(odt-rngdom-new:do-rng->rnc info n))
			      (dom-children node)
			      ;; ,
			      )))
	 (t
	  (list 'element
		:name (odt-rngdom-new:do-rng->rnc info (car (dom-children node)))
		:type (or (when (cdr (dom-children node))
			    (mapcar (lambda (n)
				      (odt-rngdom-new:do-rng->rnc info n))
				    (cdr (dom-children node))
				    ;; ,
				    ))
			  ""))))))
     ((eq 'empty (odt-dom-type node))
      (odt-dom-type node))
     ((eq 'grammar (odt-dom-type node))
      (odt-rngdom-new:do-rng->rnc info (dom-children node)))
     ((eq 'group (odt-dom-type node))
      (list 'group
	    (mapcar (lambda (n)
		      (odt-rngdom-new:do-rng->rnc info n))
		    (dom-children node)
		    ;; ,
		    ))
      ;; (format (if (or (odt-rngdom-new:is-implicitly-parenthesized dom node)
      ;;     	    (odt-rngdom-new:surrounds-element-attribute-or-ref dom node))
      ;;     	"%s"
      ;;           "(%s)")
      ;;         )
      )
     ((eq 'interleave (odt-dom-type node))
      (let ((contents (mapcar (lambda (n)
				(odt-rngdom-new:do-rng->rnc info n))
			      (dom-children node)
			      ;; &
			      )))
	(if (odt-rngdom-new:is-implicitly-parenthesized dom node)
	    (append (list 'interleave) contents)
	  (list 'interleave contents)
	  ;; "%s"
	  ;; "(%s)"
	  ))
      ;; (list 'interleave )
      ;; (format (if (odt-rngdom-new:is-implicitly-parenthesized dom node)
      ;;     	"%s"
      ;;           "(%s)"))
      )
     ((eq 'list (odt-dom-type node))
      (list 'list (mapcar (lambda (n)
			    (odt-rngdom-new:do-rng->rnc info n))
			  (dom-children node)
			  ;; ,
			  ))
      ;; (format "list { %s }")
      )
     ((eq 'oneOrMore (odt-dom-type node))
      (let ((contents (mapcar (lambda (n)
				(odt-rngdom-new:do-rng->rnc info n))
			      (dom-children node)
			      ;; ,
			      )))
	(list '+ contents)
	;; (format (if (and (odt-dom-has-only-one-child-p dom node)
	;;   	       (or (odt-rngdom-new:is-implicitly-parenthesized dom node)
	;;   		   (odt-rngdom-new:surrounds-element-attribute-or-ref dom node)
	;;   		   (odt-rngdom-new:is-explicitly-parenthesized dom node contents)))
	;;   	  (format "%s+" contents)
	;;   	(format "(%s)+"
	;;   		contents)))
	))
     ((eq 'optional (odt-dom-type node))
      (let ((contents (mapcar (lambda (n)
				(odt-rngdom-new:do-rng->rnc info n))
			      (dom-children node)
			      ;; ,
			      )))
	;; (message "optional contents: %S" contents)
	;; (list 'optional contents)
	(if (or (odt-rngdom-new:surrounds-element-attribute-or-ref dom node)
		(odt-rngdom-new:is-explicitly-parenthesized dom node contents))
	    (progn
	      (cl-assert (null (cdr contents)))
	      (plist-put (cdar contents) :multiplicity 'optional)
	      (car contents))
	  (list 'optional contents)
	  ;; "(%s)?"
	  )
	;; (format (if (or (odt-rngdom-new:surrounds-element-attribute-or-ref dom node)
	;;   	      (odt-rngdom-new:is-explicitly-parenthesized dom node contents))
	;;   	  "%s?"
	;;   	"(%s)?"))
	))
     ((eq 'param (odt-dom-type node))
      (cons (dom-attr node 'name)
	    (odt-rngdom-new:do-rng->rnc info (dom-children node))))
     ((eq 'ref (odt-dom-type node))
      (let ((name (dom-attr node 'name)))
	(list 'ref
	      :name (cond
		     ((string= "string" name)
		      "\\string")
		     (t name)))))
     ((eq 'value (odt-dom-type node))
      (format "\"%s\""
	      (car (dom-children node))))
     ((eq 'top (odt-dom-type node))
      (odt-rngdom-new:do-rng->rnc info (dom-children node)))
     ((eq 'zeroOrMore (odt-dom-type node))
      (let ((contents (mapcar (lambda (n)
				(odt-rngdom-new:do-rng->rnc info n))
			      (dom-children node)
			      ;; ,
			      )))
	(if (or (odt-rngdom-new:surrounds-element-attribute-or-ref dom node)
		(odt-rngdom-new:is-explicitly-parenthesized dom node contents))
	    (list 'zeroormore (apply #'append contents))
	  (list 'zeroormore contents))
	;; (format (if (or (odt-rngdom-new:surrounds-element-attribute-or-ref dom node)
	;;   	      (odt-rngdom-new:is-explicitly-parenthesized dom node contents))
	;;   	  "%s*"
	;;   	"(%s)*"))
	))
     ((eq 'start (odt-dom-type node))
      (list 'start
	    (mapcar (lambda (n)
		      (odt-rngdom-new:do-rng->rnc info n))
		    (dom-children node)
		    ;; ,
		    )))
     ((eq 'text (odt-dom-type node))
      (odt-dom-type node))
     ((eq 'mixed (odt-dom-type node))
      (format "%s { %s }"
	      (odt-dom-type node)
	      (odt-rngdom-new:do-rng->rnc info (car (dom-children node)))))
     ((eq 'name (odt-dom-type node))
      (odt-rngdom-new:do-rng->rnc info (car (dom-children node))))
     ((odt-dom-type node)
      (error "You aren't handling %S" (odt-dom-type node)))
     (t
      (mapcar (lambda (n)
		(odt-rngdom-new:do-rng->rnc info n))
	      node)))))

(defun odt-rngdom-new:rng->rnc-dash (&optional rnc-file-name rng-file-name-or-rng-nodes)
  (interactive)
  (let* ((rng-file-name-or-rng-nodes
	  (or rng-file-name-or-rng-nodes
	      (expand-file-name "odf1.2/OpenDocument-v1.2-os-schema.rng" org-odt-schema-dir)))
	 (rng-nodes (when (consp rng-file-name-or-rng-nodes)
		      rng-file-name-or-rng-nodes))
	 (info-for-rnc (list :dom rng-nodes :use-newline t))
	 (info-for-simplified (list :dom rng-nodes))
	 (rng-file-name
	  (when (stringp rng-file-name-or-rng-nodes)
	    rng-file-name-or-rng-nodes))
	 (rng-nodes (or rng-nodes
			(list (odt-file-to-dom rng-file-name (not 'strip-comment-nodes-p)))))
	 (rnc-file-dir
	  (concat (file-name-parent-directory org-odt-lib-dir)
		  "testing/examples/odtxml/"))
	 (rnc-file-name (cond
			 (rnc-file-name
			  rnc-file-name)
			 (rng-file-name
			  (expand-file-name
			   (format "my-%s.rnc" (file-name-sans-extension (file-name-nondirectory rng-file-name)))
			   rnc-file-dir))
			 (t
			  (expand-file-name
			   (format "my-%s.rnc" "tmp")
			   rnc-file-dir)))))
    (when (file-exists-p rnc-file-name)
      (delete-file rnc-file-name))
    (cl-loop for n1 in rng-nodes
	     for n = (odt-rngdom:remove-description n1)
	     collect (list n
			   (odt-rngdom:do-rng->rnc info-for-rnc
						   n)
			   (odt-rngdom-new:do-rng->rnc info-for-simplified n)))))

(defun odt-rngdom-new:rng->rnc (&optional org-file-name rng-file-name-or-rng-nodes)
  (interactive)
  (let* ((results (odt-rngdom-new:rng->rnc-dash org-file-name rng-file-name-or-rng-nodes))
	 (rng-file-name-or-rng-nodes
	  (or rng-file-name-or-rng-nodes
	      (expand-file-name "odf1.2/OpenDocument-v1.2-os-schema.rng" org-odt-schema-dir)))
	 (rng-file-name
	  (when (stringp rng-file-name-or-rng-nodes)
	    rng-file-name-or-rng-nodes))
	 (rnc-file-dir
	  (concat (file-name-parent-directory org-odt-lib-dir)
		  "testing/examples/odtxml/"))
	 (org-file-name (cond
			 (org-file-name
			  org-file-name)
			 (rng-file-name
			  (expand-file-name
			   (format "my-%s.org" (file-name-sans-extension (file-name-nondirectory rng-file-name)))
			   rnc-file-dir))
			 (t
			  (expand-file-name
			   (format "my-%s.org" "tmp")
			   rnc-file-dir)))))
    (when (file-exists-p org-file-name)
      (delete-file org-file-name))
    (with-current-buffer (find-file-noselect org-file-name t)
      (erase-buffer)
      (pop-to-buffer (current-buffer))
      (cl-loop for (rng rnc simple) in results
	       do (insert "\n\n;;;\n\n")
	       (when (eq 'define (odt-dom-type rng))
		 (insert "\n\n* " (odt-dom-property rng 'name)))
	       (insert "\n\n#+caption: RNG")
	       (insert "\n#+begin_src emacs-lisp\n")
	       (odt-pp rng)
	       (insert "#+end_src")
	       (insert "\n\n#+caption: RNC")
	       (insert "\n#+begin_src rnc\n")
	       (insert
		(with-temp-buffer
		  (rnc-mode)
		  (insert rnc)
		  (indent-region (point-min) (point-max))
		  (buffer-substring-no-properties (point-min) (point-max))))
	       (insert "\n#+end_src")
	       (insert "\n\n")
	       (insert "\n\n#+caption: SIMPLIFIED")
	       (insert "\n#+begin_src emacs-lisp\n")
	       (odt-pp simple)
	       (insert "#+end_src"))
      (save-buffer 0)
      (goto-char (point-min)))))

(provide 'odt-continued)
;;; odt-continued.el ends here
