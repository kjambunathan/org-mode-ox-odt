;; odt-d288 --- Odt 288 -*- lexical-binding: t; coding: utf-8-emacs; -*-

;;; Commentary

;; - See Discussion#288: How do I create an ODT document where the
;;   default paragraph style uses First Line Indent style AND the
;;   paragraph styles are chosen based on style:next-style-name of
;;   styles.xml?

;;     - https://github.com/kjambunathan/org-mode-ox-odt/discussions/288

;; ----------------  NOTES ----------------

;; - `keiser-odt-d288/build-style-and-next-style' uses
;;   `kjn-odt-d288/parse-styles-file' to parse a `styles.xml' file and
;;   gathers an alist of `style:name' and its associated
;;   `style:next-style-name'.

;; - `keiser-odt-d288/sneak-paragraph-style-overrides-to-org-ast'
;;   relies on the alist built by the preceding
;;   'keiser-odt-d288/build-style-and-next-style', and forces a
;;   `:style' attribute on paragraph AST (= Abstract Syntax Tree)
;;   elements that do NOT have any explicit style specified.  The
;;   styles "chosen" for a paragraph, in the present case, is guided
;;   by the `style:next-style-name' property in `styles.xml'.

;;   As a bonus, this function augments the ENVIRONMENT of export
;;   engine--the `info' structure--with info collected from the
;;   `styles.xml'.  This additional info is available to the ODT
;;   transcoders (see `org-export-define-backend').  In the present
;;   case, this additional info is not used elsewhere.

;; See README.org for how to test drive this file `odt-d288.el' and its
;; sibling `odt-d288-1.el'.

;; ---------------- NOTE ON NAMING CONVENTION ----------------

;; - Functions that are prefixed with `kjn-odt-d288/' are CORE APIs
;;   which will make its way in to the ODT export engine in due
;;   course.

;; - Functions that are prefixed with `keiser-odt-d288/' are
;;   USER-defined 3rd Party functions to accomplish whatever that USER
;;   desires.

;;; Code:

(require 'ox-odt)

;;; Org Element Type to ODT style Mappings

(defvar kjn-odt-d288/element-type-and-odt-style
  '(
    (verse-block			 . "OrgVerse")
    (quote-block			 . "Quotations")
    (paragraph				 . "Text_20_body")
    (footnote-definition		 . "Footnote")
    ((footnote-definition quote-block)	 . "OrgFootnoteQuotations")
    ((footnote-definition center-block)	 . "OrgFootnoteCenter")
    (center-block			 . "OrgCenter")
    ))


;;; APIs

;;;; API  to convert `styles.xml' to a DOM

(defun kjn-odt-d288/parse-styles-file (backend &optional odt-styles-file
					       ;; odt-styles-dir
					       copy-to-dir
					       )
  (message "\n\n\n%S" `(kjn-odt-d288/parse-styles-file ,odt-styles-file))
  (let* ((copy-to-dir (or copy-to-dir (file-name-as-directory (make-temp-file "odt-" t))))
	 (styles-file
	  (or odt-styles-file
	      (org-odt-get-backend-property backend :styles-file)))
	 (styles-file-type (file-name-extension styles-file)))

    (message "\n\n\n%S" `(kjn-odt-d288/parse-styles-file ,styles-file))
    (unless (file-name-absolute-p styles-file)
      (user-error "`kjn-odt-d288/parse-styles-file' expects an absolute path for `odt-styles-file' (= %s)" styles-file))
    ;; Complain if the styles file doesn't exist.
    (unless (file-readable-p styles-file)
      (user-error "Cannot read styles file: %s" styles-file))
    ;; Check the type of styles file.
    (pcase styles-file-type
      ;; If it is of type `odt', `ott', `ods', `ots' etc (i.e., a zip
      ;; file), then the styles.xml within the zip file becomes the
      ;; styles.xml of the target file.  Extra images, if any, also
      ;; comes from within this zip file.
      ((pred (lambda (extn)
	       (eq backend
		   (map-elt org-odt-styles-file-extension-and-backend extn))))
       (let ((archive styles-file)
	     (members "styles.xml"))
	 (org-odt--zip-extract archive members copy-to-dir)))
      ;; If it is of type `xml', then it becomes the styles.xml of the
      ;; target file.  Extra images, if any, comes from the user's
      ;; file system.
      ("xml"
       ;; User-provided styles-file could be read only, but we want the
       ;; copy of this file in zip dir to be writable.  So, first create
       ;; an "empty" styles file in the zip dir and then overwrite it
       ;; with the user-provided styles file.
       (with-temp-buffer
	 (write-region nil nil (concat copy-to-dir "styles.xml")))
       (copy-file styles-file (concat copy-to-dir "styles.xml") t))
      (_ (error "Styles file `%s' is incompatible with export backend `%s'.
Expecting Styles file with any of the following extensions %S"
		(file-name-nondirectory styles-file)
		backend
		(cons "xml" (map-elt org-odt-backend-and-styles-file-extensions backend)))))
    ;; Retrieve Styles.xml as DOM.
    ;; (plist-put info :odt-styles-dom
    ;;            (odt-dom:file->dom
    ;;     	(concat copy-to-dir "styles.xml")
    ;;     	'strip-comment-nodes-p))
    (prog1
        (odt-dom:file->dom
	 (concat copy-to-dir "styles.xml")
	 (not 'strip-comment-nodes-p))
      (cond
       (copy-to-dir
	;; Do Nothing
	)
       (t
        (delete-directory copy-to-dir 'recursive (not 'trash)))))))

;;;; API for augmenting `info' with information from a style's DOM

(defun kjn-odt-d288/augment-info (dom fs)
  (let ((additional-info   (thread-last fs
					(seq-map
					 (lambda (f)
					   (funcall f dom)))
					(apply #'org-combine-plists))))
    (prog1 additional-info
      (message "\n\n\n%S => %S" `(kjn-odt-d288/augment-info DOM ',fs)
	       additional-info))))

;;; @Keiser/Discussion#288: https://github.com/kjambunathan/org-mode-ox-odt/discussions/288

;;;; Styles and Next Styles

;;;;; Keiser: Build an alist of style and next-styles

(defun keiser-odt-d288/build-style-and-next-style (dom)
  (list :style-and-next-style
	(let* ((nodes
		(odt-dom--query dom
				;; Under DOM's children
				%_all
				;; ... look for all nodes that  explicitly specify
				;; `style:next-style-name'
				%style:next-style-name
				;; ... and collect the `style:name',
				;; `style:next-style-name' properties
				;; of those nodes
				(cons %style:name
				      ;; :display-name %style:display-name
				      ;; :parent-style-name %style:parent-style-name
				      ;; :next-style-name
				      %style:next-style-name))))
	  (thread-last nodes
		       ;; Sort the above list based on `style:name'
		       (seq-sort
			(pcase-lambda (`(,a . _) `(,b . _))
			  (string< a b)))))))

;;;;; Keiser: Sneak in the paragraph style overrides to Org Buffer's AST

(defun keiser-odt-d288/sneak-paragraph-style-overrides-to-org-ast (data backend info)
  (message "\n\n\n%S"
	   `(AT-ENTRY-TO-keiser-odt-sneak-paragraph-style-overrides-to-org-ast
	     :info ,(thread-last '(:odt-zip-dir :odt-styles-file :styles-file :input-file
						:style-and-next-style)
				 (seq-mapcat
				  (lambda (it)
				    (list it (plist-get info it)))))))
  (prog1 data
    (when (eq backend 'odt)
      (pcase-let* ((odt-styles-file
		    (let ((file (plist-get info :odt-styles-file)))
		      (cond
		       ((or (not file) (string= file ""))
			(org-odt-get-backend-property org-export-current-backend :styles-file))
		       ((file-name-absolute-p file) file)
		       (t (expand-file-name
			   file (file-name-directory (plist-get info :input-file))))))))
        ;; Augment the `info' node with additional info collected from
        ;; `styles.xml'.
	(nconc info (kjn-odt-d288/augment-info
		     (kjn-odt-d288/parse-styles-file org-export-current-backend
						     odt-styles-file)
		     (list 'keiser-odt-d288/build-style-and-next-style)))

	(message "\n\n\n%S"
		 `(AT-EXIT-OF-keiser-odt-sneak-paragraph-style-overrides-to-org-ast
		   :info ,(thread-last '(:odt-zip-dir :odt-styles-file :styles-file :input-file
						      :style-and-next-style)
				       (seq-mapcat
					(lambda (it)
					  (list it (plist-get info it)))))))
	(org-element-map data 'paragraph
	  (lambda (p)
	    (let* ((parent (org-export-get-parent p))
		   (previous-siblings (reverse (org-export-get-previous-element p info 'all)))
		   (_genealogy (reverse (cons p (org-element-lineage p))))
		   (inferred-style
		    (unless
			;; Don't touch the style of the following
			;; types of paragraphs:
			;;
			;; - paragraphs that are enclosed in SPECIAL
			;; - BLOCKs, or FOOTNOTE-DEFINITION
			;;
			;; - paragarphs that are EXPLICITLY styled
			;; - with a `:style' attribute
			(or (org-element-type-p parent '(verse-block center-block footnote-definition
								     quote-block special-block))
			    (org-element-property :style p)
			    (org-odt--read-attribute p :style))
		      (or
		       ;; When the paragraph is THE FIRST paragraph
		       ;; in an outline, use `Text_20_body'
		       ;; style.
		       (when (and (org-element-type-p parent '(section))
				  (org-export-first-sibling-p p info))
			 ;; "Text_20_body"
			 (map-elt kjn-odt-d288/element-type-and-odt-style 'paragraph))
		       ;; When the paragraph IMMEDIATELY FOLLOWS a
		       ;; QUOTE-BLOCK use, whatever style is
		       ;; `style:next-style-name' of the QUOTE-BLOCK.
		       (when (org-element-type-p (car previous-siblings) '(quote-block))
			 ;; "Text_20_body"
			 (or (map-elt
			      (plist-get info :style-and-next-style)
			      (map-elt kjn-odt-d288/element-type-and-odt-style 'quote-block))))
                       ;; In the default case, use the
                       ;; `style:next-style-name' configured for the
                       ;; default paragraph style.
		       (or (map-elt
			    (plist-get info :style-and-next-style)
			    (map-elt kjn-odt-d288/element-type-and-odt-style 'paragraph)))))))
	      (when inferred-style
		(org-element-put-property p :style inferred-style))))
	  info nil nil t)))))

(add-to-list
 'org-export-filter-parse-tree-functions
 'keiser-odt-d288/sneak-paragraph-style-overrides-to-org-ast
 'append)

;; (setq org-export-filter-parse-tree-functions
;;       (delq 'keiser-odt-d288/sneak-paragraph-style-overrides-to-org-ast org-export-filter-parse-tree-functions ))

(provide 'odt-d288)
;;; odt-d288.el ends here
