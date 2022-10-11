(require 'odt)

(odt-styles-get-style "Text_20_body" org-odt-styles-dom)

;; ((style:style
;;   ((style:name . "Text_20_body")
;;    (style:display-name . "Text body")
;;    (style:family . "paragraph")
;;    (style:parent-style-name . "Standard")
;;    (style:class . "text"))
;;   (style:paragraph-properties
;;    ((fo:margin-top . "0cm")
;;     (fo:margin-bottom . "0.212cm")))))


(odt-styles-inspect-type 'style:style org-odt-styles-dom)

;; (:parent-tags
;;  (office:styles office:automatic-styles)
;;  :attributes
;;  (style:class style:default-outline-level style:display-name
;; 	      style:family style:list-style-name style:name
;; 	      style:next-style-name style:parent-style-name)
;;  :child-tags
;;  (LITERAL style:graphic-properties style:paragraph-properties style:text-properties))

(odt-rng-get-element 'style:style)

;; ((element
;;   ((name . "style:style"))
;;   (ref
;;    ((name . "style-style-attlist")))
;;   (ref
;;    ((name . "style-style-content")))
;;   (zeroOrMore nil
;; 	      (ref
;; 	       ((name . "style-map"))))))

;; (odt-rng-ref-name->ref-names
;;  (delete-dups (odt-rng-do-referenced-ref-names
;; 	       (odt-rng-get-element 'style:style)
;; 	       odt-rng-odf-v1.2-os-schema)))

(odt-rng-get-define "style-style-attlist")

;; (setq rng
;;       '(define
;; 	((name . "style-style-attlist"))
;; 	(interleave nil
;; 		    (attribute
;; 		     ((name . "style:name"))
;; 		     (ref
;; 		      ((name . "styleName"))))
;; 		    (optional nil
;; 			      (attribute
;; 			       ((name . "style:display-name"))
;; 			       (ref
;; 				((name . "string")))))
;; 		    (optional nil
;; 			      (attribute
;; 			       ((name . "style:parent-style-name"))
;; 			       (ref
;; 				((name . "styleNameRef")))))
;; 		    (optional nil
;; 			      (attribute
;; 			       ((name . "style:next-style-name"))
;; 			       (ref
;; 				((name . "styleNameRef")))))
;; 		    (optional nil
;; 			      (attribute
;; 			       ((name . "style:list-level"))
;; 			       (choice nil
;; 				       (ref
;; 					((name . "positiveInteger")))
;; 				       (empty nil))))
;; 		    (optional nil
;; 			      (attribute
;; 			       ((name . "style:list-style-name"))
;; 			       (choice nil
;; 				       (ref
;; 					((name . "styleName")))
;; 				       (empty nil))))
;; 		    (optional nil
;; 			      (attribute
;; 			       ((name . "style:master-page-name"))
;; 			       (ref
;; 				((name . "styleNameRef")))))
;; 		    (optional nil
;; 			      (attribute
;; 			       ((name . "style:auto-update"))
;; 			       (ref
;; 				((name . "boolean")))))
;; 		    (optional nil
;; 			      (attribute
;; 			       ((name . "style:data-style-name"))
;; 			       (ref
;; 				((name . "styleNameRef")))))
;; 		    (optional nil
;; 			      (attribute
;; 			       ((name . "style:percentage-data-style-name"))
;; 			       (ref
;; 				((name . "styleNameRef")))))
;; 		    (optional nil
;; 			      (attribute
;; 			       ((name . "style:class"))
;; 			       (ref
;; 				((name . "string")))))
;; 		    (optional nil
;; 			      (attribute
;; 			       ((name . "style:default-outline-level"))
;; 			       (choice nil
;; 				       (ref
;; 					((name . "positiveInteger")))
;; 				       (empty nil)))))))

;; (insert "\n\n"
;; 	(with-temp-buffer
;; 	  (rnc-mode)
;; 	  (insert (odt-rng--rng->rnc odt-rng-odf-v1.2-os-schema rng t))
;; 	  (indent-region (point-min) (point-max))
;; 	  (buffer-substring-no-properties (point-min) (point-max))))

;; style-style-attlist =
;;     (attribute style:name { styleName }
;;     & attribute style:display-name { \string }?
;;     & attribute style:parent-style-name { styleNameRef }?
;;     & attribute style:next-style-name { styleNameRef }?
;;     & attribute style:list-level { (positiveInteger
;; 				   | empty) }?
;;     & attribute style:list-style-name { (styleName
;; 					| empty) }?
;;     & attribute style:master-page-name { styleNameRef }?
;;     & attribute style:auto-update { boolean }?
;;     & attribute style:data-style-name { styleNameRef }?
;;     & attribute style:percentage-data-style-name { styleNameRef }?
;;     & attribute style:class { \string }?
;;     & attribute style:default-outline-level { (positiveInteger
;; 					      | empty) }?)



