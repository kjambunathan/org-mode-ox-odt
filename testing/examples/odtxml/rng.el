(top nil
     (comment nil "\n        Open Document Format for Office Applications (OpenDocument) Version 1.2\n        OASIS Standard, 29 September 2011\n	Relax-NG Schema\n        Source: http://docs.oasis-open.org/office/v1.2/os/\n        Copyright (c) OASIS Open 2002-2011. All Rights Reserved.\n\n	All capitalized terms in the following text have the meanings assigned to them\n	in the OASIS Intellectual Property Rights Policy (the \"OASIS IPR Policy\"). The\n	full Policy may be found at the OASIS website.\n\n	This document and translations of it may be copied and furnished to others, and\n	derivative works that comment on or otherwise explain it or assist in its\n	implementation may be prepared, copied, published, and distributed, in whole or\n	in part, without restriction of any kind, provided that the above copyright\n	notice and this section are included on all such copies and derivative works.\n	However, this document itself may not be modified in any way, including by\n	removing the copyright notice or references to OASIS, except as needed for the\n	purpose of developing any document or deliverable produced by an OASIS\n	Technical Committee (in which case the rules applicable to copyrights, as set\n	forth in the OASIS IPR Policy, must be followed) or as required to translate it\n	into languages other than English.\n\n	The limited permissions granted above are perpetual and will not be revoked by\n	OASIS or its successors or assigns.\n\n	This document and the information contained herein is provided on an \"AS IS\"\n	basis and OASIS DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT NOT\n	LIMITED TO ANY WARRANTY THAT THE USE OF THE INFORMATION HEREIN WILL NOT\n	INFRINGE ANY OWNERSHIP RIGHTS OR ANY IMPLIED WARRANTIES OF MERCHANTABILITY OR\n	FITNESS FOR A PARTICULAR PURPOSE. \n")
     (grammar
      ((datatypeLibrary . "http://www.w3.org/2001/XMLSchema-datatypes"))
      (define
       ((name . "office-process-content"))
       (optional nil
		 (attribute
		  ((name . "office:process-content"))
		  (ref
		   ((name . "boolean"))))))
      (start nil
	     (choice nil
		     (ref
		      ((name . "office-document")))
		     (ref
		      ((name . "office-document-content")))
		     (ref
		      ((name . "office-document-styles")))
		     (ref
		      ((name . "office-document-meta")))
		     (ref
		      ((name . "office-document-settings")))))
      (define
       ((name . "office-document"))
       (element
	((name . "office:document"))
	(ref
	 ((name . "office-document-attrs")))
	(ref
	 ((name . "office-document-common-attrs")))
	(ref
	 ((name . "office-meta")))
	(ref
	 ((name . "office-settings")))
	(ref
	 ((name . "office-scripts")))
	(ref
	 ((name . "office-font-face-decls")))
	(ref
	 ((name . "office-styles")))
	(ref
	 ((name . "office-automatic-styles")))
	(ref
	 ((name . "office-master-styles")))
	(ref
	 ((name . "office-body")))))
      (define
       ((name . "office-document-content"))
       (element
	((name . "office:document-content"))
	(ref
	 ((name . "office-document-common-attrs")))
	(ref
	 ((name . "office-scripts")))
	(ref
	 ((name . "office-font-face-decls")))
	(ref
	 ((name . "office-automatic-styles")))
	(ref
	 ((name . "office-body")))))
      (define
       ((name . "office-document-styles"))
       (element
	((name . "office:document-styles"))
	(ref
	 ((name . "office-document-common-attrs")))
	(ref
	 ((name . "office-font-face-decls")))
	(ref
	 ((name . "office-styles")))
	(ref
	 ((name . "office-automatic-styles")))
	(ref
	 ((name . "office-master-styles")))))
      (define
       ((name . "office-document-meta"))
       (element
	((name . "office:document-meta"))
	(ref
	 ((name . "office-document-common-attrs")))
	(ref
	 ((name . "office-meta")))))
      (define
       ((name . "office-document-settings"))
       (element
	((name . "office:document-settings"))
	(ref
	 ((name . "office-document-common-attrs")))
	(ref
	 ((name . "office-settings")))))
      (define
       ((name . "office-document-common-attrs"))
       (interleave nil
		   (attribute
		    ((name . "office:version"))
		    (value nil "1.2"))
		   (optional nil
			     (attribute
			      ((name . "grddl:transformation"))
			      (list nil
				    (zeroOrMore nil
						(ref
						 ((name . "anyIRI")))))))))
      (define
       ((name . "office-document-attrs"))
       (attribute
	((name . "office:mimetype"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "office-meta"))
       (optional nil
		 (element
		  ((name . "office:meta"))
		  (ref
		   ((name . "office-meta-content-strict"))))))
      (define
       ((name . "office-meta-content-strict"))
       (zeroOrMore nil
		   (ref
		    ((name . "office-meta-data")))))
      (define
       ((name . "office-body"))
       (element
	((name . "office:body"))
	(ref
	 ((name . "office-body-content")))))
      (define
       ((name . "office-body-content"))
       (choice nil
	       (element
		((name . "office:text"))
		(ref
		 ((name . "office-text-attlist")))
		(ref
		 ((name . "office-text-content-prelude")))
		(ref
		 ((name . "office-text-content-main")))
		(ref
		 ((name . "office-text-content-epilogue"))))
	       (element
		((name . "office:drawing"))
		(ref
		 ((name . "office-drawing-attlist")))
		(ref
		 ((name . "office-drawing-content-prelude")))
		(ref
		 ((name . "office-drawing-content-main")))
		(ref
		 ((name . "office-drawing-content-epilogue"))))
	       (element
		((name . "office:presentation"))
		(ref
		 ((name . "office-presentation-attlist")))
		(ref
		 ((name . "office-presentation-content-prelude")))
		(ref
		 ((name . "office-presentation-content-main")))
		(ref
		 ((name . "office-presentation-content-epilogue"))))
	       (element
		((name . "office:spreadsheet"))
		(ref
		 ((name . "office-spreadsheet-attlist")))
		(ref
		 ((name . "office-spreadsheet-content-prelude")))
		(ref
		 ((name . "office-spreadsheet-content-main")))
		(ref
		 ((name . "office-spreadsheet-content-epilogue"))))
	       (element
		((name . "office:chart"))
		(ref
		 ((name . "office-chart-attlist")))
		(ref
		 ((name . "office-chart-content-prelude")))
		(ref
		 ((name . "office-chart-content-main")))
		(ref
		 ((name . "office-chart-content-epilogue"))))
	       (element
		((name . "office:image"))
		(ref
		 ((name . "office-image-attlist")))
		(ref
		 ((name . "office-image-content-prelude")))
		(ref
		 ((name . "office-image-content-main")))
		(ref
		 ((name . "office-image-content-epilogue"))))
	       (ref
		((name . "office-database")))))
      (define
       ((name . "office-text-content-prelude"))
       (ref
	((name . "office-forms")))
       (ref
	((name . "text-tracked-changes")))
       (ref
	((name . "text-decls")))
       (ref
	((name . "table-decls"))))
      (define
       ((name . "office-text-content-main"))
       (choice nil
	       (zeroOrMore nil
			   (ref
			    ((name . "text-content"))))
	       (group nil
		      (ref
		       ((name . "text-page-sequence")))
		      (zeroOrMore nil
				  (choice nil
					  (ref
					   ((name . "shape"))))))))
      (define
       ((name . "text-content"))
       (choice nil
	       (ref
		((name . "text-h")))
	       (ref
		((name . "text-p")))
	       (ref
		((name . "text-list")))
	       (ref
		((name . "text-numbered-paragraph")))
	       (ref
		((name . "table-table")))
	       (ref
		((name . "text-section")))
	       (ref
		((name . "text-soft-page-break")))
	       (ref
		((name . "text-table-of-content")))
	       (ref
		((name . "text-illustration-index")))
	       (ref
		((name . "text-table-index")))
	       (ref
		((name . "text-object-index")))
	       (ref
		((name . "text-user-index")))
	       (ref
		((name . "text-alphabetical-index")))
	       (ref
		((name . "text-bibliography")))
	       (ref
		((name . "shape")))
	       (ref
		((name . "change-marks")))))
      (define
       ((name . "office-text-content-epilogue"))
       (ref
	((name . "table-functions"))))
      (define
       ((name . "office-text-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "text:global"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:use-soft-page-breaks"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "office-drawing-attlist"))
       (empty nil))
      (define
       ((name . "office-drawing-content-prelude"))
       (ref
	((name . "text-decls")))
       (ref
	((name . "table-decls"))))
      (define
       ((name . "office-drawing-content-main"))
       (zeroOrMore nil
		   (ref
		    ((name . "draw-page")))))
      (define
       ((name . "office-drawing-content-epilogue"))
       (ref
	((name . "table-functions"))))
      (define
       ((name . "office-presentation-attlist"))
       (empty nil))
      (define
       ((name . "office-presentation-content-prelude"))
       (ref
	((name . "text-decls")))
       (ref
	((name . "table-decls")))
       (ref
	((name . "presentation-decls"))))
      (define
       ((name . "office-presentation-content-main"))
       (zeroOrMore nil
		   (ref
		    ((name . "draw-page")))))
      (define
       ((name . "office-presentation-content-epilogue"))
       (ref
	((name . "presentation-settings")))
       (ref
	((name . "table-functions"))))
      (define
       ((name . "office-spreadsheet-content-prelude"))
       (optional nil
		 (ref
		  ((name . "table-tracked-changes"))))
       (ref
	((name . "text-decls")))
       (ref
	((name . "table-decls"))))
      (define
       ((name . "table-decls"))
       (optional nil
		 (ref
		  ((name . "table-calculation-settings"))))
       (optional nil
		 (ref
		  ((name . "table-content-validations"))))
       (optional nil
		 (ref
		  ((name . "table-label-ranges")))))
      (define
       ((name . "office-spreadsheet-content-main"))
       (zeroOrMore nil
		   (ref
		    ((name . "table-table")))))
      (define
       ((name . "office-spreadsheet-content-epilogue"))
       (ref
	((name . "table-functions"))))
      (define
       ((name . "table-functions"))
       (optional nil
		 (ref
		  ((name . "table-named-expressions"))))
       (optional nil
		 (ref
		  ((name . "table-database-ranges"))))
       (optional nil
		 (ref
		  ((name . "table-data-pilot-tables"))))
       (optional nil
		 (ref
		  ((name . "table-consolidation"))))
       (optional nil
		 (ref
		  ((name . "table-dde-links")))))
      (define
       ((name . "office-chart-attlist"))
       (empty nil))
      (define
       ((name . "office-chart-content-prelude"))
       (ref
	((name . "text-decls")))
       (ref
	((name . "table-decls"))))
      (define
       ((name . "office-chart-content-main"))
       (ref
	((name . "chart-chart"))))
      (define
       ((name . "office-chart-content-epilogue"))
       (ref
	((name . "table-functions"))))
      (define
       ((name . "office-image-attlist"))
       (empty nil))
      (define
       ((name . "office-image-content-prelude"))
       (empty nil))
      (define
       ((name . "office-image-content-main"))
       (ref
	((name . "draw-frame"))))
      (define
       ((name . "office-image-content-epilogue"))
       (empty nil))
      (define
       ((name . "office-settings"))
       (optional nil
		 (element
		  ((name . "office:settings"))
		  (oneOrMore nil
			     (ref
			      ((name . "config-config-item-set")))))))
      (define
       ((name . "config-config-item-set"))
       (element
	((name . "config:config-item-set"))
	(ref
	 ((name . "config-config-item-set-attlist")))
	(ref
	 ((name . "config-items")))))
      (define
       ((name . "config-items"))
       (oneOrMore nil
		  (choice nil
			  (ref
			   ((name . "config-config-item")))
			  (ref
			   ((name . "config-config-item-set")))
			  (ref
			   ((name . "config-config-item-map-named")))
			  (ref
			   ((name . "config-config-item-map-indexed"))))))
      (define
       ((name . "config-config-item-set-attlist"))
       (attribute
	((name . "config:name"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "config-config-item"))
       (element
	((name . "config:config-item"))
	(ref
	 ((name . "config-config-item-attlist")))
	(text nil)))
      (define
       ((name . "config-config-item-attlist"))
       (interleave nil
		   (attribute
		    ((name . "config:name"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "config:type"))
		    (choice nil
			    (value nil "boolean")
			    (value nil "short")
			    (value nil "int")
			    (value nil "long")
			    (value nil "double")
			    (value nil "string")
			    (value nil "datetime")
			    (value nil "base64Binary")))))
      (define
       ((name . "config-config-item-map-indexed"))
       (element
	((name . "config:config-item-map-indexed"))
	(ref
	 ((name . "config-config-item-map-indexed-attlist")))
	(oneOrMore nil
		   (ref
		    ((name . "config-config-item-map-entry"))))))
      (define
       ((name . "config-config-item-map-indexed-attlist"))
       (attribute
	((name . "config:name"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "config-config-item-map-entry"))
       (element
	((name . "config:config-item-map-entry"))
	(ref
	 ((name . "config-config-item-map-entry-attlist")))
	(ref
	 ((name . "config-items")))))
      (define
       ((name . "config-config-item-map-entry-attlist"))
       (optional nil
		 (attribute
		  ((name . "config:name"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "config-config-item-map-named"))
       (element
	((name . "config:config-item-map-named"))
	(ref
	 ((name . "config-config-item-map-named-attlist")))
	(oneOrMore nil
		   (ref
		    ((name . "config-config-item-map-entry"))))))
      (define
       ((name . "config-config-item-map-named-attlist"))
       (attribute
	((name . "config:name"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "office-scripts"))
       (optional nil
		 (element
		  ((name . "office:scripts"))
		  (zeroOrMore nil
			      (ref
			       ((name . "office-script"))))
		  (optional nil
			    (ref
			     ((name . "office-event-listeners")))))))
      (define
       ((name . "office-script"))
       (element
	((name . "office:script"))
	(ref
	 ((name . "office-script-attlist")))
	(mixed nil
	       (ref
		((name . "anyElements"))))))
      (define
       ((name . "office-script-attlist"))
       (attribute
	((name . "script:language"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "office-font-face-decls"))
       (optional nil
		 (element
		  ((name . "office:font-face-decls"))
		  (zeroOrMore nil
			      (ref
			       ((name . "style-font-face")))))))
      (define
       ((name . "office-styles"))
       (optional nil
		 (element
		  ((name . "office:styles"))
		  (interleave nil
			      (ref
			       ((name . "styles")))
			      (zeroOrMore nil
					  (ref
					   ((name . "style-default-style"))))
			      (optional nil
					(ref
					 ((name . "style-default-page-layout"))))
			      (optional nil
					(ref
					 ((name . "text-outline-style"))))
			      (zeroOrMore nil
					  (ref
					   ((name . "text-notes-configuration"))))
			      (optional nil
					(ref
					 ((name . "text-bibliography-configuration"))))
			      (optional nil
					(ref
					 ((name . "text-linenumbering-configuration"))))
			      (zeroOrMore nil
					  (ref
					   ((name . "draw-gradient"))))
			      (zeroOrMore nil
					  (ref
					   ((name . "svg-linearGradient"))))
			      (zeroOrMore nil
					  (ref
					   ((name . "svg-radialGradient"))))
			      (zeroOrMore nil
					  (ref
					   ((name . "draw-hatch"))))
			      (zeroOrMore nil
					  (ref
					   ((name . "draw-fill-image"))))
			      (zeroOrMore nil
					  (ref
					   ((name . "draw-marker"))))
			      (zeroOrMore nil
					  (ref
					   ((name . "draw-stroke-dash"))))
			      (zeroOrMore nil
					  (ref
					   ((name . "draw-opacity"))))
			      (zeroOrMore nil
					  (ref
					   ((name . "style-presentation-page-layout"))))
			      (zeroOrMore nil
					  (ref
					   ((name . "table-table-template"))))))))
      (define
       ((name . "office-automatic-styles"))
       (optional nil
		 (element
		  ((name . "office:automatic-styles"))
		  (interleave nil
			      (ref
			       ((name . "styles")))
			      (zeroOrMore nil
					  (ref
					   ((name . "style-page-layout"))))))))
      (define
       ((name . "office-master-styles"))
       (optional nil
		 (element
		  ((name . "office:master-styles"))
		  (interleave nil
			      (zeroOrMore nil
					  (ref
					   ((name . "style-master-page"))))
			      (optional nil
					(ref
					 ((name . "style-handout-master"))))
			      (optional nil
					(ref
					 ((name . "draw-layer-set"))))))))
      (define
       ((name . "styles"))
       (interleave nil
		   (zeroOrMore nil
			       (ref
				((name . "style-style"))))
		   (zeroOrMore nil
			       (ref
				((name . "text-list-style"))))
		   (zeroOrMore nil
			       (ref
				((name . "number-number-style"))))
		   (zeroOrMore nil
			       (ref
				((name . "number-currency-style"))))
		   (zeroOrMore nil
			       (ref
				((name . "number-percentage-style"))))
		   (zeroOrMore nil
			       (ref
				((name . "number-date-style"))))
		   (zeroOrMore nil
			       (ref
				((name . "number-time-style"))))
		   (zeroOrMore nil
			       (ref
				((name . "number-boolean-style"))))
		   (zeroOrMore nil
			       (ref
				((name . "number-text-style"))))))
      (define
       ((name . "office-meta-data"))
       (choice nil
	       (element
		((name . "meta:generator"))
		(ref
		 ((name . "string"))))
	       (element
		((name . "dc:title"))
		(ref
		 ((name . "string"))))
	       (element
		((name . "dc:description"))
		(ref
		 ((name . "string"))))
	       (element
		((name . "dc:subject"))
		(ref
		 ((name . "string"))))
	       (element
		((name . "meta:keyword"))
		(ref
		 ((name . "string"))))
	       (element
		((name . "meta:initial-creator"))
		(ref
		 ((name . "string"))))
	       (ref
		((name . "dc-creator")))
	       (element
		((name . "meta:printed-by"))
		(ref
		 ((name . "string"))))
	       (element
		((name . "meta:creation-date"))
		(ref
		 ((name . "dateTime"))))
	       (ref
		((name . "dc-date")))
	       (element
		((name . "meta:print-date"))
		(ref
		 ((name . "dateTime"))))
	       (element
		((name . "meta:template"))
		(attribute
		 ((name . "xlink:type"))
		 (value nil "simple"))
		(attribute
		 ((name . "xlink:href"))
		 (ref
		  ((name . "anyIRI"))))
		(optional nil
			  (attribute
			   ((name . "xlink:actuate"))
			   (value nil "onRequest")))
		(optional nil
			  (attribute
			   ((name . "xlink:title"))
			   (ref
			    ((name . "string")))))
		(optional nil
			  (attribute
			   ((name . "meta:date"))
			   (ref
			    ((name . "dateTime"))))))
	       (element
		((name . "meta:auto-reload"))
		(optional nil
			  (attribute
			   ((name . "xlink:type"))
			   (value nil "simple"))
			  (attribute
			   ((name . "xlink:href"))
			   (ref
			    ((name . "anyIRI"))))
			  (optional nil
				    (attribute
				     ((name . "xlink:show"))
				     (value nil "replace")))
			  (optional nil
				    (attribute
				     ((name . "xlink:actuate"))
				     (value nil "onLoad"))))
		(optional nil
			  (attribute
			   ((name . "meta:delay"))
			   (ref
			    ((name . "duration"))))))
	       (element
		((name . "meta:hyperlink-behaviour"))
		(optional nil
			  (attribute
			   ((name . "office:target-frame-name"))
			   (ref
			    ((name . "targetFrameName")))))
		(optional nil
			  (attribute
			   ((name . "xlink:show"))
			   (choice nil
				   (value nil "new")
				   (value nil "replace")))))
	       (element
		((name . "dc:language"))
		(ref
		 ((name . "language"))))
	       (element
		((name . "meta:editing-cycles"))
		(ref
		 ((name . "nonNegativeInteger"))))
	       (element
		((name . "meta:editing-duration"))
		(ref
		 ((name . "duration"))))
	       (element
		((name . "meta:document-statistic"))
		(optional nil
			  (attribute
			   ((name . "meta:page-count"))
			   (ref
			    ((name . "nonNegativeInteger")))))
		(optional nil
			  (attribute
			   ((name . "meta:table-count"))
			   (ref
			    ((name . "nonNegativeInteger")))))
		(optional nil
			  (attribute
			   ((name . "meta:draw-count"))
			   (ref
			    ((name . "nonNegativeInteger")))))
		(optional nil
			  (attribute
			   ((name . "meta:image-count"))
			   (ref
			    ((name . "nonNegativeInteger")))))
		(optional nil
			  (attribute
			   ((name . "meta:ole-object-count"))
			   (ref
			    ((name . "nonNegativeInteger")))))
		(optional nil
			  (attribute
			   ((name . "meta:object-count"))
			   (ref
			    ((name . "nonNegativeInteger")))))
		(optional nil
			  (attribute
			   ((name . "meta:paragraph-count"))
			   (ref
			    ((name . "nonNegativeInteger")))))
		(optional nil
			  (attribute
			   ((name . "meta:word-count"))
			   (ref
			    ((name . "nonNegativeInteger")))))
		(optional nil
			  (attribute
			   ((name . "meta:character-count"))
			   (ref
			    ((name . "nonNegativeInteger")))))
		(optional nil
			  (attribute
			   ((name . "meta:frame-count"))
			   (ref
			    ((name . "nonNegativeInteger")))))
		(optional nil
			  (attribute
			   ((name . "meta:sentence-count"))
			   (ref
			    ((name . "nonNegativeInteger")))))
		(optional nil
			  (attribute
			   ((name . "meta:syllable-count"))
			   (ref
			    ((name . "nonNegativeInteger")))))
		(optional nil
			  (attribute
			   ((name . "meta:non-whitespace-character-count"))
			   (ref
			    ((name . "nonNegativeInteger")))))
		(optional nil
			  (attribute
			   ((name . "meta:row-count"))
			   (ref
			    ((name . "nonNegativeInteger")))))
		(optional nil
			  (attribute
			   ((name . "meta:cell-count"))
			   (ref
			    ((name . "nonNegativeInteger"))))))
	       (element
		((name . "meta:user-defined"))
		(attribute
		 ((name . "meta:name"))
		 (ref
		  ((name . "string"))))
		(choice nil
			(group nil
			       (attribute
				((name . "meta:value-type"))
				(value nil "float"))
			       (ref
				((name . "double"))))
			(group nil
			       (attribute
				((name . "meta:value-type"))
				(value nil "date"))
			       (ref
				((name . "dateOrDateTime"))))
			(group nil
			       (attribute
				((name . "meta:value-type"))
				(value nil "time"))
			       (ref
				((name . "duration"))))
			(group nil
			       (attribute
				((name . "meta:value-type"))
				(value nil "boolean"))
			       (ref
				((name . "boolean"))))
			(group nil
			       (attribute
				((name . "meta:value-type"))
				(value nil "string"))
			       (ref
				((name . "string"))))
			(text nil)))))
      (define
       ((name . "dc-creator"))
       (element
	((name . "dc:creator"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "dc-date"))
       (element
	((name . "dc:date"))
	(ref
	 ((name . "dateTime")))))
      (define
       ((name . "text-h"))
       (element
	((name . "text:h"))
	(ref
	 ((name . "heading-attrs")))
	(ref
	 ((name . "paragraph-attrs")))
	(optional nil
		  (ref
		   ((name . "text-number"))))
	(zeroOrMore nil
		    (ref
		     ((name . "paragraph-content-or-hyperlink"))))))
      (define
       ((name . "heading-attrs"))
       (interleave nil
		   (attribute
		    ((name . "text:outline-level"))
		    (ref
		     ((name . "positiveInteger"))))
		   (optional nil
			     (attribute
			      ((name . "text:restart-numbering"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:start-value"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (optional nil
			     (attribute
			      ((name . "text:is-list-header"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "text-number"))
       (element
	((name . "text:number"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "text-p"))
       (element
	((name . "text:p"))
	(ref
	 ((name . "paragraph-attrs")))
	(zeroOrMore nil
		    (ref
		     ((name . "paragraph-content-or-hyperlink"))))))
      (define
       ((name . "paragraph-attrs"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "text:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "text:class-names"))
			      (ref
			       ((name . "styleNameRefs")))))
		   (optional nil
			     (attribute
			      ((name . "text:cond-style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (group nil
				    (ref
				     ((name . "xml-id")))
				    (optional nil
					      (attribute
					       ((name . "text:id"))
					       (ref
						((name . "NCName")))))))
		   (optional nil
			     (ref
			      ((name . "common-in-content-meta-attlist"))))))
      (define
       ((name . "text-page-sequence"))
       (element
	((name . "text:page-sequence"))
	(oneOrMore nil
		   (ref
		    ((name . "text-page"))))))
      (define
       ((name . "text-page"))
       (element
	((name . "text:page"))
	(ref
	 ((name . "text-page-attlist")))
	(empty nil)))
      (define
       ((name . "text-page-attlist"))
       (attribute
	((name . "text:master-page-name"))
	(ref
	 ((name . "styleNameRef")))))
      (define
       ((name . "text-list"))
       (element
	((name . "text:list"))
	(ref
	 ((name . "text-list-attr")))
	(optional nil
		  (ref
		   ((name . "text-list-header"))))
	(zeroOrMore nil
		    (ref
		     ((name . "text-list-item"))))))
      (define
       ((name . "text-list-attr"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "text:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "text:continue-numbering"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:continue-list"))
			      (ref
			       ((name . "IDREF")))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "text-list-item"))
       (element
	((name . "text:list-item"))
	(ref
	 ((name . "text-list-item-attr")))
	(ref
	 ((name . "text-list-item-content")))))
      (define
       ((name . "text-list-item-content"))
       (optional nil
		 (ref
		  ((name . "text-number"))))
       (zeroOrMore nil
		   (choice nil
			   (ref
			    ((name . "text-p")))
			   (ref
			    ((name . "text-h")))
			   (ref
			    ((name . "text-list")))
			   (ref
			    ((name . "text-soft-page-break"))))))
      (define
       ((name . "text-list-item-attr"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "text:start-value"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (optional nil
			     (attribute
			      ((name . "text:style-override"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "text-list-header"))
       (element
	((name . "text:list-header"))
	(ref
	 ((name . "text-list-header-attr")))
	(ref
	 ((name . "text-list-item-content")))))
      (define
       ((name . "text-list-header-attr"))
       (optional nil
		 (ref
		  ((name . "xml-id")))))
      (define
       ((name . "text-numbered-paragraph"))
       (element
	((name . "text:numbered-paragraph"))
	(ref
	 ((name . "text-numbered-paragraph-attr")))
	(optional nil
		  (ref
		   ((name . "text-number"))))
	(choice nil
		(ref
		 ((name . "text-p")))
		(ref
		 ((name . "text-h"))))))
      (define
       ((name . "text-numbered-paragraph-attr"))
       (interleave nil
		   (attribute
		    ((name . "text:list-id"))
		    (ref
		     ((name . "NCName"))))
		   (optional nil
			     (attribute
			      ((name . "text:level"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "text:style-name"))
			      (ref
			       ((name . "styleNameRef"))))
			     (attribute
			      ((name . "text:continue-numbering"))
			      (ref
			       ((name . "boolean"))))
			     (attribute
			      ((name . "text:start-value"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "text-section"))
       (element
	((name . "text:section"))
	(ref
	 ((name . "text-section-attlist")))
	(choice nil
		(ref
		 ((name . "text-section-source")))
		(ref
		 ((name . "text-section-source-dde")))
		(empty nil))
	(zeroOrMore nil
		    (ref
		     ((name . "text-content"))))))
      (define
       ((name . "text-section-attlist"))
       (interleave nil
		   (ref
		    ((name . "common-section-attlist")))
		   (choice nil
			   (attribute
			    ((name . "text:display"))
			    (choice nil
				    (value nil "true")
				    (value nil "none")))
			   (group nil
				  (attribute
				   ((name . "text:display"))
				   (value nil "condition"))
				  (attribute
				   ((name . "text:condition"))
				   (ref
				    ((name . "string")))))
			   (empty nil))))
      (define
       ((name . "common-section-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "text:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (attribute
		    ((name . "text:name"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "text:protected"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:protection-key"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "text:protection-key-digest-algorithm"))
			      (ref
			       ((name . "anyIRI")))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "text-section-source"))
       (element
	((name . "text:section-source"))
	(ref
	 ((name . "text-section-source-attr")))))
      (define
       ((name . "text-section-source-attr"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "xlink:type"))
			      (value nil "simple"))
			     (attribute
			      ((name . "xlink:href"))
			      (ref
			       ((name . "anyIRI"))))
			     (optional nil
				       (attribute
					((name . "xlink:show"))
					(value nil "embed"))))
		   (optional nil
			     (attribute
			      ((name . "text:section-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "text:filter-name"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "text-section-source-dde"))
       (ref
	((name . "office-dde-source"))))
      (define
       ((name . "text-tracked-changes"))
       (optional nil
		 (element
		  ((name . "text:tracked-changes"))
		  (ref
		   ((name . "text-tracked-changes-attr")))
		  (zeroOrMore nil
			      (ref
			       ((name . "text-changed-region")))))))
      (define
       ((name . "text-tracked-changes-attr"))
       (optional nil
		 (attribute
		  ((name . "text:track-changes"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "text-changed-region"))
       (element
	((name . "text:changed-region"))
	(ref
	 ((name . "text-changed-region-attr")))
	(ref
	 ((name . "text-changed-region-content")))))
      (define
       ((name . "text-changed-region-attr"))
       (group nil
	      (ref
	       ((name . "xml-id")))
	      (optional nil
			(attribute
			 ((name . "text:id"))
			 (ref
			  ((name . "NCName")))))))
      (define
       ((name . "text-changed-region-content"))
       (choice nil
	       (element
		((name . "text:insertion"))
		(ref
		 ((name . "office-change-info"))))
	       (element
		((name . "text:deletion"))
		(ref
		 ((name . "office-change-info")))
		(zeroOrMore nil
			    (ref
			     ((name . "text-content")))))
	       (element
		((name . "text:format-change"))
		(ref
		 ((name . "office-change-info"))))))
      (define
       ((name . "change-marks"))
       (choice nil
	       (element
		((name . "text:change"))
		(ref
		 ((name . "change-mark-attr"))))
	       (element
		((name . "text:change-start"))
		(ref
		 ((name . "change-mark-attr"))))
	       (element
		((name . "text:change-end"))
		(ref
		 ((name . "change-mark-attr"))))))
      (define
       ((name . "change-mark-attr"))
       (attribute
	((name . "text:change-id"))
	(ref
	 ((name . "IDREF")))))
      (define
       ((name . "text-soft-page-break"))
       (element
	((name . "text:soft-page-break"))
	(empty nil)))
      (define
       ((name . "text-decls"))
       (optional nil
		 (element
		  ((name . "text:variable-decls"))
		  (zeroOrMore nil
			      (ref
			       ((name . "text-variable-decl"))))))
       (optional nil
		 (element
		  ((name . "text:sequence-decls"))
		  (zeroOrMore nil
			      (ref
			       ((name . "text-sequence-decl"))))))
       (optional nil
		 (element
		  ((name . "text:user-field-decls"))
		  (zeroOrMore nil
			      (ref
			       ((name . "text-user-field-decl"))))))
       (optional nil
		 (element
		  ((name . "text:dde-connection-decls"))
		  (zeroOrMore nil
			      (ref
			       ((name . "text-dde-connection-decl"))))))
       (optional nil
		 (ref
		  ((name . "text-alphabetical-index-auto-mark-file")))))
      (define
       ((name . "paragraph-content-or-hyperlink"))
       (choice nil
	       (ref
		((name . "paragraph-content")))
	       (ref
		((name . "text-a")))))
      (define
       ((name . "paragraph-content"))
       (choice nil
	       (text nil)
	       (element
		((name . "text:s"))
		(optional nil
			  (attribute
			   ((name . "text:c"))
			   (ref
			    ((name . "nonNegativeInteger"))))))
	       (element
		((name . "text:tab"))
		(ref
		 ((name . "text-tab-attr"))))
	       (element
		((name . "text:line-break"))
		(empty nil))
	       (ref
		((name . "text-soft-page-break")))
	       (element
		((name . "text:span"))
		(optional nil
			  (attribute
			   ((name . "text:style-name"))
			   (ref
			    ((name . "styleNameRef")))))
		(optional nil
			  (attribute
			   ((name . "text:class-names"))
			   (ref
			    ((name . "styleNameRefs")))))
		(zeroOrMore nil
			    (ref
			     ((name . "paragraph-content-or-hyperlink")))))
	       (element
		((name . "text:meta"))
		(ref
		 ((name . "text-meta-attlist")))
		(zeroOrMore nil
			    (ref
			     ((name . "paragraph-content-or-hyperlink")))))
	       (choice nil
		       (ref
			((name . "text-bookmark")))
		       (ref
			((name . "text-bookmark-start")))
		       (ref
			((name . "text-bookmark-end"))))
	       (element
		((name . "text:reference-mark"))
		(attribute
		 ((name . "text:name"))
		 (ref
		  ((name . "string")))))
	       (choice nil
		       (element
			((name . "text:reference-mark-start"))
			(attribute
			 ((name . "text:name"))
			 (ref
			  ((name . "string")))))
		       (element
			((name . "text:reference-mark-end"))
			(attribute
			 ((name . "text:name"))
			 (ref
			  ((name . "string"))))))
	       (element
		((name . "text:note"))
		(ref
		 ((name . "text-note-class")))
		(optional nil
			  (attribute
			   ((name . "text:id"))
			   (ref
			    ((name . "string")))))
		(element
		 ((name . "text:note-citation"))
		 (optional nil
			   (attribute
			    ((name . "text:label"))
			    (ref
			     ((name . "string")))))
		 (text nil))
		(element
		 ((name . "text:note-body"))
		 (zeroOrMore nil
			     (ref
			      ((name . "text-content"))))))
	       (element
		((name . "text:ruby"))
		(optional nil
			  (attribute
			   ((name . "text:style-name"))
			   (ref
			    ((name . "styleNameRef")))))
		(element
		 ((name . "text:ruby-base"))
		 (zeroOrMore nil
			     (ref
			      ((name . "paragraph-content-or-hyperlink")))))
		(element
		 ((name . "text:ruby-text"))
		 (optional nil
			   (attribute
			    ((name . "text:style-name"))
			    (ref
			     ((name . "styleNameRef")))))
		 (text nil)))
	       (choice nil
		       (ref
			((name . "office-annotation")))
		       (ref
			((name . "office-annotation-end"))))
	       (ref
		((name . "change-marks")))
	       (ref
		((name . "shape")))
	       (element
		((name . "text:date"))
		(ref
		 ((name . "text-date-attlist")))
		(text nil))
	       (element
		((name . "text:time"))
		(ref
		 ((name . "text-time-attlist")))
		(text nil))
	       (element
		((name . "text:page-number"))
		(ref
		 ((name . "text-page-number-attlist")))
		(text nil))
	       (element
		((name . "text:page-continuation"))
		(ref
		 ((name . "text-page-continuation-attlist")))
		(text nil))
	       (element
		((name . "text:sender-firstname"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:sender-lastname"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:sender-initials"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:sender-title"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:sender-position"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:sender-email"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:sender-phone-private"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:sender-fax"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:sender-company"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:sender-phone-work"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:sender-street"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:sender-city"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:sender-postal-code"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:sender-country"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:sender-state-or-province"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:author-name"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:author-initials"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:chapter"))
		(ref
		 ((name . "text-chapter-attlist")))
		(text nil))
	       (element
		((name . "text:file-name"))
		(ref
		 ((name . "text-file-name-attlist")))
		(text nil))
	       (element
		((name . "text:template-name"))
		(ref
		 ((name . "text-template-name-attlist")))
		(text nil))
	       (element
		((name . "text:sheet-name"))
		(text nil))
	       (element
		((name . "text:variable-set"))
		(interleave nil
			    (ref
			     ((name . "common-field-name-attlist")))
			    (ref
			     ((name . "common-field-formula-attlist")))
			    (ref
			     ((name . "common-value-and-type-attlist")))
			    (ref
			     ((name . "common-field-display-value-none-attlist")))
			    (ref
			     ((name . "common-field-data-style-name-attlist"))))
		(text nil))
	       (element
		((name . "text:variable-get"))
		(interleave nil
			    (ref
			     ((name . "common-field-name-attlist")))
			    (ref
			     ((name . "common-field-display-value-formula-attlist")))
			    (ref
			     ((name . "common-field-data-style-name-attlist"))))
		(text nil))
	       (element
		((name . "text:variable-input"))
		(interleave nil
			    (ref
			     ((name . "common-field-name-attlist")))
			    (ref
			     ((name . "common-field-description-attlist")))
			    (ref
			     ((name . "common-value-type-attlist")))
			    (ref
			     ((name . "common-field-display-value-none-attlist")))
			    (ref
			     ((name . "common-field-data-style-name-attlist"))))
		(text nil))
	       (element
		((name . "text:user-field-get"))
		(interleave nil
			    (ref
			     ((name . "common-field-name-attlist")))
			    (ref
			     ((name . "common-field-display-value-formula-none-attlist")))
			    (ref
			     ((name . "common-field-data-style-name-attlist"))))
		(text nil))
	       (element
		((name . "text:user-field-input"))
		(interleave nil
			    (ref
			     ((name . "common-field-name-attlist")))
			    (ref
			     ((name . "common-field-description-attlist")))
			    (ref
			     ((name . "common-field-data-style-name-attlist"))))
		(text nil))
	       (element
		((name . "text:sequence"))
		(interleave nil
			    (ref
			     ((name . "common-field-name-attlist")))
			    (ref
			     ((name . "common-field-formula-attlist")))
			    (ref
			     ((name . "common-field-num-format-attlist")))
			    (ref
			     ((name . "text-sequence-ref-name"))))
		(text nil))
	       (element
		((name . "text:expression"))
		(interleave nil
			    (ref
			     ((name . "common-field-formula-attlist")))
			    (optional nil
				      (ref
				       ((name . "common-value-and-type-attlist"))))
			    (ref
			     ((name . "common-field-display-value-formula-attlist")))
			    (ref
			     ((name . "common-field-data-style-name-attlist"))))
		(text nil))
	       (element
		((name . "text:text-input"))
		(ref
		 ((name . "common-field-description-attlist")))
		(text nil))
	       (element
		((name . "text:initial-creator"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:creation-date"))
		(interleave nil
			    (ref
			     ((name . "common-field-fixed-attlist")))
			    (ref
			     ((name . "common-field-data-style-name-attlist")))
			    (optional nil
				      (attribute
				       ((name . "text:date-value"))
				       (ref
					((name . "dateOrDateTime"))))))
		(text nil))
	       (element
		((name . "text:creation-time"))
		(interleave nil
			    (ref
			     ((name . "common-field-fixed-attlist")))
			    (ref
			     ((name . "common-field-data-style-name-attlist")))
			    (optional nil
				      (attribute
				       ((name . "text:time-value"))
				       (ref
					((name . "timeOrDateTime"))))))
		(text nil))
	       (element
		((name . "text:description"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:user-defined"))
		(interleave nil
			    (ref
			     ((name . "common-field-fixed-attlist")))
			    (attribute
			     ((name . "text:name"))
			     (ref
			      ((name . "string"))))
			    (ref
			     ((name . "common-field-data-style-name-attlist")))
			    (optional nil
				      (attribute
				       ((name . "office:value"))
				       (ref
					((name . "double")))))
			    (optional nil
				      (attribute
				       ((name . "office:date-value"))
				       (ref
					((name . "dateOrDateTime")))))
			    (optional nil
				      (attribute
				       ((name . "office:time-value"))
				       (ref
					((name . "duration")))))
			    (optional nil
				      (attribute
				       ((name . "office:boolean-value"))
				       (ref
					((name . "boolean")))))
			    (optional nil
				      (attribute
				       ((name . "office:string-value"))
				       (ref
					((name . "string"))))))
		(text nil))
	       (element
		((name . "text:print-time"))
		(interleave nil
			    (ref
			     ((name . "common-field-fixed-attlist")))
			    (ref
			     ((name . "common-field-data-style-name-attlist")))
			    (optional nil
				      (attribute
				       ((name . "text:time-value"))
				       (ref
					((name . "time"))))))
		(text nil))
	       (element
		((name . "text:print-date"))
		(interleave nil
			    (ref
			     ((name . "common-field-fixed-attlist")))
			    (ref
			     ((name . "common-field-data-style-name-attlist")))
			    (optional nil
				      (attribute
				       ((name . "text:date-value"))
				       (ref
					((name . "date"))))))
		(text nil))
	       (element
		((name . "text:printed-by"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:title"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:subject"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:keywords"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:editing-cycles"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element
		((name . "text:editing-duration"))
		(interleave nil
			    (ref
			     ((name . "common-field-fixed-attlist")))
			    (ref
			     ((name . "common-field-data-style-name-attlist")))
			    (optional nil
				      (attribute
				       ((name . "text:duration"))
				       (ref
					((name . "duration"))))))
		(text nil))
	       (element
		((name . "text:modification-time"))
		(interleave nil
			    (ref
			     ((name . "common-field-fixed-attlist")))
			    (ref
			     ((name . "common-field-data-style-name-attlist")))
			    (optional nil
				      (attribute
				       ((name . "text:time-value"))
				       (ref
					((name . "time"))))))
		(text nil))
	       (element
		((name . "text:modification-date"))
		(interleave nil
			    (ref
			     ((name . "common-field-fixed-attlist")))
			    (ref
			     ((name . "common-field-data-style-name-attlist")))
			    (optional nil
				      (attribute
				       ((name . "text:date-value"))
				       (ref
					((name . "date"))))))
		(text nil))
	       (element
		((name . "text:creator"))
		(ref
		 ((name . "common-field-fixed-attlist")))
		(text nil))
	       (element nil
			(choice nil
				(name nil "text:page-count")
				(name nil "text:paragraph-count")
				(name nil "text:word-count")
				(name nil "text:character-count")
				(name nil "text:table-count")
				(name nil "text:image-count")
				(name nil "text:object-count"))
			(ref
			 ((name . "common-field-num-format-attlist")))
			(text nil))
	       (element
		((name . "text:database-display"))
		(ref
		 ((name . "text-database-display-attlist")))
		(text nil))
	       (element
		((name . "text:database-next"))
		(ref
		 ((name . "text-database-next-attlist"))))
	       (element
		((name . "text:database-row-select"))
		(ref
		 ((name . "text-database-row-select-attlist"))))
	       (element
		((name . "text:database-row-number"))
		(interleave nil
			    (ref
			     ((name . "common-field-database-table")))
			    (ref
			     ((name . "common-field-num-format-attlist")))
			    (optional nil
				      (attribute
				       ((name . "text:value"))
				       (ref
					((name . "nonNegativeInteger"))))))
		(text nil))
	       (element
		((name . "text:database-name"))
		(ref
		 ((name . "common-field-database-table")))
		(text nil))
	       (element
		((name . "text:page-variable-set"))
		(ref
		 ((name . "text-set-page-variable-attlist")))
		(text nil))
	       (element
		((name . "text:page-variable-get"))
		(ref
		 ((name . "text-get-page-variable-attlist")))
		(text nil))
	       (element
		((name . "text:placeholder"))
		(ref
		 ((name . "text-placeholder-attlist")))
		(text nil))
	       (element
		((name . "text:conditional-text"))
		(ref
		 ((name . "text-conditional-text-attlist")))
		(text nil))
	       (element
		((name . "text:hidden-text"))
		(ref
		 ((name . "text-hidden-text-attlist")))
		(text nil))
	       (element nil
			(choice nil
				(name nil "text:reference-ref")
				(name nil "text:bookmark-ref"))
			(interleave nil
				    (ref
				     ((name . "text-common-ref-content")))
				    (ref
				     ((name . "text-bookmark-ref-content")))))
	       (element
		((name . "text:note-ref"))
		(interleave nil
			    (ref
			     ((name . "text-common-ref-content")))
			    (ref
			     ((name . "text-note-ref-content")))))
	       (element
		((name . "text:sequence-ref"))
		(interleave nil
			    (ref
			     ((name . "text-common-ref-content")))
			    (ref
			     ((name . "text-sequence-ref-content")))))
	       (element
		((name . "text:script"))
		(interleave nil
			    (choice nil
				    (group nil
					   (attribute
					    ((name . "xlink:type"))
					    (value nil "simple"))
					   (attribute
					    ((name . "xlink:href"))
					    (ref
					     ((name . "anyIRI")))))
				    (text nil))
			    (optional nil
				      (attribute
				       ((name . "script:language"))
				       (ref
					((name . "string")))))))
	       (element
		((name . "text:execute-macro"))
		(optional nil
			  (attribute
			   ((name . "text:name"))
			   (ref
			    ((name . "string")))))
		(optional nil
			  (ref
			   ((name . "office-event-listeners"))))
		(text nil))
	       (element
		((name . "text:hidden-paragraph"))
		(ref
		 ((name . "text-hidden-paragraph-attlist")))
		(text nil))
	       (element
		((name . "text:dde-connection"))
		(attribute
		 ((name . "text:connection-name"))
		 (ref
		  ((name . "string"))))
		(text nil))
	       (element
		((name . "text:measure"))
		(attribute
		 ((name . "text:kind"))
		 (choice nil
			 (value nil "value")
			 (value nil "unit")
			 (value nil "gap")))
		(text nil))
	       (element
		((name . "text:table-formula"))
		(interleave nil
			    (ref
			     ((name . "common-field-formula-attlist")))
			    (ref
			     ((name . "common-field-display-value-formula-attlist")))
			    (ref
			     ((name . "common-field-data-style-name-attlist"))))
		(text nil))
	       (element
		((name . "text:meta-field"))
		(ref
		 ((name . "text-meta-field-attlist")))
		(zeroOrMore nil
			    (ref
			     ((name . "paragraph-content-or-hyperlink")))))
	       (element
		((name . "text:toc-mark-start"))
		(ref
		 ((name . "text-toc-mark-start-attrs"))))
	       (element
		((name . "text:toc-mark-end"))
		(ref
		 ((name . "text-id"))))
	       (element
		((name . "text:toc-mark"))
		(attribute
		 ((name . "text:string-value"))
		 (ref
		  ((name . "string"))))
		(ref
		 ((name . "text-outline-level"))))
	       (element
		((name . "text:user-index-mark-start"))
		(ref
		 ((name . "text-id")))
		(ref
		 ((name . "text-outline-level")))
		(ref
		 ((name . "text-index-name"))))
	       (element
		((name . "text:user-index-mark-end"))
		(ref
		 ((name . "text-id"))))
	       (element
		((name . "text:user-index-mark"))
		(attribute
		 ((name . "text:string-value"))
		 (ref
		  ((name . "string"))))
		(ref
		 ((name . "text-outline-level")))
		(ref
		 ((name . "text-index-name"))))
	       (element
		((name . "text:alphabetical-index-mark-start"))
		(ref
		 ((name . "text-id")))
		(ref
		 ((name . "text-alphabetical-index-mark-attrs"))))
	       (element
		((name . "text:alphabetical-index-mark-end"))
		(ref
		 ((name . "text-id"))))
	       (element
		((name . "text:alphabetical-index-mark"))
		(attribute
		 ((name . "text:string-value"))
		 (ref
		  ((name . "string"))))
		(ref
		 ((name . "text-alphabetical-index-mark-attrs"))))
	       (element
		((name . "text:bibliography-mark"))
		(attribute
		 ((name . "text:bibliography-type"))
		 (ref
		  ((name . "text-bibliography-types"))))
		(zeroOrMore nil
			    (attribute nil
				       (choice nil
					       (name nil "text:identifier")
					       (name nil "text:address")
					       (name nil "text:annote")
					       (name nil "text:author")
					       (name nil "text:booktitle")
					       (name nil "text:chapter")
					       (name nil "text:edition")
					       (name nil "text:editor")
					       (name nil "text:howpublished")
					       (name nil "text:institution")
					       (name nil "text:journal")
					       (name nil "text:month")
					       (name nil "text:note")
					       (name nil "text:number")
					       (name nil "text:organizations")
					       (name nil "text:pages")
					       (name nil "text:publisher")
					       (name nil "text:school")
					       (name nil "text:series")
					       (name nil "text:title")
					       (name nil "text:report-type")
					       (name nil "text:volume")
					       (name nil "text:year")
					       (name nil "text:url")
					       (name nil "text:custom1")
					       (name nil "text:custom2")
					       (name nil "text:custom3")
					       (name nil "text:custom4")
					       (name nil "text:custom5")
					       (name nil "text:isbn")
					       (name nil "text:issn"))
				       (ref
					((name . "string")))))
		(text nil))
	       (element
		((name . "presentation:header"))
		(empty nil))
	       (element
		((name . "presentation:footer"))
		(empty nil))
	       (element
		((name . "presentation:date-time"))
		(empty nil))))
      (define
       ((name . "text-tab-attr"))
       (optional nil
		 (attribute
		  ((name . "text:tab-ref"))
		  (ref
		   ((name . "nonNegativeInteger"))))))
      (define
       ((name . "text-a"))
       (element
	((name . "text:a"))
	(ref
	 ((name . "text-a-attlist")))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))
	(zeroOrMore nil
		    (ref
		     ((name . "paragraph-content"))))))
      (define
       ((name . "text-a-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "office:name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "office:title"))
			      (ref
			       ((name . "string")))))
		   (attribute
		    ((name . "xlink:type"))
		    (value nil "simple"))
		   (attribute
		    ((name . "xlink:href"))
		    (ref
		     ((name . "anyIRI"))))
		   (optional nil
			     (attribute
			      ((name . "xlink:actuate"))
			      (value nil "onRequest")))
		   (optional nil
			     (attribute
			      ((name . "office:target-frame-name"))
			      (ref
			       ((name . "targetFrameName")))))
		   (optional nil
			     (attribute
			      ((name . "xlink:show"))
			      (choice nil
				      (value nil "new")
				      (value nil "replace"))))
		   (optional nil
			     (attribute
			      ((name . "text:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "text:visited-style-name"))
			      (ref
			       ((name . "styleNameRef")))))))
      (define
       ((name . "text-meta-attlist"))
       (interleave nil
		   (optional nil
			     (ref
			      ((name . "common-in-content-meta-attlist"))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "text-bookmark"))
       (element
	((name . "text:bookmark"))
	(ref
	 ((name . "text-bookmark-attlist")))
	(empty nil)))
      (define
       ((name . "text-bookmark-start"))
       (element
	((name . "text:bookmark-start"))
	(ref
	 ((name . "text-bookmark-start-attlist")))
	(empty nil)))
      (define
       ((name . "text-bookmark-end"))
       (element
	((name . "text:bookmark-end"))
	(ref
	 ((name . "text-bookmark-end-attlist")))
	(empty nil)))
      (define
       ((name . "text-bookmark-attlist"))
       (interleave nil
		   (attribute
		    ((name . "text:name"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "text-bookmark-start-attlist"))
       (interleave nil
		   (attribute
		    ((name . "text:name"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))
		   (optional nil
			     (ref
			      ((name . "common-in-content-meta-attlist"))))))
      (define
       ((name . "text-bookmark-end-attlist"))
       (attribute
	((name . "text:name"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "text-note-class"))
       (attribute
	((name . "text:note-class"))
	(choice nil
		(value nil "footnote")
		(value nil "endnote"))))
      (define
       ((name . "text-date-attlist"))
       (interleave nil
		   (interleave nil
			       (ref
				((name . "common-field-fixed-attlist")))
			       (ref
				((name . "common-field-data-style-name-attlist"))))
		   (optional nil
			     (attribute
			      ((name . "text:date-value"))
			      (ref
			       ((name . "dateOrDateTime")))))
		   (optional nil
			     (attribute
			      ((name . "text:date-adjust"))
			      (ref
			       ((name . "duration")))))))
      (define
       ((name . "text-time-attlist"))
       (interleave nil
		   (interleave nil
			       (ref
				((name . "common-field-fixed-attlist")))
			       (ref
				((name . "common-field-data-style-name-attlist"))))
		   (optional nil
			     (attribute
			      ((name . "text:time-value"))
			      (ref
			       ((name . "timeOrDateTime")))))
		   (optional nil
			     (attribute
			      ((name . "text:time-adjust"))
			      (ref
			       ((name . "duration")))))))
      (define
       ((name . "text-page-number-attlist"))
       (interleave nil
		   (interleave nil
			       (ref
				((name . "common-field-num-format-attlist")))
			       (ref
				((name . "common-field-fixed-attlist"))))
		   (optional nil
			     (attribute
			      ((name . "text:page-adjust"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "text:select-page"))
			      (choice nil
				      (value nil "previous")
				      (value nil "current")
				      (value nil "next"))))))
      (define
       ((name . "text-page-continuation-attlist"))
       (interleave nil
		   (attribute
		    ((name . "text:select-page"))
		    (choice nil
			    (value nil "previous")
			    (value nil "next")))
		   (optional nil
			     (attribute
			      ((name . "text:string-value"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "text-chapter-attlist"))
       (interleave nil
		   (attribute
		    ((name . "text:display"))
		    (choice nil
			    (value nil "name")
			    (value nil "number")
			    (value nil "number-and-name")
			    (value nil "plain-number-and-name")
			    (value nil "plain-number")))
		   (attribute
		    ((name . "text:outline-level"))
		    (ref
		     ((name . "nonNegativeInteger"))))))
      (define
       ((name . "text-file-name-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "text:display"))
			      (choice nil
				      (value nil "full")
				      (value nil "path")
				      (value nil "name")
				      (value nil "name-and-extension"))))
		   (ref
		    ((name . "common-field-fixed-attlist")))))
      (define
       ((name . "text-template-name-attlist"))
       (optional nil
		 (attribute
		  ((name . "text:display"))
		  (choice nil
			  (value nil "full")
			  (value nil "path")
			  (value nil "name")
			  (value nil "name-and-extension")
			  (value nil "area")
			  (value nil "title")))))
      (define
       ((name . "text-variable-decl"))
       (element
	((name . "text:variable-decl"))
	(ref
	 ((name . "common-field-name-attlist")))
	(ref
	 ((name . "common-value-type-attlist")))))
      (define
       ((name . "text-user-field-decl"))
       (element
	((name . "text:user-field-decl"))
	(ref
	 ((name . "common-field-name-attlist")))
	(optional nil
		  (ref
		   ((name . "common-field-formula-attlist"))))
	(ref
	 ((name . "common-value-and-type-attlist")))))
      (define
       ((name . "text-sequence-decl"))
       (element
	((name . "text:sequence-decl"))
	(ref
	 ((name . "text-sequence-decl-attlist")))))
      (define
       ((name . "text-sequence-decl-attlist"))
       (interleave nil
		   (ref
		    ((name . "common-field-name-attlist")))
		   (attribute
		    ((name . "text:display-outline-level"))
		    (ref
		     ((name . "nonNegativeInteger"))))
		   (optional nil
			     (attribute
			      ((name . "text:separation-character"))
			      (ref
			       ((name . "character")))))))
      (define
       ((name . "text-sequence-ref-name"))
       (optional nil
		 (attribute
		  ((name . "text:ref-name"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "common-field-database-table"))
       (ref
	((name . "common-field-database-table-attlist")))
       (ref
	((name . "common-field-database-name"))))
      (define
       ((name . "common-field-database-name"))
       (choice nil
	       (optional nil
			 (attribute
			  ((name . "text:database-name"))
			  (ref
			   ((name . "string")))))
	       (ref
		((name . "form-connection-resource")))))
      (define
       ((name . "common-field-database-table-attlist"))
       (interleave nil
		   (attribute
		    ((name . "text:table-name"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "text:table-type"))
			      (choice nil
				      (value nil "table")
				      (value nil "query")
				      (value nil "command"))))))
      (define
       ((name . "text-database-display-attlist"))
       (interleave nil
		   (ref
		    ((name . "common-field-database-table")))
		   (ref
		    ((name . "common-field-data-style-name-attlist")))
		   (attribute
		    ((name . "text:column-name"))
		    (ref
		     ((name . "string"))))))
      (define
       ((name . "text-database-next-attlist"))
       (interleave nil
		   (ref
		    ((name . "common-field-database-table")))
		   (optional nil
			     (attribute
			      ((name . "text:condition"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "text-database-row-select-attlist"))
       (interleave nil
		   (ref
		    ((name . "common-field-database-table")))
		   (optional nil
			     (attribute
			      ((name . "text:condition"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "text:row-number"))
			      (ref
			       ((name . "nonNegativeInteger")))))))
      (define
       ((name . "text-set-page-variable-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "text:active"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:page-adjust"))
			      (ref
			       ((name . "integer")))))))
      (define
       ((name . "text-get-page-variable-attlist"))
       (ref
	((name . "common-field-num-format-attlist"))))
      (define
       ((name . "text-placeholder-attlist"))
       (interleave nil
		   (attribute
		    ((name . "text:placeholder-type"))
		    (choice nil
			    (value nil "text")
			    (value nil "table")
			    (value nil "text-box")
			    (value nil "image")
			    (value nil "object")))
		   (ref
		    ((name . "common-field-description-attlist")))))
      (define
       ((name . "text-conditional-text-attlist"))
       (interleave nil
		   (attribute
		    ((name . "text:condition"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "text:string-value-if-true"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "text:string-value-if-false"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "text:current-value"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "text-hidden-text-attlist"))
       (interleave nil
		   (attribute
		    ((name . "text:condition"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "text:string-value"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "text:is-hidden"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "text-common-ref-content"))
       (interleave nil
		   (text nil)
		   (optional nil
			     (attribute
			      ((name . "text:ref-name"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "text-bookmark-ref-content"))
       (optional nil
		 (attribute
		  ((name . "text:reference-format"))
		  (choice nil
			  (ref
			   ((name . "common-ref-format-values")))
			  (value nil "number-no-superior")
			  (value nil "number-all-superior")
			  (value nil "number")))))
      (define
       ((name . "text-note-ref-content"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "text:reference-format"))
			      (choice nil
				      (ref
				       ((name . "common-ref-format-values"))))))
		   (ref
		    ((name . "text-note-class")))))
      (define
       ((name . "text-sequence-ref-content"))
       (optional nil
		 (attribute
		  ((name . "text:reference-format"))
		  (choice nil
			  (ref
			   ((name . "common-ref-format-values")))
			  (value nil "category-and-value")
			  (value nil "caption")
			  (value nil "value")))))
      (define
       ((name . "common-ref-format-values"))
       (choice nil
	       (value nil "page")
	       (value nil "chapter")
	       (value nil "direction")
	       (value nil "text")))
      (define
       ((name . "text-hidden-paragraph-attlist"))
       (interleave nil
		   (attribute
		    ((name . "text:condition"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "text:is-hidden"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "text-meta-field-attlist"))
       (interleave nil
		   (ref
		    ((name . "xml-id")))
		   (ref
		    ((name . "common-field-data-style-name-attlist")))))
      (define
       ((name . "common-value-type-attlist"))
       (attribute
	((name . "office:value-type"))
	(ref
	 ((name . "valueType")))))
      (define
       ((name . "common-value-and-type-attlist"))
       (choice nil
	       (group nil
		      (attribute
		       ((name . "office:value-type"))
		       (value nil "float"))
		      (attribute
		       ((name . "office:value"))
		       (ref
			((name . "double")))))
	       (group nil
		      (attribute
		       ((name . "office:value-type"))
		       (value nil "percentage"))
		      (attribute
		       ((name . "office:value"))
		       (ref
			((name . "double")))))
	       (group nil
		      (attribute
		       ((name . "office:value-type"))
		       (value nil "currency"))
		      (attribute
		       ((name . "office:value"))
		       (ref
			((name . "double"))))
		      (optional nil
				(attribute
				 ((name . "office:currency"))
				 (ref
				  ((name . "string"))))))
	       (group nil
		      (attribute
		       ((name . "office:value-type"))
		       (value nil "date"))
		      (attribute
		       ((name . "office:date-value"))
		       (ref
			((name . "dateOrDateTime")))))
	       (group nil
		      (attribute
		       ((name . "office:value-type"))
		       (value nil "time"))
		      (attribute
		       ((name . "office:time-value"))
		       (ref
			((name . "duration")))))
	       (group nil
		      (attribute
		       ((name . "office:value-type"))
		       (value nil "boolean"))
		      (attribute
		       ((name . "office:boolean-value"))
		       (ref
			((name . "boolean")))))
	       (group nil
		      (attribute
		       ((name . "office:value-type"))
		       (value nil "string"))
		      (optional nil
				(attribute
				 ((name . "office:string-value"))
				 (ref
				  ((name . "string"))))))))
      (define
       ((name . "common-field-fixed-attlist"))
       (optional nil
		 (attribute
		  ((name . "text:fixed"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "common-field-name-attlist"))
       (attribute
	((name . "text:name"))
	(ref
	 ((name . "variableName")))))
      (define
       ((name . "common-field-description-attlist"))
       (optional nil
		 (attribute
		  ((name . "text:description"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "common-field-display-value-none-attlist"))
       (optional nil
		 (attribute
		  ((name . "text:display"))
		  (choice nil
			  (value nil "value")
			  (value nil "none")))))
      (define
       ((name . "common-field-display-value-formula-none-attlist"))
       (optional nil
		 (attribute
		  ((name . "text:display"))
		  (choice nil
			  (value nil "value")
			  (value nil "formula")
			  (value nil "none")))))
      (define
       ((name . "common-field-display-value-formula-attlist"))
       (optional nil
		 (attribute
		  ((name . "text:display"))
		  (choice nil
			  (value nil "value")
			  (value nil "formula")))))
      (define
       ((name . "common-field-formula-attlist"))
       (optional nil
		 (attribute
		  ((name . "text:formula"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "common-field-data-style-name-attlist"))
       (optional nil
		 (attribute
		  ((name . "style:data-style-name"))
		  (ref
		   ((name . "styleNameRef"))))))
      (define
       ((name . "common-field-num-format-attlist"))
       (optional nil
		 (ref
		  ((name . "common-num-format-attlist")))))
      (define
       ((name . "text-toc-mark-start-attrs"))
       (ref
	((name . "text-id")))
       (ref
	((name . "text-outline-level"))))
      (define
       ((name . "text-outline-level"))
       (optional nil
		 (attribute
		  ((name . "text:outline-level"))
		  (ref
		   ((name . "positiveInteger"))))))
      (define
       ((name . "text-id"))
       (attribute
	((name . "text:id"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "text-index-name"))
       (attribute
	((name . "text:index-name"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "text-alphabetical-index-mark-attrs"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "text:key1"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "text:key2"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "text:string-value-phonetic"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "text:key1-phonetic"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "text:key2-phonetic"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "text:main-entry"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "text-bibliography-types"))
       (choice nil
	       (value nil "article")
	       (value nil "book")
	       (value nil "booklet")
	       (value nil "conference")
	       (value nil "custom1")
	       (value nil "custom2")
	       (value nil "custom3")
	       (value nil "custom4")
	       (value nil "custom5")
	       (value nil "email")
	       (value nil "inbook")
	       (value nil "incollection")
	       (value nil "inproceedings")
	       (value nil "journal")
	       (value nil "manual")
	       (value nil "mastersthesis")
	       (value nil "misc")
	       (value nil "phdthesis")
	       (value nil "proceedings")
	       (value nil "techreport")
	       (value nil "unpublished")
	       (value nil "www")))
      (define
       ((name . "text-index-body"))
       (element
	((name . "text:index-body"))
	(zeroOrMore nil
		    (ref
		     ((name . "index-content-main"))))))
      (define
       ((name . "index-content-main"))
       (choice nil
	       (ref
		((name . "text-content")))
	       (ref
		((name . "text-index-title")))))
      (define
       ((name . "text-index-title"))
       (element
	((name . "text:index-title"))
	(ref
	 ((name . "common-section-attlist")))
	(zeroOrMore nil
		    (ref
		     ((name . "index-content-main"))))))
      (define
       ((name . "text-table-of-content"))
       (element
	((name . "text:table-of-content"))
	(ref
	 ((name . "common-section-attlist")))
	(ref
	 ((name . "text-table-of-content-source")))
	(ref
	 ((name . "text-index-body")))))
      (define
       ((name . "text-table-of-content-source"))
       (element
	((name . "text:table-of-content-source"))
	(ref
	 ((name . "text-table-of-content-source-attlist")))
	(optional nil
		  (ref
		   ((name . "text-index-title-template"))))
	(zeroOrMore nil
		    (ref
		     ((name . "text-table-of-content-entry-template"))))
	(zeroOrMore nil
		    (ref
		     ((name . "text-index-source-styles"))))))
      (define
       ((name . "text-table-of-content-source-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "text:outline-level"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "text:use-outline-level"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:use-index-marks"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:use-index-source-styles"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:index-scope"))
			      (choice nil
				      (value nil "document")
				      (value nil "chapter"))))
		   (optional nil
			     (attribute
			      ((name . "text:relative-tab-stop-position"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "text-table-of-content-entry-template"))
       (element
	((name . "text:table-of-content-entry-template"))
	(ref
	 ((name . "text-table-of-content-entry-template-attlist")))
	(zeroOrMore nil
		    (ref
		     ((name . "text-table-of-content-children"))))))
      (define
       ((name . "text-table-of-content-children"))
       (choice nil
	       (ref
		((name . "text-index-entry-chapter")))
	       (ref
		((name . "text-index-entry-page-number")))
	       (ref
		((name . "text-index-entry-text")))
	       (ref
		((name . "text-index-entry-span")))
	       (ref
		((name . "text-index-entry-tab-stop")))
	       (ref
		((name . "text-index-entry-link-start")))
	       (ref
		((name . "text-index-entry-link-end")))))
      (define
       ((name . "text-table-of-content-entry-template-attlist"))
       (interleave nil
		   (attribute
		    ((name . "text:outline-level"))
		    (ref
		     ((name . "positiveInteger"))))
		   (attribute
		    ((name . "text:style-name"))
		    (ref
		     ((name . "styleNameRef"))))))
      (define
       ((name . "text-illustration-index"))
       (element
	((name . "text:illustration-index"))
	(ref
	 ((name . "common-section-attlist")))
	(ref
	 ((name . "text-illustration-index-source")))
	(ref
	 ((name . "text-index-body")))))
      (define
       ((name . "text-illustration-index-source"))
       (element
	((name . "text:illustration-index-source"))
	(ref
	 ((name . "text-illustration-index-source-attrs")))
	(optional nil
		  (ref
		   ((name . "text-index-title-template"))))
	(optional nil
		  (ref
		   ((name . "text-illustration-index-entry-template"))))))
      (define
       ((name . "text-illustration-index-source-attrs"))
       (interleave nil
		   (ref
		    ((name . "text-index-scope-attr")))
		   (ref
		    ((name . "text-relative-tab-stop-position-attr")))
		   (optional nil
			     (attribute
			      ((name . "text:use-caption"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:caption-sequence-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "text:caption-sequence-format"))
			      (choice nil
				      (value nil "text")
				      (value nil "category-and-value")
				      (value nil "caption"))))))
      (define
       ((name . "text-index-scope-attr"))
       (optional nil
		 (attribute
		  ((name . "text:index-scope"))
		  (choice nil
			  (value nil "document")
			  (value nil "chapter")))))
      (define
       ((name . "text-relative-tab-stop-position-attr"))
       (optional nil
		 (attribute
		  ((name . "text:relative-tab-stop-position"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "text-illustration-index-entry-template"))
       (element
	((name . "text:illustration-index-entry-template"))
	(ref
	 ((name . "text-illustration-index-entry-content")))))
      (define
       ((name . "text-illustration-index-entry-content"))
       (ref
	((name . "text-illustration-index-entry-template-attrs")))
       (zeroOrMore nil
		   (choice nil
			   (ref
			    ((name . "text-index-entry-chapter")))
			   (ref
			    ((name . "text-index-entry-page-number")))
			   (ref
			    ((name . "text-index-entry-text")))
			   (ref
			    ((name . "text-index-entry-span")))
			   (ref
			    ((name . "text-index-entry-tab-stop"))))))
      (define
       ((name . "text-illustration-index-entry-template-attrs"))
       (attribute
	((name . "text:style-name"))
	(ref
	 ((name . "styleNameRef")))))
      (define
       ((name . "text-table-index"))
       (element
	((name . "text:table-index"))
	(ref
	 ((name . "common-section-attlist")))
	(ref
	 ((name . "text-table-index-source")))
	(ref
	 ((name . "text-index-body")))))
      (define
       ((name . "text-table-index-source"))
       (element
	((name . "text:table-index-source"))
	(ref
	 ((name . "text-illustration-index-source-attrs")))
	(optional nil
		  (ref
		   ((name . "text-index-title-template"))))
	(optional nil
		  (ref
		   ((name . "text-table-index-entry-template"))))))
      (define
       ((name . "text-table-index-entry-template"))
       (element
	((name . "text:table-index-entry-template"))
	(ref
	 ((name . "text-illustration-index-entry-content")))))
      (define
       ((name . "text-object-index"))
       (element
	((name . "text:object-index"))
	(ref
	 ((name . "common-section-attlist")))
	(ref
	 ((name . "text-object-index-source")))
	(ref
	 ((name . "text-index-body")))))
      (define
       ((name . "text-object-index-source"))
       (element
	((name . "text:object-index-source"))
	(ref
	 ((name . "text-object-index-source-attrs")))
	(optional nil
		  (ref
		   ((name . "text-index-title-template"))))
	(optional nil
		  (ref
		   ((name . "text-object-index-entry-template"))))))
      (define
       ((name . "text-object-index-source-attrs"))
       (interleave nil
		   (ref
		    ((name . "text-index-scope-attr")))
		   (ref
		    ((name . "text-relative-tab-stop-position-attr")))
		   (optional nil
			     (attribute
			      ((name . "text:use-spreadsheet-objects"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:use-math-objects"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:use-draw-objects"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:use-chart-objects"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:use-other-objects"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "text-object-index-entry-template"))
       (element
	((name . "text:object-index-entry-template"))
	(ref
	 ((name . "text-illustration-index-entry-content")))))
      (define
       ((name . "text-user-index"))
       (element
	((name . "text:user-index"))
	(ref
	 ((name . "common-section-attlist")))
	(ref
	 ((name . "text-user-index-source")))
	(ref
	 ((name . "text-index-body")))))
      (define
       ((name . "text-user-index-source"))
       (element
	((name . "text:user-index-source"))
	(ref
	 ((name . "text-user-index-source-attr")))
	(optional nil
		  (ref
		   ((name . "text-index-title-template"))))
	(zeroOrMore nil
		    (ref
		     ((name . "text-user-index-entry-template"))))
	(zeroOrMore nil
		    (ref
		     ((name . "text-index-source-styles"))))))
      (define
       ((name . "text-user-index-source-attr"))
       (interleave nil
		   (ref
		    ((name . "text-index-scope-attr")))
		   (ref
		    ((name . "text-relative-tab-stop-position-attr")))
		   (optional nil
			     (attribute
			      ((name . "text:use-index-marks"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:use-index-source-styles"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:use-graphics"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:use-tables"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:use-floating-frames"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:use-objects"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:copy-outline-levels"))
			      (ref
			       ((name . "boolean")))))
		   (attribute
		    ((name . "text:index-name"))
		    (ref
		     ((name . "string"))))))
      (define
       ((name . "text-user-index-entry-template"))
       (element
	((name . "text:user-index-entry-template"))
	(ref
	 ((name . "text-user-index-entry-template-attrs")))
	(zeroOrMore nil
		    (choice nil
			    (ref
			     ((name . "text-index-entry-chapter")))
			    (ref
			     ((name . "text-index-entry-page-number")))
			    (ref
			     ((name . "text-index-entry-text")))
			    (ref
			     ((name . "text-index-entry-span")))
			    (ref
			     ((name . "text-index-entry-tab-stop")))))))
      (define
       ((name . "text-user-index-entry-template-attrs"))
       (interleave nil
		   (attribute
		    ((name . "text:outline-level"))
		    (ref
		     ((name . "positiveInteger"))))
		   (attribute
		    ((name . "text:style-name"))
		    (ref
		     ((name . "styleNameRef"))))))
      (define
       ((name . "text-alphabetical-index"))
       (element
	((name . "text:alphabetical-index"))
	(ref
	 ((name . "common-section-attlist")))
	(ref
	 ((name . "text-alphabetical-index-source")))
	(ref
	 ((name . "text-index-body")))))
      (define
       ((name . "text-alphabetical-index-source"))
       (element
	((name . "text:alphabetical-index-source"))
	(ref
	 ((name . "text-alphabetical-index-source-attrs")))
	(optional nil
		  (ref
		   ((name . "text-index-title-template"))))
	(zeroOrMore nil
		    (ref
		     ((name . "text-alphabetical-index-entry-template"))))))
      (define
       ((name . "text-alphabetical-index-source-attrs"))
       (interleave nil
		   (ref
		    ((name . "text-index-scope-attr")))
		   (ref
		    ((name . "text-relative-tab-stop-position-attr")))
		   (optional nil
			     (attribute
			      ((name . "text:ignore-case"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:main-entry-style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "text:alphabetical-separators"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:combine-entries"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:combine-entries-with-dash"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:combine-entries-with-pp"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:use-keys-as-entries"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:capitalize-entries"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:comma-separated"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "fo:language"))
			      (ref
			       ((name . "languageCode")))))
		   (optional nil
			     (attribute
			      ((name . "fo:country"))
			      (ref
			       ((name . "countryCode")))))
		   (optional nil
			     (attribute
			      ((name . "fo:script"))
			      (ref
			       ((name . "scriptCode")))))
		   (optional nil
			     (attribute
			      ((name . "style:rfc-language-tag"))
			      (ref
			       ((name . "language")))))
		   (optional nil
			     (attribute
			      ((name . "text:sort-algorithm"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "text-alphabetical-index-auto-mark-file"))
       (element
	((name . "text:alphabetical-index-auto-mark-file"))
	(attribute
	 ((name . "xlink:type"))
	 (value nil "simple"))
	(attribute
	 ((name . "xlink:href"))
	 (ref
	  ((name . "anyIRI"))))))
      (define
       ((name . "text-alphabetical-index-entry-template"))
       (element
	((name . "text:alphabetical-index-entry-template"))
	(ref
	 ((name . "text-alphabetical-index-entry-template-attrs")))
	(zeroOrMore nil
		    (choice nil
			    (ref
			     ((name . "text-index-entry-chapter")))
			    (ref
			     ((name . "text-index-entry-page-number")))
			    (ref
			     ((name . "text-index-entry-text")))
			    (ref
			     ((name . "text-index-entry-span")))
			    (ref
			     ((name . "text-index-entry-tab-stop")))))))
      (define
       ((name . "text-alphabetical-index-entry-template-attrs"))
       (interleave nil
		   (attribute
		    ((name . "text:outline-level"))
		    (choice nil
			    (value nil "1")
			    (value nil "2")
			    (value nil "3")
			    (value nil "separator")))
		   (attribute
		    ((name . "text:style-name"))
		    (ref
		     ((name . "styleNameRef"))))))
      (define
       ((name . "text-bibliography"))
       (element
	((name . "text:bibliography"))
	(ref
	 ((name . "common-section-attlist")))
	(ref
	 ((name . "text-bibliography-source")))
	(ref
	 ((name . "text-index-body")))))
      (define
       ((name . "text-bibliography-source"))
       (element
	((name . "text:bibliography-source"))
	(optional nil
		  (ref
		   ((name . "text-index-title-template"))))
	(zeroOrMore nil
		    (ref
		     ((name . "text-bibliography-entry-template"))))))
      (define
       ((name . "text-bibliography-entry-template"))
       (element
	((name . "text:bibliography-entry-template"))
	(ref
	 ((name . "text-bibliography-entry-template-attrs")))
	(zeroOrMore nil
		    (choice nil
			    (ref
			     ((name . "text-index-entry-span")))
			    (ref
			     ((name . "text-index-entry-tab-stop")))
			    (ref
			     ((name . "text-index-entry-bibliography")))))))
      (define
       ((name . "text-bibliography-entry-template-attrs"))
       (interleave nil
		   (attribute
		    ((name . "text:bibliography-type"))
		    (ref
		     ((name . "text-bibliography-types"))))
		   (attribute
		    ((name . "text:style-name"))
		    (ref
		     ((name . "styleNameRef"))))))
      (define
       ((name . "text-index-source-styles"))
       (element
	((name . "text:index-source-styles"))
	(attribute
	 ((name . "text:outline-level"))
	 (ref
	  ((name . "positiveInteger"))))
	(zeroOrMore nil
		    (ref
		     ((name . "text-index-source-style"))))))
      (define
       ((name . "text-index-source-style"))
       (element
	((name . "text:index-source-style"))
	(attribute
	 ((name . "text:style-name"))
	 (ref
	  ((name . "styleName"))))
	(empty nil)))
      (define
       ((name . "text-index-title-template"))
       (element
	((name . "text:index-title-template"))
	(optional nil
		  (attribute
		   ((name . "text:style-name"))
		   (ref
		    ((name . "styleNameRef")))))
	(text nil)))
      (define
       ((name . "text-index-entry-chapter"))
       (element
	((name . "text:index-entry-chapter"))
	(optional nil
		  (attribute
		   ((name . "text:style-name"))
		   (ref
		    ((name . "styleNameRef")))))
	(ref
	 ((name . "text-index-entry-chapter-attrs")))))
      (define
       ((name . "text-index-entry-chapter-attrs"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "text:display"))
			      (choice nil
				      (value nil "name")
				      (value nil "number")
				      (value nil "number-and-name")
				      (value nil "plain-number")
				      (value nil "plain-number-and-name"))))
		   (optional nil
			     (attribute
			      ((name . "text:outline-level"))
			      (ref
			       ((name . "positiveInteger")))))))
      (define
       ((name . "text-index-entry-text"))
       (element
	((name . "text:index-entry-text"))
	(optional nil
		  (attribute
		   ((name . "text:style-name"))
		   (ref
		    ((name . "styleNameRef")))))))
      (define
       ((name . "text-index-entry-page-number"))
       (element
	((name . "text:index-entry-page-number"))
	(optional nil
		  (attribute
		   ((name . "text:style-name"))
		   (ref
		    ((name . "styleNameRef")))))))
      (define
       ((name . "text-index-entry-span"))
       (element
	((name . "text:index-entry-span"))
	(optional nil
		  (attribute
		   ((name . "text:style-name"))
		   (ref
		    ((name . "styleNameRef")))))
	(text nil)))
      (define
       ((name . "text-index-entry-bibliography"))
       (element
	((name . "text:index-entry-bibliography"))
	(ref
	 ((name . "text-index-entry-bibliography-attrs")))))
      (define
       ((name . "text-index-entry-bibliography-attrs"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "text:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (attribute
		    ((name . "text:bibliography-data-field"))
		    (choice nil
			    (value nil "address")
			    (value nil "annote")
			    (value nil "author")
			    (value nil "bibliography-type")
			    (value nil "booktitle")
			    (value nil "chapter")
			    (value nil "custom1")
			    (value nil "custom2")
			    (value nil "custom3")
			    (value nil "custom4")
			    (value nil "custom5")
			    (value nil "edition")
			    (value nil "editor")
			    (value nil "howpublished")
			    (value nil "identifier")
			    (value nil "institution")
			    (value nil "isbn")
			    (value nil "issn")
			    (value nil "journal")
			    (value nil "month")
			    (value nil "note")
			    (value nil "number")
			    (value nil "organizations")
			    (value nil "pages")
			    (value nil "publisher")
			    (value nil "report-type")
			    (value nil "school")
			    (value nil "series")
			    (value nil "title")
			    (value nil "url")
			    (value nil "volume")
			    (value nil "year")))))
      (define
       ((name . "text-index-entry-tab-stop"))
       (element
	((name . "text:index-entry-tab-stop"))
	(optional nil
		  (attribute
		   ((name . "text:style-name"))
		   (ref
		    ((name . "styleNameRef")))))
	(ref
	 ((name . "text-index-entry-tab-stop-attrs")))))
      (define
       ((name . "text-index-entry-tab-stop-attrs"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "style:leader-char"))
			      (ref
			       ((name . "character")))))
		   (choice nil
			   (attribute
			    ((name . "style:type"))
			    (value nil "right"))
			   (group nil
				  (attribute
				   ((name . "style:type"))
				   (value nil "left"))
				  (attribute
				   ((name . "style:position"))
				   (ref
				    ((name . "length"))))))))
      (define
       ((name . "text-index-entry-link-start"))
       (element
	((name . "text:index-entry-link-start"))
	(optional nil
		  (attribute
		   ((name . "text:style-name"))
		   (ref
		    ((name . "styleNameRef")))))))
      (define
       ((name . "text-index-entry-link-end"))
       (element
	((name . "text:index-entry-link-end"))
	(optional nil
		  (attribute
		   ((name . "text:style-name"))
		   (ref
		    ((name . "styleNameRef")))))))
      (define
       ((name . "table-table"))
       (element
	((name . "table:table"))
	(ref
	 ((name . "table-table-attlist")))
	(optional nil
		  (ref
		   ((name . "table-title"))))
	(optional nil
		  (ref
		   ((name . "table-desc"))))
	(optional nil
		  (ref
		   ((name . "table-table-source"))))
	(optional nil
		  (ref
		   ((name . "office-dde-source"))))
	(optional nil
		  (ref
		   ((name . "table-scenario"))))
	(optional nil
		  (ref
		   ((name . "office-forms"))))
	(optional nil
		  (ref
		   ((name . "table-shapes"))))
	(ref
	 ((name . "table-columns-and-groups")))
	(ref
	 ((name . "table-rows-and-groups")))
	(optional nil
		  (ref
		   ((name . "table-named-expressions"))))))
      (define
       ((name . "table-columns-and-groups"))
       (oneOrMore nil
		  (choice nil
			  (ref
			   ((name . "table-table-column-group")))
			  (ref
			   ((name . "table-columns-no-group"))))))
      (define
       ((name . "table-columns-no-group"))
       (choice nil
	       (group nil
		      (ref
		       ((name . "table-columns")))
		      (optional nil
				(ref
				 ((name . "table-table-header-columns")))
				(optional nil
					  (ref
					   ((name . "table-columns"))))))
	       (group nil
		      (ref
		       ((name . "table-table-header-columns")))
		      (optional nil
				(ref
				 ((name . "table-columns")))))))
      (define
       ((name . "table-columns"))
       (choice nil
	       (ref
		((name . "table-table-columns")))
	       (oneOrMore nil
			  (ref
			   ((name . "table-table-column"))))))
      (define
       ((name . "table-rows-and-groups"))
       (oneOrMore nil
		  (choice nil
			  (ref
			   ((name . "table-table-row-group")))
			  (ref
			   ((name . "table-rows-no-group"))))))
      (define
       ((name . "table-rows-no-group"))
       (choice nil
	       (group nil
		      (ref
		       ((name . "table-rows")))
		      (optional nil
				(ref
				 ((name . "table-table-header-rows")))
				(optional nil
					  (ref
					   ((name . "table-rows"))))))
	       (group nil
		      (ref
		       ((name . "table-table-header-rows")))
		      (optional nil
				(ref
				 ((name . "table-rows")))))))
      (define
       ((name . "table-rows"))
       (choice nil
	       (ref
		((name . "table-table-rows")))
	       (oneOrMore nil
			  (optional nil
				    (ref
				     ((name . "text-soft-page-break"))))
			  (ref
			   ((name . "table-table-row"))))))
      (define
       ((name . "table-table-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "table:name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "table:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "table:template-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "table:use-first-row-styles"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:use-last-row-styles"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:use-first-column-styles"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:use-last-column-styles"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:use-banding-rows-styles"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:use-banding-columns-styles"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:protected"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:protection-key"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "table:protection-key-digest-algorithm"))
			      (ref
			       ((name . "anyIRI")))))
		   (optional nil
			     (attribute
			      ((name . "table:print"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:print-ranges"))
			      (ref
			       ((name . "cellRangeAddressList")))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))
		   (optional nil
			     (attribute
			      ((name . "table:is-sub-table"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "table-title"))
       (element
	((name . "table:title"))
	(text nil)))
      (define
       ((name . "table-desc"))
       (element
	((name . "table:desc"))
	(text nil)))
      (define
       ((name . "table-table-row"))
       (element
	((name . "table:table-row"))
	(ref
	 ((name . "table-table-row-attlist")))
	(oneOrMore nil
		   (choice nil
			   (ref
			    ((name . "table-table-cell")))
			   (ref
			    ((name . "table-covered-table-cell")))))))
      (define
       ((name . "table-table-row-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "table:number-rows-repeated"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "table:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "table:default-cell-style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "table:visibility"))
			      (ref
			       ((name . "table-visibility-value")))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "table-visibility-value"))
       (choice nil
	       (value nil "visible")
	       (value nil "collapse")
	       (value nil "filter")))
      (define
       ((name . "table-table-cell"))
       (element
	((name . "table:table-cell"))
	(ref
	 ((name . "table-table-cell-attlist")))
	(ref
	 ((name . "table-table-cell-attlist-extra")))
	(ref
	 ((name . "table-table-cell-content")))))
      (define
       ((name . "table-covered-table-cell"))
       (element
	((name . "table:covered-table-cell"))
	(ref
	 ((name . "table-table-cell-attlist")))
	(ref
	 ((name . "table-table-cell-content")))))
      (define
       ((name . "table-table-cell-content"))
       (optional nil
		 (ref
		  ((name . "table-cell-range-source"))))
       (optional nil
		 (ref
		  ((name . "office-annotation"))))
       (optional nil
		 (ref
		  ((name . "table-detective"))))
       (zeroOrMore nil
		   (ref
		    ((name . "text-content")))))
      (define
       ((name . "table-table-cell-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "table:number-columns-repeated"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "table:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "table:content-validation-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "table:formula"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (ref
			      ((name . "common-value-and-type-attlist"))))
		   (optional nil
			     (attribute
			      ((name . "table:protect"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:protected"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))
		   (optional nil
			     (ref
			      ((name . "common-in-content-meta-attlist"))))))
      (define
       ((name . "table-table-cell-attlist-extra"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "table:number-columns-spanned"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "table:number-rows-spanned"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "table:number-matrix-columns-spanned"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "table:number-matrix-rows-spanned"))
			      (ref
			       ((name . "positiveInteger")))))))
      (define
       ((name . "table-table-column"))
       (element
	((name . "table:table-column"))
	(ref
	 ((name . "table-table-column-attlist")))
	(empty nil)))
      (define
       ((name . "table-table-column-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "table:number-columns-repeated"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "table:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "table:visibility"))
			      (ref
			       ((name . "table-visibility-value")))))
		   (optional nil
			     (attribute
			      ((name . "table:default-cell-style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "table-table-header-columns"))
       (element
	((name . "table:table-header-columns"))
	(oneOrMore nil
		   (ref
		    ((name . "table-table-column"))))))
      (define
       ((name . "table-table-columns"))
       (element
	((name . "table:table-columns"))
	(oneOrMore nil
		   (ref
		    ((name . "table-table-column"))))))
      (define
       ((name . "table-table-column-group"))
       (element
	((name . "table:table-column-group"))
	(ref
	 ((name . "table-table-column-group-attlist")))
	(ref
	 ((name . "table-columns-and-groups")))))
      (define
       ((name . "table-table-column-group-attlist"))
       (optional nil
		 (attribute
		  ((name . "table:display"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "table-table-header-rows"))
       (element
	((name . "table:table-header-rows"))
	(oneOrMore nil
		   (optional nil
			     (ref
			      ((name . "text-soft-page-break"))))
		   (ref
		    ((name . "table-table-row"))))))
      (define
       ((name . "table-table-rows"))
       (element
	((name . "table:table-rows"))
	(oneOrMore nil
		   (optional nil
			     (ref
			      ((name . "text-soft-page-break"))))
		   (ref
		    ((name . "table-table-row"))))))
      (define
       ((name . "table-table-row-group"))
       (element
	((name . "table:table-row-group"))
	(ref
	 ((name . "table-table-row-group-attlist")))
	(ref
	 ((name . "table-rows-and-groups")))))
      (define
       ((name . "table-table-row-group-attlist"))
       (optional nil
		 (attribute
		  ((name . "table:display"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "cellAddress"))
       (data
	((type . "string"))
	(param
	 ((name . "pattern"))
	 "($?([^\\. ']+|'([^']|'')+'))?\\.$?[A-Z]+$?[0-9]+")))
      (define
       ((name . "cellRangeAddress"))
       (choice nil
	       (data
		((type . "string"))
		(param
		 ((name . "pattern"))
		 "($?([^\\. ']+|'([^']|'')+'))?\\.$?[A-Z]+$?[0-9]+(:($?([^\\. ']+|'([^']|'')+'))?\\.$?[A-Z]+$?[0-9]+)?"))
	       (data
		((type . "string"))
		(param
		 ((name . "pattern"))
		 "($?([^\\. ']+|'([^']|'')+'))?\\.$?[0-9]+:($?([^\\. ']+|'([^']|'')+'))?\\.$?[0-9]+"))
	       (data
		((type . "string"))
		(param
		 ((name . "pattern"))
		 "($?([^\\. ']+|'([^']|'')+'))?\\.$?[A-Z]+:($?([^\\. ']+|'([^']|'')+'))?\\.$?[A-Z]+"))))
      (define
       ((name . "cellRangeAddressList"))
       (data
	((type . "string")))
       (description nil "Value is a space separated list of \"cellRangeAddress\" patterns"))
      (define
       ((name . "table-table-source"))
       (element
	((name . "table:table-source"))
	(ref
	 ((name . "table-table-source-attlist")))
	(ref
	 ((name . "table-linked-source-attlist")))
	(empty nil)))
      (define
       ((name . "table-table-source-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "table:mode"))
			      (choice nil
				      (value nil "copy-all")
				      (value nil "copy-results-only"))))
		   (optional nil
			     (attribute
			      ((name . "table:table-name"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "table-linked-source-attlist"))
       (interleave nil
		   (attribute
		    ((name . "xlink:type"))
		    (value nil "simple"))
		   (attribute
		    ((name . "xlink:href"))
		    (ref
		     ((name . "anyIRI"))))
		   (optional nil
			     (attribute
			      ((name . "xlink:actuate"))
			      (value nil "onRequest")))
		   (optional nil
			     (attribute
			      ((name . "table:filter-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "table:filter-options"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "table:refresh-delay"))
			      (ref
			       ((name . "duration")))))))
      (define
       ((name . "table-scenario"))
       (element
	((name . "table:scenario"))
	(ref
	 ((name . "table-scenario-attlist")))
	(empty nil)))
      (define
       ((name . "table-scenario-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:scenario-ranges"))
		    (ref
		     ((name . "cellRangeAddressList"))))
		   (attribute
		    ((name . "table:is-active"))
		    (ref
		     ((name . "boolean"))))
		   (optional nil
			     (attribute
			      ((name . "table:display-border"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:border-color"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "table:copy-back"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:copy-styles"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:copy-formulas"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:comment"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "table:protected"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "table-shapes"))
       (element
	((name . "table:shapes"))
	(oneOrMore nil
		   (ref
		    ((name . "shape"))))))
      (define
       ((name . "table-cell-range-source"))
       (element
	((name . "table:cell-range-source"))
	(ref
	 ((name . "table-table-cell-range-source-attlist")))
	(ref
	 ((name . "table-linked-source-attlist")))
	(empty nil)))
      (define
       ((name . "table-table-cell-range-source-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:name"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "table:last-column-spanned"))
		    (ref
		     ((name . "positiveInteger"))))
		   (attribute
		    ((name . "table:last-row-spanned"))
		    (ref
		     ((name . "positiveInteger"))))))
      (define
       ((name . "table-detective"))
       (element
	((name . "table:detective"))
	(zeroOrMore nil
		    (ref
		     ((name . "table-highlighted-range"))))
	(zeroOrMore nil
		    (ref
		     ((name . "table-operation"))))))
      (define
       ((name . "table-operation"))
       (element
	((name . "table:operation"))
	(ref
	 ((name . "table-operation-attlist")))
	(empty nil)))
      (define
       ((name . "table-operation-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:name"))
		    (choice nil
			    (value nil "trace-dependents")
			    (value nil "remove-dependents")
			    (value nil "trace-precedents")
			    (value nil "remove-precedents")
			    (value nil "trace-errors")))
		   (attribute
		    ((name . "table:index"))
		    (ref
		     ((name . "nonNegativeInteger"))))))
      (define
       ((name . "table-highlighted-range"))
       (element
	((name . "table:highlighted-range"))
	(choice nil
		(group nil
		       (ref
			((name . "table-highlighted-range-attlist"))))
		(group nil
		       (ref
			((name . "table-highlighted-range-attlist-invalid")))))
	(empty nil)))
      (define
       ((name . "table-highlighted-range-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "table:cell-range-address"))
			      (ref
			       ((name . "cellRangeAddress")))))
		   (attribute
		    ((name . "table:direction"))
		    (choice nil
			    (value nil "from-another-table")
			    (value nil "to-another-table")
			    (value nil "from-same-table")))
		   (optional nil
			     (attribute
			      ((name . "table:contains-error"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "table-highlighted-range-attlist-invalid"))
       (attribute
	((name . "table:marked-invalid"))
	(ref
	 ((name . "boolean")))))
      (define
       ((name . "office-spreadsheet-attlist"))
       (optional nil
		 (attribute
		  ((name . "table:structure-protected"))
		  (ref
		   ((name . "boolean")))))
       (optional nil
		 (attribute
		  ((name . "table:protection-key"))
		  (ref
		   ((name . "string")))))
       (optional nil
		 (attribute
		  ((name . "table:protection-key-digest-algorithm"))
		  (ref
		   ((name . "anyIRI"))))))
      (define
       ((name . "table-calculation-settings"))
       (element
	((name . "table:calculation-settings"))
	(ref
	 ((name . "table-calculation-setting-attlist")))
	(optional nil
		  (ref
		   ((name . "table-null-date"))))
	(optional nil
		  (ref
		   ((name . "table-iteration"))))))
      (define
       ((name . "table-calculation-setting-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "table:case-sensitive"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:precision-as-shown"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:search-criteria-must-apply-to-whole-cell"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:automatic-find-labels"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:use-regular-expressions"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:use-wildcards"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:null-year"))
			      (ref
			       ((name . "positiveInteger")))))))
      (define
       ((name . "table-null-date"))
       (element
	((name . "table:null-date"))
	(optional nil
		  (attribute
		   ((name . "table:value-type"))
		   (value nil "date")))
	(optional nil
		  (attribute
		   ((name . "table:date-value"))
		   (ref
		    ((name . "date")))))
	(empty nil)))
      (define
       ((name . "table-iteration"))
       (element
	((name . "table:iteration"))
	(optional nil
		  (attribute
		   ((name . "table:status"))
		   (choice nil
			   (value nil "enable")
			   (value nil "disable"))))
	(optional nil
		  (attribute
		   ((name . "table:steps"))
		   (ref
		    ((name . "positiveInteger")))))
	(optional nil
		  (attribute
		   ((name . "table:maximum-difference"))
		   (ref
		    ((name . "double")))))
	(empty nil)))
      (define
       ((name . "table-content-validations"))
       (element
	((name . "table:content-validations"))
	(oneOrMore nil
		   (ref
		    ((name . "table-content-validation"))))))
      (define
       ((name . "table-content-validation"))
       (element
	((name . "table:content-validation"))
	(ref
	 ((name . "table-validation-attlist")))
	(optional nil
		  (ref
		   ((name . "table-help-message"))))
	(optional nil
		  (choice nil
			  (ref
			   ((name . "table-error-message")))
			  (group nil
				 (ref
				  ((name . "table-error-macro")))
				 (ref
				  ((name . "office-event-listeners"))))))))
      (define
       ((name . "table-validation-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:name"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "table:condition"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "table:base-cell-address"))
			      (ref
			       ((name . "cellAddress")))))
		   (optional nil
			     (attribute
			      ((name . "table:allow-empty-cell"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:display-list"))
			      (choice nil
				      (value nil "none")
				      (value nil "unsorted")
				      (value nil "sort-ascending"))))))
      (define
       ((name . "table-help-message"))
       (element
	((name . "table:help-message"))
	(optional nil
		  (attribute
		   ((name . "table:title"))
		   (ref
		    ((name . "string")))))
	(optional nil
		  (attribute
		   ((name . "table:display"))
		   (ref
		    ((name . "boolean")))))
	(zeroOrMore nil
		    (ref
		     ((name . "text-p"))))))
      (define
       ((name . "table-error-message"))
       (element
	((name . "table:error-message"))
	(optional nil
		  (attribute
		   ((name . "table:title"))
		   (ref
		    ((name . "string")))))
	(optional nil
		  (attribute
		   ((name . "table:display"))
		   (ref
		    ((name . "boolean")))))
	(optional nil
		  (attribute
		   ((name . "table:message-type"))
		   (choice nil
			   (value nil "stop")
			   (value nil "warning")
			   (value nil "information"))))
	(zeroOrMore nil
		    (ref
		     ((name . "text-p"))))))
      (define
       ((name . "table-error-macro"))
       (element
	((name . "table:error-macro"))
	(optional nil
		  (attribute
		   ((name . "table:execute"))
		   (ref
		    ((name . "boolean")))))))
      (define
       ((name . "table-label-ranges"))
       (element
	((name . "table:label-ranges"))
	(zeroOrMore nil
		    (ref
		     ((name . "table-label-range"))))))
      (define
       ((name . "table-label-range"))
       (element
	((name . "table:label-range"))
	(ref
	 ((name . "table-label-range-attlist")))
	(empty nil)))
      (define
       ((name . "table-label-range-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:label-cell-range-address"))
		    (ref
		     ((name . "cellRangeAddress"))))
		   (attribute
		    ((name . "table:data-cell-range-address"))
		    (ref
		     ((name . "cellRangeAddress"))))
		   (attribute
		    ((name . "table:orientation"))
		    (choice nil
			    (value nil "column")
			    (value nil "row")))))
      (define
       ((name . "table-named-expressions"))
       (element
	((name . "table:named-expressions"))
	(zeroOrMore nil
		    (choice nil
			    (ref
			     ((name . "table-named-range")))
			    (ref
			     ((name . "table-named-expression")))))))
      (define
       ((name . "table-named-range"))
       (element
	((name . "table:named-range"))
	(ref
	 ((name . "table-named-range-attlist")))
	(empty nil)))
      (define
       ((name . "table-named-range-attlist"))
       (attribute
	((name . "table:name"))
	(ref
	 ((name . "string"))))
       (attribute
	((name . "table:cell-range-address"))
	(ref
	 ((name . "cellRangeAddress"))))
       (optional nil
		 (attribute
		  ((name . "table:base-cell-address"))
		  (ref
		   ((name . "cellAddress")))))
       (optional nil
		 (attribute
		  ((name . "table:range-usable-as"))
		  (choice nil
			  (value nil "none")
			  (list nil
				(oneOrMore nil
					   (choice nil
						   (value nil "print-range")
						   (value nil "filter")
						   (value nil "repeat-row")
						   (value nil "repeat-column"))))))))
      (define
       ((name . "table-named-expression"))
       (element
	((name . "table:named-expression"))
	(ref
	 ((name . "table-named-expression-attlist")))
	(empty nil)))
      (define
       ((name . "table-named-expression-attlist"))
       (attribute
	((name . "table:name"))
	(ref
	 ((name . "string"))))
       (attribute
	((name . "table:expression"))
	(ref
	 ((name . "string"))))
       (optional nil
		 (attribute
		  ((name . "table:base-cell-address"))
		  (ref
		   ((name . "cellAddress"))))))
      (define
       ((name . "table-database-ranges"))
       (element
	((name . "table:database-ranges"))
	(zeroOrMore nil
		    (ref
		     ((name . "table-database-range"))))))
      (define
       ((name . "table-database-range"))
       (element
	((name . "table:database-range"))
	(ref
	 ((name . "table-database-range-attlist")))
	(optional nil
		  (choice nil
			  (ref
			   ((name . "table-database-source-sql")))
			  (ref
			   ((name . "table-database-source-table")))
			  (ref
			   ((name . "table-database-source-query")))))
	(optional nil
		  (ref
		   ((name . "table-filter"))))
	(optional nil
		  (ref
		   ((name . "table-sort"))))
	(optional nil
		  (ref
		   ((name . "table-subtotal-rules"))))))
      (define
       ((name . "table-database-range-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "table:name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "table:is-selection"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:on-update-keep-styles"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:on-update-keep-size"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:has-persistent-data"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:orientation"))
			      (choice nil
				      (value nil "column")
				      (value nil "row"))))
		   (optional nil
			     (attribute
			      ((name . "table:contains-header"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:display-filter-buttons"))
			      (ref
			       ((name . "boolean")))))
		   (attribute
		    ((name . "table:target-range-address"))
		    (ref
		     ((name . "cellRangeAddress"))))
		   (optional nil
			     (attribute
			      ((name . "table:refresh-delay"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "table-database-source-sql"))
       (element
	((name . "table:database-source-sql"))
	(ref
	 ((name . "table-database-source-sql-attlist")))
	(empty nil)))
      (define
       ((name . "table-database-source-sql-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:database-name"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "table:sql-statement"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "table:parse-sql-statement"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "table-database-source-query"))
       (element
	((name . "table:database-source-table"))
	(ref
	 ((name . "table-database-source-table-attlist")))
	(empty nil)))
      (define
       ((name . "table-database-source-table-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:database-name"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "table:database-table-name"))
		    (ref
		     ((name . "string"))))))
      (define
       ((name . "table-database-source-table"))
       (element
	((name . "table:database-source-query"))
	(ref
	 ((name . "table-database-source-query-attlist")))
	(empty nil)))
      (define
       ((name . "table-database-source-query-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:database-name"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "table:query-name"))
		    (ref
		     ((name . "string"))))))
      (define
       ((name . "table-sort"))
       (element
	((name . "table:sort"))
	(ref
	 ((name . "table-sort-attlist")))
	(oneOrMore nil
		   (ref
		    ((name . "table-sort-by"))))))
      (define
       ((name . "table-sort-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "table:bind-styles-to-content"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:target-range-address"))
			      (ref
			       ((name . "cellRangeAddress")))))
		   (optional nil
			     (attribute
			      ((name . "table:case-sensitive"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:language"))
			      (ref
			       ((name . "languageCode")))))
		   (optional nil
			     (attribute
			      ((name . "table:country"))
			      (ref
			       ((name . "countryCode")))))
		   (optional nil
			     (attribute
			      ((name . "table:script"))
			      (ref
			       ((name . "scriptCode")))))
		   (optional nil
			     (attribute
			      ((name . "table:rfc-language-tag"))
			      (ref
			       ((name . "language")))))
		   (optional nil
			     (attribute
			      ((name . "table:algorithm"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "table:embedded-number-behavior"))
			      (choice nil
				      (value nil "alpha-numeric")
				      (value nil "integer")
				      (value nil "double"))))))
      (define
       ((name . "table-sort-by"))
       (element
	((name . "table:sort-by"))
	(ref
	 ((name . "table-sort-by-attlist")))
	(empty nil)))
      (define
       ((name . "table-sort-by-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:field-number"))
		    (ref
		     ((name . "nonNegativeInteger"))))
		   (optional nil
			     (attribute
			      ((name . "table:data-type"))
			      (choice nil
				      (value nil "text")
				      (value nil "number")
				      (value nil "automatic")
				      (ref
				       ((name . "string"))))))
		   (optional nil
			     (attribute
			      ((name . "table:order"))
			      (choice nil
				      (value nil "ascending")
				      (value nil "descending"))))))
      (define
       ((name . "table-subtotal-rules"))
       (element
	((name . "table:subtotal-rules"))
	(ref
	 ((name . "table-subtotal-rules-attlist")))
	(optional nil
		  (ref
		   ((name . "table-sort-groups"))))
	(zeroOrMore nil
		    (ref
		     ((name . "table-subtotal-rule"))))))
      (define
       ((name . "table-subtotal-rules-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "table:bind-styles-to-content"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:case-sensitive"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:page-breaks-on-group-change"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "table-sort-groups"))
       (element
	((name . "table:sort-groups"))
	(ref
	 ((name . "table-sort-groups-attlist")))
	(empty nil)))
      (define
       ((name . "table-sort-groups-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "table:data-type"))
			      (choice nil
				      (value nil "text")
				      (value nil "number")
				      (value nil "automatic")
				      (ref
				       ((name . "string"))))))
		   (optional nil
			     (attribute
			      ((name . "table:order"))
			      (choice nil
				      (value nil "ascending")
				      (value nil "descending"))))))
      (define
       ((name . "table-subtotal-rule"))
       (element
	((name . "table:subtotal-rule"))
	(ref
	 ((name . "table-subtotal-rule-attlist")))
	(zeroOrMore nil
		    (ref
		     ((name . "table-subtotal-field"))))))
      (define
       ((name . "table-subtotal-rule-attlist"))
       (attribute
	((name . "table:group-by-field-number"))
	(ref
	 ((name . "nonNegativeInteger")))))
      (define
       ((name . "table-subtotal-field"))
       (element
	((name . "table:subtotal-field"))
	(ref
	 ((name . "table-subtotal-field-attlist")))
	(empty nil)))
      (define
       ((name . "table-subtotal-field-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:field-number"))
		    (ref
		     ((name . "nonNegativeInteger"))))
		   (attribute
		    ((name . "table:function"))
		    (choice nil
			    (value nil "average")
			    (value nil "count")
			    (value nil "countnums")
			    (value nil "max")
			    (value nil "min")
			    (value nil "product")
			    (value nil "stdev")
			    (value nil "stdevp")
			    (value nil "sum")
			    (value nil "var")
			    (value nil "varp")
			    (ref
			     ((name . "string")))))))
      (define
       ((name . "table-filter"))
       (element
	((name . "table:filter"))
	(ref
	 ((name . "table-filter-attlist")))
	(choice nil
		(ref
		 ((name . "table-filter-condition")))
		(ref
		 ((name . "table-filter-and")))
		(ref
		 ((name . "table-filter-or"))))))
      (define
       ((name . "table-filter-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "table:target-range-address"))
			      (ref
			       ((name . "cellRangeAddress")))))
		   (optional nil
			     (attribute
			      ((name . "table:condition-source"))
			      (choice nil
				      (value nil "self")
				      (value nil "cell-range"))))
		   (optional nil
			     (attribute
			      ((name . "table:condition-source-range-address"))
			      (ref
			       ((name . "cellRangeAddress")))))
		   (optional nil
			     (attribute
			      ((name . "table:display-duplicates"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "table-filter-and"))
       (element
	((name . "table:filter-and"))
	(oneOrMore nil
		   (choice nil
			   (ref
			    ((name . "table-filter-or")))
			   (ref
			    ((name . "table-filter-condition")))))))
      (define
       ((name . "table-filter-or"))
       (element
	((name . "table:filter-or"))
	(oneOrMore nil
		   (choice nil
			   (ref
			    ((name . "table-filter-and")))
			   (ref
			    ((name . "table-filter-condition")))))))
      (define
       ((name . "table-filter-condition"))
       (element
	((name . "table:filter-condition"))
	(ref
	 ((name . "table-filter-condition-attlist")))
	(zeroOrMore nil
		    (ref
		     ((name . "table-filter-set-item"))))))
      (define
       ((name . "table-filter-condition-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:field-number"))
		    (ref
		     ((name . "nonNegativeInteger"))))
		   (attribute
		    ((name . "table:value"))
		    (choice nil
			    (ref
			     ((name . "string")))
			    (ref
			     ((name . "double")))))
		   (attribute
		    ((name . "table:operator"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "table:case-sensitive"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "table:data-type"))
			      (choice nil
				      (value nil "text")
				      (value nil "number"))))))
      (define
       ((name . "table-filter-set-item"))
       (element
	((name . "table:filter-set-item"))
	(attribute
	 ((name . "table:value"))
	 (ref
	  ((name . "string"))))
	(empty nil)))
      (define
       ((name . "table-data-pilot-tables"))
       (element
	((name . "table:data-pilot-tables"))
	(zeroOrMore nil
		    (ref
		     ((name . "table-data-pilot-table"))))))
      (define
       ((name . "table-data-pilot-table"))
       (element
	((name . "table:data-pilot-table"))
	(ref
	 ((name . "table-data-pilot-table-attlist")))
	(optional nil
		  (choice nil
			  (ref
			   ((name . "table-database-source-sql")))
			  (ref
			   ((name . "table-database-source-table")))
			  (ref
			   ((name . "table-database-source-query")))
			  (ref
			   ((name . "table-source-service")))
			  (ref
			   ((name . "table-source-cell-range")))))
	(oneOrMore nil
		   (ref
		    ((name . "table-data-pilot-field"))))))
      (define
       ((name . "table-data-pilot-table-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:name"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "table:application-data"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "table:grand-total"))
			      (choice nil
				      (value nil "none")
				      (value nil "row")
				      (value nil "column")
				      (value nil "both"))))
		   (optional nil
			     (attribute
			      ((name . "table:ignore-empty-rows"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:identify-categories"))
			      (ref
			       ((name . "boolean")))))
		   (attribute
		    ((name . "table:target-range-address"))
		    (ref
		     ((name . "cellRangeAddress"))))
		   (optional nil
			     (attribute
			      ((name . "table:buttons"))
			      (ref
			       ((name . "cellRangeAddressList")))))
		   (optional nil
			     (attribute
			      ((name . "table:show-filter-button"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:drill-down-on-double-click"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "table-source-cell-range"))
       (element
	((name . "table:source-cell-range"))
	(ref
	 ((name . "table-source-cell-range-attlist")))
	(optional nil
		  (ref
		   ((name . "table-filter"))))))
      (define
       ((name . "table-source-cell-range-attlist"))
       (attribute
	((name . "table:cell-range-address"))
	(ref
	 ((name . "cellRangeAddress")))))
      (define
       ((name . "table-source-service"))
       (element
	((name . "table:source-service"))
	(ref
	 ((name . "table-source-service-attlist")))
	(empty nil)))
      (define
       ((name . "table-source-service-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:name"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "table:source-name"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "table:object-name"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "table:user-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "table:password"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "table-data-pilot-field"))
       (element
	((name . "table:data-pilot-field"))
	(ref
	 ((name . "table-data-pilot-field-attlist")))
	(optional nil
		  (ref
		   ((name . "table-data-pilot-level"))))
	(optional nil
		  (ref
		   ((name . "table-data-pilot-field-reference"))))
	(optional nil
		  (ref
		   ((name . "table-data-pilot-groups"))))))
      (define
       ((name . "table-data-pilot-field-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:source-field-name"))
		    (ref
		     ((name . "string"))))
		   (choice nil
			   (attribute
			    ((name . "table:orientation"))
			    (choice nil
				    (value nil "row")
				    (value nil "column")
				    (value nil "data")
				    (value nil "hidden")))
			   (group nil
				  (attribute
				   ((name . "table:orientation"))
				   (value nil "page"))
				  (attribute
				   ((name . "table:selected-page"))
				   (ref
				    ((name . "string"))))))
		   (optional nil
			     (attribute
			      ((name . "table:is-data-layout-field"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "table:function"))
			      (choice nil
				      (value nil "auto")
				      (value nil "average")
				      (value nil "count")
				      (value nil "countnums")
				      (value nil "max")
				      (value nil "min")
				      (value nil "product")
				      (value nil "stdev")
				      (value nil "stdevp")
				      (value nil "sum")
				      (value nil "var")
				      (value nil "varp")
				      (ref
				       ((name . "string"))))))
		   (optional nil
			     (attribute
			      ((name . "table:used-hierarchy"))
			      (ref
			       ((name . "integer")))))))
      (define
       ((name . "table-data-pilot-level"))
       (element
	((name . "table:data-pilot-level"))
	(ref
	 ((name . "table-data-pilot-level-attlist")))
	(optional nil
		  (ref
		   ((name . "table-data-pilot-subtotals"))))
	(optional nil
		  (ref
		   ((name . "table-data-pilot-members"))))
	(optional nil
		  (ref
		   ((name . "table-data-pilot-display-info"))))
	(optional nil
		  (ref
		   ((name . "table-data-pilot-sort-info"))))
	(optional nil
		  (ref
		   ((name . "table-data-pilot-layout-info"))))))
      (define
       ((name . "table-data-pilot-level-attlist"))
       (optional nil
		 (attribute
		  ((name . "table:show-empty"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "table-data-pilot-subtotals"))
       (element
	((name . "table:data-pilot-subtotals"))
	(zeroOrMore nil
		    (ref
		     ((name . "table-data-pilot-subtotal"))))))
      (define
       ((name . "table-data-pilot-subtotal"))
       (element
	((name . "table:data-pilot-subtotal"))
	(ref
	 ((name . "table-data-pilot-subtotal-attlist")))
	(empty nil)))
      (define
       ((name . "table-data-pilot-subtotal-attlist"))
       (attribute
	((name . "table:function"))
	(choice nil
		(value nil "auto")
		(value nil "average")
		(value nil "count")
		(value nil "countnums")
		(value nil "max")
		(value nil "min")
		(value nil "product")
		(value nil "stdev")
		(value nil "stdevp")
		(value nil "sum")
		(value nil "var")
		(value nil "varp")
		(ref
		 ((name . "string"))))))
      (define
       ((name . "table-data-pilot-members"))
       (element
	((name . "table:data-pilot-members"))
	(zeroOrMore nil
		    (ref
		     ((name . "table-data-pilot-member"))))))
      (define
       ((name . "table-data-pilot-member"))
       (element
	((name . "table:data-pilot-member"))
	(ref
	 ((name . "table-data-pilot-member-attlist")))
	(empty nil)))
      (define
       ((name . "table-data-pilot-member-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:name"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "table:display"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:show-details"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "table-data-pilot-display-info"))
       (element
	((name . "table:data-pilot-display-info"))
	(ref
	 ((name . "table-data-pilot-display-info-attlist")))
	(empty nil)))
      (define
       ((name . "table-data-pilot-display-info-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:enabled"))
		    (ref
		     ((name . "boolean"))))
		   (attribute
		    ((name . "table:data-field"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "table:member-count"))
		    (ref
		     ((name . "nonNegativeInteger"))))
		   (attribute
		    ((name . "table:display-member-mode"))
		    (choice nil
			    (value nil "from-top")
			    (value nil "from-bottom")))))
      (define
       ((name . "table-data-pilot-sort-info"))
       (element
	((name . "table:data-pilot-sort-info"))
	(ref
	 ((name . "table-data-pilot-sort-info-attlist")))
	(empty nil)))
      (define
       ((name . "table-data-pilot-sort-info-attlist"))
       (interleave nil
		   (choice nil
			   (group nil
				  (attribute
				   ((name . "table:sort-mode"))
				   (value nil "data"))
				  (attribute
				   ((name . "table:data-field"))
				   (ref
				    ((name . "string")))))
			   (attribute
			    ((name . "table:sort-mode"))
			    (choice nil
				    (value nil "none")
				    (value nil "manual")
				    (value nil "name"))))
		   (attribute
		    ((name . "table:order"))
		    (choice nil
			    (value nil "ascending")
			    (value nil "descending")))))
      (define
       ((name . "table-data-pilot-layout-info"))
       (element
	((name . "table:data-pilot-layout-info"))
	(ref
	 ((name . "table-data-pilot-layout-info-attlist")))
	(empty nil)))
      (define
       ((name . "table-data-pilot-layout-info-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:layout-mode"))
		    (choice nil
			    (value nil "tabular-layout")
			    (value nil "outline-subtotals-top")
			    (value nil "outline-subtotals-bottom")))
		   (attribute
		    ((name . "table:add-empty-lines"))
		    (ref
		     ((name . "boolean"))))))
      (define
       ((name . "table-data-pilot-field-reference"))
       (element
	((name . "table:data-pilot-field-reference"))
	(ref
	 ((name . "table-data-pilot-field-reference-attlist")))))
      (define
       ((name . "table-data-pilot-field-reference-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:field-name"))
		    (ref
		     ((name . "string"))))
		   (choice nil
			   (group nil
				  (attribute
				   ((name . "table:member-type"))
				   (value nil "named"))
				  (attribute
				   ((name . "table:member-name"))
				   (ref
				    ((name . "string")))))
			   (attribute
			    ((name . "table:member-type"))
			    (choice nil
				    (value nil "previous")
				    (value nil "next"))))
		   (attribute
		    ((name . "table:type"))
		    (choice nil
			    (value nil "none")
			    (value nil "member-difference")
			    (value nil "member-percentage")
			    (value nil "member-percentage-difference")
			    (value nil "running-total")
			    (value nil "row-percentage")
			    (value nil "column-percentage")
			    (value nil "total-percentage")
			    (value nil "index")))))
      (define
       ((name . "table-data-pilot-groups"))
       (element
	((name . "table:data-pilot-groups"))
	(ref
	 ((name . "table-data-pilot-groups-attlist")))
	(oneOrMore nil
		   (ref
		    ((name . "table-data-pilot-group"))))))
      (define
       ((name . "table-data-pilot-groups-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:source-field-name"))
		    (ref
		     ((name . "string"))))
		   (choice nil
			   (attribute
			    ((name . "table:date-start"))
			    (choice nil
				    (ref
				     ((name . "dateOrDateTime")))
				    (value nil "auto")))
			   (attribute
			    ((name . "table:start"))
			    (choice nil
				    (ref
				     ((name . "double")))
				    (value nil "auto"))))
		   (choice nil
			   (attribute
			    ((name . "table:date-end"))
			    (choice nil
				    (ref
				     ((name . "dateOrDateTime")))
				    (value nil "auto")))
			   (attribute
			    ((name . "table:end"))
			    (choice nil
				    (ref
				     ((name . "double")))
				    (value nil "auto"))))
		   (attribute
		    ((name . "table:step"))
		    (ref
		     ((name . "double"))))
		   (attribute
		    ((name . "table:grouped-by"))
		    (choice nil
			    (value nil "seconds")
			    (value nil "minutes")
			    (value nil "hours")
			    (value nil "days")
			    (value nil "months")
			    (value nil "quarters")
			    (value nil "years")))))
      (define
       ((name . "table-data-pilot-group"))
       (element
	((name . "table:data-pilot-group"))
	(ref
	 ((name . "table-data-pilot-group-attlist")))
	(oneOrMore nil
		   (ref
		    ((name . "table-data-pilot-group-member"))))))
      (define
       ((name . "table-data-pilot-group-attlist"))
       (attribute
	((name . "table:name"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "table-data-pilot-group-member"))
       (element
	((name . "table:data-pilot-group-member"))
	(ref
	 ((name . "table-data-pilot-group-member-attlist")))))
      (define
       ((name . "table-data-pilot-group-member-attlist"))
       (attribute
	((name . "table:name"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "table-consolidation"))
       (element
	((name . "table:consolidation"))
	(ref
	 ((name . "table-consolidation-attlist")))
	(empty nil)))
      (define
       ((name . "table-consolidation-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:function"))
		    (choice nil
			    (value nil "average")
			    (value nil "count")
			    (value nil "countnums")
			    (value nil "max")
			    (value nil "min")
			    (value nil "product")
			    (value nil "stdev")
			    (value nil "stdevp")
			    (value nil "sum")
			    (value nil "var")
			    (value nil "varp")
			    (ref
			     ((name . "string")))))
		   (attribute
		    ((name . "table:source-cell-range-addresses"))
		    (ref
		     ((name . "cellRangeAddressList"))))
		   (attribute
		    ((name . "table:target-cell-address"))
		    (ref
		     ((name . "cellAddress"))))
		   (optional nil
			     (attribute
			      ((name . "table:use-labels"))
			      (choice nil
				      (value nil "none")
				      (value nil "row")
				      (value nil "column")
				      (value nil "both"))))
		   (optional nil
			     (attribute
			      ((name . "table:link-to-source-data"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "table-dde-links"))
       (element
	((name . "table:dde-links"))
	(oneOrMore nil
		   (ref
		    ((name . "table-dde-link"))))))
      (define
       ((name . "table-tracked-changes"))
       (element
	((name . "table:tracked-changes"))
	(ref
	 ((name . "table-tracked-changes-attlist")))
	(zeroOrMore nil
		    (choice nil
			    (ref
			     ((name . "table-cell-content-change")))
			    (ref
			     ((name . "table-insertion")))
			    (ref
			     ((name . "table-deletion")))
			    (ref
			     ((name . "table-movement")))))))
      (define
       ((name . "table-tracked-changes-attlist"))
       (optional nil
		 (attribute
		  ((name . "table:track-changes"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "table-insertion"))
       (element
	((name . "table:insertion"))
	(ref
	 ((name . "table-insertion-attlist")))
	(ref
	 ((name . "common-table-change-attlist")))
	(ref
	 ((name . "office-change-info")))
	(optional nil
		  (ref
		   ((name . "table-dependencies"))))
	(optional nil
		  (ref
		   ((name . "table-deletions"))))))
      (define
       ((name . "table-insertion-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:type"))
		    (choice nil
			    (value nil "row")
			    (value nil "column")
			    (value nil "table")))
		   (attribute
		    ((name . "table:position"))
		    (ref
		     ((name . "integer"))))
		   (optional nil
			     (attribute
			      ((name . "table:count"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "table:table"))
			      (ref
			       ((name . "integer")))))))
      (define
       ((name . "table-dependencies"))
       (element
	((name . "table:dependencies"))
	(oneOrMore nil
		   (ref
		    ((name . "table-dependency"))))))
      (define
       ((name . "table-dependency"))
       (element
	((name . "table:dependency"))
	(attribute
	 ((name . "table:id"))
	 (ref
	  ((name . "string"))))
	(empty nil)))
      (define
       ((name . "table-deletions"))
       (element
	((name . "table:deletions"))
	(oneOrMore nil
		   (choice nil
			   (ref
			    ((name . "table-cell-content-deletion")))
			   (ref
			    ((name . "table-change-deletion")))))))
      (define
       ((name . "table-cell-content-deletion"))
       (element
	((name . "table:cell-content-deletion"))
	(optional nil
		  (attribute
		   ((name . "table:id"))
		   (ref
		    ((name . "string")))))
	(optional nil
		  (ref
		   ((name . "table-cell-address"))))
	(optional nil
		  (ref
		   ((name . "table-change-track-table-cell"))))))
      (define
       ((name . "table-change-deletion"))
       (element
	((name . "table:change-deletion"))
	(optional nil
		  (attribute
		   ((name . "table:id"))
		   (ref
		    ((name . "string")))))
	(empty nil)))
      (define
       ((name . "table-deletion"))
       (element
	((name . "table:deletion"))
	(ref
	 ((name . "table-deletion-attlist")))
	(ref
	 ((name . "common-table-change-attlist")))
	(ref
	 ((name . "office-change-info")))
	(optional nil
		  (ref
		   ((name . "table-dependencies"))))
	(optional nil
		  (ref
		   ((name . "table-deletions"))))
	(optional nil
		  (ref
		   ((name . "table-cut-offs"))))))
      (define
       ((name . "table-deletion-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:type"))
		    (choice nil
			    (value nil "row")
			    (value nil "column")
			    (value nil "table")))
		   (attribute
		    ((name . "table:position"))
		    (ref
		     ((name . "integer"))))
		   (optional nil
			     (attribute
			      ((name . "table:table"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "table:multi-deletion-spanned"))
			      (ref
			       ((name . "integer")))))))
      (define
       ((name . "table-cut-offs"))
       (element
	((name . "table:cut-offs"))
	(choice nil
		(oneOrMore nil
			   (ref
			    ((name . "table-movement-cut-off"))))
		(group nil
		       (ref
			((name . "table-insertion-cut-off")))
		       (zeroOrMore nil
				   (ref
				    ((name . "table-movement-cut-off"))))))))
      (define
       ((name . "table-insertion-cut-off"))
       (element
	((name . "table:insertion-cut-off"))
	(ref
	 ((name . "table-insertion-cut-off-attlist")))
	(empty nil)))
      (define
       ((name . "table-insertion-cut-off-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:id"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "table:position"))
		    (ref
		     ((name . "integer"))))))
      (define
       ((name . "table-movement-cut-off"))
       (element
	((name . "table:movement-cut-off"))
	(ref
	 ((name . "table-movement-cut-off-attlist")))
	(empty nil)))
      (define
       ((name . "table-movement-cut-off-attlist"))
       (choice nil
	       (attribute
		((name . "table:position"))
		(ref
		 ((name . "integer"))))
	       (group nil
		      (attribute
		       ((name . "table:start-position"))
		       (ref
			((name . "integer"))))
		      (attribute
		       ((name . "table:end-position"))
		       (ref
			((name . "integer")))))))
      (define
       ((name . "table-movement"))
       (element
	((name . "table:movement"))
	(ref
	 ((name . "common-table-change-attlist")))
	(ref
	 ((name . "table-source-range-address")))
	(ref
	 ((name . "table-target-range-address")))
	(ref
	 ((name . "office-change-info")))
	(optional nil
		  (ref
		   ((name . "table-dependencies"))))
	(optional nil
		  (ref
		   ((name . "table-deletions"))))))
      (define
       ((name . "table-source-range-address"))
       (element
	((name . "table:source-range-address"))
	(ref
	 ((name . "common-table-range-attlist")))
	(empty nil)))
      (define
       ((name . "table-target-range-address"))
       (element
	((name . "table:target-range-address"))
	(ref
	 ((name . "common-table-range-attlist")))
	(empty nil)))
      (define
       ((name . "common-table-range-attlist"))
       (choice nil
	       (group nil
		      (ref
		       ((name . "common-table-cell-address-attlist"))))
	       (group nil
		      (ref
		       ((name . "common-table-cell-range-address-attlist"))))))
      (define
       ((name . "common-table-cell-address-attlist"))
       (attribute
	((name . "table:column"))
	(ref
	 ((name . "integer"))))
       (attribute
	((name . "table:row"))
	(ref
	 ((name . "integer"))))
       (attribute
	((name . "table:table"))
	(ref
	 ((name . "integer")))))
      (define
       ((name . "common-table-cell-range-address-attlist"))
       (attribute
	((name . "table:start-column"))
	(ref
	 ((name . "integer"))))
       (attribute
	((name . "table:start-row"))
	(ref
	 ((name . "integer"))))
       (attribute
	((name . "table:start-table"))
	(ref
	 ((name . "integer"))))
       (attribute
	((name . "table:end-column"))
	(ref
	 ((name . "integer"))))
       (attribute
	((name . "table:end-row"))
	(ref
	 ((name . "integer"))))
       (attribute
	((name . "table:end-table"))
	(ref
	 ((name . "integer")))))
      (define
       ((name . "table-change-track-table-cell"))
       (element
	((name . "table:change-track-table-cell"))
	(ref
	 ((name . "table-change-track-table-cell-attlist")))
	(zeroOrMore nil
		    (ref
		     ((name . "text-p"))))))
      (define
       ((name . "table-change-track-table-cell-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "table:cell-address"))
			      (ref
			       ((name . "cellAddress")))))
		   (optional nil
			     (attribute
			      ((name . "table:matrix-covered"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:formula"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "table:number-matrix-columns-spanned"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "table:number-matrix-rows-spanned"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (ref
			      ((name . "common-value-and-type-attlist"))))))
      (define
       ((name . "table-cell-content-change"))
       (element
	((name . "table:cell-content-change"))
	(ref
	 ((name . "common-table-change-attlist")))
	(ref
	 ((name . "table-cell-address")))
	(ref
	 ((name . "office-change-info")))
	(optional nil
		  (ref
		   ((name . "table-dependencies"))))
	(optional nil
		  (ref
		   ((name . "table-deletions"))))
	(ref
	 ((name . "table-previous")))))
      (define
       ((name . "table-cell-address"))
       (element
	((name . "table:cell-address"))
	(ref
	 ((name . "common-table-cell-address-attlist")))
	(empty nil)))
      (define
       ((name . "table-previous"))
       (element
	((name . "table:previous"))
	(optional nil
		  (attribute
		   ((name . "table:id"))
		   (ref
		    ((name . "string")))))
	(ref
	 ((name . "table-change-track-table-cell")))))
      (define
       ((name . "common-table-change-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:id"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "table:acceptance-state"))
			      (choice nil
				      (value nil "accepted")
				      (value nil "rejected")
				      (value nil "pending"))))
		   (optional nil
			     (attribute
			      ((name . "table:rejecting-change-id"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "style-handout-master"))
       (element
	((name . "style:handout-master"))
	(ref
	 ((name . "common-presentation-header-footer-attlist")))
	(ref
	 ((name . "style-handout-master-attlist")))
	(zeroOrMore nil
		    (ref
		     ((name . "shape"))))))
      (define
       ((name . "style-handout-master-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "presentation:presentation-page-layout-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (attribute
		    ((name . "style:page-layout-name"))
		    (ref
		     ((name . "styleNameRef"))))
		   (optional nil
			     (attribute
			      ((name . "draw:style-name"))
			      (ref
			       ((name . "styleNameRef")))))))
      (define
       ((name . "draw-layer-set"))
       (element
	((name . "draw:layer-set"))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-layer"))))))
      (define
       ((name . "draw-layer"))
       (element
	((name . "draw:layer"))
	(ref
	 ((name . "draw-layer-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))))
      (define
       ((name . "draw-layer-attlist"))
       (interleave nil
		   (attribute
		    ((name . "draw:name"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "draw:protected"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:display"))
			      (choice nil
				      (value nil "always")
				      (value nil "screen")
				      (value nil "printer")
				      (value nil "none"))))))
      (define
       ((name . "draw-page"))
       (element
	((name . "draw:page"))
	(ref
	 ((name . "common-presentation-header-footer-attlist")))
	(ref
	 ((name . "draw-page-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (ref
		   ((name . "draw-layer-set"))))
	(optional nil
		  (ref
		   ((name . "office-forms"))))
	(zeroOrMore nil
		    (ref
		     ((name . "shape"))))
	(optional nil
		  (choice nil
			  (ref
			   ((name . "presentation-animations")))
			  (ref
			   ((name . "animation-element")))))
	(optional nil
		  (ref
		   ((name . "presentation-notes"))))))
      (define
       ((name . "draw-page-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (attribute
		    ((name . "draw:master-page-name"))
		    (ref
		     ((name . "styleNameRef"))))
		   (optional nil
			     (attribute
			      ((name . "presentation:presentation-page-layout-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (group nil
				    (ref
				     ((name . "xml-id")))
				    (optional nil
					      (attribute
					       ((name . "draw:id"))
					       (ref
						((name . "NCName")))))))
		   (optional nil
			     (attribute
			      ((name . "draw:nav-order"))
			      (ref
			       ((name . "IDREFS")))))))
      (define
       ((name . "common-presentation-header-footer-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "presentation:use-header-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:use-footer-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:use-date-time-name"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "shape"))
       (choice nil
	       (ref
		((name . "shape-instance")))
	       (ref
		((name . "draw-a")))))
      (define
       ((name . "shape-instance"))
       (choice nil
	       (ref
		((name . "draw-rect")))
	       (ref
		((name . "draw-line")))
	       (ref
		((name . "draw-polyline")))
	       (ref
		((name . "draw-polygon")))
	       (ref
		((name . "draw-regular-polygon")))
	       (ref
		((name . "draw-path")))
	       (ref
		((name . "draw-circle")))
	       (ref
		((name . "draw-ellipse")))
	       (ref
		((name . "draw-g")))
	       (ref
		((name . "draw-page-thumbnail")))
	       (ref
		((name . "draw-frame")))
	       (ref
		((name . "draw-measure")))
	       (ref
		((name . "draw-caption")))
	       (ref
		((name . "draw-connector")))
	       (ref
		((name . "draw-control")))
	       (ref
		((name . "dr3d-scene")))
	       (ref
		((name . "draw-custom-shape")))))
      (define
       ((name . "draw-rect"))
       (element
	((name . "draw:rect"))
	(ref
	 ((name . "draw-rect-attlist")))
	(ref
	 ((name . "common-draw-position-attlist")))
	(ref
	 ((name . "common-draw-size-attlist")))
	(ref
	 ((name . "common-draw-shape-with-text-and-styles-attlist")))
	(ref
	 ((name . "common-draw-caption-id-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-glue-point"))))
	(ref
	 ((name . "draw-text")))))
      (define
       ((name . "draw-rect-attlist"))
       (choice nil
	       (optional nil
			 (attribute
			  ((name . "draw:corner-radius"))
			  (ref
			   ((name . "nonNegativeLength")))))
	       (group nil
		      (optional nil
				(attribute
				 ((name . "svg:rx"))
				 (ref
				  ((name . "nonNegativeLength")))))
		      (optional nil
				(attribute
				 ((name . "svg:ry"))
				 (ref
				  ((name . "nonNegativeLength"))))))))
      (define
       ((name . "draw-line"))
       (element
	((name . "draw:line"))
	(ref
	 ((name . "draw-line-attlist")))
	(ref
	 ((name . "common-draw-shape-with-text-and-styles-attlist")))
	(ref
	 ((name . "common-draw-caption-id-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-glue-point"))))
	(ref
	 ((name . "draw-text")))))
      (define
       ((name . "draw-line-attlist"))
       (interleave nil
		   (attribute
		    ((name . "svg:x1"))
		    (ref
		     ((name . "coordinate"))))
		   (attribute
		    ((name . "svg:y1"))
		    (ref
		     ((name . "coordinate"))))
		   (attribute
		    ((name . "svg:x2"))
		    (ref
		     ((name . "coordinate"))))
		   (attribute
		    ((name . "svg:y2"))
		    (ref
		     ((name . "coordinate"))))))
      (define
       ((name . "draw-polyline"))
       (element
	((name . "draw:polyline"))
	(ref
	 ((name . "common-draw-points-attlist")))
	(ref
	 ((name . "common-draw-position-attlist")))
	(ref
	 ((name . "common-draw-size-attlist")))
	(ref
	 ((name . "common-draw-viewbox-attlist")))
	(ref
	 ((name . "common-draw-shape-with-text-and-styles-attlist")))
	(ref
	 ((name . "common-draw-caption-id-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-glue-point"))))
	(ref
	 ((name . "draw-text")))))
      (define
       ((name . "common-draw-points-attlist"))
       (attribute
	((name . "draw:points"))
	(ref
	 ((name . "points")))))
      (define
       ((name . "draw-polygon"))
       (element
	((name . "draw:polygon"))
	(ref
	 ((name . "common-draw-points-attlist")))
	(ref
	 ((name . "common-draw-position-attlist")))
	(ref
	 ((name . "common-draw-size-attlist")))
	(ref
	 ((name . "common-draw-viewbox-attlist")))
	(ref
	 ((name . "common-draw-shape-with-text-and-styles-attlist")))
	(ref
	 ((name . "common-draw-caption-id-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-glue-point"))))
	(ref
	 ((name . "draw-text")))))
      (define
       ((name . "draw-regular-polygon"))
       (element
	((name . "draw:regular-polygon"))
	(ref
	 ((name . "draw-regular-polygon-attlist")))
	(ref
	 ((name . "common-draw-position-attlist")))
	(ref
	 ((name . "common-draw-size-attlist")))
	(ref
	 ((name . "common-draw-shape-with-text-and-styles-attlist")))
	(ref
	 ((name . "common-draw-caption-id-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-glue-point"))))
	(ref
	 ((name . "draw-text")))))
      (define
       ((name . "draw-regular-polygon-attlist"))
       (interleave nil
		   (choice nil
			   (attribute
			    ((name . "draw:concave"))
			    (value nil "false"))
			   (group nil
				  (attribute
				   ((name . "draw:concave"))
				   (value nil "true"))
				  (ref
				   ((name . "draw-regular-polygon-sharpness-attlist")))))
		   (attribute
		    ((name . "draw:corners"))
		    (ref
		     ((name . "positiveInteger"))))))
      (define
       ((name . "draw-regular-polygon-sharpness-attlist"))
       (attribute
	((name . "draw:sharpness"))
	(ref
	 ((name . "percent")))))
      (define
       ((name . "draw-path"))
       (element
	((name . "draw:path"))
	(ref
	 ((name . "common-draw-path-data-attlist")))
	(ref
	 ((name . "common-draw-position-attlist")))
	(ref
	 ((name . "common-draw-size-attlist")))
	(ref
	 ((name . "common-draw-viewbox-attlist")))
	(ref
	 ((name . "common-draw-shape-with-text-and-styles-attlist")))
	(ref
	 ((name . "common-draw-caption-id-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-glue-point"))))
	(ref
	 ((name . "draw-text")))))
      (define
       ((name . "common-draw-path-data-attlist"))
       (attribute
	((name . "svg:d"))
	(ref
	 ((name . "pathData")))))
      (define
       ((name . "draw-circle"))
       (element
	((name . "draw:circle"))
	(choice nil
		(group nil
		       (ref
			((name . "draw-circle-attlist")))
		       (ref
			((name . "common-draw-circle-ellipse-pos-attlist"))))
		(group nil
		       (ref
			((name . "common-draw-position-attlist")))
		       (ref
			((name . "common-draw-size-attlist")))))
	(ref
	 ((name . "common-draw-circle-ellipse-attlist")))
	(ref
	 ((name . "common-draw-shape-with-text-and-styles-attlist")))
	(ref
	 ((name . "common-draw-caption-id-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-glue-point"))))
	(ref
	 ((name . "draw-text")))))
      (define
       ((name . "common-draw-circle-ellipse-pos-attlist"))
       (attribute
	((name . "svg:cx"))
	(ref
	 ((name . "coordinate"))))
       (attribute
	((name . "svg:cy"))
	(ref
	 ((name . "coordinate")))))
      (define
       ((name . "draw-circle-attlist"))
       (attribute
	((name . "svg:r"))
	(ref
	 ((name . "length")))))
      (define
       ((name . "common-draw-circle-ellipse-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:kind"))
			      (choice nil
				      (value nil "full")
				      (value nil "section")
				      (value nil "cut")
				      (value nil "arc"))))
		   (optional nil
			     (attribute
			      ((name . "draw:start-angle"))
			      (ref
			       ((name . "angle")))))
		   (optional nil
			     (attribute
			      ((name . "draw:end-angle"))
			      (ref
			       ((name . "angle")))))))
      (define
       ((name . "draw-ellipse"))
       (element
	((name . "draw:ellipse"))
	(choice nil
		(group nil
		       (ref
			((name . "draw-ellipse-attlist")))
		       (ref
			((name . "common-draw-circle-ellipse-pos-attlist"))))
		(group nil
		       (ref
			((name . "common-draw-position-attlist")))
		       (ref
			((name . "common-draw-size-attlist")))))
	(ref
	 ((name . "common-draw-circle-ellipse-attlist")))
	(ref
	 ((name . "common-draw-shape-with-text-and-styles-attlist")))
	(ref
	 ((name . "common-draw-caption-id-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-glue-point"))))
	(ref
	 ((name . "draw-text")))))
      (define
       ((name . "draw-ellipse-attlist"))
       (attribute
	((name . "svg:rx"))
	(ref
	 ((name . "length"))))
       (attribute
	((name . "svg:ry"))
	(ref
	 ((name . "length")))))
      (define
       ((name . "draw-connector"))
       (element
	((name . "draw:connector"))
	(ref
	 ((name . "draw-connector-attlist")))
	(ref
	 ((name . "common-draw-shape-with-text-and-styles-attlist")))
	(ref
	 ((name . "common-draw-caption-id-attlist")))
	(ref
	 ((name . "common-draw-viewbox-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-glue-point"))))
	(ref
	 ((name . "draw-text")))))
      (define
       ((name . "draw-connector-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:type"))
			      (choice nil
				      (value nil "standard")
				      (value nil "lines")
				      (value nil "line")
				      (value nil "curve"))))
		   (optional nil
			     (attribute
			      ((name . "svg:x1"))
			      (ref
			       ((name . "coordinate"))))
			     (attribute
			      ((name . "svg:y1"))
			      (ref
			       ((name . "coordinate")))))
		   (optional nil
			     (attribute
			      ((name . "draw:start-shape"))
			      (ref
			       ((name . "IDREF")))))
		   (optional nil
			     (attribute
			      ((name . "draw:start-glue-point"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (optional nil
			     (attribute
			      ((name . "svg:x2"))
			      (ref
			       ((name . "coordinate"))))
			     (attribute
			      ((name . "svg:y2"))
			      (ref
			       ((name . "coordinate")))))
		   (optional nil
			     (attribute
			      ((name . "draw:end-shape"))
			      (ref
			       ((name . "IDREF")))))
		   (optional nil
			     (attribute
			      ((name . "draw:end-glue-point"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (optional nil
			     (attribute
			      ((name . "draw:line-skew"))
			      (list nil
				    (ref
				     ((name . "length")))
				    (optional nil
					      (ref
					       ((name . "length")))
					      (optional nil
							(ref
							 ((name . "length"))))))))
		   (optional nil
			     (attribute
			      ((name . "svg:d"))
			      (ref
			       ((name . "pathData")))))))
      (define
       ((name . "draw-caption"))
       (element
	((name . "draw:caption"))
	(ref
	 ((name . "draw-caption-attlist")))
	(ref
	 ((name . "common-draw-position-attlist")))
	(ref
	 ((name . "common-draw-size-attlist")))
	(ref
	 ((name . "common-draw-shape-with-text-and-styles-attlist")))
	(ref
	 ((name . "common-draw-caption-id-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-glue-point"))))
	(ref
	 ((name . "draw-text")))))
      (define
       ((name . "draw-caption-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:caption-point-x"))
			      (ref
			       ((name . "coordinate"))))
			     (attribute
			      ((name . "draw:caption-point-y"))
			      (ref
			       ((name . "coordinate")))))
		   (optional nil
			     (attribute
			      ((name . "draw:corner-radius"))
			      (ref
			       ((name . "nonNegativeLength")))))))
      (define
       ((name . "draw-measure"))
       (element
	((name . "draw:measure"))
	(ref
	 ((name . "draw-measure-attlist")))
	(ref
	 ((name . "common-draw-shape-with-text-and-styles-attlist")))
	(ref
	 ((name . "common-draw-caption-id-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-glue-point"))))
	(ref
	 ((name . "draw-text")))))
      (define
       ((name . "draw-measure-attlist"))
       (interleave nil
		   (attribute
		    ((name . "svg:x1"))
		    (ref
		     ((name . "coordinate"))))
		   (attribute
		    ((name . "svg:y1"))
		    (ref
		     ((name . "coordinate"))))
		   (attribute
		    ((name . "svg:x2"))
		    (ref
		     ((name . "coordinate"))))
		   (attribute
		    ((name . "svg:y2"))
		    (ref
		     ((name . "coordinate"))))))
      (define
       ((name . "draw-control"))
       (element
	((name . "draw:control"))
	(ref
	 ((name . "draw-control-attlist")))
	(ref
	 ((name . "common-draw-position-attlist")))
	(ref
	 ((name . "common-draw-size-attlist")))
	(ref
	 ((name . "common-draw-shape-with-text-and-styles-attlist")))
	(ref
	 ((name . "common-draw-caption-id-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-glue-point"))))))
      (define
       ((name . "draw-control-attlist"))
       (attribute
	((name . "draw:control"))
	(ref
	 ((name . "IDREF")))))
      (define
       ((name . "draw-page-thumbnail"))
       (element
	((name . "draw:page-thumbnail"))
	(ref
	 ((name . "draw-page-thumbnail-attlist")))
	(ref
	 ((name . "common-draw-position-attlist")))
	(ref
	 ((name . "common-draw-size-attlist")))
	(ref
	 ((name . "presentation-shape-attlist")))
	(ref
	 ((name . "common-draw-shape-with-styles-attlist")))
	(ref
	 ((name . "common-draw-caption-id-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))))
      (define
       ((name . "draw-page-thumbnail-attlist"))
       (optional nil
		 (attribute
		  ((name . "draw:page-number"))
		  (ref
		   ((name . "positiveInteger"))))))
      (define
       ((name . "draw-g"))
       (element
	((name . "draw:g"))
	(ref
	 ((name . "draw-g-attlist")))
	(ref
	 ((name . "common-draw-z-index-attlist")))
	(ref
	 ((name . "common-draw-name-attlist")))
	(ref
	 ((name . "common-draw-id-attlist")))
	(ref
	 ((name . "common-draw-style-name-attlist")))
	(ref
	 ((name . "common-text-spreadsheet-shape-attlist")))
	(ref
	 ((name . "common-draw-caption-id-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-glue-point"))))
	(zeroOrMore nil
		    (ref
		     ((name . "shape"))))))
      (define
       ((name . "draw-g-attlist"))
       (optional nil
		 (attribute
		  ((name . "svg:y"))
		  (ref
		   ((name . "coordinate"))))))
      (define
       ((name . "common-draw-name-attlist"))
       (optional nil
		 (attribute
		  ((name . "draw:name"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "common-draw-caption-id-attlist"))
       (optional nil
		 (attribute
		  ((name . "draw:caption-id"))
		  (ref
		   ((name . "IDREF"))))))
      (define
       ((name . "common-draw-position-attlist"))
       (optional nil
		 (attribute
		  ((name . "svg:x"))
		  (ref
		   ((name . "coordinate")))))
       (optional nil
		 (attribute
		  ((name . "svg:y"))
		  (ref
		   ((name . "coordinate"))))))
      (define
       ((name . "common-draw-size-attlist"))
       (optional nil
		 (attribute
		  ((name . "svg:width"))
		  (ref
		   ((name . "length")))))
       (optional nil
		 (attribute
		  ((name . "svg:height"))
		  (ref
		   ((name . "length"))))))
      (define
       ((name . "common-draw-transform-attlist"))
       (optional nil
		 (attribute
		  ((name . "draw:transform"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "common-draw-viewbox-attlist"))
       (attribute
	((name . "svg:viewBox"))
	(list nil
	      (ref
	       ((name . "integer")))
	      (ref
	       ((name . "integer")))
	      (ref
	       ((name . "integer")))
	      (ref
	       ((name . "integer"))))))
      (define
       ((name . "common-draw-style-name-attlist"))
       (choice nil
	       (group nil
		      (optional nil
				(attribute
				 ((name . "draw:style-name"))
				 (ref
				  ((name . "styleNameRef")))))
		      (optional nil
				(attribute
				 ((name . "draw:class-names"))
				 (ref
				  ((name . "styleNameRefs"))))))
	       (group nil
		      (optional nil
				(attribute
				 ((name . "presentation:style-name"))
				 (ref
				  ((name . "styleNameRef")))))
		      (optional nil
				(attribute
				 ((name . "presentation:class-names"))
				 (ref
				  ((name . "styleNameRefs"))))))))
      (define
       ((name . "common-draw-text-style-name-attlist"))
       (optional nil
		 (attribute
		  ((name . "draw:text-style-name"))
		  (ref
		   ((name . "styleNameRef"))))))
      (define
       ((name . "common-draw-layer-name-attlist"))
       (optional nil
		 (attribute
		  ((name . "draw:layer"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "common-draw-id-attlist"))
       (optional nil
		 (group nil
			(ref
			 ((name . "xml-id")))
			(optional nil
				  (attribute
				   ((name . "draw:id"))
				   (ref
				    ((name . "NCName"))))))))
      (define
       ((name . "common-draw-z-index-attlist"))
       (optional nil
		 (attribute
		  ((name . "draw:z-index"))
		  (ref
		   ((name . "nonNegativeInteger"))))))
      (define
       ((name . "common-text-spreadsheet-shape-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "table:end-cell-address"))
			      (ref
			       ((name . "cellAddress")))))
		   (optional nil
			     (attribute
			      ((name . "table:end-x"))
			      (ref
			       ((name . "coordinate")))))
		   (optional nil
			     (attribute
			      ((name . "table:end-y"))
			      (ref
			       ((name . "coordinate")))))
		   (optional nil
			     (attribute
			      ((name . "table:table-background"))
			      (ref
			       ((name . "boolean")))))
		   (ref
		    ((name . "common-text-anchor-attlist")))))
      (define
       ((name . "common-text-anchor-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "text:anchor-type"))
			      (choice nil
				      (value nil "page")
				      (value nil "frame")
				      (value nil "paragraph")
				      (value nil "char")
				      (value nil "as-char"))))
		   (optional nil
			     (attribute
			      ((name . "text:anchor-page-number"))
			      (ref
			       ((name . "positiveInteger")))))))
      (define
       ((name . "draw-text"))
       (zeroOrMore nil
		   (choice nil
			   (ref
			    ((name . "text-p")))
			   (ref
			    ((name . "text-list"))))))
      (define
       ((name . "common-draw-shape-with-styles-attlist"))
       (ref
	((name . "common-draw-z-index-attlist")))
       (ref
	((name . "common-draw-id-attlist")))
       (ref
	((name . "common-draw-layer-name-attlist")))
       (ref
	((name . "common-draw-style-name-attlist")))
       (ref
	((name . "common-draw-transform-attlist")))
       (ref
	((name . "common-draw-name-attlist")))
       (ref
	((name . "common-text-spreadsheet-shape-attlist"))))
      (define
       ((name . "common-draw-shape-with-text-and-styles-attlist"))
       (ref
	((name . "common-draw-shape-with-styles-attlist")))
       (ref
	((name . "common-draw-text-style-name-attlist"))))
      (define
       ((name . "draw-glue-point"))
       (element
	((name . "draw:glue-point"))
	(ref
	 ((name . "draw-glue-point-attlist")))
	(empty nil)))
      (define
       ((name . "draw-glue-point-attlist"))
       (interleave nil
		   (attribute
		    ((name . "draw:id"))
		    (ref
		     ((name . "nonNegativeInteger"))))
		   (attribute
		    ((name . "svg:x"))
		    (choice nil
			    (ref
			     ((name . "distance")))
			    (ref
			     ((name . "percent")))))
		   (attribute
		    ((name . "svg:y"))
		    (choice nil
			    (ref
			     ((name . "distance")))
			    (ref
			     ((name . "percent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:align"))
			      (choice nil
				      (value nil "top-left")
				      (value nil "top")
				      (value nil "top-right")
				      (value nil "left")
				      (value nil "center")
				      (value nil "right")
				      (value nil "bottom-left")
				      (value nil "bottom-right"))))
		   (attribute
		    ((name . "draw:escape-direction"))
		    (choice nil
			    (value nil "auto")
			    (value nil "left")
			    (value nil "right")
			    (value nil "up")
			    (value nil "down")
			    (value nil "horizontal")
			    (value nil "vertical")))))
      (define
       ((name . "svg-title"))
       (element
	((name . "svg:title"))
	(text nil)))
      (define
       ((name . "svg-desc"))
       (element
	((name . "svg:desc"))
	(text nil)))
      (define
       ((name . "draw-frame"))
       (element
	((name . "draw:frame"))
	(ref
	 ((name . "common-draw-shape-with-text-and-styles-attlist")))
	(ref
	 ((name . "common-draw-position-attlist")))
	(ref
	 ((name . "common-draw-rel-size-attlist")))
	(ref
	 ((name . "common-draw-caption-id-attlist")))
	(ref
	 ((name . "presentation-shape-attlist")))
	(ref
	 ((name . "draw-frame-attlist")))
	(zeroOrMore nil
		    (choice nil
			    (ref
			     ((name . "draw-text-box")))
			    (ref
			     ((name . "draw-image")))
			    (ref
			     ((name . "draw-object")))
			    (ref
			     ((name . "draw-object-ole")))
			    (ref
			     ((name . "draw-applet")))
			    (ref
			     ((name . "draw-floating-frame")))
			    (ref
			     ((name . "draw-plugin")))
			    (ref
			     ((name . "table-table")))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-glue-point"))))
	(optional nil
		  (ref
		   ((name . "draw-image-map"))))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (choice nil
			  (ref
			   ((name . "draw-contour-polygon")))
			  (ref
			   ((name . "draw-contour-path")))))))
      (define
       ((name . "common-draw-rel-size-attlist"))
       (ref
	((name . "common-draw-size-attlist")))
       (optional nil
		 (attribute
		  ((name . "style:rel-width"))
		  (choice nil
			  (ref
			   ((name . "percent")))
			  (value nil "scale")
			  (value nil "scale-min"))))
       (optional nil
		 (attribute
		  ((name . "style:rel-height"))
		  (choice nil
			  (ref
			   ((name . "percent")))
			  (value nil "scale")
			  (value nil "scale-min")))))
      (define
       ((name . "draw-frame-attlist"))
       (optional nil
		 (attribute
		  ((name . "draw:copy-of"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "draw-text-box"))
       (element
	((name . "draw:text-box"))
	(ref
	 ((name . "draw-text-box-attlist")))
	(zeroOrMore nil
		    (ref
		     ((name . "text-content"))))))
      (define
       ((name . "draw-text-box-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:chain-next-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:corner-radius"))
			      (ref
			       ((name . "nonNegativeLength")))))
		   (optional nil
			     (attribute
			      ((name . "fo:min-height"))
			      (choice nil
				      (ref
				       ((name . "length")))
				      (ref
				       ((name . "percent"))))))
		   (optional nil
			     (attribute
			      ((name . "fo:min-width"))
			      (choice nil
				      (ref
				       ((name . "length")))
				      (ref
				       ((name . "percent"))))))
		   (optional nil
			     (attribute
			      ((name . "fo:max-height"))
			      (choice nil
				      (ref
				       ((name . "length")))
				      (ref
				       ((name . "percent"))))))
		   (optional nil
			     (attribute
			      ((name . "fo:max-width"))
			      (choice nil
				      (ref
				       ((name . "length")))
				      (ref
				       ((name . "percent"))))))
		   (optional nil
			     (group nil
				    (ref
				     ((name . "xml-id")))
				    (optional nil
					      (attribute
					       ((name . "text:id"))
					       (ref
						((name . "NCName")))))))))
      (define
       ((name . "draw-image"))
       (element
	((name . "draw:image"))
	(ref
	 ((name . "draw-image-attlist")))
	(choice nil
		(ref
		 ((name . "common-draw-data-attlist")))
		(ref
		 ((name . "office-binary-data"))))
	(ref
	 ((name . "draw-text")))))
      (define
       ((name . "common-draw-data-attlist"))
       (group nil
	      (attribute
	       ((name . "xlink:type"))
	       (value nil "simple"))
	      (attribute
	       ((name . "xlink:href"))
	       (ref
		((name . "anyIRI"))))
	      (optional nil
			(attribute
			 ((name . "xlink:show"))
			 (value nil "embed")))
	      (optional nil
			(attribute
			 ((name . "xlink:actuate"))
			 (value nil "onLoad")))))
      (define
       ((name . "office-binary-data"))
       (element
	((name . "office:binary-data"))
	(ref
	 ((name . "base64Binary")))))
      (define
       ((name . "draw-image-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:filter-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "draw-object"))
       (element
	((name . "draw:object"))
	(ref
	 ((name . "draw-object-attlist")))
	(choice nil
		(ref
		 ((name . "common-draw-data-attlist")))
		(ref
		 ((name . "office-document")))
		(ref
		 ((name . "math-math"))))))
      (define
       ((name . "draw-object-ole"))
       (element
	((name . "draw:object-ole"))
	(ref
	 ((name . "draw-object-ole-attlist")))
	(choice nil
		(ref
		 ((name . "common-draw-data-attlist")))
		(ref
		 ((name . "office-binary-data"))))))
      (define
       ((name . "draw-object-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:notify-on-update-of-ranges"))
			      (choice nil
				      (ref
				       ((name . "cellRangeAddressList")))
				      (ref
				       ((name . "string"))))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "draw-object-ole-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:class-id"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "draw-applet"))
       (element
	((name . "draw:applet"))
	(ref
	 ((name . "draw-applet-attlist")))
	(optional nil
		  (ref
		   ((name . "common-draw-data-attlist"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-param"))))))
      (define
       ((name . "draw-applet-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:code"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:object"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:archive"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:may-script"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "draw-plugin"))
       (element
	((name . "draw:plugin"))
	(ref
	 ((name . "draw-plugin-attlist")))
	(ref
	 ((name . "common-draw-data-attlist")))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-param"))))))
      (define
       ((name . "draw-plugin-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:mime-type"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "draw-param"))
       (element
	((name . "draw:param"))
	(ref
	 ((name . "draw-param-attlist")))
	(empty nil)))
      (define
       ((name . "draw-param-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:value"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "draw-floating-frame"))
       (element
	((name . "draw:floating-frame"))
	(ref
	 ((name . "draw-floating-frame-attlist")))
	(ref
	 ((name . "common-draw-data-attlist")))))
      (define
       ((name . "draw-floating-frame-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:frame-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "draw-contour-polygon"))
       (element
	((name . "draw:contour-polygon"))
	(ref
	 ((name . "common-contour-attlist")))
	(ref
	 ((name . "common-draw-size-attlist")))
	(ref
	 ((name . "common-draw-viewbox-attlist")))
	(ref
	 ((name . "common-draw-points-attlist")))
	(empty nil)))
      (define
       ((name . "draw-contour-path"))
       (element
	((name . "draw:contour-path"))
	(ref
	 ((name . "common-contour-attlist")))
	(ref
	 ((name . "common-draw-size-attlist")))
	(ref
	 ((name . "common-draw-viewbox-attlist")))
	(ref
	 ((name . "common-draw-path-data-attlist")))
	(empty nil)))
      (define
       ((name . "common-contour-attlist"))
       (attribute
	((name . "draw:recreate-on-edit"))
	(ref
	 ((name . "boolean")))))
      (define
       ((name . "draw-a"))
       (element
	((name . "draw:a"))
	(ref
	 ((name . "draw-a-attlist")))
	(ref
	 ((name . "shape-instance")))))
      (define
       ((name . "draw-a-attlist"))
       (interleave nil
		   (attribute
		    ((name . "xlink:type"))
		    (value nil "simple"))
		   (attribute
		    ((name . "xlink:href"))
		    (ref
		     ((name . "anyIRI"))))
		   (optional nil
			     (attribute
			      ((name . "xlink:actuate"))
			      (value nil "onRequest")))
		   (optional nil
			     (attribute
			      ((name . "office:target-frame-name"))
			      (ref
			       ((name . "targetFrameName")))))
		   (optional nil
			     (attribute
			      ((name . "xlink:show"))
			      (choice nil
				      (value nil "new")
				      (value nil "replace"))))
		   (optional nil
			     (attribute
			      ((name . "office:name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "office:title"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "office:server-map"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "draw-image-map"))
       (element
	((name . "draw:image-map"))
	(zeroOrMore nil
		    (choice nil
			    (ref
			     ((name . "draw-area-rectangle")))
			    (ref
			     ((name . "draw-area-circle")))
			    (ref
			     ((name . "draw-area-polygon")))))))
      (define
       ((name . "draw-area-rectangle"))
       (element
	((name . "draw:area-rectangle"))
	(ref
	 ((name . "common-draw-area-attlist")))
	(attribute
	 ((name . "svg:x"))
	 (ref
	  ((name . "coordinate"))))
	(attribute
	 ((name . "svg:y"))
	 (ref
	  ((name . "coordinate"))))
	(attribute
	 ((name . "svg:width"))
	 (ref
	  ((name . "length"))))
	(attribute
	 ((name . "svg:height"))
	 (ref
	  ((name . "length"))))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))))
      (define
       ((name . "draw-area-circle"))
       (element
	((name . "draw:area-circle"))
	(ref
	 ((name . "common-draw-area-attlist")))
	(attribute
	 ((name . "svg:cx"))
	 (ref
	  ((name . "coordinate"))))
	(attribute
	 ((name . "svg:cy"))
	 (ref
	  ((name . "coordinate"))))
	(attribute
	 ((name . "svg:r"))
	 (ref
	  ((name . "length"))))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))))
      (define
       ((name . "draw-area-polygon"))
       (element
	((name . "draw:area-polygon"))
	(ref
	 ((name . "common-draw-area-attlist")))
	(attribute
	 ((name . "svg:x"))
	 (ref
	  ((name . "coordinate"))))
	(attribute
	 ((name . "svg:y"))
	 (ref
	  ((name . "coordinate"))))
	(attribute
	 ((name . "svg:width"))
	 (ref
	  ((name . "length"))))
	(attribute
	 ((name . "svg:height"))
	 (ref
	  ((name . "length"))))
	(ref
	 ((name . "common-draw-viewbox-attlist")))
	(ref
	 ((name . "common-draw-points-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))))
      (define
       ((name . "common-draw-area-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "xlink:type"))
			      (value nil "simple"))
			     (attribute
			      ((name . "xlink:href"))
			      (ref
			       ((name . "anyIRI"))))
			     (optional nil
				       (attribute
					((name . "office:target-frame-name"))
					(ref
					 ((name . "targetFrameName")))))
			     (optional nil
				       (attribute
					((name . "xlink:show"))
					(choice nil
						(value nil "new")
						(value nil "replace")))))
		   (optional nil
			     (attribute
			      ((name . "office:name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:nohref"))
			      (value nil "nohref")))))
      (define
       ((name . "dr3d-scene"))
       (element
	((name . "dr3d:scene"))
	(ref
	 ((name . "dr3d-scene-attlist")))
	(ref
	 ((name . "common-draw-position-attlist")))
	(ref
	 ((name . "common-draw-size-attlist")))
	(ref
	 ((name . "common-draw-style-name-attlist")))
	(ref
	 ((name . "common-draw-z-index-attlist")))
	(ref
	 ((name . "common-draw-id-attlist")))
	(ref
	 ((name . "common-draw-layer-name-attlist")))
	(ref
	 ((name . "common-text-spreadsheet-shape-attlist")))
	(ref
	 ((name . "common-dr3d-transform-attlist")))
	(ref
	 ((name . "common-draw-caption-id-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(zeroOrMore nil
		    (ref
		     ((name . "dr3d-light"))))
	(zeroOrMore nil
		    (ref
		     ((name . "shapes3d"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-glue-point"))))))
      (define
       ((name . "shapes3d"))
       (choice nil
	       (ref
		((name . "dr3d-scene")))
	       (ref
		((name . "dr3d-extrude")))
	       (ref
		((name . "dr3d-sphere")))
	       (ref
		((name . "dr3d-rotate")))
	       (ref
		((name . "dr3d-cube")))))
      (define
       ((name . "dr3d-scene-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "dr3d:vrp"))
			      (ref
			       ((name . "vector3D")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:vpn"))
			      (ref
			       ((name . "vector3D")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:vup"))
			      (ref
			       ((name . "vector3D")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:projection"))
			      (choice nil
				      (value nil "parallel")
				      (value nil "perspective"))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:distance"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:focal-length"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:shadow-slant"))
			      (ref
			       ((name . "angle")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:shade-mode"))
			      (choice nil
				      (value nil "flat")
				      (value nil "phong")
				      (value nil "gouraud")
				      (value nil "draft"))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:ambient-color"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:lighting-mode"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "common-dr3d-transform-attlist"))
       (optional nil
		 (attribute
		  ((name . "dr3d:transform"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "dr3d-light"))
       (element
	((name . "dr3d:light"))
	(ref
	 ((name . "dr3d-light-attlist")))
	(empty nil)))
      (define
       ((name . "dr3d-light-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "dr3d:diffuse-color"))
			      (ref
			       ((name . "color")))))
		   (attribute
		    ((name . "dr3d:direction"))
		    (ref
		     ((name . "vector3D"))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:enabled"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:specular"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "dr3d-cube"))
       (element
	((name . "dr3d:cube"))
	(ref
	 ((name . "dr3d-cube-attlist")))
	(ref
	 ((name . "common-draw-z-index-attlist")))
	(ref
	 ((name . "common-draw-id-attlist")))
	(ref
	 ((name . "common-draw-layer-name-attlist")))
	(ref
	 ((name . "common-draw-style-name-attlist")))
	(ref
	 ((name . "common-dr3d-transform-attlist")))
	(empty nil)))
      (define
       ((name . "dr3d-cube-attlist"))
       (optional nil
		 (attribute
		  ((name . "dr3d:min-edge"))
		  (ref
		   ((name . "vector3D")))))
       (optional nil
		 (attribute
		  ((name . "dr3d:max-edge"))
		  (ref
		   ((name . "vector3D"))))))
      (define
       ((name . "dr3d-sphere"))
       (element
	((name . "dr3d:sphere"))
	(ref
	 ((name . "dr3d-sphere-attlist")))
	(ref
	 ((name . "common-draw-z-index-attlist")))
	(ref
	 ((name . "common-draw-id-attlist")))
	(ref
	 ((name . "common-draw-layer-name-attlist")))
	(ref
	 ((name . "common-draw-style-name-attlist")))
	(ref
	 ((name . "common-dr3d-transform-attlist")))
	(empty nil)))
      (define
       ((name . "dr3d-sphere-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "dr3d:center"))
			      (ref
			       ((name . "vector3D")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:size"))
			      (ref
			       ((name . "vector3D")))))))
      (define
       ((name . "dr3d-extrude"))
       (element
	((name . "dr3d:extrude"))
	(ref
	 ((name . "common-draw-path-data-attlist")))
	(ref
	 ((name . "common-draw-viewbox-attlist")))
	(ref
	 ((name . "common-draw-id-attlist")))
	(ref
	 ((name . "common-draw-z-index-attlist")))
	(ref
	 ((name . "common-draw-layer-name-attlist")))
	(ref
	 ((name . "common-draw-style-name-attlist")))
	(ref
	 ((name . "common-dr3d-transform-attlist")))
	(empty nil)))
      (define
       ((name . "dr3d-rotate"))
       (element
	((name . "dr3d:rotate"))
	(ref
	 ((name . "common-draw-viewbox-attlist")))
	(ref
	 ((name . "common-draw-path-data-attlist")))
	(ref
	 ((name . "common-draw-z-index-attlist")))
	(ref
	 ((name . "common-draw-id-attlist")))
	(ref
	 ((name . "common-draw-layer-name-attlist")))
	(ref
	 ((name . "common-draw-style-name-attlist")))
	(ref
	 ((name . "common-dr3d-transform-attlist")))
	(empty nil)))
      (define
       ((name . "draw-custom-shape"))
       (element
	((name . "draw:custom-shape"))
	(ref
	 ((name . "draw-custom-shape-attlist")))
	(ref
	 ((name . "common-draw-position-attlist")))
	(ref
	 ((name . "common-draw-size-attlist")))
	(ref
	 ((name . "common-draw-shape-with-text-and-styles-attlist")))
	(ref
	 ((name . "common-draw-caption-id-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-title"))))
	(optional nil
		  (ref
		   ((name . "svg-desc"))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-glue-point"))))
	(ref
	 ((name . "draw-text")))
	(optional nil
		  (ref
		   ((name . "draw-enhanced-geometry"))))))
      (define
       ((name . "draw-custom-shape-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:engine"))
			      (ref
			       ((name . "namespacedToken")))))
		   (optional nil
			     (attribute
			      ((name . "draw:data"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "draw-enhanced-geometry"))
       (element
	((name . "draw:enhanced-geometry"))
	(ref
	 ((name . "draw-enhanced-geometry-attlist")))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-equation"))))
	(zeroOrMore nil
		    (ref
		     ((name . "draw-handle"))))))
      (define
       ((name . "draw-enhanced-geometry-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:type"))
			      (ref
			       ((name . "custom-shape-type")))))
		   (optional nil
			     (attribute
			      ((name . "svg:viewBox"))
			      (list nil
				    (ref
				     ((name . "integer")))
				    (ref
				     ((name . "integer")))
				    (ref
				     ((name . "integer")))
				    (ref
				     ((name . "integer"))))))
		   (optional nil
			     (attribute
			      ((name . "draw:mirror-vertical"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:mirror-horizontal"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:text-rotate-angle"))
			      (ref
			       ((name . "angle")))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-allowed"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:text-path-allowed"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:concentric-gradient-fill-allowed"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-brightness"))
			      (ref
			       ((name . "zeroToHundredPercent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-depth"))
			      (list nil
				    (ref
				     ((name . "length")))
				    (ref
				     ((name . "double"))))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-diffusion"))
			      (ref
			       ((name . "percent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-number-of-line-segments"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-light-face"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-first-light-harsh"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-second-light-harsh"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-first-light-level"))
			      (ref
			       ((name . "zeroToHundredPercent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-second-light-level"))
			      (ref
			       ((name . "zeroToHundredPercent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-first-light-direction"))
			      (ref
			       ((name . "vector3D")))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-second-light-direction"))
			      (ref
			       ((name . "vector3D")))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-metal"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:shade-mode"))
			      (choice nil
				      (value nil "flat")
				      (value nil "phong")
				      (value nil "gouraud")
				      (value nil "draft"))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-rotation-angle"))
			      (list nil
				    (ref
				     ((name . "angle")))
				    (ref
				     ((name . "angle"))))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-rotation-center"))
			      (ref
			       ((name . "vector3D")))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-shininess"))
			      (ref
			       ((name . "zeroToHundredPercent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-skew"))
			      (list nil
				    (ref
				     ((name . "double")))
				    (ref
				     ((name . "angle"))))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-specularity"))
			      (ref
			       ((name . "zeroToHundredPercent")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:projection"))
			      (choice nil
				      (value nil "parallel")
				      (value nil "perspective"))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-viewpoint"))
			      (ref
			       ((name . "point3D")))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-origin"))
			      (list nil
				    (ref
				     ((name . "extrusionOrigin")))
				    (ref
				     ((name . "extrusionOrigin"))))))
		   (optional nil
			     (attribute
			      ((name . "draw:extrusion-color"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:enhanced-path"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:path-stretchpoint-x"))
			      (ref
			       ((name . "double")))))
		   (optional nil
			     (attribute
			      ((name . "draw:path-stretchpoint-y"))
			      (ref
			       ((name . "double")))))
		   (optional nil
			     (attribute
			      ((name . "draw:text-areas"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:glue-points"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:glue-point-type"))
			      (choice nil
				      (value nil "none")
				      (value nil "segments")
				      (value nil "rectangle"))))
		   (optional nil
			     (attribute
			      ((name . "draw:glue-point-leaving-directions"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:text-path"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:text-path-mode"))
			      (choice nil
				      (value nil "normal")
				      (value nil "path")
				      (value nil "shape"))))
		   (optional nil
			     (attribute
			      ((name . "draw:text-path-scale"))
			      (choice nil
				      (value nil "path")
				      (value nil "shape"))))
		   (optional nil
			     (attribute
			      ((name . "draw:text-path-same-letter-heights"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:modifiers"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "custom-shape-type"))
       (choice nil
	       (value nil "non-primitive")
	       (ref
		((name . "string")))))
      (define
       ((name . "point3D"))
       (data
	((type . "string"))
	(param
	 ((name . "pattern"))
	 "\\([ ]*-?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc))([ ]+-?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc))){2}[ ]*\\)")))
      (define
       ((name . "extrusionOrigin"))
       (data
	((type . "double"))
	(param
	 ((name . "minInclusive"))
	 "-0.5")
	(param
	 ((name . "maxInclusive"))
	 "0.5")))
      (define
       ((name . "draw-equation"))
       (element
	((name . "draw:equation"))
	(ref
	 ((name . "draw-equation-attlist")))
	(empty nil)))
      (define
       ((name . "draw-equation-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:formula"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "draw-handle"))
       (element
	((name . "draw:handle"))
	(ref
	 ((name . "draw-handle-attlist")))
	(empty nil)))
      (define
       ((name . "draw-handle-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:handle-mirror-vertical"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:handle-mirror-horizontal"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:handle-switched"))
			      (ref
			       ((name . "boolean")))))
		   (attribute
		    ((name . "draw:handle-position"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "draw:handle-range-x-minimum"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:handle-range-x-maximum"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:handle-range-y-minimum"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:handle-range-y-maximum"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:handle-polar"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:handle-radius-range-minimum"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:handle-radius-range-maximum"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "presentation-shape-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "presentation:class"))
			      (ref
			       ((name . "presentation-classes")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:placeholder"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:user-transformed"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "presentation-classes"))
       (choice nil
	       (value nil "title")
	       (value nil "outline")
	       (value nil "subtitle")
	       (value nil "text")
	       (value nil "graphic")
	       (value nil "object")
	       (value nil "chart")
	       (value nil "table")
	       (value nil "orgchart")
	       (value nil "page")
	       (value nil "notes")
	       (value nil "handout")
	       (value nil "header")
	       (value nil "footer")
	       (value nil "date-time")
	       (value nil "page-number")))
      (define
       ((name . "presentation-animations"))
       (element
	((name . "presentation:animations"))
	(zeroOrMore nil
		    (choice nil
			    (ref
			     ((name . "presentation-animation-elements")))
			    (ref
			     ((name . "presentation-animation-group")))))))
      (define
       ((name . "presentation-animation-elements"))
       (choice nil
	       (ref
		((name . "presentation-show-shape")))
	       (ref
		((name . "presentation-show-text")))
	       (ref
		((name . "presentation-hide-shape")))
	       (ref
		((name . "presentation-hide-text")))
	       (ref
		((name . "presentation-dim")))
	       (ref
		((name . "presentation-play")))))
      (define
       ((name . "presentation-sound"))
       (element
	((name . "presentation:sound"))
	(ref
	 ((name . "presentation-sound-attlist")))
	(attribute
	 ((name . "xlink:type"))
	 (value nil "simple"))
	(attribute
	 ((name . "xlink:href"))
	 (ref
	  ((name . "anyIRI"))))
	(optional nil
		  (attribute
		   ((name . "xlink:actuate"))
		   (value nil "onRequest")))
	(optional nil
		  (attribute
		   ((name . "xlink:show"))
		   (choice nil
			   (value nil "new")
			   (value nil "replace"))))
	(empty nil)))
      (define
       ((name . "presentation-sound-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "presentation:play-full"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "presentation-show-shape"))
       (element
	((name . "presentation:show-shape"))
	(ref
	 ((name . "common-presentation-effect-attlist")))
	(optional nil
		  (ref
		   ((name . "presentation-sound"))))))
      (define
       ((name . "common-presentation-effect-attlist"))
       (interleave nil
		   (attribute
		    ((name . "draw:shape-id"))
		    (ref
		     ((name . "IDREF"))))
		   (optional nil
			     (attribute
			      ((name . "presentation:effect"))
			      (ref
			       ((name . "presentationEffects")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:direction"))
			      (ref
			       ((name . "presentationEffectDirections")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:speed"))
			      (ref
			       ((name . "presentationSpeeds")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:delay"))
			      (ref
			       ((name . "duration")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:start-scale"))
			      (ref
			       ((name . "percent")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:path-id"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "presentationEffects"))
       (choice nil
	       (value nil "none")
	       (value nil "fade")
	       (value nil "move")
	       (value nil "stripes")
	       (value nil "open")
	       (value nil "close")
	       (value nil "dissolve")
	       (value nil "wavyline")
	       (value nil "random")
	       (value nil "lines")
	       (value nil "laser")
	       (value nil "appear")
	       (value nil "hide")
	       (value nil "move-short")
	       (value nil "checkerboard")
	       (value nil "rotate")
	       (value nil "stretch")))
      (define
       ((name . "presentationEffectDirections"))
       (choice nil
	       (value nil "none")
	       (value nil "from-left")
	       (value nil "from-top")
	       (value nil "from-right")
	       (value nil "from-bottom")
	       (value nil "from-center")
	       (value nil "from-upper-left")
	       (value nil "from-upper-right")
	       (value nil "from-lower-left")
	       (value nil "from-lower-right")
	       (value nil "to-left")
	       (value nil "to-top")
	       (value nil "to-right")
	       (value nil "to-bottom")
	       (value nil "to-upper-left")
	       (value nil "to-upper-right")
	       (value nil "to-lower-right")
	       (value nil "to-lower-left")
	       (value nil "path")
	       (value nil "spiral-inward-left")
	       (value nil "spiral-inward-right")
	       (value nil "spiral-outward-left")
	       (value nil "spiral-outward-right")
	       (value nil "vertical")
	       (value nil "horizontal")
	       (value nil "to-center")
	       (value nil "clockwise")
	       (value nil "counter-clockwise")))
      (define
       ((name . "presentationSpeeds"))
       (choice nil
	       (value nil "slow")
	       (value nil "medium")
	       (value nil "fast")))
      (define
       ((name . "presentation-show-text"))
       (element
	((name . "presentation:show-text"))
	(ref
	 ((name . "common-presentation-effect-attlist")))
	(optional nil
		  (ref
		   ((name . "presentation-sound"))))))
      (define
       ((name . "presentation-hide-shape"))
       (element
	((name . "presentation:hide-shape"))
	(ref
	 ((name . "common-presentation-effect-attlist")))
	(optional nil
		  (ref
		   ((name . "presentation-sound"))))))
      (define
       ((name . "presentation-hide-text"))
       (element
	((name . "presentation:hide-text"))
	(ref
	 ((name . "common-presentation-effect-attlist")))
	(optional nil
		  (ref
		   ((name . "presentation-sound"))))))
      (define
       ((name . "presentation-dim"))
       (element
	((name . "presentation:dim"))
	(ref
	 ((name . "presentation-dim-attlist")))
	(optional nil
		  (ref
		   ((name . "presentation-sound"))))))
      (define
       ((name . "presentation-dim-attlist"))
       (interleave nil
		   (attribute
		    ((name . "draw:shape-id"))
		    (ref
		     ((name . "IDREF"))))
		   (attribute
		    ((name . "draw:color"))
		    (ref
		     ((name . "color"))))))
      (define
       ((name . "presentation-play"))
       (element
	((name . "presentation:play"))
	(ref
	 ((name . "presentation-play-attlist")))
	(empty nil)))
      (define
       ((name . "presentation-play-attlist"))
       (attribute
	((name . "draw:shape-id"))
	(ref
	 ((name . "IDREF"))))
       (optional nil
		 (attribute
		  ((name . "presentation:speed"))
		  (ref
		   ((name . "presentationSpeeds"))))))
      (define
       ((name . "presentation-animation-group"))
       (element
	((name . "presentation:animation-group"))
	(zeroOrMore nil
		    (ref
		     ((name . "presentation-animation-elements"))))))
      (define
       ((name . "common-anim-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "presentation:node-type"))
			      (choice nil
				      (value nil "default")
				      (value nil "on-click")
				      (value nil "with-previous")
				      (value nil "after-previous")
				      (value nil "timing-root")
				      (value nil "main-sequence")
				      (value nil "interactive-sequence"))))
		   (optional nil
			     (attribute
			      ((name . "presentation:preset-id"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:preset-sub-type"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:preset-class"))
			      (choice nil
				      (value nil "custom")
				      (value nil "entrance")
				      (value nil "exit")
				      (value nil "emphasis")
				      (value nil "motion-path")
				      (value nil "ole-action")
				      (value nil "media-call"))))
		   (optional nil
			     (attribute
			      ((name . "presentation:master-element"))
			      (ref
			       ((name . "IDREF")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:group-id"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (group nil
				    (ref
				     ((name . "xml-id")))
				    (optional nil
					      (attribute
					       ((name . "anim:id"))
					       (ref
						((name . "NCName")))))))))
      (define
       ((name . "presentation-event-listener"))
       (element
	((name . "presentation:event-listener"))
	(ref
	 ((name . "presentation-event-listener-attlist")))
	(optional nil
		  (ref
		   ((name . "presentation-sound"))))))
      (define
       ((name . "presentation-event-listener-attlist"))
       (interleave nil
		   (attribute
		    ((name . "script:event-name"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "presentation:action"))
		    (choice nil
			    (value nil "none")
			    (value nil "previous-page")
			    (value nil "next-page")
			    (value nil "first-page")
			    (value nil "last-page")
			    (value nil "hide")
			    (value nil "stop")
			    (value nil "execute")
			    (value nil "show")
			    (value nil "verb")
			    (value nil "fade-out")
			    (value nil "sound")
			    (value nil "last-visited-page")))
		   (optional nil
			     (attribute
			      ((name . "presentation:effect"))
			      (ref
			       ((name . "presentationEffects")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:direction"))
			      (ref
			       ((name . "presentationEffectDirections")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:speed"))
			      (ref
			       ((name . "presentationSpeeds")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:start-scale"))
			      (ref
			       ((name . "percent")))))
		   (optional nil
			     (attribute
			      ((name . "xlink:type"))
			      (value nil "simple"))
			     (attribute
			      ((name . "xlink:href"))
			      (ref
			       ((name . "anyIRI"))))
			     (optional nil
				       (attribute
					((name . "xlink:show"))
					(value nil "embed")))
			     (optional nil
				       (attribute
					((name . "xlink:actuate"))
					(value nil "onRequest"))))
		   (optional nil
			     (attribute
			      ((name . "presentation:verb"))
			      (ref
			       ((name . "nonNegativeInteger")))))))
      (define
       ((name . "presentation-decls"))
       (zeroOrMore nil
		   (ref
		    ((name . "presentation-decl")))))
      (define
       ((name . "presentation-decl"))
       (choice nil
	       (element
		((name . "presentation:header-decl"))
		(ref
		 ((name . "presentation-header-decl-attlist")))
		(text nil))
	       (element
		((name . "presentation:footer-decl"))
		(ref
		 ((name . "presentation-footer-decl-attlist")))
		(text nil))
	       (element
		((name . "presentation:date-time-decl"))
		(ref
		 ((name . "presentation-date-time-decl-attlist")))
		(text nil))))
      (define
       ((name . "presentation-header-decl-attlist"))
       (attribute
	((name . "presentation:name"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "presentation-footer-decl-attlist"))
       (attribute
	((name . "presentation:name"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "presentation-date-time-decl-attlist"))
       (interleave nil
		   (attribute
		    ((name . "presentation:name"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "presentation:source"))
		    (choice nil
			    (value nil "fixed")
			    (value nil "current-date")))
		   (optional nil
			     (attribute
			      ((name . "style:data-style-name"))
			      (ref
			       ((name . "styleNameRef")))))))
      (define
       ((name . "presentation-settings"))
       (optional nil
		 (element
		  ((name . "presentation:settings"))
		  (ref
		   ((name . "presentation-settings-attlist")))
		  (zeroOrMore nil
			      (ref
			       ((name . "presentation-show")))))))
      (define
       ((name . "presentation-settings-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "presentation:start-page"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:show"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:full-screen"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:endless"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:pause"))
			      (ref
			       ((name . "duration")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:show-logo"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:force-manual"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:mouse-visible"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:mouse-as-pen"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:start-with-navigator"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:animations"))
			      (choice nil
				      (value nil "enabled")
				      (value nil "disabled"))))
		   (optional nil
			     (attribute
			      ((name . "presentation:transition-on-click"))
			      (choice nil
				      (value nil "enabled")
				      (value nil "disabled"))))
		   (optional nil
			     (attribute
			      ((name . "presentation:stay-on-top"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:show-end-of-presentation-slide"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "presentation-show"))
       (element
	((name . "presentation:show"))
	(ref
	 ((name . "presentation-show-attlist")))
	(empty nil)))
      (define
       ((name . "presentation-show-attlist"))
       (interleave nil
		   (attribute
		    ((name . "presentation:name"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "presentation:pages"))
		    (ref
		     ((name . "string"))))))
      (define
       ((name . "chart-chart"))
       (element
	((name . "chart:chart"))
	(ref
	 ((name . "chart-chart-attlist")))
	(optional nil
		  (ref
		   ((name . "chart-title"))))
	(optional nil
		  (ref
		   ((name . "chart-subtitle"))))
	(optional nil
		  (ref
		   ((name . "chart-footer"))))
	(optional nil
		  (ref
		   ((name . "chart-legend"))))
	(ref
	 ((name . "chart-plot-area")))
	(optional nil
		  (ref
		   ((name . "table-table"))))))
      (define
       ((name . "chart-chart-attlist"))
       (interleave nil
		   (attribute
		    ((name . "chart:class"))
		    (ref
		     ((name . "namespacedToken"))))
		   (ref
		    ((name . "common-draw-size-attlist")))
		   (optional nil
			     (attribute
			      ((name . "chart:column-mapping"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "chart:row-mapping"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "chart:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (group nil
				    (attribute
				     ((name . "xlink:type"))
				     (value nil "simple"))
				    (attribute
				     ((name . "xlink:href"))
				     (ref
				      ((name . "anyIRI"))))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "chart-title"))
       (element
	((name . "chart:title"))
	(ref
	 ((name . "chart-title-attlist")))
	(optional nil
		  (ref
		   ((name . "text-p"))))))
      (define
       ((name . "chart-title-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "table:cell-range"))
			      (ref
			       ((name . "cellRangeAddressList")))))
		   (ref
		    ((name . "common-draw-position-attlist")))
		   (optional nil
			     (attribute
			      ((name . "chart:style-name"))
			      (ref
			       ((name . "styleNameRef")))))))
      (define
       ((name . "chart-subtitle"))
       (element
	((name . "chart:subtitle"))
	(ref
	 ((name . "chart-title-attlist")))
	(optional nil
		  (ref
		   ((name . "text-p"))))))
      (define
       ((name . "chart-footer"))
       (element
	((name . "chart:footer"))
	(ref
	 ((name . "chart-title-attlist")))
	(optional nil
		  (ref
		   ((name . "text-p"))))))
      (define
       ((name . "chart-legend"))
       (element
	((name . "chart:legend"))
	(ref
	 ((name . "chart-legend-attlist")))
	(optional nil
		  (ref
		   ((name . "text-p"))))))
      (define
       ((name . "chart-legend-attlist"))
       (interleave nil
		   (choice nil
			   (group nil
				  (attribute
				   ((name . "chart:legend-position"))
				   (choice nil
					   (value nil "start")
					   (value nil "end")
					   (value nil "top")
					   (value nil "bottom")))
				  (optional nil
					    (attribute
					     ((name . "chart:legend-align"))
					     (choice nil
						     (value nil "start")
						     (value nil "center")
						     (value nil "end")))))
			   (attribute
			    ((name . "chart:legend-position"))
			    (choice nil
				    (value nil "top-start")
				    (value nil "bottom-start")
				    (value nil "top-end")
				    (value nil "bottom-end")))
			   (empty nil))
		   (ref
		    ((name . "common-draw-position-attlist")))
		   (choice nil
			   (attribute
			    ((name . "style:legend-expansion"))
			    (choice nil
				    (value nil "wide")
				    (value nil "high")
				    (value nil "balanced")))
			   (group nil
				  (attribute
				   ((name . "style:legend-expansion"))
				   (value nil "custom"))
				  (attribute
				   ((name . "style:legend-expansion-aspect-ratio"))
				   (ref
				    ((name . "double")))))
			   (empty nil))
		   (optional nil
			     (attribute
			      ((name . "chart:style-name"))
			      (ref
			       ((name . "styleNameRef")))))))
      (define
       ((name . "chart-plot-area"))
       (element
	((name . "chart:plot-area"))
	(ref
	 ((name . "chart-plot-area-attlist")))
	(zeroOrMore nil
		    (ref
		     ((name . "dr3d-light"))))
	(zeroOrMore nil
		    (ref
		     ((name . "chart-axis"))))
	(zeroOrMore nil
		    (ref
		     ((name . "chart-series"))))
	(optional nil
		  (ref
		   ((name . "chart-stock-gain-marker"))))
	(optional nil
		  (ref
		   ((name . "chart-stock-loss-marker"))))
	(optional nil
		  (ref
		   ((name . "chart-stock-range-line"))))
	(optional nil
		  (ref
		   ((name . "chart-wall"))))
	(optional nil
		  (ref
		   ((name . "chart-floor"))))))
      (define
       ((name . "chart-plot-area-attlist"))
       (interleave nil
		   (ref
		    ((name . "common-draw-position-attlist")))
		   (ref
		    ((name . "common-draw-size-attlist")))
		   (optional nil
			     (attribute
			      ((name . "chart:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "table:cell-range-address"))
			      (ref
			       ((name . "cellRangeAddressList")))))
		   (optional nil
			     (attribute
			      ((name . "chart:data-source-has-labels"))
			      (choice nil
				      (value nil "none")
				      (value nil "row")
				      (value nil "column")
				      (value nil "both"))))
		   (ref
		    ((name . "dr3d-scene-attlist")))
		   (ref
		    ((name . "common-dr3d-transform-attlist")))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "chart-wall"))
       (element
	((name . "chart:wall"))
	(ref
	 ((name . "chart-wall-attlist")))
	(empty nil)))
      (define
       ((name . "chart-wall-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "svg:width"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "chart:style-name"))
			      (ref
			       ((name . "styleNameRef")))))))
      (define
       ((name . "chart-floor"))
       (element
	((name . "chart:floor"))
	(ref
	 ((name . "chart-floor-attlist")))
	(empty nil)))
      (define
       ((name . "chart-floor-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "svg:width"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "chart:style-name"))
			      (ref
			       ((name . "styleNameRef")))))))
      (define
       ((name . "chart-axis"))
       (element
	((name . "chart:axis"))
	(ref
	 ((name . "chart-axis-attlist")))
	(optional nil
		  (ref
		   ((name . "chart-title"))))
	(optional nil
		  (ref
		   ((name . "chart-categories"))))
	(zeroOrMore nil
		    (ref
		     ((name . "chart-grid"))))))
      (define
       ((name . "chart-axis-attlist"))
       (interleave nil
		   (attribute
		    ((name . "chart:dimension"))
		    (ref
		     ((name . "chart-dimension"))))
		   (optional nil
			     (attribute
			      ((name . "chart:name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "chart:style-name"))
			      (ref
			       ((name . "styleNameRef")))))))
      (define
       ((name . "chart-dimension"))
       (choice nil
	       (value nil "x")
	       (value nil "y")
	       (value nil "z")))
      (define
       ((name . "chart-categories"))
       (element
	((name . "chart:categories"))
	(optional nil
		  (attribute
		   ((name . "table:cell-range-address"))
		   (ref
		    ((name . "cellRangeAddressList")))))))
      (define
       ((name . "chart-grid"))
       (element
	((name . "chart:grid"))
	(ref
	 ((name . "chart-grid-attlist")))))
      (define
       ((name . "chart-grid-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "chart:class"))
			      (choice nil
				      (value nil "major")
				      (value nil "minor"))))
		   (optional nil
			     (attribute
			      ((name . "chart:style-name"))
			      (ref
			       ((name . "styleNameRef")))))))
      (define
       ((name . "chart-series"))
       (element
	((name . "chart:series"))
	(ref
	 ((name . "chart-series-attlist")))
	(zeroOrMore nil
		    (ref
		     ((name . "chart-domain"))))
	(optional nil
		  (ref
		   ((name . "chart-mean-value"))))
	(zeroOrMore nil
		    (ref
		     ((name . "chart-regression-curve"))))
	(zeroOrMore nil
		    (ref
		     ((name . "chart-error-indicator"))))
	(zeroOrMore nil
		    (ref
		     ((name . "chart-data-point"))))
	(optional nil
		  (ref
		   ((name . "chart-data-label"))))))
      (define
       ((name . "chart-series-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "chart:values-cell-range-address"))
			      (ref
			       ((name . "cellRangeAddressList")))))
		   (optional nil
			     (attribute
			      ((name . "chart:label-cell-address"))
			      (ref
			       ((name . "cellRangeAddressList")))))
		   (optional nil
			     (attribute
			      ((name . "chart:class"))
			      (ref
			       ((name . "namespacedToken")))))
		   (optional nil
			     (attribute
			      ((name . "chart:attached-axis"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "chart:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "chart-domain"))
       (element
	((name . "chart:domain"))
	(optional nil
		  (attribute
		   ((name . "table:cell-range-address"))
		   (ref
		    ((name . "cellRangeAddressList")))))))
      (define
       ((name . "chart-data-point"))
       (element
	((name . "chart:data-point"))
	(ref
	 ((name . "chart-data-point-attlist")))
	(optional nil
		  (ref
		   ((name . "chart-data-label"))))))
      (define
       ((name . "chart-data-point-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "chart:repeated"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "chart:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (ref
			      ((name . "xml-id"))))))
      (define
       ((name . "chart-data-label"))
       (element
	((name . "chart:data-label"))
	(ref
	 ((name . "chart-data-label-attlist")))
	(optional nil
		  (ref
		   ((name . "text-p"))))))
      (define
       ((name . "chart-data-label-attlist"))
       (interleave nil
		   (ref
		    ((name . "common-draw-position-attlist")))
		   (optional nil
			     (attribute
			      ((name . "chart:style-name"))
			      (ref
			       ((name . "styleNameRef")))))))
      (define
       ((name . "chart-mean-value"))
       (element
	((name . "chart:mean-value"))
	(ref
	 ((name . "chart-mean-value-attlist")))
	(empty nil)))
      (define
       ((name . "chart-mean-value-attlist"))
       (optional nil
		 (attribute
		  ((name . "chart:style-name"))
		  (ref
		   ((name . "styleNameRef"))))))
      (define
       ((name . "chart-error-indicator"))
       (element
	((name . "chart:error-indicator"))
	(ref
	 ((name . "chart-error-indicator-attlist")))
	(empty nil)))
      (define
       ((name . "chart-error-indicator-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "chart:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (attribute
		    ((name . "chart:dimension"))
		    (ref
		     ((name . "chart-dimension"))))))
      (define
       ((name . "chart-regression-curve"))
       (element
	((name . "chart:regression-curve"))
	(ref
	 ((name . "chart-regression-curve-attlist")))
	(optional nil
		  (ref
		   ((name . "chart-equation"))))))
      (define
       ((name . "chart-regression-curve-attlist"))
       (optional nil
		 (attribute
		  ((name . "chart:style-name"))
		  (ref
		   ((name . "styleNameRef"))))))
      (define
       ((name . "chart-equation"))
       (element
	((name . "chart:equation"))
	(ref
	 ((name . "chart-equation-attlist")))
	(optional nil
		  (ref
		   ((name . "text-p"))))))
      (define
       ((name . "chart-equation-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "chart:automatic-content"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:display-r-square"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:display-equation"))
			      (ref
			       ((name . "boolean")))))
		   (ref
		    ((name . "common-draw-position-attlist")))
		   (optional nil
			     (attribute
			      ((name . "chart:style-name"))
			      (ref
			       ((name . "styleNameRef")))))))
      (define
       ((name . "chart-stock-gain-marker"))
       (element
	((name . "chart:stock-gain-marker"))
	(ref
	 ((name . "common-stock-marker-attlist")))))
      (define
       ((name . "chart-stock-loss-marker"))
       (element
	((name . "chart:stock-loss-marker"))
	(ref
	 ((name . "common-stock-marker-attlist")))))
      (define
       ((name . "chart-stock-range-line"))
       (element
	((name . "chart:stock-range-line"))
	(ref
	 ((name . "common-stock-marker-attlist")))))
      (define
       ((name . "common-stock-marker-attlist"))
       (optional nil
		 (attribute
		  ((name . "chart:style-name"))
		  (ref
		   ((name . "styleNameRef"))))))
      (define
       ((name . "office-database"))
       (element
	((name . "office:database"))
	(ref
	 ((name . "db-data-source")))
	(optional nil
		  (ref
		   ((name . "db-forms"))))
	(optional nil
		  (ref
		   ((name . "db-reports"))))
	(optional nil
		  (ref
		   ((name . "db-queries"))))
	(optional nil
		  (ref
		   ((name . "db-table-presentations"))))
	(optional nil
		  (ref
		   ((name . "db-schema-definition"))))))
      (define
       ((name . "db-data-source"))
       (element
	((name . "db:data-source"))
	(ref
	 ((name . "db-data-source-attlist")))
	(ref
	 ((name . "db-connection-data")))
	(optional nil
		  (ref
		   ((name . "db-driver-settings"))))
	(optional nil
		  (ref
		   ((name . "db-application-connection-settings"))))))
      (define
       ((name . "db-data-source-attlist"))
       (empty nil))
      (define
       ((name . "db-connection-data"))
       (element
	((name . "db:connection-data"))
	(ref
	 ((name . "db-connection-data-attlist")))
	(choice nil
		(ref
		 ((name . "db-database-description")))
		(ref
		 ((name . "db-connection-resource"))))
	(optional nil
		  (ref
		   ((name . "db-login"))))))
      (define
       ((name . "db-connection-data-attlist"))
       (empty nil))
      (define
       ((name . "db-database-description"))
       (element
	((name . "db:database-description"))
	(ref
	 ((name . "db-database-description-attlist")))
	(choice nil
		(ref
		 ((name . "db-file-based-database")))
		(ref
		 ((name . "db-server-database"))))))
      (define
       ((name . "db-database-description-attlist"))
       (empty nil))
      (define
       ((name . "db-file-based-database"))
       (element
	((name . "db:file-based-database"))
	(ref
	 ((name . "db-file-based-database-attlist")))))
      (define
       ((name . "db-file-based-database-attlist"))
       (interleave nil
		   (attribute
		    ((name . "xlink:type"))
		    (value nil "simple"))
		   (attribute
		    ((name . "xlink:href"))
		    (ref
		     ((name . "anyIRI"))))
		   (attribute
		    ((name . "db:media-type"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "db:extension"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "db-server-database"))
       (element
	((name . "db:server-database"))
	(ref
	 ((name . "db-server-database-attlist")))
	(empty nil)))
      (define
       ((name . "db-server-database-attlist"))
       (interleave nil
		   (attribute
		    ((name . "db:type"))
		    (ref
		     ((name . "namespacedToken"))))
		   (choice nil
			   (ref
			    ((name . "db-host-and-port")))
			   (ref
			    ((name . "db-local-socket-name"))))
		   (optional nil
			     (attribute
			      ((name . "db:database-name"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "db-host-and-port"))
       (attribute
	((name . "db:hostname"))
	(ref
	 ((name . "string"))))
       (optional nil
		 (attribute
		  ((name . "db:port"))
		  (ref
		   ((name . "positiveInteger"))))))
      (define
       ((name . "db-local-socket-name"))
       (optional nil
		 (attribute
		  ((name . "db:local-socket"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "db-connection-resource"))
       (element
	((name . "db:connection-resource"))
	(ref
	 ((name . "db-connection-resource-attlist")))
	(empty nil)))
      (define
       ((name . "db-connection-resource-attlist"))
       (attribute
	((name . "xlink:type"))
	(value nil "simple"))
       (attribute
	((name . "xlink:href"))
	(ref
	 ((name . "anyIRI"))))
       (optional nil
		 (attribute
		  ((name . "xlink:show"))
		  (value nil "none")))
       (optional nil
		 (attribute
		  ((name . "xlink:actuate"))
		  (value nil "onRequest"))))
      (define
       ((name . "db-login"))
       (element
	((name . "db:login"))
	(ref
	 ((name . "db-login-attlist")))
	(empty nil)))
      (define
       ((name . "db-login-attlist"))
       (interleave nil
		   (optional nil
			     (choice nil
				     (attribute
				      ((name . "db:user-name"))
				      (ref
				       ((name . "string"))))
				     (attribute
				      ((name . "db:use-system-user"))
				      (ref
				       ((name . "boolean"))))))
		   (optional nil
			     (attribute
			      ((name . "db:is-password-required"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "db:login-timeout"))
			      (ref
			       ((name . "positiveInteger")))))))
      (define
       ((name . "db-driver-settings"))
       (element
	((name . "db:driver-settings"))
	(ref
	 ((name . "db-driver-settings-attlist")))
	(optional nil
		  (ref
		   ((name . "db-auto-increment"))))
	(optional nil
		  (ref
		   ((name . "db-delimiter"))))
	(optional nil
		  (ref
		   ((name . "db-character-set"))))
	(optional nil
		  (ref
		   ((name . "db-table-settings"))))))
      (define
       ((name . "db-driver-settings-attlist"))
       (interleave nil
		   (ref
		    ((name . "db-show-deleted")))
		   (optional nil
			     (attribute
			      ((name . "db:system-driver-settings"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "db:base-dn"))
			      (ref
			       ((name . "string")))))
		   (ref
		    ((name . "db-is-first-row-header-line")))
		   (optional nil
			     (attribute
			      ((name . "db:parameter-name-substitution"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "db-show-deleted"))
       (optional nil
		 (attribute
		  ((name . "db:show-deleted"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "db-is-first-row-header-line"))
       (optional nil
		 (attribute
		  ((name . "db:is-first-row-header-line"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "db-auto-increment"))
       (element
	((name . "db:auto-increment"))
	(ref
	 ((name . "db-auto-increment-attlist")))
	(empty nil)))
      (define
       ((name . "db-auto-increment-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "db:additional-column-statement"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "db:row-retrieving-statement"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "db-delimiter"))
       (element
	((name . "db:delimiter"))
	(ref
	 ((name . "db-delimiter-attlist")))
	(empty nil)))
      (define
       ((name . "db-delimiter-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "db:field"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "db:string"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "db:decimal"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "db:thousand"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "db-character-set"))
       (element
	((name . "db:character-set"))
	(ref
	 ((name . "db-character-set-attlist")))
	(empty nil)))
      (define
       ((name . "db-character-set-attlist"))
       (optional nil
		 (attribute
		  ((name . "db:encoding"))
		  (ref
		   ((name . "textEncoding"))))))
      (define
       ((name . "db-table-settings"))
       (element
	((name . "db:table-settings"))
	(zeroOrMore nil
		    (ref
		     ((name . "db-table-setting"))))))
      (define
       ((name . "db-table-setting"))
       (element
	((name . "db:table-setting"))
	(ref
	 ((name . "db-table-setting-attlist")))
	(optional nil
		  (ref
		   ((name . "db-delimiter"))))
	(optional nil
		  (ref
		   ((name . "db-character-set"))))
	(empty nil)))
      (define
       ((name . "db-table-setting-attlist"))
       (ref
	((name . "db-is-first-row-header-line")))
       (ref
	((name . "db-show-deleted"))))
      (define
       ((name . "db-application-connection-settings"))
       (element
	((name . "db:application-connection-settings"))
	(ref
	 ((name . "db-application-connection-settings-attlist")))
	(optional nil
		  (ref
		   ((name . "db-table-filter"))))
	(optional nil
		  (ref
		   ((name . "db-table-type-filter"))))
	(optional nil
		  (ref
		   ((name . "db-data-source-settings"))))))
      (define
       ((name . "db-application-connection-settings-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "db:is-table-name-length-limited"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "db:enable-sql92-check"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "db:append-table-alias-name"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "db:ignore-driver-privileges"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "db:boolean-comparison-mode"))
			      (choice nil
				      (value nil "equal-integer")
				      (value nil "is-boolean")
				      (value nil "equal-boolean")
				      (value nil "equal-use-only-zero"))))
		   (optional nil
			     (attribute
			      ((name . "db:use-catalog"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "db:max-row-count"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "db:suppress-version-columns"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "db-table-filter"))
       (element
	((name . "db:table-filter"))
	(ref
	 ((name . "db-table-filter-attlist")))
	(optional nil
		  (ref
		   ((name . "db-table-include-filter"))))
	(optional nil
		  (ref
		   ((name . "db-table-exclude-filter"))))))
      (define
       ((name . "db-table-filter-attlist"))
       (empty nil))
      (define
       ((name . "db-table-include-filter"))
       (element
	((name . "db:table-include-filter"))
	(ref
	 ((name . "db-table-include-filter-attlist")))
	(oneOrMore nil
		   (ref
		    ((name . "db-table-filter-pattern"))))))
      (define
       ((name . "db-table-include-filter-attlist"))
       (empty nil))
      (define
       ((name . "db-table-exclude-filter"))
       (element
	((name . "db:table-exclude-filter"))
	(ref
	 ((name . "db-table-exclude-filter-attlist")))
	(oneOrMore nil
		   (ref
		    ((name . "db-table-filter-pattern"))))))
      (define
       ((name . "db-table-exclude-filter-attlist"))
       (empty nil))
      (define
       ((name . "db-table-filter-pattern"))
       (element
	((name . "db:table-filter-pattern"))
	(ref
	 ((name . "db-table-filter-pattern-attlist")))
	(ref
	 ((name . "string")))))
      (define
       ((name . "db-table-filter-pattern-attlist"))
       (empty nil))
      (define
       ((name . "db-table-type-filter"))
       (element
	((name . "db:table-type-filter"))
	(ref
	 ((name . "db-table-type-filter-attlist")))
	(zeroOrMore nil
		    (ref
		     ((name . "db-table-type"))))))
      (define
       ((name . "db-table-type-filter-attlist"))
       (empty nil))
      (define
       ((name . "db-table-type"))
       (element
	((name . "db:table-type"))
	(ref
	 ((name . "db-table-type-attlist")))
	(ref
	 ((name . "string")))))
      (define
       ((name . "db-table-type-attlist"))
       (empty nil))
      (define
       ((name . "db-data-source-settings"))
       (element
	((name . "db:data-source-settings"))
	(ref
	 ((name . "db-data-source-settings-attlist")))
	(oneOrMore nil
		   (ref
		    ((name . "db-data-source-setting"))))))
      (define
       ((name . "db-data-source-settings-attlist"))
       (empty nil))
      (define
       ((name . "db-data-source-setting"))
       (element
	((name . "db:data-source-setting"))
	(ref
	 ((name . "db-data-source-setting-attlist")))
	(oneOrMore nil
		   (ref
		    ((name . "db-data-source-setting-value"))))))
      (define
       ((name . "db-data-source-setting-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "db:data-source-setting-is-list"))
			      (ref
			       ((name . "boolean")))))
		   (attribute
		    ((name . "db:data-source-setting-name"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "db:data-source-setting-type"))
		    (ref
		     ((name . "db-data-source-setting-types"))))))
      (define
       ((name . "db-data-source-setting-types"))
       (choice nil
	       (value nil "boolean")
	       (value nil "short")
	       (value nil "int")
	       (value nil "long")
	       (value nil "double")
	       (value nil "string")))
      (define
       ((name . "db-data-source-setting-value"))
       (element
	((name . "db:data-source-setting-value"))
	(ref
	 ((name . "db-data-source-setting-value-attlist")))
	(ref
	 ((name . "string")))))
      (define
       ((name . "db-data-source-setting-value-attlist"))
       (empty nil))
      (define
       ((name . "db-forms"))
       (element
	((name . "db:forms"))
	(ref
	 ((name . "db-forms-attlist")))
	(zeroOrMore nil
		    (choice nil
			    (ref
			     ((name . "db-component")))
			    (ref
			     ((name . "db-component-collection")))))))
      (define
       ((name . "db-forms-attlist"))
       (empty nil))
      (define
       ((name . "db-reports"))
       (element
	((name . "db:reports"))
	(ref
	 ((name . "db-reports-attlist")))
	(zeroOrMore nil
		    (choice nil
			    (ref
			     ((name . "db-component")))
			    (ref
			     ((name . "db-component-collection")))))))
      (define
       ((name . "db-reports-attlist"))
       (empty nil))
      (define
       ((name . "db-component-collection"))
       (element
	((name . "db:component-collection"))
	(ref
	 ((name . "db-component-collection-attlist")))
	(ref
	 ((name . "common-db-object-name")))
	(ref
	 ((name . "common-db-object-title")))
	(ref
	 ((name . "common-db-object-description")))
	(zeroOrMore nil
		    (choice nil
			    (ref
			     ((name . "db-component")))
			    (ref
			     ((name . "db-component-collection")))))))
      (define
       ((name . "db-component-collection-attlist"))
       (empty nil))
      (define
       ((name . "db-component"))
       (element
	((name . "db:component"))
	(ref
	 ((name . "db-component-attlist")))
	(ref
	 ((name . "common-db-object-name")))
	(ref
	 ((name . "common-db-object-title")))
	(ref
	 ((name . "common-db-object-description")))
	(optional nil
		  (choice nil
			  (ref
			   ((name . "office-document")))
			  (ref
			   ((name . "math-math")))))))
      (define
       ((name . "db-component-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "xlink:type"))
			      (value nil "simple"))
			     (attribute
			      ((name . "xlink:href"))
			      (ref
			       ((name . "anyIRI"))))
			     (optional nil
				       (attribute
					((name . "xlink:show"))
					(value nil "none")))
			     (optional nil
				       (attribute
					((name . "xlink:actuate"))
					(value nil "onRequest"))))
		   (optional nil
			     (attribute
			      ((name . "db:as-template"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "db-queries"))
       (element
	((name . "db:queries"))
	(ref
	 ((name . "db-queries-attlist")))
	(zeroOrMore nil
		    (choice nil
			    (ref
			     ((name . "db-query")))
			    (ref
			     ((name . "db-query-collection")))))))
      (define
       ((name . "db-queries-attlist"))
       (empty nil))
      (define
       ((name . "db-query-collection"))
       (element
	((name . "db:query-collection"))
	(ref
	 ((name . "db-query-collection-attlist")))
	(ref
	 ((name . "common-db-object-name")))
	(ref
	 ((name . "common-db-object-title")))
	(ref
	 ((name . "common-db-object-description")))
	(zeroOrMore nil
		    (choice nil
			    (ref
			     ((name . "db-query")))
			    (ref
			     ((name . "db-query-collection")))))))
      (define
       ((name . "db-query-collection-attlist"))
       (empty nil))
      (define
       ((name . "db-query"))
       (element
	((name . "db:query"))
	(ref
	 ((name . "db-query-attlist")))
	(ref
	 ((name . "common-db-object-name")))
	(ref
	 ((name . "common-db-object-title")))
	(ref
	 ((name . "common-db-object-description")))
	(ref
	 ((name . "common-db-table-style-name")))
	(optional nil
		  (ref
		   ((name . "db-order-statement"))))
	(optional nil
		  (ref
		   ((name . "db-filter-statement"))))
	(optional nil
		  (ref
		   ((name . "db-columns"))))
	(optional nil
		  (ref
		   ((name . "db-update-table"))))))
      (define
       ((name . "db-query-attlist"))
       (interleave nil
		   (attribute
		    ((name . "db:command"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "db:escape-processing"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "db-order-statement"))
       (element
	((name . "db:order-statement"))
	(ref
	 ((name . "db-command")))
	(ref
	 ((name . "db-apply-command")))
	(empty nil)))
      (define
       ((name . "db-filter-statement"))
       (element
	((name . "db:filter-statement"))
	(ref
	 ((name . "db-command")))
	(ref
	 ((name . "db-apply-command")))
	(empty nil)))
      (define
       ((name . "db-update-table"))
       (element
	((name . "db:update-table"))
	(ref
	 ((name . "common-db-table-name-attlist")))))
      (define
       ((name . "db-table-presentations"))
       (element
	((name . "db:table-representations"))
	(ref
	 ((name . "db-table-presentations-attlist")))
	(zeroOrMore nil
		    (ref
		     ((name . "db-table-presentation"))))))
      (define
       ((name . "db-table-presentations-attlist"))
       (empty nil))
      (define
       ((name . "db-table-presentation"))
       (element
	((name . "db:table-representation"))
	(ref
	 ((name . "db-table-presentation-attlist")))
	(ref
	 ((name . "common-db-table-name-attlist")))
	(ref
	 ((name . "common-db-object-title")))
	(ref
	 ((name . "common-db-object-description")))
	(ref
	 ((name . "common-db-table-style-name")))
	(optional nil
		  (ref
		   ((name . "db-order-statement"))))
	(optional nil
		  (ref
		   ((name . "db-filter-statement"))))
	(optional nil
		  (ref
		   ((name . "db-columns"))))))
      (define
       ((name . "db-table-presentation-attlist"))
       (empty nil))
      (define
       ((name . "db-columns"))
       (element
	((name . "db:columns"))
	(ref
	 ((name . "db-columns-attlist")))
	(oneOrMore nil
		   (ref
		    ((name . "db-column"))))))
      (define
       ((name . "db-columns-attlist"))
       (empty nil))
      (define
       ((name . "db-column"))
       (element
	((name . "db:column"))
	(ref
	 ((name . "db-column-attlist")))
	(ref
	 ((name . "common-db-object-name")))
	(ref
	 ((name . "common-db-object-title")))
	(ref
	 ((name . "common-db-object-description")))
	(ref
	 ((name . "common-db-default-value")))))
      (define
       ((name . "db-column-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "db:visible"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "db:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "db:default-cell-style-name"))
			      (ref
			       ((name . "styleNameRef")))))))
      (define
       ((name . "db-command"))
       (attribute
	((name . "db:command"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "db-apply-command"))
       (optional nil
		 (attribute
		  ((name . "db:apply-command"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "common-db-table-name-attlist"))
       (interleave nil
		   (attribute
		    ((name . "db:name"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "db:catalog-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "db:schema-name"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "common-db-object-name"))
       (attribute
	((name . "db:name"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "common-db-object-title"))
       (optional nil
		 (attribute
		  ((name . "db:title"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "common-db-object-description"))
       (optional nil
		 (attribute
		  ((name . "db:description"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "common-db-table-style-name"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "db:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "db:default-row-style-name"))
			      (ref
			       ((name . "styleNameRef")))))))
      (define
       ((name . "common-db-default-value"))
       (optional nil
		 (ref
		  ((name . "common-value-and-type-attlist")))))
      (define
       ((name . "db-schema-definition"))
       (element
	((name . "db:schema-definition"))
	(ref
	 ((name . "db-schema-definition-attlist")))
	(ref
	 ((name . "db-table-definitions")))))
      (define
       ((name . "db-schema-definition-attlist"))
       (empty nil))
      (define
       ((name . "db-table-definitions"))
       (element
	((name . "db:table-definitions"))
	(ref
	 ((name . "db-table-definitions-attlist")))
	(zeroOrMore nil
		    (ref
		     ((name . "db-table-definition"))))))
      (define
       ((name . "db-table-definitions-attlist"))
       (empty nil))
      (define
       ((name . "db-table-definition"))
       (element
	((name . "db:table-definition"))
	(ref
	 ((name . "common-db-table-name-attlist")))
	(ref
	 ((name . "db-table-definition-attlist")))
	(ref
	 ((name . "db-column-definitions")))
	(optional nil
		  (ref
		   ((name . "db-keys"))))
	(optional nil
		  (ref
		   ((name . "db-indices"))))))
      (define
       ((name . "db-table-definition-attlist"))
       (optional nil
		 (attribute
		  ((name . "db:type"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "db-column-definitions"))
       (element
	((name . "db:column-definitions"))
	(ref
	 ((name . "db-column-definitions-attlist")))
	(oneOrMore nil
		   (ref
		    ((name . "db-column-definition"))))))
      (define
       ((name . "db-column-definitions-attlist"))
       (empty nil))
      (define
       ((name . "db-column-definition"))
       (element
	((name . "db:column-definition"))
	(ref
	 ((name . "db-column-definition-attlist")))
	(ref
	 ((name . "common-db-default-value")))))
      (define
       ((name . "db-column-definition-attlist"))
       (interleave nil
		   (attribute
		    ((name . "db:name"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "db:data-type"))
			      (ref
			       ((name . "db-data-types")))))
		   (optional nil
			     (attribute
			      ((name . "db:type-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "db:precision"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "db:scale"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "db:is-nullable"))
			      (choice nil
				      (value nil "no-nulls")
				      (value nil "nullable"))))
		   (optional nil
			     (attribute
			      ((name . "db:is-empty-allowed"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "db:is-autoincrement"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "db-data-types"))
       (choice nil
	       (value nil "bit")
	       (value nil "boolean")
	       (value nil "tinyint")
	       (value nil "smallint")
	       (value nil "integer")
	       (value nil "bigint")
	       (value nil "float")
	       (value nil "real")
	       (value nil "double")
	       (value nil "numeric")
	       (value nil "decimal")
	       (value nil "char")
	       (value nil "varchar")
	       (value nil "longvarchar")
	       (value nil "date")
	       (value nil "time")
	       (value nil "timestmp")
	       (value nil "binary")
	       (value nil "varbinary")
	       (value nil "longvarbinary")
	       (value nil "sqlnull")
	       (value nil "other")
	       (value nil "object")
	       (value nil "distinct")
	       (value nil "struct")
	       (value nil "array")
	       (value nil "blob")
	       (value nil "clob")
	       (value nil "ref")))
      (define
       ((name . "db-keys"))
       (element
	((name . "db:keys"))
	(ref
	 ((name . "db-keys-attlist")))
	(oneOrMore nil
		   (ref
		    ((name . "db-key"))))))
      (define
       ((name . "db-keys-attlist"))
       (empty nil))
      (define
       ((name . "db-key"))
       (element
	((name . "db:key"))
	(ref
	 ((name . "db-key-attlist")))
	(oneOrMore nil
		   (ref
		    ((name . "db-key-columns"))))))
      (define
       ((name . "db-key-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "db:name"))
			      (ref
			       ((name . "string")))))
		   (attribute
		    ((name . "db:type"))
		    (choice nil
			    (value nil "primary")
			    (value nil "unique")
			    (value nil "foreign")))
		   (optional nil
			     (attribute
			      ((name . "db:referenced-table-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "db:update-rule"))
			      (choice nil
				      (value nil "cascade")
				      (value nil "restrict")
				      (value nil "set-null")
				      (value nil "no-action")
				      (value nil "set-default"))))
		   (optional nil
			     (attribute
			      ((name . "db:delete-rule"))
			      (choice nil
				      (value nil "cascade")
				      (value nil "restrict")
				      (value nil "set-null")
				      (value nil "no-action")
				      (value nil "set-default"))))))
      (define
       ((name . "db-key-columns"))
       (element
	((name . "db:key-columns"))
	(ref
	 ((name . "db-key-columns-attlist")))
	(oneOrMore nil
		   (ref
		    ((name . "db-key-column"))))))
      (define
       ((name . "db-key-columns-attlist"))
       (empty nil))
      (define
       ((name . "db-key-column"))
       (element
	((name . "db:key-column"))
	(ref
	 ((name . "db-key-column-attlist")))
	(empty nil)))
      (define
       ((name . "db-key-column-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "db:name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "db:related-column-name"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "db-indices"))
       (element
	((name . "db:indices"))
	(ref
	 ((name . "db-indices-attlist")))
	(oneOrMore nil
		   (ref
		    ((name . "db-index"))))))
      (define
       ((name . "db-indices-attlist"))
       (empty nil))
      (define
       ((name . "db-index"))
       (element
	((name . "db:index"))
	(ref
	 ((name . "db-index-attlist")))
	(oneOrMore nil
		   (ref
		    ((name . "db-index-columns"))))))
      (define
       ((name . "db-index-attlist"))
       (interleave nil
		   (attribute
		    ((name . "db:name"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "db:catalog-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "db:is-unique"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "db:is-clustered"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "db-index-columns"))
       (element
	((name . "db:index-columns"))
	(oneOrMore nil
		   (ref
		    ((name . "db-index-column"))))))
      (define
       ((name . "db-index-column"))
       (element
	((name . "db:index-column"))
	(ref
	 ((name . "db-index-column-attlist")))
	(empty nil)))
      (define
       ((name . "db-index-column-attlist"))
       (interleave nil
		   (attribute
		    ((name . "db:name"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "db:is-ascending"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "office-forms"))
       (optional nil
		 (element
		  ((name . "office:forms"))
		  (ref
		   ((name . "office-forms-attlist")))
		  (zeroOrMore nil
			      (choice nil
				      (ref
				       ((name . "form-form")))
				      (ref
				       ((name . "xforms-model"))))))))
      (define
       ((name . "office-forms-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "form:automatic-focus"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "form:apply-design-mode"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "form-form"))
       (element
	((name . "form:form"))
	(ref
	 ((name . "common-form-control-attlist")))
	(ref
	 ((name . "form-form-attlist")))
	(optional nil
		  (ref
		   ((name . "form-properties"))))
	(optional nil
		  (ref
		   ((name . "office-event-listeners"))))
	(zeroOrMore nil
		    (choice nil
			    (ref
			     ((name . "controls")))
			    (ref
			     ((name . "form-form")))))
	(optional nil
		  (ref
		   ((name . "form-connection-resource"))))))
      (define
       ((name . "form-form-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "xlink:type"))
			      (value nil "simple"))
			     (attribute
			      ((name . "xlink:href"))
			      (ref
			       ((name . "anyIRI"))))
			     (optional nil
				       (attribute
					((name . "xlink:actuate"))
					(value nil "onRequest"))))
		   (optional nil
			     (attribute
			      ((name . "office:target-frame"))
			      (ref
			       ((name . "targetFrameName")))))
		   (optional nil
			     (attribute
			      ((name . "form:method"))
			      (choice nil
				      (value nil "get")
				      (value nil "post")
				      (ref
				       ((name . "string"))))))
		   (optional nil
			     (attribute
			      ((name . "form:enctype"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "form:allow-deletes"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "form:allow-inserts"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "form:allow-updates"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "form:apply-filter"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "form:command-type"))
			      (choice nil
				      (value nil "table")
				      (value nil "query")
				      (value nil "command"))))
		   (optional nil
			     (attribute
			      ((name . "form:command"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "form:datasource"))
			      (choice nil
				      (ref
				       ((name . "anyIRI")))
				      (ref
				       ((name . "string"))))))
		   (optional nil
			     (attribute
			      ((name . "form:master-fields"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "form:detail-fields"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "form:escape-processing"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "form:filter"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "form:ignore-result"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "form:navigation-mode"))
			      (ref
			       ((name . "navigation")))))
		   (optional nil
			     (attribute
			      ((name . "form:order"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "form:tab-cycle"))
			      (ref
			       ((name . "tab-cycles")))))))
      (define
       ((name . "navigation"))
       (choice nil
	       (value nil "none")
	       (value nil "current")
	       (value nil "parent")))
      (define
       ((name . "tab-cycles"))
       (choice nil
	       (value nil "records")
	       (value nil "current")
	       (value nil "page")))
      (define
       ((name . "form-connection-resource"))
       (element
	((name . "form:connection-resource"))
	(attribute
	 ((name . "xlink:href"))
	 (ref
	  ((name . "anyIRI"))))
	(empty nil)))
      (define
       ((name . "xforms-model"))
       (element
	((name . "xforms:model"))
	(ref
	 ((name . "anyAttListOrElements")))))
      (define
       ((name . "column-controls"))
       (choice nil
	       (element
		((name . "form:text"))
		(ref
		 ((name . "form-text-attlist")))
		(ref
		 ((name . "common-form-control-content"))))
	       (element
		((name . "form:textarea"))
		(ref
		 ((name . "form-textarea-attlist")))
		(ref
		 ((name . "common-form-control-content")))
		(zeroOrMore nil
			    (ref
			     ((name . "text-p")))))
	       (element
		((name . "form:formatted-text"))
		(ref
		 ((name . "form-formatted-text-attlist")))
		(ref
		 ((name . "common-form-control-content"))))
	       (element
		((name . "form:number"))
		(ref
		 ((name . "form-number-attlist")))
		(ref
		 ((name . "common-numeric-control-attlist")))
		(ref
		 ((name . "common-form-control-content")))
		(ref
		 ((name . "common-linked-cell")))
		(ref
		 ((name . "common-spin-button")))
		(ref
		 ((name . "common-repeat")))
		(ref
		 ((name . "common-delay-for-repeat"))))
	       (element
		((name . "form:date"))
		(ref
		 ((name . "form-date-attlist")))
		(ref
		 ((name . "common-numeric-control-attlist")))
		(ref
		 ((name . "common-form-control-content")))
		(ref
		 ((name . "common-linked-cell")))
		(ref
		 ((name . "common-spin-button")))
		(ref
		 ((name . "common-repeat")))
		(ref
		 ((name . "common-delay-for-repeat"))))
	       (element
		((name . "form:time"))
		(ref
		 ((name . "form-time-attlist")))
		(ref
		 ((name . "common-numeric-control-attlist")))
		(ref
		 ((name . "common-form-control-content")))
		(ref
		 ((name . "common-linked-cell")))
		(ref
		 ((name . "common-spin-button")))
		(ref
		 ((name . "common-repeat")))
		(ref
		 ((name . "common-delay-for-repeat"))))
	       (element
		((name . "form:combobox"))
		(ref
		 ((name . "form-combobox-attlist")))
		(ref
		 ((name . "common-form-control-content")))
		(zeroOrMore nil
			    (ref
			     ((name . "form-item")))))
	       (element
		((name . "form:listbox"))
		(ref
		 ((name . "form-listbox-attlist")))
		(ref
		 ((name . "common-form-control-content")))
		(zeroOrMore nil
			    (ref
			     ((name . "form-option")))))
	       (element
		((name . "form:checkbox"))
		(ref
		 ((name . "form-checkbox-attlist")))
		(ref
		 ((name . "common-form-control-content"))))))
      (define
       ((name . "controls"))
       (choice nil
	       (ref
		((name . "column-controls")))
	       (element
		((name . "form:password"))
		(ref
		 ((name . "form-password-attlist")))
		(ref
		 ((name . "common-form-control-content"))))
	       (element
		((name . "form:file"))
		(ref
		 ((name . "form-file-attlist")))
		(ref
		 ((name . "common-form-control-content"))))
	       (element
		((name . "form:fixed-text"))
		(ref
		 ((name . "form-fixed-text-attlist")))
		(ref
		 ((name . "common-form-control-content"))))
	       (element
		((name . "form:button"))
		(ref
		 ((name . "form-button-attlist")))
		(ref
		 ((name . "common-form-control-content"))))
	       (element
		((name . "form:image"))
		(ref
		 ((name . "form-image-attlist")))
		(ref
		 ((name . "common-form-control-content"))))
	       (element
		((name . "form:radio"))
		(ref
		 ((name . "form-radio-attlist")))
		(ref
		 ((name . "common-form-control-content"))))
	       (element
		((name . "form:frame"))
		(ref
		 ((name . "form-frame-attlist")))
		(ref
		 ((name . "common-form-control-content"))))
	       (element
		((name . "form:image-frame"))
		(ref
		 ((name . "form-image-frame-attlist")))
		(ref
		 ((name . "common-form-control-content"))))
	       (element
		((name . "form:hidden"))
		(ref
		 ((name . "form-hidden-attlist")))
		(ref
		 ((name . "common-form-control-content"))))
	       (element
		((name . "form:grid"))
		(ref
		 ((name . "form-grid-attlist")))
		(ref
		 ((name . "common-form-control-content")))
		(zeroOrMore nil
			    (ref
			     ((name . "form-column")))))
	       (element
		((name . "form:value-range"))
		(ref
		 ((name . "form-value-range-attlist")))
		(ref
		 ((name . "common-form-control-content"))))
	       (element
		((name . "form:generic-control"))
		(ref
		 ((name . "form-generic-control-attlist")))
		(ref
		 ((name . "common-form-control-content"))))))
      (define
       ((name . "form-text-attlist"))
       (ref
	((name . "form-control-attlist")))
       (ref
	((name . "common-current-value-attlist")))
       (ref
	((name . "common-disabled-attlist")))
       (ref
	((name . "common-maxlength-attlist")))
       (ref
	((name . "common-printable-attlist")))
       (ref
	((name . "common-readonly-attlist")))
       (ref
	((name . "common-tab-attlist")))
       (ref
	((name . "common-title-attlist")))
       (ref
	((name . "common-value-attlist")))
       (ref
	((name . "common-convert-empty-attlist")))
       (ref
	((name . "common-data-field-attlist")))
       (ref
	((name . "common-linked-cell"))))
      (define
       ((name . "form-control-attlist"))
       (ref
	((name . "common-form-control-attlist")))
       (ref
	((name . "common-control-id-attlist")))
       (ref
	((name . "xforms-bind-attlist"))))
      (define
       ((name . "common-form-control-content"))
       (optional nil
		 (ref
		  ((name . "form-properties"))))
       (optional nil
		 (ref
		  ((name . "office-event-listeners")))))
      (define
       ((name . "form-textarea-attlist"))
       (ref
	((name . "form-control-attlist")))
       (ref
	((name . "common-current-value-attlist")))
       (ref
	((name . "common-disabled-attlist")))
       (ref
	((name . "common-maxlength-attlist")))
       (ref
	((name . "common-printable-attlist")))
       (ref
	((name . "common-readonly-attlist")))
       (ref
	((name . "common-tab-attlist")))
       (ref
	((name . "common-title-attlist")))
       (ref
	((name . "common-value-attlist")))
       (ref
	((name . "common-convert-empty-attlist")))
       (ref
	((name . "common-data-field-attlist")))
       (ref
	((name . "common-linked-cell"))))
      (define
       ((name . "form-password-attlist"))
       (interleave nil
		   (ref
		    ((name . "form-control-attlist")))
		   (ref
		    ((name . "common-disabled-attlist")))
		   (ref
		    ((name . "common-maxlength-attlist")))
		   (ref
		    ((name . "common-printable-attlist")))
		   (ref
		    ((name . "common-tab-attlist")))
		   (ref
		    ((name . "common-title-attlist")))
		   (ref
		    ((name . "common-value-attlist")))
		   (ref
		    ((name . "common-convert-empty-attlist")))
		   (ref
		    ((name . "common-linked-cell")))
		   (optional nil
			     (attribute
			      ((name . "form:echo-char"))
			      (ref
			       ((name . "character")))))))
      (define
       ((name . "form-file-attlist"))
       (ref
	((name . "form-control-attlist")))
       (ref
	((name . "common-current-value-attlist")))
       (ref
	((name . "common-disabled-attlist")))
       (ref
	((name . "common-maxlength-attlist")))
       (ref
	((name . "common-printable-attlist")))
       (ref
	((name . "common-readonly-attlist")))
       (ref
	((name . "common-tab-attlist")))
       (ref
	((name . "common-title-attlist")))
       (ref
	((name . "common-value-attlist")))
       (ref
	((name . "common-linked-cell"))))
      (define
       ((name . "form-formatted-text-attlist"))
       (interleave nil
		   (ref
		    ((name . "form-control-attlist")))
		   (ref
		    ((name . "common-current-value-attlist")))
		   (ref
		    ((name . "common-disabled-attlist")))
		   (ref
		    ((name . "common-maxlength-attlist")))
		   (ref
		    ((name . "common-printable-attlist")))
		   (ref
		    ((name . "common-readonly-attlist")))
		   (ref
		    ((name . "common-tab-attlist")))
		   (ref
		    ((name . "common-title-attlist")))
		   (ref
		    ((name . "common-value-attlist")))
		   (ref
		    ((name . "common-convert-empty-attlist")))
		   (ref
		    ((name . "common-data-field-attlist")))
		   (ref
		    ((name . "common-linked-cell")))
		   (ref
		    ((name . "common-spin-button")))
		   (ref
		    ((name . "common-repeat")))
		   (ref
		    ((name . "common-delay-for-repeat")))
		   (optional nil
			     (attribute
			      ((name . "form:max-value"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "form:min-value"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "form:validation"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "common-numeric-control-attlist"))
       (ref
	((name . "form-control-attlist")))
       (ref
	((name . "common-disabled-attlist")))
       (ref
	((name . "common-maxlength-attlist")))
       (ref
	((name . "common-printable-attlist")))
       (ref
	((name . "common-readonly-attlist")))
       (ref
	((name . "common-tab-attlist")))
       (ref
	((name . "common-title-attlist")))
       (ref
	((name . "common-convert-empty-attlist")))
       (ref
	((name . "common-data-field-attlist"))))
      (define
       ((name . "form-number-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "form:value"))
			      (ref
			       ((name . "double")))))
		   (optional nil
			     (attribute
			      ((name . "form:current-value"))
			      (ref
			       ((name . "double")))))
		   (optional nil
			     (attribute
			      ((name . "form:min-value"))
			      (ref
			       ((name . "double")))))
		   (optional nil
			     (attribute
			      ((name . "form:max-value"))
			      (ref
			       ((name . "double")))))))
      (define
       ((name . "form-date-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "form:value"))
			      (ref
			       ((name . "date")))))
		   (optional nil
			     (attribute
			      ((name . "form:current-value"))
			      (ref
			       ((name . "date")))))
		   (optional nil
			     (attribute
			      ((name . "form:min-value"))
			      (ref
			       ((name . "date")))))
		   (optional nil
			     (attribute
			      ((name . "form:max-value"))
			      (ref
			       ((name . "date")))))))
      (define
       ((name . "form-time-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "form:value"))
			      (ref
			       ((name . "time")))))
		   (optional nil
			     (attribute
			      ((name . "form:current-value"))
			      (ref
			       ((name . "time")))))
		   (optional nil
			     (attribute
			      ((name . "form:min-value"))
			      (ref
			       ((name . "time")))))
		   (optional nil
			     (attribute
			      ((name . "form:max-value"))
			      (ref
			       ((name . "time")))))))
      (define
       ((name . "form-fixed-text-attlist"))
       (interleave nil
		   (ref
		    ((name . "form-control-attlist")))
		   (ref
		    ((name . "for")))
		   (ref
		    ((name . "common-disabled-attlist")))
		   (ref
		    ((name . "label")))
		   (ref
		    ((name . "common-printable-attlist")))
		   (ref
		    ((name . "common-title-attlist")))
		   (optional nil
			     (attribute
			      ((name . "form:multi-line"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "form-combobox-attlist"))
       (interleave nil
		   (ref
		    ((name . "form-control-attlist")))
		   (ref
		    ((name . "common-current-value-attlist")))
		   (ref
		    ((name . "common-disabled-attlist")))
		   (ref
		    ((name . "dropdown")))
		   (ref
		    ((name . "common-maxlength-attlist")))
		   (ref
		    ((name . "common-printable-attlist")))
		   (ref
		    ((name . "common-readonly-attlist")))
		   (ref
		    ((name . "size")))
		   (ref
		    ((name . "common-tab-attlist")))
		   (ref
		    ((name . "common-title-attlist")))
		   (ref
		    ((name . "common-value-attlist")))
		   (ref
		    ((name . "common-convert-empty-attlist")))
		   (ref
		    ((name . "common-data-field-attlist")))
		   (ref
		    ((name . "list-source")))
		   (ref
		    ((name . "list-source-type")))
		   (ref
		    ((name . "common-linked-cell")))
		   (ref
		    ((name . "common-source-cell-range")))
		   (optional nil
			     (attribute
			      ((name . "form:auto-complete"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "form-item"))
       (element
	((name . "form:item"))
	(ref
	 ((name . "form-item-attlist")))
	(text nil)))
      (define
       ((name . "form-item-attlist"))
       (ref
	((name . "label"))))
      (define
       ((name . "form-listbox-attlist"))
       (interleave nil
		   (ref
		    ((name . "form-control-attlist")))
		   (ref
		    ((name . "common-disabled-attlist")))
		   (ref
		    ((name . "dropdown")))
		   (ref
		    ((name . "common-printable-attlist")))
		   (ref
		    ((name . "size")))
		   (ref
		    ((name . "common-tab-attlist")))
		   (ref
		    ((name . "common-title-attlist")))
		   (ref
		    ((name . "bound-column")))
		   (ref
		    ((name . "common-data-field-attlist")))
		   (ref
		    ((name . "list-source")))
		   (ref
		    ((name . "list-source-type")))
		   (ref
		    ((name . "common-linked-cell")))
		   (ref
		    ((name . "list-linkage-type")))
		   (ref
		    ((name . "common-source-cell-range")))
		   (optional nil
			     (attribute
			      ((name . "form:multiple"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "form:xforms-list-source"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "list-linkage-type"))
       (optional nil
		 (attribute
		  ((name . "form:list-linkage-type"))
		  (choice nil
			  (value nil "selection")
			  (value nil "selection-indices")))))
      (define
       ((name . "form-option"))
       (element
	((name . "form:option"))
	(ref
	 ((name . "form-option-attlist")))
	(text nil)))
      (define
       ((name . "form-option-attlist"))
       (ref
	((name . "current-selected")))
       (ref
	((name . "selected")))
       (ref
	((name . "label")))
       (ref
	((name . "common-value-attlist"))))
      (define
       ((name . "form-button-attlist"))
       (interleave nil
		   (ref
		    ((name . "form-control-attlist")))
		   (ref
		    ((name . "button-type")))
		   (ref
		    ((name . "common-disabled-attlist")))
		   (ref
		    ((name . "label")))
		   (ref
		    ((name . "image-data")))
		   (ref
		    ((name . "common-printable-attlist")))
		   (ref
		    ((name . "common-tab-attlist")))
		   (ref
		    ((name . "target-frame")))
		   (ref
		    ((name . "target-location")))
		   (ref
		    ((name . "common-title-attlist")))
		   (ref
		    ((name . "common-value-attlist")))
		   (ref
		    ((name . "common-form-relative-image-position-attlist")))
		   (ref
		    ((name . "common-repeat")))
		   (ref
		    ((name . "common-delay-for-repeat")))
		   (optional nil
			     (attribute
			      ((name . "form:default-button"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "form:toggle"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "form:focus-on-click"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "form:xforms-submission"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "form-image-attlist"))
       (ref
	((name . "form-control-attlist")))
       (ref
	((name . "button-type")))
       (ref
	((name . "common-disabled-attlist")))
       (ref
	((name . "image-data")))
       (ref
	((name . "common-printable-attlist")))
       (ref
	((name . "common-tab-attlist")))
       (ref
	((name . "target-frame")))
       (ref
	((name . "target-location")))
       (ref
	((name . "common-title-attlist")))
       (ref
	((name . "common-value-attlist"))))
      (define
       ((name . "form-checkbox-attlist"))
       (interleave nil
		   (ref
		    ((name . "form-control-attlist")))
		   (ref
		    ((name . "common-disabled-attlist")))
		   (ref
		    ((name . "label")))
		   (ref
		    ((name . "common-printable-attlist")))
		   (ref
		    ((name . "common-tab-attlist")))
		   (ref
		    ((name . "common-title-attlist")))
		   (ref
		    ((name . "common-value-attlist")))
		   (ref
		    ((name . "common-data-field-attlist")))
		   (ref
		    ((name . "common-form-visual-effect-attlist")))
		   (ref
		    ((name . "common-form-relative-image-position-attlist")))
		   (ref
		    ((name . "common-linked-cell")))
		   (optional nil
			     (attribute
			      ((name . "form:current-state"))
			      (ref
			       ((name . "states")))))
		   (optional nil
			     (attribute
			      ((name . "form:is-tristate"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "form:state"))
			      (ref
			       ((name . "states")))))))
      (define
       ((name . "states"))
       (choice nil
	       (value nil "unchecked")
	       (value nil "checked")
	       (value nil "unknown")))
      (define
       ((name . "form-radio-attlist"))
       (ref
	((name . "form-control-attlist")))
       (ref
	((name . "current-selected")))
       (ref
	((name . "common-disabled-attlist")))
       (ref
	((name . "label")))
       (ref
	((name . "common-printable-attlist")))
       (ref
	((name . "selected")))
       (ref
	((name . "common-tab-attlist")))
       (ref
	((name . "common-title-attlist")))
       (ref
	((name . "common-value-attlist")))
       (ref
	((name . "common-data-field-attlist")))
       (ref
	((name . "common-form-visual-effect-attlist")))
       (ref
	((name . "common-form-relative-image-position-attlist")))
       (ref
	((name . "common-linked-cell"))))
      (define
       ((name . "form-frame-attlist"))
       (ref
	((name . "form-control-attlist")))
       (ref
	((name . "common-disabled-attlist")))
       (ref
	((name . "for")))
       (ref
	((name . "label")))
       (ref
	((name . "common-printable-attlist")))
       (ref
	((name . "common-title-attlist"))))
      (define
       ((name . "form-image-frame-attlist"))
       (ref
	((name . "form-control-attlist")))
       (ref
	((name . "common-disabled-attlist")))
       (ref
	((name . "image-data")))
       (ref
	((name . "common-printable-attlist")))
       (ref
	((name . "common-readonly-attlist")))
       (ref
	((name . "common-title-attlist")))
       (ref
	((name . "common-data-field-attlist"))))
      (define
       ((name . "form-hidden-attlist"))
       (ref
	((name . "form-control-attlist")))
       (ref
	((name . "common-value-attlist"))))
      (define
       ((name . "form-grid-attlist"))
       (ref
	((name . "form-control-attlist")))
       (ref
	((name . "common-disabled-attlist")))
       (ref
	((name . "common-printable-attlist")))
       (ref
	((name . "common-tab-attlist")))
       (ref
	((name . "common-title-attlist"))))
      (define
       ((name . "form-column"))
       (element
	((name . "form:column"))
	(ref
	 ((name . "form-column-attlist")))
	(oneOrMore nil
		   (ref
		    ((name . "column-controls"))))))
      (define
       ((name . "form-column-attlist"))
       (ref
	((name . "common-form-control-attlist")))
       (ref
	((name . "label")))
       (ref
	((name . "text-style-name"))))
      (define
       ((name . "text-style-name"))
       (optional nil
		 (attribute
		  ((name . "form:text-style-name"))
		  (ref
		   ((name . "styleNameRef"))))))
      (define
       ((name . "form-value-range-attlist"))
       (interleave nil
		   (ref
		    ((name . "form-control-attlist")))
		   (ref
		    ((name . "common-disabled-attlist")))
		   (ref
		    ((name . "common-printable-attlist")))
		   (ref
		    ((name . "common-tab-attlist")))
		   (ref
		    ((name . "common-title-attlist")))
		   (ref
		    ((name . "common-value-attlist")))
		   (ref
		    ((name . "common-linked-cell")))
		   (ref
		    ((name . "common-repeat")))
		   (ref
		    ((name . "common-delay-for-repeat")))
		   (optional nil
			     (attribute
			      ((name . "form:max-value"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "form:min-value"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "form:step-size"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "form:page-step-size"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "form:orientation"))
			      (choice nil
				      (value nil "horizontal")
				      (value nil "vertical"))))))
      (define
       ((name . "form-generic-control-attlist"))
       (ref
	((name . "form-control-attlist"))))
      (define
       ((name . "common-form-control-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "form:name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "form:control-implementation"))
			      (ref
			       ((name . "namespacedToken")))))))
      (define
       ((name . "xforms-bind-attlist"))
       (optional nil
		 (attribute
		  ((name . "xforms:bind"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "types"))
       (choice nil
	       (value nil "submit")
	       (value nil "reset")
	       (value nil "push")
	       (value nil "url")))
      (define
       ((name . "button-type"))
       (optional nil
		 (attribute
		  ((name . "form:button-type"))
		  (ref
		   ((name . "types"))))))
      (define
       ((name . "common-control-id-attlist"))
       (group nil
	      (ref
	       ((name . "xml-id")))
	      (optional nil
			(attribute
			 ((name . "form:id"))
			 (ref
			  ((name . "NCName")))))))
      (define
       ((name . "current-selected"))
       (optional nil
		 (attribute
		  ((name . "form:current-selected"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "common-value-attlist"))
       (optional nil
		 (attribute
		  ((name . "form:value"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "common-current-value-attlist"))
       (optional nil
		 (attribute
		  ((name . "form:current-value"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "common-disabled-attlist"))
       (optional nil
		 (attribute
		  ((name . "form:disabled"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "dropdown"))
       (optional nil
		 (attribute
		  ((name . "form:dropdown"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "for"))
       (optional nil
		 (attribute
		  ((name . "form:for"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "image-data"))
       (optional nil
		 (attribute
		  ((name . "form:image-data"))
		  (ref
		   ((name . "anyIRI"))))))
      (define
       ((name . "label"))
       (optional nil
		 (attribute
		  ((name . "form:label"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "common-maxlength-attlist"))
       (optional nil
		 (attribute
		  ((name . "form:max-length"))
		  (ref
		   ((name . "nonNegativeInteger"))))))
      (define
       ((name . "common-printable-attlist"))
       (optional nil
		 (attribute
		  ((name . "form:printable"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "common-readonly-attlist"))
       (optional nil
		 (attribute
		  ((name . "form:readonly"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "selected"))
       (optional nil
		 (attribute
		  ((name . "form:selected"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "size"))
       (optional nil
		 (attribute
		  ((name . "form:size"))
		  (ref
		   ((name . "nonNegativeInteger"))))))
      (define
       ((name . "common-tab-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "form:tab-index"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (optional nil
			     (attribute
			      ((name . "form:tab-stop"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "target-frame"))
       (optional nil
		 (attribute
		  ((name . "office:target-frame"))
		  (ref
		   ((name . "targetFrameName"))))))
      (define
       ((name . "target-location"))
       (optional nil
		 (attribute
		  ((name . "xlink:href"))
		  (ref
		   ((name . "anyIRI"))))))
      (define
       ((name . "common-title-attlist"))
       (optional nil
		 (attribute
		  ((name . "form:title"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "common-form-visual-effect-attlist"))
       (optional nil
		 (attribute
		  ((name . "form:visual-effect"))
		  (choice nil
			  (value nil "flat")
			  (value nil "3d")))))
      (define
       ((name . "common-form-relative-image-position-attlist"))
       (choice nil
	       (optional nil
			 (attribute
			  ((name . "form:image-position"))
			  (value nil "center")))
	       (group nil
		      (attribute
		       ((name . "form:image-position"))
		       (choice nil
			       (value nil "start")
			       (value nil "end")
			       (value nil "top")
			       (value nil "bottom")))
		      (optional nil
				(attribute
				 ((name . "form:image-align"))
				 (choice nil
					 (value nil "start")
					 (value nil "center")
					 (value nil "end")))))))
      (define
       ((name . "bound-column"))
       (optional nil
		 (attribute
		  ((name . "form:bound-column"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "common-convert-empty-attlist"))
       (optional nil
		 (attribute
		  ((name . "form:convert-empty-to-null"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "common-data-field-attlist"))
       (optional nil
		 (attribute
		  ((name . "form:data-field"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "list-source"))
       (optional nil
		 (attribute
		  ((name . "form:list-source"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "list-source-type"))
       (optional nil
		 (attribute
		  ((name . "form:list-source-type"))
		  (choice nil
			  (value nil "table")
			  (value nil "query")
			  (value nil "sql")
			  (value nil "sql-pass-through")
			  (value nil "value-list")
			  (value nil "table-fields")))))
      (define
       ((name . "common-linked-cell"))
       (optional nil
		 (attribute
		  ((name . "form:linked-cell"))
		  (choice nil
			  (ref
			   ((name . "cellAddress")))
			  (ref
			   ((name . "string")))))))
      (define
       ((name . "common-source-cell-range"))
       (optional nil
		 (attribute
		  ((name . "form:source-cell-range"))
		  (choice nil
			  (ref
			   ((name . "cellRangeAddress")))
			  (ref
			   ((name . "string")))))))
      (define
       ((name . "common-spin-button"))
       (optional nil
		 (attribute
		  ((name . "form:spin-button"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "common-repeat"))
       (optional nil
		 (attribute
		  ((name . "form:repeat"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "common-delay-for-repeat"))
       (optional nil
		 (attribute
		  ((name . "form:delay-for-repeat"))
		  (ref
		   ((name . "duration"))))))
      (define
       ((name . "form-properties"))
       (element
	((name . "form:properties"))
	(oneOrMore nil
		   (ref
		    ((name . "form-property"))))))
      (define
       ((name . "form-property"))
       (choice nil
	       (element
		((name . "form:property"))
		(ref
		 ((name . "form-property-name")))
		(ref
		 ((name . "form-property-value-and-type-attlist"))))
	       (element
		((name . "form:list-property"))
		(ref
		 ((name . "form-property-name")))
		(ref
		 ((name . "form-property-type-and-value-list"))))))
      (define
       ((name . "form-property-name"))
       (attribute
	((name . "form:property-name"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "form-property-value-and-type-attlist"))
       (choice nil
	       (ref
		((name . "common-value-and-type-attlist")))
	       (attribute
		((name . "office:value-type"))
		(value nil "void"))))
      (define
       ((name . "form-property-type-and-value-list"))
       (choice nil
	       (group nil
		      (attribute
		       ((name . "office:value-type"))
		       (value nil "float"))
		      (zeroOrMore nil
				  (element
				   ((name . "form:list-value"))
				   (attribute
				    ((name . "office:value"))
				    (ref
				     ((name . "double")))))))
	       (group nil
		      (attribute
		       ((name . "office:value-type"))
		       (value nil "percentage"))
		      (zeroOrMore nil
				  (element
				   ((name . "form:list-value"))
				   (attribute
				    ((name . "office:value"))
				    (ref
				     ((name . "double")))))))
	       (group nil
		      (attribute
		       ((name . "office:value-type"))
		       (value nil "currency"))
		      (zeroOrMore nil
				  (element
				   ((name . "form:list-value"))
				   (attribute
				    ((name . "office:value"))
				    (ref
				     ((name . "double"))))
				   (optional nil
					     (attribute
					      ((name . "office:currency"))
					      (ref
					       ((name . "string"))))))))
	       (group nil
		      (attribute
		       ((name . "office:value-type"))
		       (value nil "date"))
		      (zeroOrMore nil
				  (element
				   ((name . "form:list-value"))
				   (attribute
				    ((name . "office:date-value"))
				    (ref
				     ((name . "dateOrDateTime")))))))
	       (group nil
		      (attribute
		       ((name . "office:value-type"))
		       (value nil "time"))
		      (zeroOrMore nil
				  (element
				   ((name . "form:list-value"))
				   (attribute
				    ((name . "office:time-value"))
				    (ref
				     ((name . "duration")))))))
	       (group nil
		      (attribute
		       ((name . "office:value-type"))
		       (value nil "boolean"))
		      (zeroOrMore nil
				  (element
				   ((name . "form:list-value"))
				   (attribute
				    ((name . "office:boolean-value"))
				    (ref
				     ((name . "boolean")))))))
	       (group nil
		      (attribute
		       ((name . "office:value-type"))
		       (value nil "string"))
		      (zeroOrMore nil
				  (element
				   ((name . "form:list-value"))
				   (attribute
				    ((name . "office:string-value"))
				    (ref
				     ((name . "string")))))))
	       (attribute
		((name . "office:value-type"))
		(value nil "void"))))
      (define
       ((name . "office-annotation"))
       (element
	((name . "office:annotation"))
	(ref
	 ((name . "office-annotation-attlist")))
	(ref
	 ((name . "draw-caption-attlist")))
	(ref
	 ((name . "common-draw-position-attlist")))
	(ref
	 ((name . "common-draw-size-attlist")))
	(ref
	 ((name . "common-draw-shape-with-text-and-styles-attlist")))
	(optional nil
		  (ref
		   ((name . "dc-creator"))))
	(optional nil
		  (ref
		   ((name . "dc-date"))))
	(optional nil
		  (ref
		   ((name . "meta-date-string"))))
	(zeroOrMore nil
		    (choice nil
			    (ref
			     ((name . "text-p")))
			    (ref
			     ((name . "text-list")))))))
      (define
       ((name . "office-annotation-end"))
       (element
	((name . "office:annotation-end"))
	(ref
	 ((name . "office-annotation-end-attlist")))))
      (define
       ((name . "office-annotation-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "office:display"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (ref
			      ((name . "common-office-annotation-name-attlist"))))))
      (define
       ((name . "office-annotation-end-attlist"))
       (ref
	((name . "common-office-annotation-name-attlist"))))
      (define
       ((name . "common-office-annotation-name-attlist"))
       (attribute
	((name . "office:name"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "meta-date-string"))
       (element
	((name . "meta:date-string"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "common-num-format-prefix-suffix-attlist"))
       (optional nil
		 (attribute
		  ((name . "style:num-prefix"))
		  (ref
		   ((name . "string")))))
       (optional nil
		 (attribute
		  ((name . "style:num-suffix"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "common-num-format-attlist"))
       (choice nil
	       (attribute
		((name . "style:num-format"))
		(choice nil
			(value nil "1")
			(value nil "i")
			(value nil "I")
			(ref
			 ((name . "string")))
			(empty nil)))
	       (group nil
		      (attribute
		       ((name . "style:num-format"))
		       (choice nil
			       (value nil "a")
			       (value nil "A")))
		      (ref
		       ((name . "style-num-letter-sync-attlist"))))
	       (empty nil)))
      (define
       ((name . "style-num-letter-sync-attlist"))
       (optional nil
		 (attribute
		  ((name . "style:num-letter-sync"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "office-change-info"))
       (element
	((name . "office:change-info"))
	(ref
	 ((name . "dc-creator")))
	(ref
	 ((name . "dc-date")))
	(zeroOrMore nil
		    (ref
		     ((name . "text-p"))))))
      (define
       ((name . "office-event-listeners"))
       (element
	((name . "office:event-listeners"))
	(zeroOrMore nil
		    (choice nil
			    (ref
			     ((name . "script-event-listener")))
			    (ref
			     ((name . "presentation-event-listener")))))))
      (define
       ((name . "script-event-listener"))
       (element
	((name . "script:event-listener"))
	(ref
	 ((name . "script-event-listener-attlist")))
	(empty nil)))
      (define
       ((name . "script-event-listener-attlist"))
       (interleave nil
		   (attribute
		    ((name . "script:event-name"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "script:language"))
		    (ref
		     ((name . "string"))))
		   (choice nil
			   (attribute
			    ((name . "script:macro-name"))
			    (ref
			     ((name . "string"))))
			   (group nil
				  (attribute
				   ((name . "xlink:type"))
				   (value nil "simple"))
				  (attribute
				   ((name . "xlink:href"))
				   (ref
				    ((name . "anyIRI"))))
				  (optional nil
					    (attribute
					     ((name . "xlink:actuate"))
					     (value nil "onRequest")))))))
      (define
       ((name . "math-math"))
       (element
	((name . "math:math"))
	(ref
	 ((name . "mathMarkup")))))
      (define
       ((name . "mathMarkup"))
       (description nil "To avoid inclusion of the complete MathML schema, anything is allowed within a math:math top-level element")
       (zeroOrMore nil
		   (choice nil
			   (attribute nil
				      (anyName nil))
			   (text nil)
			   (element nil
				    (anyName nil)
				    (ref
				     ((name . "mathMarkup")))))))
      (define
       ((name . "text-dde-connection-decl"))
       (element
	((name . "text:dde-connection-decl"))
	(ref
	 ((name . "text-dde-connection-decl-attlist")))
	(ref
	 ((name . "common-dde-connection-decl-attlist")))))
      (define
       ((name . "text-dde-connection-decl-attlist"))
       (attribute
	((name . "office:name"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "common-dde-connection-decl-attlist"))
       (interleave nil
		   (attribute
		    ((name . "office:dde-application"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "office:dde-topic"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "office:dde-item"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "office:automatic-update"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "table-dde-link"))
       (element
	((name . "table:dde-link"))
	(ref
	 ((name . "office-dde-source")))
	(ref
	 ((name . "table-table")))))
      (define
       ((name . "office-dde-source"))
       (element
	((name . "office:dde-source"))
	(ref
	 ((name . "office-dde-source-attlist")))
	(ref
	 ((name . "common-dde-connection-decl-attlist")))))
      (define
       ((name . "office-dde-source-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "office:name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "office:conversion-mode"))
			      (choice nil
				      (value nil "into-default-style-data-style")
				      (value nil "into-english-number")
				      (value nil "keep-text"))))))
      (define
       ((name . "animation-element"))
       (choice nil
	       (element
		((name . "anim:animate"))
		(ref
		 ((name . "common-anim-target-attlist")))
		(ref
		 ((name . "common-anim-named-target-attlist")))
		(ref
		 ((name . "common-anim-values-attlist")))
		(ref
		 ((name . "common-anim-spline-mode-attlist")))
		(ref
		 ((name . "common-spline-anim-value-attlist")))
		(ref
		 ((name . "common-timing-attlist")))
		(ref
		 ((name . "common-anim-add-accum-attlist"))))
	       (element
		((name . "anim:set"))
		(ref
		 ((name . "common-anim-target-attlist")))
		(ref
		 ((name . "common-anim-named-target-attlist")))
		(ref
		 ((name . "common-anim-set-values-attlist")))
		(ref
		 ((name . "common-timing-attlist")))
		(ref
		 ((name . "common-anim-add-accum-attlist"))))
	       (element
		((name . "anim:animateMotion"))
		(ref
		 ((name . "anim-animate-motion-attlist")))
		(ref
		 ((name . "common-anim-target-attlist")))
		(ref
		 ((name . "common-anim-named-target-attlist")))
		(ref
		 ((name . "common-anim-add-accum-attlist")))
		(ref
		 ((name . "common-anim-values-attlist")))
		(ref
		 ((name . "common-timing-attlist")))
		(ref
		 ((name . "common-spline-anim-value-attlist"))))
	       (element
		((name . "anim:animateColor"))
		(ref
		 ((name . "common-anim-target-attlist")))
		(ref
		 ((name . "common-anim-named-target-attlist")))
		(ref
		 ((name . "common-anim-add-accum-attlist")))
		(ref
		 ((name . "common-anim-values-attlist")))
		(ref
		 ((name . "common-anim-spline-mode-attlist")))
		(ref
		 ((name . "common-spline-anim-value-attlist")))
		(ref
		 ((name . "anim-animate-color-attlist")))
		(ref
		 ((name . "common-timing-attlist"))))
	       (element
		((name . "anim:animateTransform"))
		(ref
		 ((name . "common-anim-target-attlist")))
		(ref
		 ((name . "common-anim-named-target-attlist")))
		(ref
		 ((name . "common-anim-add-accum-attlist")))
		(ref
		 ((name . "common-anim-values-attlist")))
		(ref
		 ((name . "anim-animate-transform-attlist")))
		(ref
		 ((name . "common-timing-attlist"))))
	       (element
		((name . "anim:transitionFilter"))
		(ref
		 ((name . "common-anim-target-attlist")))
		(ref
		 ((name . "common-anim-add-accum-attlist")))
		(ref
		 ((name . "common-anim-values-attlist")))
		(ref
		 ((name . "common-anim-spline-mode-attlist")))
		(ref
		 ((name . "anim-transition-filter-attlist")))
		(ref
		 ((name . "common-timing-attlist"))))
	       (element
		((name . "anim:par"))
		(ref
		 ((name . "common-anim-attlist")))
		(ref
		 ((name . "common-timing-attlist")))
		(ref
		 ((name . "common-endsync-timing-attlist")))
		(zeroOrMore nil
			    (ref
			     ((name . "animation-element")))))
	       (element
		((name . "anim:seq"))
		(ref
		 ((name . "common-anim-attlist")))
		(ref
		 ((name . "common-endsync-timing-attlist")))
		(ref
		 ((name . "common-timing-attlist")))
		(zeroOrMore nil
			    (ref
			     ((name . "animation-element")))))
	       (element
		((name . "anim:iterate"))
		(ref
		 ((name . "common-anim-attlist")))
		(ref
		 ((name . "anim-iterate-attlist")))
		(ref
		 ((name . "common-timing-attlist")))
		(ref
		 ((name . "common-endsync-timing-attlist")))
		(zeroOrMore nil
			    (ref
			     ((name . "animation-element")))))
	       (element
		((name . "anim:audio"))
		(ref
		 ((name . "common-anim-attlist")))
		(ref
		 ((name . "anim-audio-attlist")))
		(ref
		 ((name . "common-basic-timing-attlist"))))
	       (element
		((name . "anim:command"))
		(ref
		 ((name . "common-anim-attlist")))
		(ref
		 ((name . "anim-command-attlist")))
		(ref
		 ((name . "common-begin-end-timing-attlist")))
		(ref
		 ((name . "common-anim-target-attlist")))
		(zeroOrMore nil
			    (element
			     ((name . "anim:param"))
			     (attribute
			      ((name . "anim:name"))
			      (ref
			       ((name . "string"))))
			     (attribute
			      ((name . "anim:value"))
			      (ref
			       ((name . "string")))))))))
      (define
       ((name . "anim-animate-motion-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "svg:path"))
			      (ref
			       ((name . "pathData")))))
		   (optional nil
			     (attribute
			      ((name . "svg:origin"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "smil:calcMode"))
			      (choice nil
				      (value nil "discrete")
				      (value nil "linear")
				      (value nil "paced")
				      (value nil "spline"))))))
      (define
       ((name . "anim-animate-color-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "anim:color-interpolation"))
			      (choice nil
				      (value nil "rgb")
				      (value nil "hsl"))))
		   (optional nil
			     (attribute
			      ((name . "anim:color-interpolation-direction"))
			      (choice nil
				      (value nil "clockwise")
				      (value nil "counter-clockwise"))))))
      (define
       ((name . "anim-animate-transform-attlist"))
       (attribute
	((name . "svg:type"))
	(choice nil
		(value nil "translate")
		(value nil "scale")
		(value nil "rotate")
		(value nil "skewX")
		(value nil "skewY"))))
      (define
       ((name . "anim-transition-filter-attlist"))
       (interleave nil
		   (attribute
		    ((name . "smil:type"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "smil:subtype"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "smil:direction"))
			      (choice nil
				      (value nil "forward")
				      (value nil "reverse"))))
		   (optional nil
			     (attribute
			      ((name . "smil:fadeColor"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "smil:mode"))
			      (choice nil
				      (value nil "in")
				      (value nil "out"))))))
      (define
       ((name . "common-anim-target-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "smil:targetElement"))
			      (ref
			       ((name . "IDREF")))))
		   (optional nil
			     (attribute
			      ((name . "anim:sub-item"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "common-anim-named-target-attlist"))
       (attribute
	((name . "smil:attributeName"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "common-anim-values-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "smil:values"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "anim:formula"))
			      (ref
			       ((name . "string")))))
		   (ref
		    ((name . "common-anim-set-values-attlist")))
		   (optional nil
			     (attribute
			      ((name . "smil:from"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "smil:by"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "common-anim-spline-mode-attlist"))
       (optional nil
		 (attribute
		  ((name . "smil:calcMode"))
		  (choice nil
			  (value nil "discrete")
			  (value nil "linear")
			  (value nil "paced")
			  (value nil "spline")))))
      (define
       ((name . "common-spline-anim-value-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "smil:keyTimes"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "smil:keySplines"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "common-anim-add-accum-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "smil:accumulate"))
			      (choice nil
				      (value nil "none")
				      (value nil "sum"))))
		   (optional nil
			     (attribute
			      ((name . "smil:additive"))
			      (choice nil
				      (value nil "replace")
				      (value nil "sum"))))))
      (define
       ((name . "common-anim-set-values-attlist"))
       (optional nil
		 (attribute
		  ((name . "smil:to"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "common-begin-end-timing-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "smil:begin"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "smil:end"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "common-dur-timing-attlist"))
       (optional nil
		 (attribute
		  ((name . "smil:dur"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "common-endsync-timing-attlist"))
       (optional nil
		 (attribute
		  ((name . "smil:endsync"))
		  (choice nil
			  (value nil "first")
			  (value nil "last")
			  (value nil "all")
			  (value nil "media")
			  (ref
			   ((name . "IDREF")))))))
      (define
       ((name . "common-repeat-timing-attlist"))
       (optional nil
		 (attribute
		  ((name . "smil:repeatDur"))
		  (ref
		   ((name . "string")))))
       (optional nil
		 (attribute
		  ((name . "smil:repeatCount"))
		  (choice nil
			  (ref
			   ((name . "nonNegativeDecimal")))
			  (value nil "indefinite")))))
      (define
       ((name . "nonNegativeDecimal"))
       (data
	((type . "decimal"))
	(param
	 ((name . "minInclusive"))
	 "0.0")))
      (define
       ((name . "common-fill-timing-attlist"))
       (optional nil
		 (attribute
		  ((name . "smil:fill"))
		  (choice nil
			  (value nil "remove")
			  (value nil "freeze")
			  (value nil "hold")
			  (value nil "auto")
			  (value nil "default")
			  (value nil "transition")))))
      (define
       ((name . "common-fill-default-attlist"))
       (optional nil
		 (attribute
		  ((name . "smil:fillDefault"))
		  (choice nil
			  (value nil "remove")
			  (value nil "freeze")
			  (value nil "hold")
			  (value nil "transition")
			  (value nil "auto")
			  (value nil "inherit")))))
      (define
       ((name . "common-restart-timing-attlist"))
       (optional nil
		 (attribute
		  ((name . "smil:restart"))
		  (choice nil
			  (value nil "never")
			  (value nil "always")
			  (value nil "whenNotActive")
			  (value nil "default")))))
      (define
       ((name . "common-restart-default-attlist"))
       (optional nil
		 (attribute
		  ((name . "smil:restartDefault"))
		  (choice nil
			  (value nil "never")
			  (value nil "always")
			  (value nil "whenNotActive")
			  (value nil "inherit")))))
      (define
       ((name . "common-time-manip-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "smil:accelerate"))
			      (ref
			       ((name . "zeroToOneDecimal")))))
		   (optional nil
			     (attribute
			      ((name . "smil:decelerate"))
			      (ref
			       ((name . "zeroToOneDecimal")))))
		   (optional nil
			     (attribute
			      ((name . "smil:autoReverse"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "zeroToOneDecimal"))
       (data
	((type . "decimal"))
	(param
	 ((name . "minInclusive"))
	 "0")
	(param
	 ((name . "maxInclusive"))
	 "1")))
      (define
       ((name . "common-basic-timing-attlist"))
       (ref
	((name . "common-begin-end-timing-attlist")))
       (ref
	((name . "common-dur-timing-attlist")))
       (ref
	((name . "common-repeat-timing-attlist")))
       (ref
	((name . "common-restart-timing-attlist")))
       (ref
	((name . "common-restart-default-attlist")))
       (ref
	((name . "common-fill-timing-attlist")))
       (ref
	((name . "common-fill-default-attlist"))))
      (define
       ((name . "common-timing-attlist"))
       (ref
	((name . "common-basic-timing-attlist")))
       (ref
	((name . "common-time-manip-attlist"))))
      (define
       ((name . "anim-iterate-attlist"))
       (interleave nil
		   (ref
		    ((name . "common-anim-target-attlist")))
		   (optional nil
			     (attribute
			      ((name . "anim:iterate-type"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "anim:iterate-interval"))
			      (ref
			       ((name . "duration")))))))
      (define
       ((name . "anim-audio-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "xlink:href"))
			      (ref
			       ((name . "anyIRI")))))
		   (optional nil
			     (attribute
			      ((name . "anim:audio-level"))
			      (ref
			       ((name . "double")))))))
      (define
       ((name . "anim-command-attlist"))
       (attribute
	((name . "anim:command"))
	(ref
	 ((name . "string")))))
      (define
       ((name . "style-style"))
       (element
	((name . "style:style"))
	(ref
	 ((name . "style-style-attlist")))
	(ref
	 ((name . "style-style-content")))
	(zeroOrMore nil
		    (ref
		     ((name . "style-map"))))))
      (define
       ((name . "common-in-content-meta-attlist"))
       (attribute
	((name . "xhtml:about"))
	(ref
	 ((name . "URIorSafeCURIE"))))
       (attribute
	((name . "xhtml:property"))
	(ref
	 ((name . "CURIEs"))))
       (ref
	((name . "common-meta-literal-attlist"))))
      (define
       ((name . "common-meta-literal-attlist"))
       (optional nil
		 (attribute
		  ((name . "xhtml:datatype"))
		  (ref
		   ((name . "CURIE")))))
       (optional nil
		 (attribute
		  ((name . "xhtml:content"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "xml-id"))
       (attribute
	((name . "xml:id"))
	(ref
	 ((name . "ID")))))
      (define
       ((name . "style-style-attlist"))
       (interleave nil
		   (attribute
		    ((name . "style:name"))
		    (ref
		     ((name . "styleName"))))
		   (optional nil
			     (attribute
			      ((name . "style:display-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "style:parent-style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "style:next-style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "style:list-level"))
			      (choice nil
				      (ref
				       ((name . "positiveInteger")))
				      (empty nil))))
		   (optional nil
			     (attribute
			      ((name . "style:list-style-name"))
			      (choice nil
				      (ref
				       ((name . "styleName")))
				      (empty nil))))
		   (optional nil
			     (attribute
			      ((name . "style:master-page-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "style:auto-update"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "style:data-style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "style:percentage-data-style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "style:class"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "style:default-outline-level"))
			      (choice nil
				      (ref
				       ((name . "positiveInteger")))
				      (empty nil))))))
      (define
       ((name . "style-map"))
       (element
	((name . "style:map"))
	(ref
	 ((name . "style-map-attlist")))
	(empty nil)))
      (define
       ((name . "style-map-attlist"))
       (interleave nil
		   (attribute
		    ((name . "style:condition"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "style:apply-style-name"))
		    (ref
		     ((name . "styleNameRef"))))
		   (optional nil
			     (attribute
			      ((name . "style:base-cell-address"))
			      (ref
			       ((name . "cellAddress")))))))
      (define
       ((name . "style-default-style"))
       (element
	((name . "style:default-style"))
	(ref
	 ((name . "style-style-content")))))
      (define
       ((name . "style-page-layout"))
       (element
	((name . "style:page-layout"))
	(ref
	 ((name . "style-page-layout-attlist")))
	(ref
	 ((name . "style-page-layout-content")))))
      (define
       ((name . "style-page-layout-content"))
       (optional nil
		 (ref
		  ((name . "style-page-layout-properties"))))
       (optional nil
		 (ref
		  ((name . "style-header-style"))))
       (optional nil
		 (ref
		  ((name . "style-footer-style")))))
      (define
       ((name . "style-page-layout-attlist"))
       (interleave nil
		   (attribute
		    ((name . "style:name"))
		    (ref
		     ((name . "styleName"))))
		   (optional nil
			     (attribute
			      ((name . "style:page-usage"))
			      (choice nil
				      (value nil "all")
				      (value nil "left")
				      (value nil "right")
				      (value nil "mirrored"))))))
      (define
       ((name . "style-header-style"))
       (element
	((name . "style:header-style"))
	(optional nil
		  (ref
		   ((name . "style-header-footer-properties"))))))
      (define
       ((name . "style-footer-style"))
       (element
	((name . "style:footer-style"))
	(optional nil
		  (ref
		   ((name . "style-header-footer-properties"))))))
      (define
       ((name . "style-default-page-layout"))
       (element
	((name . "style:default-page-layout"))
	(ref
	 ((name . "style-page-layout-content")))))
      (define
       ((name . "style-master-page"))
       (element
	((name . "style:master-page"))
	(ref
	 ((name . "style-master-page-attlist")))
	(optional nil
		  (ref
		   ((name . "style-header")))
		  (optional nil
			    (ref
			     ((name . "style-header-left")))))
	(optional nil
		  (ref
		   ((name . "style-footer")))
		  (optional nil
			    (ref
			     ((name . "style-footer-left")))))
	(optional nil
		  (ref
		   ((name . "draw-layer-set"))))
	(optional nil
		  (ref
		   ((name . "office-forms"))))
	(zeroOrMore nil
		    (ref
		     ((name . "shape"))))
	(optional nil
		  (ref
		   ((name . "animation-element"))))
	(optional nil
		  (ref
		   ((name . "presentation-notes"))))))
      (define
       ((name . "style-master-page-attlist"))
       (interleave nil
		   (attribute
		    ((name . "style:name"))
		    (ref
		     ((name . "styleName"))))
		   (optional nil
			     (attribute
			      ((name . "style:display-name"))
			      (ref
			       ((name . "string")))))
		   (attribute
		    ((name . "style:page-layout-name"))
		    (ref
		     ((name . "styleNameRef"))))
		   (optional nil
			     (attribute
			      ((name . "draw:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "style:next-style-name"))
			      (ref
			       ((name . "styleNameRef")))))))
      (define
       ((name . "style-header"))
       (element
	((name . "style:header"))
	(ref
	 ((name . "common-style-header-footer-attlist")))
	(ref
	 ((name . "header-footer-content")))))
      (define
       ((name . "style-footer"))
       (element
	((name . "style:footer"))
	(ref
	 ((name . "common-style-header-footer-attlist")))
	(ref
	 ((name . "header-footer-content")))))
      (define
       ((name . "style-header-left"))
       (element
	((name . "style:header-left"))
	(ref
	 ((name . "common-style-header-footer-attlist")))
	(ref
	 ((name . "header-footer-content")))))
      (define
       ((name . "style-footer-left"))
       (element
	((name . "style:footer-left"))
	(ref
	 ((name . "common-style-header-footer-attlist")))
	(ref
	 ((name . "header-footer-content")))))
      (define
       ((name . "header-footer-content"))
       (choice nil
	       (group nil
		      (ref
		       ((name . "text-tracked-changes")))
		      (ref
		       ((name . "text-decls")))
		      (zeroOrMore nil
				  (choice nil
					  (ref
					   ((name . "text-h")))
					  (ref
					   ((name . "text-p")))
					  (ref
					   ((name . "text-list")))
					  (ref
					   ((name . "table-table")))
					  (ref
					   ((name . "text-section")))
					  (ref
					   ((name . "text-table-of-content")))
					  (ref
					   ((name . "text-illustration-index")))
					  (ref
					   ((name . "text-table-index")))
					  (ref
					   ((name . "text-object-index")))
					  (ref
					   ((name . "text-user-index")))
					  (ref
					   ((name . "text-alphabetical-index")))
					  (ref
					   ((name . "text-bibliography")))
					  (ref
					   ((name . "text-index-title")))
					  (ref
					   ((name . "change-marks"))))))
	       (group nil
		      (optional nil
				(ref
				 ((name . "style-region-left"))))
		      (optional nil
				(ref
				 ((name . "style-region-center"))))
		      (optional nil
				(ref
				 ((name . "style-region-right")))))))
      (define
       ((name . "common-style-header-footer-attlist"))
       (optional nil
		 (attribute
		  ((name . "style:display"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "style-region-left"))
       (element
	((name . "style:region-left"))
	(ref
	 ((name . "region-content")))))
      (define
       ((name . "style-region-center"))
       (element
	((name . "style:region-center"))
	(ref
	 ((name . "region-content")))))
      (define
       ((name . "style-region-right"))
       (element
	((name . "style:region-right"))
	(ref
	 ((name . "region-content")))))
      (define
       ((name . "region-content"))
       (zeroOrMore nil
		   (ref
		    ((name . "text-p")))))
      (define
       ((name . "presentation-notes"))
       (element
	((name . "presentation:notes"))
	(ref
	 ((name . "common-presentation-header-footer-attlist")))
	(ref
	 ((name . "presentation-notes-attlist")))
	(ref
	 ((name . "office-forms")))
	(zeroOrMore nil
		    (ref
		     ((name . "shape"))))))
      (define
       ((name . "presentation-notes-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "style:page-layout-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "draw:style-name"))
			      (ref
			       ((name . "styleNameRef")))))))
      (define
       ((name . "table-table-template"))
       (element
	((name . "table:table-template"))
	(ref
	 ((name . "table-table-template-attlist")))
	(optional nil
		  (ref
		   ((name . "table-first-row"))))
	(optional nil
		  (ref
		   ((name . "table-last-row"))))
	(optional nil
		  (ref
		   ((name . "table-first-column"))))
	(optional nil
		  (ref
		   ((name . "table-last-column"))))
	(ref
	 ((name . "table-body")))
	(optional nil
		  (ref
		   ((name . "table-even-rows"))))
	(optional nil
		  (ref
		   ((name . "table-odd-rows"))))
	(optional nil
		  (ref
		   ((name . "table-even-columns"))))
	(optional nil
		  (ref
		   ((name . "table-odd-columns"))))
	(optional nil
		  (ref
		   ((name . "table-background"))))))
      (define
       ((name . "table-table-template-attlist"))
       (interleave nil
		   (attribute
		    ((name . "table:name"))
		    (ref
		     ((name . "string"))))
		   (attribute
		    ((name . "table:first-row-start-column"))
		    (ref
		     ((name . "rowOrCol"))))
		   (attribute
		    ((name . "table:first-row-end-column"))
		    (ref
		     ((name . "rowOrCol"))))
		   (attribute
		    ((name . "table:last-row-start-column"))
		    (ref
		     ((name . "rowOrCol"))))
		   (attribute
		    ((name . "table:last-row-end-column"))
		    (ref
		     ((name . "rowOrCol"))))))
      (define
       ((name . "rowOrCol"))
       (choice nil
	       (value nil "row")
	       (value nil "column")))
      (define
       ((name . "table-first-row"))
       (element
	((name . "table:first-row"))
	(ref
	 ((name . "common-table-template-attlist")))
	(empty nil)))
      (define
       ((name . "table-last-row"))
       (element
	((name . "table:last-row"))
	(ref
	 ((name . "common-table-template-attlist")))
	(empty nil)))
      (define
       ((name . "table-first-column"))
       (element
	((name . "table:first-column"))
	(ref
	 ((name . "common-table-template-attlist")))
	(empty nil)))
      (define
       ((name . "table-last-column"))
       (element
	((name . "table:last-column"))
	(ref
	 ((name . "common-table-template-attlist")))
	(empty nil)))
      (define
       ((name . "table-body"))
       (element
	((name . "table:body"))
	(ref
	 ((name . "common-table-template-attlist")))
	(empty nil)))
      (define
       ((name . "table-even-rows"))
       (element
	((name . "table:even-rows"))
	(ref
	 ((name . "common-table-template-attlist")))
	(empty nil)))
      (define
       ((name . "table-odd-rows"))
       (element
	((name . "table:odd-rows"))
	(ref
	 ((name . "common-table-template-attlist")))
	(empty nil)))
      (define
       ((name . "table-even-columns"))
       (element
	((name . "table:even-columns"))
	(ref
	 ((name . "common-table-template-attlist")))
	(empty nil)))
      (define
       ((name . "table-odd-columns"))
       (element
	((name . "table:odd-columns"))
	(ref
	 ((name . "common-table-template-attlist")))
	(empty nil)))
      (define
       ((name . "common-table-template-attlist"))
       (attribute
	((name . "table:style-name"))
	(ref
	 ((name . "styleNameRef"))))
       (optional nil
		 (attribute
		  ((name . "table:paragraph-style-name"))
		  (ref
		   ((name . "styleNameRef"))))))
      (define
       ((name . "table-background"))
       (element
	((name . "table:background"))
	(ref
	 ((name . "table-background-attlist")))
	(empty nil)))
      (define
       ((name . "table-background-attlist"))
       (attribute
	((name . "table:style-name"))
	(ref
	 ((name . "styleNameRef")))))
      (define
       ((name . "style-font-face"))
       (element
	((name . "style:font-face"))
	(ref
	 ((name . "style-font-face-attlist")))
	(optional nil
		  (ref
		   ((name . "svg-font-face-src"))))
	(optional nil
		  (ref
		   ((name . "svg-definition-src"))))))
      (define
       ((name . "style-font-face-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "svg:font-family"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "svg:font-style"))
			      (ref
			       ((name . "fontStyle")))))
		   (optional nil
			     (attribute
			      ((name . "svg:font-variant"))
			      (ref
			       ((name . "fontVariant")))))
		   (optional nil
			     (attribute
			      ((name . "svg:font-weight"))
			      (ref
			       ((name . "fontWeight")))))
		   (optional nil
			     (attribute
			      ((name . "svg:font-stretch"))
			      (choice nil
				      (value nil "normal")
				      (value nil "ultra-condensed")
				      (value nil "extra-condensed")
				      (value nil "condensed")
				      (value nil "semi-condensed")
				      (value nil "semi-expanded")
				      (value nil "expanded")
				      (value nil "extra-expanded")
				      (value nil "ultra-expanded"))))
		   (optional nil
			     (attribute
			      ((name . "svg:font-size"))
			      (ref
			       ((name . "positiveLength")))))
		   (optional nil
			     (attribute
			      ((name . "svg:unicode-range"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "svg:units-per-em"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:panose-1"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "svg:stemv"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:stemh"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:slope"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:cap-height"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:x-height"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:accent-height"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:ascent"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:descent"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:widths"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "svg:bbox"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "svg:ideographic"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:alphabetic"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:mathematical"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:hanging"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:v-ideographic"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:v-alphabetic"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:v-mathematical"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:v-hanging"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:underline-position"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:underline-thickness"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:strikethrough-position"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:strikethrough-thickness"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:overline-position"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "svg:overline-thickness"))
			      (ref
			       ((name . "integer")))))
		   (attribute
		    ((name . "style:name"))
		    (ref
		     ((name . "string"))))
		   (optional nil
			     (attribute
			      ((name . "style:font-adornments"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-family-generic"))
			      (ref
			       ((name . "fontFamilyGeneric")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-pitch"))
			      (ref
			       ((name . "fontPitch")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-charset"))
			      (ref
			       ((name . "textEncoding")))))))
      (define
       ((name . "svg-font-face-src"))
       (element
	((name . "svg:font-face-src"))
	(oneOrMore nil
		   (choice nil
			   (ref
			    ((name . "svg-font-face-uri")))
			   (ref
			    ((name . "svg-font-face-name")))))))
      (define
       ((name . "svg-font-face-uri"))
       (element
	((name . "svg:font-face-uri"))
	(ref
	 ((name . "common-svg-font-face-xlink-attlist")))
	(zeroOrMore nil
		    (ref
		     ((name . "svg-font-face-format"))))))
      (define
       ((name . "svg-font-face-format"))
       (element
	((name . "svg:font-face-format"))
	(optional nil
		  (attribute
		   ((name . "svg:string"))
		   (ref
		    ((name . "string")))))
	(empty nil)))
      (define
       ((name . "svg-font-face-name"))
       (element
	((name . "svg:font-face-name"))
	(optional nil
		  (attribute
		   ((name . "svg:name"))
		   (ref
		    ((name . "string")))))
	(empty nil)))
      (define
       ((name . "svg-definition-src"))
       (element
	((name . "svg:definition-src"))
	(ref
	 ((name . "common-svg-font-face-xlink-attlist")))
	(empty nil)))
      (define
       ((name . "common-svg-font-face-xlink-attlist"))
       (attribute
	((name . "xlink:type"))
	(value nil "simple"))
       (attribute
	((name . "xlink:href"))
	(ref
	 ((name . "anyIRI"))))
       (optional nil
		 (attribute
		  ((name . "xlink:actuate"))
		  (value nil "onRequest"))))
      (define
       ((name . "number-number-style"))
       (element
	((name . "number:number-style"))
	(ref
	 ((name . "common-data-style-attlist")))
	(optional nil
		  (ref
		   ((name . "style-text-properties"))))
	(optional nil
		  (ref
		   ((name . "number-text"))))
	(optional nil
		  (ref
		   ((name . "any-number")))
		  (optional nil
			    (ref
			     ((name . "number-text")))))
	(zeroOrMore nil
		    (ref
		     ((name . "style-map"))))))
      (define
       ((name . "any-number"))
       (choice nil
	       (ref
		((name . "number-number")))
	       (ref
		((name . "number-scientific-number")))
	       (ref
		((name . "number-fraction")))))
      (define
       ((name . "number-number"))
       (element
	((name . "number:number"))
	(ref
	 ((name . "number-number-attlist")))
	(ref
	 ((name . "common-decimal-places-attlist")))
	(ref
	 ((name . "common-number-attlist")))
	(zeroOrMore nil
		    (ref
		     ((name . "number-embedded-text"))))))
      (define
       ((name . "number-number-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "number:decimal-replacement"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "number:display-factor"))
			      (ref
			       ((name . "double")))))))
      (define
       ((name . "number-embedded-text"))
       (element
	((name . "number:embedded-text"))
	(ref
	 ((name . "number-embedded-text-attlist")))
	(text nil)))
      (define
       ((name . "number-embedded-text-attlist"))
       (attribute
	((name . "number:position"))
	(ref
	 ((name . "integer")))))
      (define
       ((name . "number-scientific-number"))
       (element
	((name . "number:scientific-number"))
	(ref
	 ((name . "number-scientific-number-attlist")))
	(ref
	 ((name . "common-decimal-places-attlist")))
	(ref
	 ((name . "common-number-attlist")))
	(empty nil)))
      (define
       ((name . "number-scientific-number-attlist"))
       (optional nil
		 (attribute
		  ((name . "number:min-exponent-digits"))
		  (ref
		   ((name . "integer"))))))
      (define
       ((name . "number-fraction"))
       (element
	((name . "number:fraction"))
	(ref
	 ((name . "number-fraction-attlist")))
	(ref
	 ((name . "common-number-attlist")))
	(empty nil)))
      (define
       ((name . "number-fraction-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "number:min-numerator-digits"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "number:min-denominator-digits"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "number:denominator-value"))
			      (ref
			       ((name . "integer")))))))
      (define
       ((name . "number-currency-style"))
       (element
	((name . "number:currency-style"))
	(ref
	 ((name . "common-data-style-attlist")))
	(ref
	 ((name . "common-auto-reorder-attlist")))
	(optional nil
		  (ref
		   ((name . "style-text-properties"))))
	(optional nil
		  (ref
		   ((name . "number-text"))))
	(optional nil
		  (choice nil
			  (group nil
				 (ref
				  ((name . "number-and-text")))
				 (optional nil
					   (ref
					    ((name . "currency-symbol-and-text")))))
			  (group nil
				 (ref
				  ((name . "currency-symbol-and-text")))
				 (optional nil
					   (ref
					    ((name . "number-and-text")))))))
	(zeroOrMore nil
		    (ref
		     ((name . "style-map"))))))
      (define
       ((name . "currency-symbol-and-text"))
       (ref
	((name . "number-currency-symbol")))
       (optional nil
		 (ref
		  ((name . "number-text")))))
      (define
       ((name . "number-and-text"))
       (ref
	((name . "number-number")))
       (optional nil
		 (ref
		  ((name . "number-text")))))
      (define
       ((name . "number-currency-symbol"))
       (element
	((name . "number:currency-symbol"))
	(ref
	 ((name . "number-currency-symbol-attlist")))
	(text nil)))
      (define
       ((name . "number-currency-symbol-attlist"))
       (optional nil
		 (attribute
		  ((name . "number:language"))
		  (ref
		   ((name . "languageCode")))))
       (optional nil
		 (attribute
		  ((name . "number:country"))
		  (ref
		   ((name . "countryCode")))))
       (optional nil
		 (attribute
		  ((name . "number:script"))
		  (ref
		   ((name . "scriptCode")))))
       (optional nil
		 (attribute
		  ((name . "number:rfc-language-tag"))
		  (ref
		   ((name . "language"))))))
      (define
       ((name . "number-percentage-style"))
       (element
	((name . "number:percentage-style"))
	(ref
	 ((name . "common-data-style-attlist")))
	(optional nil
		  (ref
		   ((name . "style-text-properties"))))
	(optional nil
		  (ref
		   ((name . "number-text"))))
	(optional nil
		  (ref
		   ((name . "number-and-text"))))
	(zeroOrMore nil
		    (ref
		     ((name . "style-map"))))))
      (define
       ((name . "number-date-style"))
       (element
	((name . "number:date-style"))
	(ref
	 ((name . "common-data-style-attlist")))
	(ref
	 ((name . "common-auto-reorder-attlist")))
	(ref
	 ((name . "common-format-source-attlist")))
	(optional nil
		  (ref
		   ((name . "style-text-properties"))))
	(optional nil
		  (ref
		   ((name . "number-text"))))
	(oneOrMore nil
		   (ref
		    ((name . "any-date")))
		   (optional nil
			     (ref
			      ((name . "number-text")))))
	(zeroOrMore nil
		    (ref
		     ((name . "style-map"))))))
      (define
       ((name . "any-date"))
       (choice nil
	       (ref
		((name . "number-day")))
	       (ref
		((name . "number-month")))
	       (ref
		((name . "number-year")))
	       (ref
		((name . "number-era")))
	       (ref
		((name . "number-day-of-week")))
	       (ref
		((name . "number-week-of-year")))
	       (ref
		((name . "number-quarter")))
	       (ref
		((name . "number-hours")))
	       (ref
		((name . "number-am-pm")))
	       (ref
		((name . "number-minutes")))
	       (ref
		((name . "number-seconds")))))
      (define
       ((name . "number-day"))
       (element
	((name . "number:day"))
	(ref
	 ((name . "number-day-attlist")))
	(ref
	 ((name . "common-calendar-attlist")))
	(empty nil)))
      (define
       ((name . "number-day-attlist"))
       (optional nil
		 (attribute
		  ((name . "number:style"))
		  (choice nil
			  (value nil "short")
			  (value nil "long")))))
      (define
       ((name . "number-month"))
       (element
	((name . "number:month"))
	(ref
	 ((name . "number-month-attlist")))
	(ref
	 ((name . "common-calendar-attlist")))
	(empty nil)))
      (define
       ((name . "number-month-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "number:textual"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "number:possessive-form"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "number:style"))
			      (choice nil
				      (value nil "short")
				      (value nil "long"))))))
      (define
       ((name . "number-year"))
       (element
	((name . "number:year"))
	(ref
	 ((name . "number-year-attlist")))
	(ref
	 ((name . "common-calendar-attlist")))
	(empty nil)))
      (define
       ((name . "number-year-attlist"))
       (optional nil
		 (attribute
		  ((name . "number:style"))
		  (choice nil
			  (value nil "short")
			  (value nil "long")))))
      (define
       ((name . "number-era"))
       (element
	((name . "number:era"))
	(ref
	 ((name . "number-era-attlist")))
	(ref
	 ((name . "common-calendar-attlist")))
	(empty nil)))
      (define
       ((name . "number-era-attlist"))
       (optional nil
		 (attribute
		  ((name . "number:style"))
		  (choice nil
			  (value nil "short")
			  (value nil "long")))))
      (define
       ((name . "number-day-of-week"))
       (element
	((name . "number:day-of-week"))
	(ref
	 ((name . "number-day-of-week-attlist")))
	(ref
	 ((name . "common-calendar-attlist")))
	(empty nil)))
      (define
       ((name . "number-day-of-week-attlist"))
       (optional nil
		 (attribute
		  ((name . "number:style"))
		  (choice nil
			  (value nil "short")
			  (value nil "long")))))
      (define
       ((name . "number-week-of-year"))
       (element
	((name . "number:week-of-year"))
	(ref
	 ((name . "common-calendar-attlist")))
	(empty nil)))
      (define
       ((name . "number-quarter"))
       (element
	((name . "number:quarter"))
	(ref
	 ((name . "number-quarter-attlist")))
	(ref
	 ((name . "common-calendar-attlist")))
	(empty nil)))
      (define
       ((name . "number-quarter-attlist"))
       (optional nil
		 (attribute
		  ((name . "number:style"))
		  (choice nil
			  (value nil "short")
			  (value nil "long")))))
      (define
       ((name . "number-time-style"))
       (element
	((name . "number:time-style"))
	(ref
	 ((name . "number-time-style-attlist")))
	(ref
	 ((name . "common-data-style-attlist")))
	(ref
	 ((name . "common-format-source-attlist")))
	(optional nil
		  (ref
		   ((name . "style-text-properties"))))
	(optional nil
		  (ref
		   ((name . "number-text"))))
	(oneOrMore nil
		   (ref
		    ((name . "any-time")))
		   (optional nil
			     (ref
			      ((name . "number-text")))))
	(zeroOrMore nil
		    (ref
		     ((name . "style-map"))))))
      (define
       ((name . "any-time"))
       (choice nil
	       (ref
		((name . "number-hours")))
	       (ref
		((name . "number-am-pm")))
	       (ref
		((name . "number-minutes")))
	       (ref
		((name . "number-seconds")))))
      (define
       ((name . "number-time-style-attlist"))
       (optional nil
		 (attribute
		  ((name . "number:truncate-on-overflow"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "number-hours"))
       (element
	((name . "number:hours"))
	(ref
	 ((name . "number-hours-attlist")))
	(empty nil)))
      (define
       ((name . "number-hours-attlist"))
       (optional nil
		 (attribute
		  ((name . "number:style"))
		  (choice nil
			  (value nil "short")
			  (value nil "long")))))
      (define
       ((name . "number-minutes"))
       (element
	((name . "number:minutes"))
	(ref
	 ((name . "number-minutes-attlist")))
	(empty nil)))
      (define
       ((name . "number-minutes-attlist"))
       (optional nil
		 (attribute
		  ((name . "number:style"))
		  (choice nil
			  (value nil "short")
			  (value nil "long")))))
      (define
       ((name . "number-seconds"))
       (element
	((name . "number:seconds"))
	(ref
	 ((name . "number-seconds-attlist")))
	(empty nil)))
      (define
       ((name . "number-seconds-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "number:style"))
			      (choice nil
				      (value nil "short")
				      (value nil "long"))))
		   (optional nil
			     (attribute
			      ((name . "number:decimal-places"))
			      (ref
			       ((name . "integer")))))))
      (define
       ((name . "number-am-pm"))
       (element
	((name . "number:am-pm"))
	(empty nil)))
      (define
       ((name . "number-boolean-style"))
       (element
	((name . "number:boolean-style"))
	(ref
	 ((name . "common-data-style-attlist")))
	(optional nil
		  (ref
		   ((name . "style-text-properties"))))
	(optional nil
		  (ref
		   ((name . "number-text"))))
	(optional nil
		  (ref
		   ((name . "number-boolean")))
		  (optional nil
			    (ref
			     ((name . "number-text")))))
	(zeroOrMore nil
		    (ref
		     ((name . "style-map"))))))
      (define
       ((name . "number-boolean"))
       (element
	((name . "number:boolean"))
	(empty nil)))
      (define
       ((name . "number-text-style"))
       (element
	((name . "number:text-style"))
	(ref
	 ((name . "common-data-style-attlist")))
	(optional nil
		  (ref
		   ((name . "style-text-properties"))))
	(optional nil
		  (ref
		   ((name . "number-text"))))
	(zeroOrMore nil
		    (ref
		     ((name . "number-text-content")))
		    (optional nil
			      (ref
			       ((name . "number-text")))))
	(zeroOrMore nil
		    (ref
		     ((name . "style-map"))))))
      (define
       ((name . "number-text"))
       (element
	((name . "number:text"))
	(text nil)))
      (define
       ((name . "number-text-content"))
       (element
	((name . "number:text-content"))
	(empty nil)))
      (define
       ((name . "common-data-style-attlist"))
       (interleave nil
		   (attribute
		    ((name . "style:name"))
		    (ref
		     ((name . "styleName"))))
		   (optional nil
			     (attribute
			      ((name . "style:display-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "number:language"))
			      (ref
			       ((name . "languageCode")))))
		   (optional nil
			     (attribute
			      ((name . "number:country"))
			      (ref
			       ((name . "countryCode")))))
		   (optional nil
			     (attribute
			      ((name . "number:script"))
			      (ref
			       ((name . "scriptCode")))))
		   (optional nil
			     (attribute
			      ((name . "number:rfc-language-tag"))
			      (ref
			       ((name . "language")))))
		   (optional nil
			     (attribute
			      ((name . "number:title"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "style:volatile"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "number:transliteration-format"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "number:transliteration-language"))
			      (ref
			       ((name . "countryCode")))))
		   (optional nil
			     (attribute
			      ((name . "number:transliteration-country"))
			      (ref
			       ((name . "countryCode")))))
		   (optional nil
			     (attribute
			      ((name . "number:transliteration-style"))
			      (choice nil
				      (value nil "short")
				      (value nil "medium")
				      (value nil "long"))))))
      (define
       ((name . "common-auto-reorder-attlist"))
       (optional nil
		 (attribute
		  ((name . "number:automatic-order"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "common-format-source-attlist"))
       (optional nil
		 (attribute
		  ((name . "number:format-source"))
		  (choice nil
			  (value nil "fixed")
			  (value nil "language")))))
      (define
       ((name . "common-decimal-places-attlist"))
       (optional nil
		 (attribute
		  ((name . "number:decimal-places"))
		  (ref
		   ((name . "integer"))))))
      (define
       ((name . "common-number-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "number:min-integer-digits"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "number:grouping"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "common-calendar-attlist"))
       (optional nil
		 (attribute
		  ((name . "number:calendar"))
		  (choice nil
			  (value nil "gregorian")
			  (value nil "gengou")
			  (value nil "ROC")
			  (value nil "hanja_yoil")
			  (value nil "hanja")
			  (value nil "hijri")
			  (value nil "jewish")
			  (value nil "buddhist")
			  (ref
			   ((name . "string")))))))
      (define
       ((name . "style-style-content"))
       (choice nil
	       (group nil
		      (attribute
		       ((name . "style:family"))
		       (value nil "text"))
		      (optional nil
				(ref
				 ((name . "style-text-properties")))))
	       (group nil
		      (attribute
		       ((name . "style:family"))
		       (value nil "paragraph"))
		      (optional nil
				(ref
				 ((name . "style-paragraph-properties"))))
		      (optional nil
				(ref
				 ((name . "style-text-properties")))))
	       (group nil
		      (attribute
		       ((name . "style:family"))
		       (value nil "section"))
		      (optional nil
				(ref
				 ((name . "style-section-properties")))))
	       (group nil
		      (attribute
		       ((name . "style:family"))
		       (value nil "ruby"))
		      (optional nil
				(ref
				 ((name . "style-ruby-properties")))))
	       (group nil
		      (attribute
		       ((name . "style:family"))
		       (value nil "table"))
		      (optional nil
				(ref
				 ((name . "style-table-properties")))))
	       (group nil
		      (attribute
		       ((name . "style:family"))
		       (value nil "table-column"))
		      (optional nil
				(ref
				 ((name . "style-table-column-properties")))))
	       (group nil
		      (attribute
		       ((name . "style:family"))
		       (value nil "table-row"))
		      (optional nil
				(ref
				 ((name . "style-table-row-properties")))))
	       (group nil
		      (attribute
		       ((name . "style:family"))
		       (value nil "table-cell"))
		      (optional nil
				(ref
				 ((name . "style-table-cell-properties"))))
		      (optional nil
				(ref
				 ((name . "style-paragraph-properties"))))
		      (optional nil
				(ref
				 ((name . "style-text-properties")))))
	       (group nil
		      (attribute
		       ((name . "style:family"))
		       (choice nil
			       (value nil "graphic")
			       (value nil "presentation")))
		      (optional nil
				(ref
				 ((name . "style-graphic-properties"))))
		      (optional nil
				(ref
				 ((name . "style-paragraph-properties"))))
		      (optional nil
				(ref
				 ((name . "style-text-properties")))))
	       (group nil
		      (attribute
		       ((name . "style:family"))
		       (value nil "drawing-page"))
		      (optional nil
				(ref
				 ((name . "style-drawing-page-properties")))))
	       (group nil
		      (attribute
		       ((name . "style:family"))
		       (value nil "chart"))
		      (optional nil
				(ref
				 ((name . "style-chart-properties"))))
		      (optional nil
				(ref
				 ((name . "style-graphic-properties"))))
		      (optional nil
				(ref
				 ((name . "style-paragraph-properties"))))
		      (optional nil
				(ref
				 ((name . "style-text-properties")))))))
      (define
       ((name . "text-linenumbering-configuration"))
       (element
	((name . "text:linenumbering-configuration"))
	(ref
	 ((name . "text-linenumbering-configuration-attlist")))
	(optional nil
		  (ref
		   ((name . "text-linenumbering-separator"))))))
      (define
       ((name . "text-linenumbering-configuration-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "text:number-lines"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (ref
			      ((name . "common-num-format-attlist"))))
		   (optional nil
			     (attribute
			      ((name . "text:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "text:increment"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (optional nil
			     (attribute
			      ((name . "text:number-position"))
			      (choice nil
				      (value nil "left")
				      (value nil "right")
				      (value nil "inner")
				      (value nil "outer"))))
		   (optional nil
			     (attribute
			      ((name . "text:offset"))
			      (ref
			       ((name . "nonNegativeLength")))))
		   (optional nil
			     (attribute
			      ((name . "text:count-empty-lines"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:count-in-text-boxes"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:restart-on-page"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "text-linenumbering-separator"))
       (element
	((name . "text:linenumbering-separator"))
	(optional nil
		  (attribute
		   ((name . "text:increment"))
		   (ref
		    ((name . "nonNegativeInteger")))))
	(text nil)))
      (define
       ((name . "text-notes-configuration"))
       (element
	((name . "text:notes-configuration"))
	(ref
	 ((name . "text-notes-configuration-content")))))
      (define
       ((name . "text-notes-configuration-content"))
       (interleave nil
		   (ref
		    ((name . "text-note-class")))
		   (optional nil
			     (attribute
			      ((name . "text:citation-style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "text:citation-body-style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "text:default-style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "text:master-page-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "text:start-value"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (ref
		    ((name . "common-num-format-prefix-suffix-attlist")))
		   (optional nil
			     (ref
			      ((name . "common-num-format-attlist"))))
		   (optional nil
			     (attribute
			      ((name . "text:start-numbering-at"))
			      (choice nil
				      (value nil "document")
				      (value nil "chapter")
				      (value nil "page"))))
		   (optional nil
			     (attribute
			      ((name . "text:footnotes-position"))
			      (choice nil
				      (value nil "text")
				      (value nil "page")
				      (value nil "section")
				      (value nil "document"))))
		   (optional nil
			     (element
			      ((name . "text:note-continuation-notice-forward"))
			      (text nil)))
		   (optional nil
			     (element
			      ((name . "text:note-continuation-notice-backward"))
			      (text nil)))))
      (define
       ((name . "text-bibliography-configuration"))
       (element
	((name . "text:bibliography-configuration"))
	(ref
	 ((name . "text-bibliography-configuration-attlist")))
	(zeroOrMore nil
		    (ref
		     ((name . "text-sort-key"))))))
      (define
       ((name . "text-bibliography-configuration-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "text:prefix"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "text:suffix"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "text:numbered-entries"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:sort-by-position"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "fo:language"))
			      (ref
			       ((name . "languageCode")))))
		   (optional nil
			     (attribute
			      ((name . "fo:country"))
			      (ref
			       ((name . "countryCode")))))
		   (optional nil
			     (attribute
			      ((name . "fo:script"))
			      (ref
			       ((name . "scriptCode")))))
		   (optional nil
			     (attribute
			      ((name . "style:rfc-language-tag"))
			      (ref
			       ((name . "language")))))
		   (optional nil
			     (attribute
			      ((name . "text:sort-algorithm"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "text-sort-key"))
       (element
	((name . "text:sort-key"))
	(ref
	 ((name . "text-sort-key-attlist")))
	(empty nil)))
      (define
       ((name . "text-sort-key-attlist"))
       (attribute
	((name . "text:key"))
	(choice nil
		(value nil "address")
		(value nil "annote")
		(value nil "author")
		(value nil "bibliography-type")
		(value nil "booktitle")
		(value nil "chapter")
		(value nil "custom1")
		(value nil "custom2")
		(value nil "custom3")
		(value nil "custom4")
		(value nil "custom5")
		(value nil "edition")
		(value nil "editor")
		(value nil "howpublished")
		(value nil "identifier")
		(value nil "institution")
		(value nil "isbn")
		(value nil "issn")
		(value nil "journal")
		(value nil "month")
		(value nil "note")
		(value nil "number")
		(value nil "organizations")
		(value nil "pages")
		(value nil "publisher")
		(value nil "report-type")
		(value nil "school")
		(value nil "series")
		(value nil "title")
		(value nil "url")
		(value nil "volume")
		(value nil "year")))
       (optional nil
		 (attribute
		  ((name . "text:sort-ascending"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "text-list-style"))
       (element
	((name . "text:list-style"))
	(ref
	 ((name . "text-list-style-attr")))
	(zeroOrMore nil
		    (ref
		     ((name . "text-list-style-content"))))))
      (define
       ((name . "text-list-style-attr"))
       (interleave nil
		   (attribute
		    ((name . "style:name"))
		    (ref
		     ((name . "styleName"))))
		   (optional nil
			     (attribute
			      ((name . "style:display-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "text:consecutive-numbering"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "text-list-style-content"))
       (choice nil
	       (element
		((name . "text:list-level-style-number"))
		(ref
		 ((name . "text-list-level-style-attr")))
		(ref
		 ((name . "text-list-level-style-number-attr")))
		(optional nil
			  (ref
			   ((name . "style-list-level-properties"))))
		(optional nil
			  (ref
			   ((name . "style-text-properties")))))
	       (element
		((name . "text:list-level-style-bullet"))
		(ref
		 ((name . "text-list-level-style-attr")))
		(ref
		 ((name . "text-list-level-style-bullet-attr")))
		(optional nil
			  (ref
			   ((name . "style-list-level-properties"))))
		(optional nil
			  (ref
			   ((name . "style-text-properties")))))
	       (element
		((name . "text:list-level-style-image"))
		(ref
		 ((name . "text-list-level-style-attr")))
		(ref
		 ((name . "text-list-level-style-image-attr")))
		(optional nil
			  (ref
			   ((name . "style-list-level-properties")))))))
      (define
       ((name . "text-list-level-style-number-attr"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "text:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (ref
		    ((name . "common-num-format-attlist")))
		   (ref
		    ((name . "common-num-format-prefix-suffix-attlist")))
		   (optional nil
			     (attribute
			      ((name . "text:display-levels"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "text:start-value"))
			      (ref
			       ((name . "positiveInteger")))))))
      (define
       ((name . "text-list-level-style-bullet-attr"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "text:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (attribute
		    ((name . "text:bullet-char"))
		    (ref
		     ((name . "character"))))
		   (ref
		    ((name . "common-num-format-prefix-suffix-attlist")))
		   (optional nil
			     (attribute
			      ((name . "text:bullet-relative-size"))
			      (ref
			       ((name . "percent")))))))
      (define
       ((name . "text-list-level-style-image-attr"))
       (choice nil
	       (ref
		((name . "common-draw-data-attlist")))
	       (ref
		((name . "office-binary-data")))))
      (define
       ((name . "text-list-level-style-attr"))
       (attribute
	((name . "text:level"))
	(ref
	 ((name . "positiveInteger")))))
      (define
       ((name . "text-outline-style"))
       (element
	((name . "text:outline-style"))
	(ref
	 ((name . "text-outline-style-attr")))
	(oneOrMore nil
		   (ref
		    ((name . "text-outline-level-style"))))))
      (define
       ((name . "text-outline-style-attr"))
       (attribute
	((name . "style:name"))
	(ref
	 ((name . "styleName")))))
      (define
       ((name . "text-outline-level-style"))
       (element
	((name . "text:outline-level-style"))
	(ref
	 ((name . "text-outline-level-style-attlist")))
	(optional nil
		  (ref
		   ((name . "style-list-level-properties"))))
	(optional nil
		  (ref
		   ((name . "style-text-properties"))))))
      (define
       ((name . "text-outline-level-style-attlist"))
       (interleave nil
		   (attribute
		    ((name . "text:level"))
		    (ref
		     ((name . "positiveInteger"))))
		   (optional nil
			     (attribute
			      ((name . "text:style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (ref
		    ((name . "common-num-format-attlist")))
		   (ref
		    ((name . "common-num-format-prefix-suffix-attlist")))
		   (optional nil
			     (attribute
			      ((name . "text:display-levels"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "text:start-value"))
			      (ref
			       ((name . "positiveInteger")))))))
      (define
       ((name . "style-graphic-properties"))
       (element
	((name . "style:graphic-properties"))
	(ref
	 ((name . "style-graphic-properties-content-strict")))))
      (define
       ((name . "style-graphic-properties-content-strict"))
       (ref
	((name . "style-graphic-properties-attlist")))
       (ref
	((name . "style-graphic-fill-properties-attlist")))
       (ref
	((name . "style-graphic-properties-elements"))))
      (define
       ((name . "style-drawing-page-properties"))
       (element
	((name . "style:drawing-page-properties"))
	(ref
	 ((name . "style-drawing-page-properties-content-strict")))))
      (define
       ((name . "style-drawing-page-properties-content-strict"))
       (ref
	((name . "style-graphic-fill-properties-attlist")))
       (ref
	((name . "style-drawing-page-properties-attlist")))
       (ref
	((name . "style-drawing-page-properties-elements"))))
      (define
       ((name . "draw-gradient"))
       (element
	((name . "draw:gradient"))
	(ref
	 ((name . "common-draw-gradient-attlist")))
	(ref
	 ((name . "draw-gradient-attlist")))
	(empty nil)))
      (define
       ((name . "common-draw-gradient-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:name"))
			      (ref
			       ((name . "styleName")))))
		   (optional nil
			     (attribute
			      ((name . "draw:display-name"))
			      (ref
			       ((name . "string")))))
		   (attribute
		    ((name . "draw:style"))
		    (ref
		     ((name . "gradient-style"))))
		   (optional nil
			     (attribute
			      ((name . "draw:cx"))
			      (ref
			       ((name . "percent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:cy"))
			      (ref
			       ((name . "percent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:angle"))
			      (ref
			       ((name . "angle")))))
		   (optional nil
			     (attribute
			      ((name . "draw:border"))
			      (ref
			       ((name . "percent")))))))
      (define
       ((name . "gradient-style"))
       (choice nil
	       (value nil "linear")
	       (value nil "axial")
	       (value nil "radial")
	       (value nil "ellipsoid")
	       (value nil "square")
	       (value nil "rectangular")))
      (define
       ((name . "draw-gradient-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:start-color"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "draw:end-color"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "draw:start-intensity"))
			      (ref
			       ((name . "zeroToHundredPercent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:end-intensity"))
			      (ref
			       ((name . "zeroToHundredPercent")))))))
      (define
       ((name . "svg-linearGradient"))
       (element
	((name . "svg:linearGradient"))
	(ref
	 ((name . "common-svg-gradient-attlist")))
	(optional nil
		  (attribute
		   ((name . "svg:x1"))
		   (choice nil
			   (ref
			    ((name . "coordinate")))
			   (ref
			    ((name . "percent"))))))
	(optional nil
		  (attribute
		   ((name . "svg:y1"))
		   (choice nil
			   (ref
			    ((name . "coordinate")))
			   (ref
			    ((name . "percent"))))))
	(optional nil
		  (attribute
		   ((name . "svg:x2"))
		   (choice nil
			   (ref
			    ((name . "coordinate")))
			   (ref
			    ((name . "percent"))))))
	(optional nil
		  (attribute
		   ((name . "svg:y2"))
		   (choice nil
			   (ref
			    ((name . "coordinate")))
			   (ref
			    ((name . "percent"))))))
	(zeroOrMore nil
		    (ref
		     ((name . "svg-stop"))))))
      (define
       ((name . "svg-radialGradient"))
       (element
	((name . "svg:radialGradient"))
	(ref
	 ((name . "common-svg-gradient-attlist")))
	(optional nil
		  (attribute
		   ((name . "svg:cx"))
		   (choice nil
			   (ref
			    ((name . "coordinate")))
			   (ref
			    ((name . "percent"))))))
	(optional nil
		  (attribute
		   ((name . "svg:cy"))
		   (choice nil
			   (ref
			    ((name . "coordinate")))
			   (ref
			    ((name . "percent"))))))
	(optional nil
		  (attribute
		   ((name . "svg:r"))
		   (choice nil
			   (ref
			    ((name . "coordinate")))
			   (ref
			    ((name . "percent"))))))
	(optional nil
		  (attribute
		   ((name . "svg:fx"))
		   (choice nil
			   (ref
			    ((name . "coordinate")))
			   (ref
			    ((name . "percent"))))))
	(optional nil
		  (attribute
		   ((name . "svg:fy"))
		   (choice nil
			   (ref
			    ((name . "coordinate")))
			   (ref
			    ((name . "percent"))))))
	(zeroOrMore nil
		    (ref
		     ((name . "svg-stop"))))))
      (define
       ((name . "svg-stop"))
       (element
	((name . "svg:stop"))
	(attribute
	 ((name . "svg:offset"))
	 (choice nil
		 (ref
		  ((name . "double")))
		 (ref
		  ((name . "percent")))))
	(optional nil
		  (attribute
		   ((name . "svg:stop-color"))
		   (ref
		    ((name . "color")))))
	(optional nil
		  (attribute
		   ((name . "svg:stop-opacity"))
		   (ref
		    ((name . "double")))))))
      (define
       ((name . "common-svg-gradient-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "svg:gradientUnits"))
			      (value nil "objectBoundingBox")))
		   (optional nil
			     (attribute
			      ((name . "svg:gradientTransform"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "svg:spreadMethod"))
			      (choice nil
				      (value nil "pad")
				      (value nil "reflect")
				      (value nil "repeat"))))
		   (attribute
		    ((name . "draw:name"))
		    (ref
		     ((name . "styleName"))))
		   (optional nil
			     (attribute
			      ((name . "draw:display-name"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "draw-hatch"))
       (element
	((name . "draw:hatch"))
	(ref
	 ((name . "draw-hatch-attlist")))
	(empty nil)))
      (define
       ((name . "draw-hatch-attlist"))
       (interleave nil
		   (attribute
		    ((name . "draw:name"))
		    (ref
		     ((name . "styleName"))))
		   (optional nil
			     (attribute
			      ((name . "draw:display-name"))
			      (ref
			       ((name . "string")))))
		   (attribute
		    ((name . "draw:style"))
		    (choice nil
			    (value nil "single")
			    (value nil "double")
			    (value nil "triple")))
		   (optional nil
			     (attribute
			      ((name . "draw:color"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "draw:distance"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "draw:rotation"))
			      (ref
			       ((name . "angle")))))))
      (define
       ((name . "draw-fill-image"))
       (element
	((name . "draw:fill-image"))
	(ref
	 ((name . "draw-fill-image-attlist")))
	(attribute
	 ((name . "xlink:type"))
	 (value nil "simple"))
	(attribute
	 ((name . "xlink:href"))
	 (ref
	  ((name . "anyIRI"))))
	(optional nil
		  (attribute
		   ((name . "xlink:show"))
		   (value nil "embed")))
	(optional nil
		  (attribute
		   ((name . "xlink:actuate"))
		   (value nil "onLoad")))
	(empty nil)))
      (define
       ((name . "draw-fill-image-attlist"))
       (interleave nil
		   (attribute
		    ((name . "draw:name"))
		    (ref
		     ((name . "styleName"))))
		   (optional nil
			     (attribute
			      ((name . "draw:display-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "svg:width"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "svg:height"))
			      (ref
			       ((name . "length")))))))
      (define
       ((name . "draw-opacity"))
       (element
	((name . "draw:opacity"))
	(ref
	 ((name . "common-draw-gradient-attlist")))
	(ref
	 ((name . "draw-opacity-attlist")))
	(empty nil)))
      (define
       ((name . "draw-opacity-attlist"))
       (optional nil
		 (attribute
		  ((name . "draw:start"))
		  (ref
		   ((name . "zeroToHundredPercent")))))
       (optional nil
		 (attribute
		  ((name . "draw:end"))
		  (ref
		   ((name . "zeroToHundredPercent"))))))
      (define
       ((name . "draw-marker"))
       (element
	((name . "draw:marker"))
	(ref
	 ((name . "draw-marker-attlist")))
	(ref
	 ((name . "common-draw-viewbox-attlist")))
	(ref
	 ((name . "common-draw-path-data-attlist")))
	(empty nil)))
      (define
       ((name . "draw-marker-attlist"))
       (interleave nil
		   (attribute
		    ((name . "draw:name"))
		    (ref
		     ((name . "styleName"))))
		   (optional nil
			     (attribute
			      ((name . "draw:display-name"))
			      (ref
			       ((name . "string")))))))
      (define
       ((name . "draw-stroke-dash"))
       (element
	((name . "draw:stroke-dash"))
	(ref
	 ((name . "draw-stroke-dash-attlist")))
	(empty nil)))
      (define
       ((name . "draw-stroke-dash-attlist"))
       (interleave nil
		   (attribute
		    ((name . "draw:name"))
		    (ref
		     ((name . "styleName"))))
		   (optional nil
			     (attribute
			      ((name . "draw:display-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:style"))
			      (choice nil
				      (value nil "rect")
				      (value nil "round"))))
		   (optional nil
			     (attribute
			      ((name . "draw:dots1"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "draw:dots1-length"))
			      (choice nil
				      (ref
				       ((name . "length")))
				      (ref
				       ((name . "percent"))))))
		   (optional nil
			     (attribute
			      ((name . "draw:dots2"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "draw:dots2-length"))
			      (choice nil
				      (ref
				       ((name . "length")))
				      (ref
				       ((name . "percent"))))))
		   (optional nil
			     (attribute
			      ((name . "draw:distance"))
			      (choice nil
				      (ref
				       ((name . "length")))
				      (ref
				       ((name . "percent"))))))))
      (define
       ((name . "style-presentation-page-layout"))
       (element
	((name . "style:presentation-page-layout"))
	(attribute
	 ((name . "style:name"))
	 (ref
	  ((name . "styleName"))))
	(optional nil
		  (attribute
		   ((name . "style:display-name"))
		   (ref
		    ((name . "string")))))
	(zeroOrMore nil
		    (ref
		     ((name . "presentation-placeholder"))))))
      (define
       ((name . "presentation-placeholder"))
       (element
	((name . "presentation:placeholder"))
	(attribute
	 ((name . "presentation:object"))
	 (ref
	  ((name . "presentation-classes"))))
	(attribute
	 ((name . "svg:x"))
	 (choice nil
		 (ref
		  ((name . "coordinate")))
		 (ref
		  ((name . "percent")))))
	(attribute
	 ((name . "svg:y"))
	 (choice nil
		 (ref
		  ((name . "coordinate")))
		 (ref
		  ((name . "percent")))))
	(attribute
	 ((name . "svg:width"))
	 (choice nil
		 (ref
		  ((name . "length")))
		 (ref
		  ((name . "percent")))))
	(attribute
	 ((name . "svg:height"))
	 (choice nil
		 (ref
		  ((name . "length")))
		 (ref
		  ((name . "percent")))))
	(empty nil)))
      (define
       ((name . "style-page-layout-properties"))
       (element
	((name . "style:page-layout-properties"))
	(ref
	 ((name . "style-page-layout-properties-content-strict")))))
      (define
       ((name . "style-page-layout-properties-content-strict"))
       (ref
	((name . "style-page-layout-properties-attlist")))
       (ref
	((name . "style-page-layout-properties-elements"))))
      (define
       ((name . "style-page-layout-properties-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "fo:page-width"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "fo:page-height"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (ref
			      ((name . "common-num-format-attlist"))))
		   (ref
		    ((name . "common-num-format-prefix-suffix-attlist")))
		   (optional nil
			     (attribute
			      ((name . "style:paper-tray-name"))
			      (choice nil
				      (value nil "default")
				      (ref
				       ((name . "string"))))))
		   (optional nil
			     (attribute
			      ((name . "style:print-orientation"))
			      (choice nil
				      (value nil "portrait")
				      (value nil "landscape"))))
		   (ref
		    ((name . "common-horizontal-margin-attlist")))
		   (ref
		    ((name . "common-vertical-margin-attlist")))
		   (ref
		    ((name . "common-margin-attlist")))
		   (ref
		    ((name . "common-border-attlist")))
		   (ref
		    ((name . "common-border-line-width-attlist")))
		   (ref
		    ((name . "common-padding-attlist")))
		   (ref
		    ((name . "common-shadow-attlist")))
		   (ref
		    ((name . "common-background-color-attlist")))
		   (optional nil
			     (attribute
			      ((name . "style:register-truth-ref-style-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "style:print"))
			      (list nil
				    (zeroOrMore nil
						(choice nil
							(value nil "headers")
							(value nil "grid")
							(value nil "annotations")
							(value nil "objects")
							(value nil "charts")
							(value nil "drawings")
							(value nil "formulas")
							(value nil "zero-values"))))))
		   (optional nil
			     (attribute
			      ((name . "style:print-page-order"))
			      (choice nil
				      (value nil "ttb")
				      (value nil "ltr"))))
		   (optional nil
			     (attribute
			      ((name . "style:first-page-number"))
			      (choice nil
				      (ref
				       ((name . "positiveInteger")))
				      (value nil "continue"))))
		   (optional nil
			     (attribute
			      ((name . "style:scale-to"))
			      (ref
			       ((name . "percent")))))
		   (optional nil
			     (attribute
			      ((name . "style:scale-to-pages"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "style:table-centering"))
			      (choice nil
				      (value nil "horizontal")
				      (value nil "vertical")
				      (value nil "both")
				      (value nil "none"))))
		   (optional nil
			     (attribute
			      ((name . "style:footnote-max-height"))
			      (ref
			       ((name . "length")))))
		   (ref
		    ((name . "common-writing-mode-attlist")))
		   (optional nil
			     (attribute
			      ((name . "style:layout-grid-mode"))
			      (choice nil
				      (value nil "none")
				      (value nil "line")
				      (value nil "both"))))
		   (optional nil
			     (attribute
			      ((name . "style:layout-grid-standard-mode"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "style:layout-grid-base-height"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "style:layout-grid-ruby-height"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "style:layout-grid-lines"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "style:layout-grid-base-width"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "style:layout-grid-color"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "style:layout-grid-ruby-below"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "style:layout-grid-print"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "style:layout-grid-display"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "style:layout-grid-snap-to"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "style-page-layout-properties-elements"))
       (interleave nil
		   (ref
		    ((name . "style-background-image")))
		   (ref
		    ((name . "style-columns")))
		   (ref
		    ((name . "style-footnote-sep")))))
      (define
       ((name . "style-footnote-sep"))
       (optional nil
		 (element
		  ((name . "style:footnote-sep"))
		  (ref
		   ((name . "style-footnote-sep-attlist")))
		  (empty nil))))
      (define
       ((name . "style-footnote-sep-attlist"))
       (optional nil
		 (attribute
		  ((name . "style:width"))
		  (ref
		   ((name . "length")))))
       (optional nil
		 (attribute
		  ((name . "style:rel-width"))
		  (ref
		   ((name . "percent")))))
       (optional nil
		 (attribute
		  ((name . "style:color"))
		  (ref
		   ((name . "color")))))
       (optional nil
		 (attribute
		  ((name . "style:line-style"))
		  (ref
		   ((name . "lineStyle")))))
       (optional nil
		 (attribute
		  ((name . "style:adjustment"))
		  (choice nil
			  (value nil "left")
			  (value nil "center")
			  (value nil "right"))))
       (optional nil
		 (attribute
		  ((name . "style:distance-before-sep"))
		  (ref
		   ((name . "length")))))
       (optional nil
		 (attribute
		  ((name . "style:distance-after-sep"))
		  (ref
		   ((name . "length"))))))
      (define
       ((name . "style-header-footer-properties"))
       (element
	((name . "style:header-footer-properties"))
	(ref
	 ((name . "style-header-footer-properties-content-strict")))))
      (define
       ((name . "style-header-footer-properties-content-strict"))
       (ref
	((name . "style-header-footer-properties-attlist")))
       (ref
	((name . "style-header-footer-properties-elements"))))
      (define
       ((name . "style-header-footer-properties-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "svg:height"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "fo:min-height"))
			      (ref
			       ((name . "length")))))
		   (ref
		    ((name . "common-horizontal-margin-attlist")))
		   (ref
		    ((name . "common-vertical-margin-attlist")))
		   (ref
		    ((name . "common-margin-attlist")))
		   (ref
		    ((name . "common-border-attlist")))
		   (ref
		    ((name . "common-border-line-width-attlist")))
		   (ref
		    ((name . "common-padding-attlist")))
		   (ref
		    ((name . "common-background-color-attlist")))
		   (ref
		    ((name . "common-shadow-attlist")))
		   (optional nil
			     (attribute
			      ((name . "style:dynamic-spacing"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "style-header-footer-properties-elements"))
       (ref
	((name . "style-background-image"))))
      (define
       ((name . "style-text-properties"))
       (element
	((name . "style:text-properties"))
	(ref
	 ((name . "style-text-properties-content-strict")))))
      (define
       ((name . "style-text-properties-content-strict"))
       (ref
	((name . "style-text-properties-attlist")))
       (ref
	((name . "style-text-properties-elements"))))
      (define
       ((name . "style-text-properties-elements"))
       (empty nil))
      (define
       ((name . "style-text-properties-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "fo:font-variant"))
			      (ref
			       ((name . "fontVariant")))))
		   (optional nil
			     (attribute
			      ((name . "fo:text-transform"))
			      (choice nil
				      (value nil "none")
				      (value nil "lowercase")
				      (value nil "uppercase")
				      (value nil "capitalize"))))
		   (optional nil
			     (attribute
			      ((name . "fo:color"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "style:use-window-font-color"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-outline"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-line-through-type"))
			      (ref
			       ((name . "lineType")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-line-through-style"))
			      (ref
			       ((name . "lineStyle")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-line-through-width"))
			      (ref
			       ((name . "lineWidth")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-line-through-color"))
			      (choice nil
				      (value nil "font-color")
				      (ref
				       ((name . "color"))))))
		   (optional nil
			     (attribute
			      ((name . "style:text-line-through-text"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-line-through-text-style"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-position"))
			      (list nil
				    (choice nil
					    (ref
					     ((name . "percent")))
					    (value nil "super")
					    (value nil "sub"))
				    (optional nil
					      (ref
					       ((name . "percent")))))))
		   (optional nil
			     (attribute
			      ((name . "style:font-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-name-asian"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-name-complex"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "fo:font-family"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-family-asian"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-family-complex"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-family-generic"))
			      (ref
			       ((name . "fontFamilyGeneric")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-family-generic-asian"))
			      (ref
			       ((name . "fontFamilyGeneric")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-family-generic-complex"))
			      (ref
			       ((name . "fontFamilyGeneric")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-style-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-style-name-asian"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-style-name-complex"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-pitch"))
			      (ref
			       ((name . "fontPitch")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-pitch-asian"))
			      (ref
			       ((name . "fontPitch")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-pitch-complex"))
			      (ref
			       ((name . "fontPitch")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-charset"))
			      (ref
			       ((name . "textEncoding")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-charset-asian"))
			      (ref
			       ((name . "textEncoding")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-charset-complex"))
			      (ref
			       ((name . "textEncoding")))))
		   (optional nil
			     (attribute
			      ((name . "fo:font-size"))
			      (choice nil
				      (ref
				       ((name . "positiveLength")))
				      (ref
				       ((name . "percent"))))))
		   (optional nil
			     (attribute
			      ((name . "style:font-size-asian"))
			      (choice nil
				      (ref
				       ((name . "positiveLength")))
				      (ref
				       ((name . "percent"))))))
		   (optional nil
			     (attribute
			      ((name . "style:font-size-complex"))
			      (choice nil
				      (ref
				       ((name . "positiveLength")))
				      (ref
				       ((name . "percent"))))))
		   (optional nil
			     (attribute
			      ((name . "style:font-size-rel"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-size-rel-asian"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-size-rel-complex"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "style:script-type"))
			      (choice nil
				      (value nil "latin")
				      (value nil "asian")
				      (value nil "complex")
				      (value nil "ignore"))))
		   (optional nil
			     (attribute
			      ((name . "fo:letter-spacing"))
			      (choice nil
				      (ref
				       ((name . "length")))
				      (value nil "normal"))))
		   (optional nil
			     (attribute
			      ((name . "fo:language"))
			      (ref
			       ((name . "languageCode")))))
		   (optional nil
			     (attribute
			      ((name . "style:language-asian"))
			      (ref
			       ((name . "languageCode")))))
		   (optional nil
			     (attribute
			      ((name . "style:language-complex"))
			      (ref
			       ((name . "languageCode")))))
		   (optional nil
			     (attribute
			      ((name . "fo:country"))
			      (ref
			       ((name . "countryCode")))))
		   (optional nil
			     (attribute
			      ((name . "style:country-asian"))
			      (ref
			       ((name . "countryCode")))))
		   (optional nil
			     (attribute
			      ((name . "style:country-complex"))
			      (ref
			       ((name . "countryCode")))))
		   (optional nil
			     (attribute
			      ((name . "fo:script"))
			      (ref
			       ((name . "scriptCode")))))
		   (optional nil
			     (attribute
			      ((name . "style:script-asian"))
			      (ref
			       ((name . "scriptCode")))))
		   (optional nil
			     (attribute
			      ((name . "style:script-complex"))
			      (ref
			       ((name . "scriptCode")))))
		   (optional nil
			     (attribute
			      ((name . "style:rfc-language-tag"))
			      (ref
			       ((name . "language")))))
		   (optional nil
			     (attribute
			      ((name . "style:rfc-language-tag-asian"))
			      (ref
			       ((name . "language")))))
		   (optional nil
			     (attribute
			      ((name . "style:rfc-language-tag-complex"))
			      (ref
			       ((name . "language")))))
		   (optional nil
			     (attribute
			      ((name . "fo:font-style"))
			      (ref
			       ((name . "fontStyle")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-style-asian"))
			      (ref
			       ((name . "fontStyle")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-style-complex"))
			      (ref
			       ((name . "fontStyle")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-relief"))
			      (choice nil
				      (value nil "none")
				      (value nil "embossed")
				      (value nil "engraved"))))
		   (optional nil
			     (attribute
			      ((name . "fo:text-shadow"))
			      (ref
			       ((name . "shadowType")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-underline-type"))
			      (ref
			       ((name . "lineType")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-underline-style"))
			      (ref
			       ((name . "lineStyle")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-underline-width"))
			      (ref
			       ((name . "lineWidth")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-underline-color"))
			      (choice nil
				      (value nil "font-color")
				      (ref
				       ((name . "color"))))))
		   (optional nil
			     (attribute
			      ((name . "style:text-overline-type"))
			      (ref
			       ((name . "lineType")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-overline-style"))
			      (ref
			       ((name . "lineStyle")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-overline-width"))
			      (ref
			       ((name . "lineWidth")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-overline-color"))
			      (choice nil
				      (value nil "font-color")
				      (ref
				       ((name . "color"))))))
		   (optional nil
			     (attribute
			      ((name . "style:text-overline-mode"))
			      (ref
			       ((name . "lineMode")))))
		   (optional nil
			     (attribute
			      ((name . "fo:font-weight"))
			      (ref
			       ((name . "fontWeight")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-weight-asian"))
			      (ref
			       ((name . "fontWeight")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-weight-complex"))
			      (ref
			       ((name . "fontWeight")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-underline-mode"))
			      (ref
			       ((name . "lineMode")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-line-through-mode"))
			      (ref
			       ((name . "lineMode")))))
		   (optional nil
			     (attribute
			      ((name . "style:letter-kerning"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-blinking"))
			      (ref
			       ((name . "boolean")))))
		   (ref
		    ((name . "common-background-color-attlist")))
		   (optional nil
			     (attribute
			      ((name . "style:text-combine"))
			      (choice nil
				      (value nil "none")
				      (value nil "letters")
				      (value nil "lines"))))
		   (optional nil
			     (attribute
			      ((name . "style:text-combine-start-char"))
			      (ref
			       ((name . "character")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-combine-end-char"))
			      (ref
			       ((name . "character")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-emphasize"))
			      (choice nil
				      (value nil "none")
				      (list nil
					    (choice nil
						    (value nil "none")
						    (value nil "accent")
						    (value nil "dot")
						    (value nil "circle")
						    (value nil "disc"))
					    (choice nil
						    (value nil "above")
						    (value nil "below"))))))
		   (optional nil
			     (attribute
			      ((name . "style:text-scale"))
			      (ref
			       ((name . "percent")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-rotation-angle"))
			      (ref
			       ((name . "angle")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-rotation-scale"))
			      (choice nil
				      (value nil "fixed")
				      (value nil "line-height"))))
		   (optional nil
			     (attribute
			      ((name . "fo:hyphenate"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "fo:hyphenation-remain-char-count"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "fo:hyphenation-push-char-count"))
			      (ref
			       ((name . "positiveInteger")))))
		   (choice nil
			   (attribute
			    ((name . "text:display"))
			    (value nil "true"))
			   (attribute
			    ((name . "text:display"))
			    (value nil "none"))
			   (group nil
				  (attribute
				   ((name . "text:display"))
				   (value nil "condition"))
				  (attribute
				   ((name . "text:condition"))
				   (value nil "none")))
			   (empty nil))))
      (define
       ((name . "fontVariant"))
       (choice nil
	       (value nil "normal")
	       (value nil "small-caps")))
      (define
       ((name . "fontFamilyGeneric"))
       (choice nil
	       (value nil "roman")
	       (value nil "swiss")
	       (value nil "modern")
	       (value nil "decorative")
	       (value nil "script")
	       (value nil "system")))
      (define
       ((name . "fontPitch"))
       (choice nil
	       (value nil "fixed")
	       (value nil "variable")))
      (define
       ((name . "textEncoding"))
       (data
	((type . "string"))
	(param
	 ((name . "pattern"))
	 "[A-Za-z][A-Za-z0-9._\\-]*")))
      (define
       ((name . "fontStyle"))
       (choice nil
	       (value nil "normal")
	       (value nil "italic")
	       (value nil "oblique")))
      (define
       ((name . "shadowType"))
       (choice nil
	       (value nil "none")
	       (ref
		((name . "string")))))
      (define
       ((name . "lineType"))
       (choice nil
	       (value nil "none")
	       (value nil "single")
	       (value nil "double")))
      (define
       ((name . "lineStyle"))
       (choice nil
	       (value nil "none")
	       (value nil "solid")
	       (value nil "dotted")
	       (value nil "dash")
	       (value nil "long-dash")
	       (value nil "dot-dash")
	       (value nil "dot-dot-dash")
	       (value nil "wave")))
      (define
       ((name . "lineWidth"))
       (choice nil
	       (value nil "auto")
	       (value nil "normal")
	       (value nil "bold")
	       (value nil "thin")
	       (value nil "medium")
	       (value nil "thick")
	       (ref
		((name . "positiveInteger")))
	       (ref
		((name . "percent")))
	       (ref
		((name . "positiveLength")))))
      (define
       ((name . "fontWeight"))
       (choice nil
	       (value nil "normal")
	       (value nil "bold")
	       (value nil "100")
	       (value nil "200")
	       (value nil "300")
	       (value nil "400")
	       (value nil "500")
	       (value nil "600")
	       (value nil "700")
	       (value nil "800")
	       (value nil "900")))
      (define
       ((name . "lineMode"))
       (choice nil
	       (value nil "continuous")
	       (value nil "skip-white-space")))
      (define
       ((name . "style-paragraph-properties"))
       (element
	((name . "style:paragraph-properties"))
	(ref
	 ((name . "style-paragraph-properties-content-strict")))))
      (define
       ((name . "style-paragraph-properties-content-strict"))
       (ref
	((name . "style-paragraph-properties-attlist")))
       (ref
	((name . "style-paragraph-properties-elements"))))
      (define
       ((name . "style-paragraph-properties-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "fo:line-height"))
			      (choice nil
				      (value nil "normal")
				      (ref
				       ((name . "nonNegativeLength")))
				      (ref
				       ((name . "percent"))))))
		   (optional nil
			     (attribute
			      ((name . "style:line-height-at-least"))
			      (ref
			       ((name . "nonNegativeLength")))))
		   (optional nil
			     (attribute
			      ((name . "style:line-spacing"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-independent-line-spacing"))
			      (ref
			       ((name . "boolean")))))
		   (ref
		    ((name . "common-text-align")))
		   (optional nil
			     (attribute
			      ((name . "fo:text-align-last"))
			      (choice nil
				      (value nil "start")
				      (value nil "center")
				      (value nil "justify"))))
		   (optional nil
			     (attribute
			      ((name . "style:justify-single-word"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "fo:keep-together"))
			      (choice nil
				      (value nil "auto")
				      (value nil "always"))))
		   (optional nil
			     (attribute
			      ((name . "fo:widows"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (optional nil
			     (attribute
			      ((name . "fo:orphans"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (optional nil
			     (attribute
			      ((name . "style:tab-stop-distance"))
			      (ref
			       ((name . "nonNegativeLength")))))
		   (optional nil
			     (attribute
			      ((name . "fo:hyphenation-keep"))
			      (choice nil
				      (value nil "auto")
				      (value nil "page"))))
		   (optional nil
			     (attribute
			      ((name . "fo:hyphenation-ladder-count"))
			      (choice nil
				      (value nil "no-limit")
				      (ref
				       ((name . "positiveInteger"))))))
		   (optional nil
			     (attribute
			      ((name . "style:register-true"))
			      (ref
			       ((name . "boolean")))))
		   (ref
		    ((name . "common-horizontal-margin-attlist")))
		   (optional nil
			     (attribute
			      ((name . "fo:text-indent"))
			      (choice nil
				      (ref
				       ((name . "length")))
				      (ref
				       ((name . "percent"))))))
		   (optional nil
			     (attribute
			      ((name . "style:auto-text-indent"))
			      (ref
			       ((name . "boolean")))))
		   (ref
		    ((name . "common-vertical-margin-attlist")))
		   (ref
		    ((name . "common-margin-attlist")))
		   (ref
		    ((name . "common-break-attlist")))
		   (ref
		    ((name . "common-background-color-attlist")))
		   (ref
		    ((name . "common-border-attlist")))
		   (ref
		    ((name . "common-border-line-width-attlist")))
		   (optional nil
			     (attribute
			      ((name . "style:join-border"))
			      (ref
			       ((name . "boolean")))))
		   (ref
		    ((name . "common-padding-attlist")))
		   (ref
		    ((name . "common-shadow-attlist")))
		   (ref
		    ((name . "common-keep-with-next-attlist")))
		   (optional nil
			     (attribute
			      ((name . "text:number-lines"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:line-number"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (optional nil
			     (attribute
			      ((name . "style:text-autospace"))
			      (choice nil
				      (value nil "none")
				      (value nil "ideograph-alpha"))))
		   (optional nil
			     (attribute
			      ((name . "style:punctuation-wrap"))
			      (choice nil
				      (value nil "simple")
				      (value nil "hanging"))))
		   (optional nil
			     (attribute
			      ((name . "style:line-break"))
			      (choice nil
				      (value nil "normal")
				      (value nil "strict"))))
		   (optional nil
			     (attribute
			      ((name . "style:vertical-align"))
			      (choice nil
				      (value nil "top")
				      (value nil "middle")
				      (value nil "bottom")
				      (value nil "auto")
				      (value nil "baseline"))))
		   (ref
		    ((name . "common-writing-mode-attlist")))
		   (optional nil
			     (attribute
			      ((name . "style:writing-mode-automatic"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "style:snap-to-layout-grid"))
			      (ref
			       ((name . "boolean")))))
		   (ref
		    ((name . "common-page-number-attlist")))
		   (ref
		    ((name . "common-background-transparency-attlist")))))
      (define
       ((name . "common-text-align"))
       (optional nil
		 (attribute
		  ((name . "fo:text-align"))
		  (choice nil
			  (value nil "start")
			  (value nil "end")
			  (value nil "left")
			  (value nil "right")
			  (value nil "center")
			  (value nil "justify")))))
      (define
       ((name . "style-paragraph-properties-elements"))
       (interleave nil
		   (ref
		    ((name . "style-tab-stops")))
		   (ref
		    ((name . "style-drop-cap")))
		   (ref
		    ((name . "style-background-image")))))
      (define
       ((name . "style-tab-stops"))
       (optional nil
		 (element
		  ((name . "style:tab-stops"))
		  (zeroOrMore nil
			      (ref
			       ((name . "style-tab-stop")))))))
      (define
       ((name . "style-tab-stop"))
       (element
	((name . "style:tab-stop"))
	(ref
	 ((name . "style-tab-stop-attlist")))
	(empty nil)))
      (define
       ((name . "style-tab-stop-attlist"))
       (interleave nil
		   (attribute
		    ((name . "style:position"))
		    (ref
		     ((name . "length"))))
		   (choice nil
			   (optional nil
				     (attribute
				      ((name . "style:type"))
				      (choice nil
					      (value nil "left")
					      (value nil "center")
					      (value nil "right"))))
			   (group nil
				  (attribute
				   ((name . "style:type"))
				   (value nil "char"))
				  (ref
				   ((name . "style-tab-stop-char-attlist")))))
		   (optional nil
			     (attribute
			      ((name . "style:leader-type"))
			      (ref
			       ((name . "lineType")))))
		   (optional nil
			     (attribute
			      ((name . "style:leader-style"))
			      (ref
			       ((name . "lineStyle")))))
		   (optional nil
			     (attribute
			      ((name . "style:leader-width"))
			      (ref
			       ((name . "lineWidth")))))
		   (optional nil
			     (attribute
			      ((name . "style:leader-color"))
			      (choice nil
				      (value nil "font-color")
				      (ref
				       ((name . "color"))))))
		   (optional nil
			     (attribute
			      ((name . "style:leader-text"))
			      (ref
			       ((name . "character")))))
		   (optional nil
			     (attribute
			      ((name . "style:leader-text-style"))
			      (ref
			       ((name . "styleNameRef")))))))
      (define
       ((name . "style-tab-stop-char-attlist"))
       (attribute
	((name . "style:char"))
	(ref
	 ((name . "character")))))
      (define
       ((name . "style-drop-cap"))
       (optional nil
		 (element
		  ((name . "style:drop-cap"))
		  (ref
		   ((name . "style-drop-cap-attlist")))
		  (empty nil))))
      (define
       ((name . "style-drop-cap-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "style:length"))
			      (choice nil
				      (value nil "word")
				      (ref
				       ((name . "positiveInteger"))))))
		   (optional nil
			     (attribute
			      ((name . "style:lines"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "style:distance"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "style:style-name"))
			      (ref
			       ((name . "styleNameRef")))))))
      (define
       ((name . "common-horizontal-margin-attlist"))
       (optional nil
		 (attribute
		  ((name . "fo:margin-left"))
		  (choice nil
			  (ref
			   ((name . "length")))
			  (ref
			   ((name . "percent"))))))
       (optional nil
		 (attribute
		  ((name . "fo:margin-right"))
		  (choice nil
			  (ref
			   ((name . "length")))
			  (ref
			   ((name . "percent")))))))
      (define
       ((name . "common-vertical-margin-attlist"))
       (optional nil
		 (attribute
		  ((name . "fo:margin-top"))
		  (choice nil
			  (ref
			   ((name . "nonNegativeLength")))
			  (ref
			   ((name . "percent"))))))
       (optional nil
		 (attribute
		  ((name . "fo:margin-bottom"))
		  (choice nil
			  (ref
			   ((name . "nonNegativeLength")))
			  (ref
			   ((name . "percent")))))))
      (define
       ((name . "common-margin-attlist"))
       (optional nil
		 (attribute
		  ((name . "fo:margin"))
		  (choice nil
			  (ref
			   ((name . "nonNegativeLength")))
			  (ref
			   ((name . "percent")))))))
      (define
       ((name . "common-break-attlist"))
       (optional nil
		 (attribute
		  ((name . "fo:break-before"))
		  (choice nil
			  (value nil "auto")
			  (value nil "column")
			  (value nil "page"))))
       (optional nil
		 (attribute
		  ((name . "fo:break-after"))
		  (choice nil
			  (value nil "auto")
			  (value nil "column")
			  (value nil "page")))))
      (define
       ((name . "common-background-color-attlist"))
       (optional nil
		 (attribute
		  ((name . "fo:background-color"))
		  (choice nil
			  (value nil "transparent")
			  (ref
			   ((name . "color")))))))
      (define
       ((name . "style-background-image"))
       (optional nil
		 (element
		  ((name . "style:background-image"))
		  (ref
		   ((name . "style-background-image-attlist")))
		  (choice nil
			  (ref
			   ((name . "common-draw-data-attlist")))
			  (ref
			   ((name . "office-binary-data")))
			  (empty nil)))))
      (define
       ((name . "style-background-image-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "style:repeat"))
			      (choice nil
				      (value nil "no-repeat")
				      (value nil "repeat")
				      (value nil "stretch"))))
		   (optional nil
			     (attribute
			      ((name . "style:position"))
			      (choice nil
				      (value nil "left")
				      (value nil "center")
				      (value nil "right")
				      (value nil "top")
				      (value nil "bottom")
				      (list nil
					    (ref
					     ((name . "horiBackPos")))
					    (ref
					     ((name . "vertBackPos"))))
				      (list nil
					    (ref
					     ((name . "vertBackPos")))
					    (ref
					     ((name . "horiBackPos")))))))
		   (optional nil
			     (attribute
			      ((name . "style:filter-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "draw:opacity"))
			      (ref
			       ((name . "zeroToHundredPercent")))))))
      (define
       ((name . "horiBackPos"))
       (choice nil
	       (value nil "left")
	       (value nil "center")
	       (value nil "right")))
      (define
       ((name . "vertBackPos"))
       (choice nil
	       (value nil "top")
	       (value nil "center")
	       (value nil "bottom")))
      (define
       ((name . "common-border-attlist"))
       (optional nil
		 (attribute
		  ((name . "fo:border"))
		  (ref
		   ((name . "string")))))
       (optional nil
		 (attribute
		  ((name . "fo:border-top"))
		  (ref
		   ((name . "string")))))
       (optional nil
		 (attribute
		  ((name . "fo:border-bottom"))
		  (ref
		   ((name . "string")))))
       (optional nil
		 (attribute
		  ((name . "fo:border-left"))
		  (ref
		   ((name . "string")))))
       (optional nil
		 (attribute
		  ((name . "fo:border-right"))
		  (ref
		   ((name . "string"))))))
      (define
       ((name . "common-border-line-width-attlist"))
       (optional nil
		 (attribute
		  ((name . "style:border-line-width"))
		  (ref
		   ((name . "borderWidths")))))
       (optional nil
		 (attribute
		  ((name . "style:border-line-width-top"))
		  (ref
		   ((name . "borderWidths")))))
       (optional nil
		 (attribute
		  ((name . "style:border-line-width-bottom"))
		  (ref
		   ((name . "borderWidths")))))
       (optional nil
		 (attribute
		  ((name . "style:border-line-width-left"))
		  (ref
		   ((name . "borderWidths")))))
       (optional nil
		 (attribute
		  ((name . "style:border-line-width-right"))
		  (ref
		   ((name . "borderWidths"))))))
      (define
       ((name . "borderWidths"))
       (list nil
	     (ref
	      ((name . "positiveLength")))
	     (ref
	      ((name . "positiveLength")))
	     (ref
	      ((name . "positiveLength")))))
      (define
       ((name . "common-padding-attlist"))
       (optional nil
		 (attribute
		  ((name . "fo:padding"))
		  (ref
		   ((name . "nonNegativeLength")))))
       (optional nil
		 (attribute
		  ((name . "fo:padding-top"))
		  (ref
		   ((name . "nonNegativeLength")))))
       (optional nil
		 (attribute
		  ((name . "fo:padding-bottom"))
		  (ref
		   ((name . "nonNegativeLength")))))
       (optional nil
		 (attribute
		  ((name . "fo:padding-left"))
		  (ref
		   ((name . "nonNegativeLength")))))
       (optional nil
		 (attribute
		  ((name . "fo:padding-right"))
		  (ref
		   ((name . "nonNegativeLength"))))))
      (define
       ((name . "common-shadow-attlist"))
       (optional nil
		 (attribute
		  ((name . "style:shadow"))
		  (ref
		   ((name . "shadowType"))))))
      (define
       ((name . "common-keep-with-next-attlist"))
       (optional nil
		 (attribute
		  ((name . "fo:keep-with-next"))
		  (choice nil
			  (value nil "auto")
			  (value nil "always")))))
      (define
       ((name . "common-writing-mode-attlist"))
       (optional nil
		 (attribute
		  ((name . "style:writing-mode"))
		  (choice nil
			  (value nil "lr-tb")
			  (value nil "rl-tb")
			  (value nil "tb-rl")
			  (value nil "tb-lr")
			  (value nil "lr")
			  (value nil "rl")
			  (value nil "tb")
			  (value nil "page")))))
      (define
       ((name . "common-page-number-attlist"))
       (optional nil
		 (attribute
		  ((name . "style:page-number"))
		  (choice nil
			  (ref
			   ((name . "positiveInteger")))
			  (value nil "auto")))))
      (define
       ((name . "common-background-transparency-attlist"))
       (optional nil
		 (attribute
		  ((name . "style:background-transparency"))
		  (ref
		   ((name . "zeroToHundredPercent"))))))
      (define
       ((name . "style-ruby-properties"))
       (element
	((name . "style:ruby-properties"))
	(ref
	 ((name . "style-ruby-properties-content-strict")))))
      (define
       ((name . "style-ruby-properties-content-strict"))
       (ref
	((name . "style-ruby-properties-attlist")))
       (ref
	((name . "style-ruby-properties-elements"))))
      (define
       ((name . "style-ruby-properties-elements"))
       (empty nil))
      (define
       ((name . "style-ruby-properties-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "style:ruby-position"))
			      (choice nil
				      (value nil "above")
				      (value nil "below"))))
		   (optional nil
			     (attribute
			      ((name . "style:ruby-align"))
			      (choice nil
				      (value nil "left")
				      (value nil "center")
				      (value nil "right")
				      (value nil "distribute-letter")
				      (value nil "distribute-space"))))))
      (define
       ((name . "style-section-properties"))
       (element
	((name . "style:section-properties"))
	(ref
	 ((name . "style-section-properties-content-strict")))))
      (define
       ((name . "style-section-properties-content-strict"))
       (ref
	((name . "style-section-properties-attlist")))
       (ref
	((name . "style-section-properties-elements"))))
      (define
       ((name . "style-section-properties-attlist"))
       (interleave nil
		   (ref
		    ((name . "common-background-color-attlist")))
		   (ref
		    ((name . "common-horizontal-margin-attlist")))
		   (optional nil
			     (attribute
			      ((name . "style:protect"))
			      (ref
			       ((name . "boolean")))))
		   (ref
		    ((name . "common-editable-attlist")))
		   (optional nil
			     (attribute
			      ((name . "text:dont-balance-text-columns"))
			      (ref
			       ((name . "boolean")))))
		   (ref
		    ((name . "common-writing-mode-attlist")))))
      (define
       ((name . "style-section-properties-elements"))
       (interleave nil
		   (ref
		    ((name . "style-background-image")))
		   (ref
		    ((name . "style-columns")))
		   (zeroOrMore nil
			       (ref
				((name . "text-notes-configuration"))))))
      (define
       ((name . "style-columns"))
       (optional nil
		 (element
		  ((name . "style:columns"))
		  (ref
		   ((name . "style-columns-attlist")))
		  (optional nil
			    (ref
			     ((name . "style-column-sep"))))
		  (zeroOrMore nil
			      (ref
			       ((name . "style-column")))))))
      (define
       ((name . "style-columns-attlist"))
       (interleave nil
		   (attribute
		    ((name . "fo:column-count"))
		    (ref
		     ((name . "positiveInteger"))))
		   (optional nil
			     (attribute
			      ((name . "fo:column-gap"))
			      (ref
			       ((name . "length")))))))
      (define
       ((name . "style-column"))
       (element
	((name . "style:column"))
	(ref
	 ((name . "style-column-attlist")))))
      (define
       ((name . "style-column-attlist"))
       (interleave nil
		   (attribute
		    ((name . "style:rel-width"))
		    (ref
		     ((name . "relativeLength"))))
		   (optional nil
			     (attribute
			      ((name . "fo:start-indent"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "fo:end-indent"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "fo:space-before"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "fo:space-after"))
			      (ref
			       ((name . "length")))))))
      (define
       ((name . "style-column-sep"))
       (element
	((name . "style:column-sep"))
	(ref
	 ((name . "style-column-sep-attlist")))))
      (define
       ((name . "style-column-sep-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "style:style"))
			      (choice nil
				      (value nil "none")
				      (value nil "solid")
				      (value nil "dotted")
				      (value nil "dashed")
				      (value nil "dot-dashed"))))
		   (attribute
		    ((name . "style:width"))
		    (ref
		     ((name . "length"))))
		   (optional nil
			     (attribute
			      ((name . "style:height"))
			      (ref
			       ((name . "zeroToHundredPercent")))))
		   (optional nil
			     (attribute
			      ((name . "style:vertical-align"))
			      (choice nil
				      (value nil "top")
				      (value nil "middle")
				      (value nil "bottom"))))
		   (optional nil
			     (attribute
			      ((name . "style:color"))
			      (ref
			       ((name . "color")))))))
      (define
       ((name . "style-table-properties"))
       (element
	((name . "style:table-properties"))
	(ref
	 ((name . "style-table-properties-content-strict")))))
      (define
       ((name . "style-table-properties-content-strict"))
       (ref
	((name . "style-table-properties-attlist")))
       (ref
	((name . "style-table-properties-elements"))))
      (define
       ((name . "style-table-properties-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "style:width"))
			      (ref
			       ((name . "positiveLength")))))
		   (optional nil
			     (attribute
			      ((name . "style:rel-width"))
			      (ref
			       ((name . "percent")))))
		   (optional nil
			     (attribute
			      ((name . "table:align"))
			      (choice nil
				      (value nil "left")
				      (value nil "center")
				      (value nil "right")
				      (value nil "margins"))))
		   (ref
		    ((name . "common-horizontal-margin-attlist")))
		   (ref
		    ((name . "common-vertical-margin-attlist")))
		   (ref
		    ((name . "common-margin-attlist")))
		   (ref
		    ((name . "common-page-number-attlist")))
		   (ref
		    ((name . "common-break-attlist")))
		   (ref
		    ((name . "common-background-color-attlist")))
		   (ref
		    ((name . "common-shadow-attlist")))
		   (ref
		    ((name . "common-keep-with-next-attlist")))
		   (optional nil
			     (attribute
			      ((name . "style:may-break-between-rows"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "table:border-model"))
			      (choice nil
				      (value nil "collapsing")
				      (value nil "separating"))))
		   (ref
		    ((name . "common-writing-mode-attlist")))
		   (optional nil
			     (attribute
			      ((name . "table:display"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "style-table-properties-elements"))
       (ref
	((name . "style-background-image"))))
      (define
       ((name . "style-table-column-properties"))
       (element
	((name . "style:table-column-properties"))
	(ref
	 ((name . "style-table-column-properties-content-strict")))))
      (define
       ((name . "style-table-column-properties-content-strict"))
       (ref
	((name . "style-table-column-properties-attlist")))
       (ref
	((name . "style-table-column-properties-elements"))))
      (define
       ((name . "style-table-column-properties-elements"))
       (empty nil))
      (define
       ((name . "style-table-column-properties-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "style:column-width"))
			      (ref
			       ((name . "positiveLength")))))
		   (optional nil
			     (attribute
			      ((name . "style:rel-column-width"))
			      (ref
			       ((name . "relativeLength")))))
		   (optional nil
			     (attribute
			      ((name . "style:use-optimal-column-width"))
			      (ref
			       ((name . "boolean")))))
		   (ref
		    ((name . "common-break-attlist")))))
      (define
       ((name . "style-table-row-properties"))
       (element
	((name . "style:table-row-properties"))
	(ref
	 ((name . "style-table-row-properties-content-strict")))))
      (define
       ((name . "style-table-row-properties-content-strict"))
       (ref
	((name . "style-table-row-properties-attlist")))
       (ref
	((name . "style-table-row-properties-elements"))))
      (define
       ((name . "style-table-row-properties-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "style:row-height"))
			      (ref
			       ((name . "positiveLength")))))
		   (optional nil
			     (attribute
			      ((name . "style:min-row-height"))
			      (ref
			       ((name . "nonNegativeLength")))))
		   (optional nil
			     (attribute
			      ((name . "style:use-optimal-row-height"))
			      (ref
			       ((name . "boolean")))))
		   (ref
		    ((name . "common-background-color-attlist")))
		   (ref
		    ((name . "common-break-attlist")))
		   (optional nil
			     (attribute
			      ((name . "fo:keep-together"))
			      (choice nil
				      (value nil "auto")
				      (value nil "always"))))))
      (define
       ((name . "style-table-row-properties-elements"))
       (ref
	((name . "style-background-image"))))
      (define
       ((name . "style-table-cell-properties"))
       (element
	((name . "style:table-cell-properties"))
	(ref
	 ((name . "style-table-cell-properties-content-strict")))))
      (define
       ((name . "style-table-cell-properties-content-strict"))
       (ref
	((name . "style-table-cell-properties-attlist")))
       (ref
	((name . "style-table-cell-properties-elements"))))
      (define
       ((name . "style-table-cell-properties-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "style:vertical-align"))
			      (choice nil
				      (value nil "top")
				      (value nil "middle")
				      (value nil "bottom")
				      (value nil "automatic"))))
		   (optional nil
			     (attribute
			      ((name . "style:text-align-source"))
			      (choice nil
				      (value nil "fix")
				      (value nil "value-type"))))
		   (ref
		    ((name . "common-style-direction-attlist")))
		   (optional nil
			     (attribute
			      ((name . "style:glyph-orientation-vertical"))
			      (choice nil
				      (value nil "auto")
				      (value nil "0")
				      (value nil "0deg")
				      (value nil "0rad")
				      (value nil "0grad"))))
		   (ref
		    ((name . "common-writing-mode-attlist")))
		   (ref
		    ((name . "common-shadow-attlist")))
		   (ref
		    ((name . "common-background-color-attlist")))
		   (ref
		    ((name . "common-border-attlist")))
		   (optional nil
			     (attribute
			      ((name . "style:diagonal-tl-br"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "style:diagonal-tl-br-widths"))
			      (ref
			       ((name . "borderWidths")))))
		   (optional nil
			     (attribute
			      ((name . "style:diagonal-bl-tr"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "style:diagonal-bl-tr-widths"))
			      (ref
			       ((name . "borderWidths")))))
		   (ref
		    ((name . "common-border-line-width-attlist")))
		   (ref
		    ((name . "common-padding-attlist")))
		   (optional nil
			     (attribute
			      ((name . "fo:wrap-option"))
			      (choice nil
				      (value nil "no-wrap")
				      (value nil "wrap"))))
		   (ref
		    ((name . "common-rotation-angle-attlist")))
		   (optional nil
			     (attribute
			      ((name . "style:rotation-align"))
			      (choice nil
				      (value nil "none")
				      (value nil "bottom")
				      (value nil "top")
				      (value nil "center"))))
		   (optional nil
			     (attribute
			      ((name . "style:cell-protect"))
			      (choice nil
				      (value nil "none")
				      (value nil "hidden-and-protected")
				      (list nil
					    (oneOrMore nil
						       (choice nil
							       (value nil "protected")
							       (value nil "formula-hidden")))))))
		   (optional nil
			     (attribute
			      ((name . "style:print-content"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "style:decimal-places"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (optional nil
			     (attribute
			      ((name . "style:repeat-content"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "style:shrink-to-fit"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "common-style-direction-attlist"))
       (optional nil
		 (attribute
		  ((name . "style:direction"))
		  (choice nil
			  (value nil "ltr")
			  (value nil "ttb")))))
      (define
       ((name . "style-table-cell-properties-elements"))
       (ref
	((name . "style-background-image"))))
      (define
       ((name . "common-rotation-angle-attlist"))
       (optional nil
		 (attribute
		  ((name . "style:rotation-angle"))
		  (ref
		   ((name . "angle"))))))
      (define
       ((name . "style-list-level-properties"))
       (element
	((name . "style:list-level-properties"))
	(ref
	 ((name . "style-list-level-properties-content-strict")))))
      (define
       ((name . "style-list-level-properties-content-strict"))
       (ref
	((name . "style-list-level-properties-attlist")))
       (ref
	((name . "style-list-level-properties-elements"))))
      (define
       ((name . "style-list-level-properties-attlist"))
       (interleave nil
		   (ref
		    ((name . "common-text-align")))
		   (optional nil
			     (attribute
			      ((name . "text:space-before"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "text:min-label-width"))
			      (ref
			       ((name . "nonNegativeLength")))))
		   (optional nil
			     (attribute
			      ((name . "text:min-label-distance"))
			      (ref
			       ((name . "nonNegativeLength")))))
		   (optional nil
			     (attribute
			      ((name . "style:font-name"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "fo:width"))
			      (ref
			       ((name . "positiveLength")))))
		   (optional nil
			     (attribute
			      ((name . "fo:height"))
			      (ref
			       ((name . "positiveLength")))))
		   (ref
		    ((name . "common-vertical-rel-attlist")))
		   (ref
		    ((name . "common-vertical-pos-attlist")))
		   (optional nil
			     (attribute
			      ((name . "text:list-level-position-and-space-mode"))
			      (choice nil
				      (value nil "label-width-and-position")
				      (value nil "label-alignment"))))))
      (define
       ((name . "style-list-level-properties-elements"))
       (ref
	((name . "style-list-level-label-alignment"))))
      (define
       ((name . "style-list-level-label-alignment"))
       (optional nil
		 (element
		  ((name . "style:list-level-label-alignment"))
		  (ref
		   ((name . "style-list-level-label-alignment-attlist")))
		  (empty nil))))
      (define
       ((name . "style-list-level-label-alignment-attlist"))
       (interleave nil
		   (attribute
		    ((name . "text:label-followed-by"))
		    (choice nil
			    (value nil "listtab")
			    (value nil "space")
			    (value nil "nothing")))
		   (optional nil
			     (attribute
			      ((name . "text:list-tab-stop-position"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "fo:text-indent"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "fo:margin-left"))
			      (ref
			       ((name . "length")))))))
      (define
       ((name . "style-graphic-properties-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:stroke"))
			      (choice nil
				      (value nil "none")
				      (value nil "dash")
				      (value nil "solid"))))
		   (optional nil
			     (attribute
			      ((name . "draw:stroke-dash"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "draw:stroke-dash-names"))
			      (ref
			       ((name . "styleNameRefs")))))
		   (optional nil
			     (attribute
			      ((name . "svg:stroke-width"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "svg:stroke-color"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "draw:marker-start"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "draw:marker-end"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "draw:marker-start-width"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "draw:marker-end-width"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "draw:marker-start-center"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:marker-end-center"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "svg:stroke-opacity"))
			      (choice nil
				      (data
				       ((type . "double"))
				       (param
					((name . "minInclusive"))
					"0")
				       (param
					((name . "maxInclusive"))
					"1"))
				      (ref
				       ((name . "zeroToHundredPercent"))))))
		   (optional nil
			     (attribute
			      ((name . "draw:stroke-linejoin"))
			      (choice nil
				      (value nil "miter")
				      (value nil "round")
				      (value nil "bevel")
				      (value nil "middle")
				      (value nil "none"))))
		   (optional nil
			     (attribute
			      ((name . "svg:stroke-linecap"))
			      (choice nil
				      (value nil "butt")
				      (value nil "square")
				      (value nil "round"))))
		   (optional nil
			     (attribute
			      ((name . "draw:symbol-color"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "text:animation"))
			      (choice nil
				      (value nil "none")
				      (value nil "scroll")
				      (value nil "alternate")
				      (value nil "slide"))))
		   (optional nil
			     (attribute
			      ((name . "text:animation-direction"))
			      (choice nil
				      (value nil "left")
				      (value nil "right")
				      (value nil "up")
				      (value nil "down"))))
		   (optional nil
			     (attribute
			      ((name . "text:animation-start-inside"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:animation-stop-inside"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:animation-repeat"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (optional nil
			     (attribute
			      ((name . "text:animation-delay"))
			      (ref
			       ((name . "duration")))))
		   (optional nil
			     (attribute
			      ((name . "text:animation-steps"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "draw:auto-grow-width"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:auto-grow-height"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:fit-to-size"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:fit-to-contour"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:textarea-vertical-align"))
			      (choice nil
				      (value nil "top")
				      (value nil "middle")
				      (value nil "bottom")
				      (value nil "justify"))))
		   (optional nil
			     (attribute
			      ((name . "draw:textarea-horizontal-align"))
			      (choice nil
				      (value nil "left")
				      (value nil "center")
				      (value nil "right")
				      (value nil "justify"))))
		   (optional nil
			     (attribute
			      ((name . "fo:wrap-option"))
			      (choice nil
				      (value nil "no-wrap")
				      (value nil "wrap"))))
		   (optional nil
			     (attribute
			      ((name . "style:shrink-to-fit"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:color-mode"))
			      (choice nil
				      (value nil "greyscale")
				      (value nil "mono")
				      (value nil "watermark")
				      (value nil "standard"))))
		   (optional nil
			     (attribute
			      ((name . "draw:color-inversion"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:luminance"))
			      (ref
			       ((name . "zeroToHundredPercent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:contrast"))
			      (ref
			       ((name . "percent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:gamma"))
			      (ref
			       ((name . "percent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:red"))
			      (ref
			       ((name . "signedZeroToHundredPercent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:green"))
			      (ref
			       ((name . "signedZeroToHundredPercent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:blue"))
			      (ref
			       ((name . "signedZeroToHundredPercent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:image-opacity"))
			      (ref
			       ((name . "zeroToHundredPercent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:shadow"))
			      (choice nil
				      (value nil "visible")
				      (value nil "hidden"))))
		   (optional nil
			     (attribute
			      ((name . "draw:shadow-offset-x"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "draw:shadow-offset-y"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "draw:shadow-color"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "draw:shadow-opacity"))
			      (ref
			       ((name . "zeroToHundredPercent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:start-line-spacing-horizontal"))
			      (ref
			       ((name . "distance")))))
		   (optional nil
			     (attribute
			      ((name . "draw:start-line-spacing-vertical"))
			      (ref
			       ((name . "distance")))))
		   (optional nil
			     (attribute
			      ((name . "draw:end-line-spacing-horizontal"))
			      (ref
			       ((name . "distance")))))
		   (optional nil
			     (attribute
			      ((name . "draw:end-line-spacing-vertical"))
			      (ref
			       ((name . "distance")))))
		   (optional nil
			     (attribute
			      ((name . "draw:line-distance"))
			      (ref
			       ((name . "distance")))))
		   (optional nil
			     (attribute
			      ((name . "draw:guide-overhang"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "draw:guide-distance"))
			      (ref
			       ((name . "distance")))))
		   (optional nil
			     (attribute
			      ((name . "draw:start-guide"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "draw:end-guide"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "draw:placing"))
			      (choice nil
				      (value nil "below")
				      (value nil "above"))))
		   (optional nil
			     (attribute
			      ((name . "draw:parallel"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:measure-align"))
			      (choice nil
				      (value nil "automatic")
				      (value nil "left-outside")
				      (value nil "inside")
				      (value nil "right-outside"))))
		   (optional nil
			     (attribute
			      ((name . "draw:measure-vertical-align"))
			      (choice nil
				      (value nil "automatic")
				      (value nil "above")
				      (value nil "below")
				      (value nil "center"))))
		   (optional nil
			     (attribute
			      ((name . "draw:unit"))
			      (choice nil
				      (value nil "automatic")
				      (value nil "mm")
				      (value nil "cm")
				      (value nil "m")
				      (value nil "km")
				      (value nil "pt")
				      (value nil "pc")
				      (value nil "inch")
				      (value nil "ft")
				      (value nil "mi"))))
		   (optional nil
			     (attribute
			      ((name . "draw:show-unit"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:decimal-places"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (optional nil
			     (attribute
			      ((name . "draw:caption-type"))
			      (choice nil
				      (value nil "straight-line")
				      (value nil "angled-line")
				      (value nil "angled-connector-line"))))
		   (optional nil
			     (attribute
			      ((name . "draw:caption-angle-type"))
			      (choice nil
				      (value nil "fixed")
				      (value nil "free"))))
		   (optional nil
			     (attribute
			      ((name . "draw:caption-angle"))
			      (ref
			       ((name . "angle")))))
		   (optional nil
			     (attribute
			      ((name . "draw:caption-gap"))
			      (ref
			       ((name . "distance")))))
		   (optional nil
			     (attribute
			      ((name . "draw:caption-escape-direction"))
			      (choice nil
				      (value nil "horizontal")
				      (value nil "vertical")
				      (value nil "auto"))))
		   (optional nil
			     (attribute
			      ((name . "draw:caption-escape"))
			      (choice nil
				      (ref
				       ((name . "length")))
				      (ref
				       ((name . "percent"))))))
		   (optional nil
			     (attribute
			      ((name . "draw:caption-line-length"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "draw:caption-fit-line-length"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:horizontal-segments"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:vertical-segments"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:edge-rounding"))
			      (ref
			       ((name . "percent")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:edge-rounding-mode"))
			      (choice nil
				      (value nil "correct")
				      (value nil "attractive"))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:back-scale"))
			      (ref
			       ((name . "percent")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:depth"))
			      (ref
			       ((name . "length")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:backface-culling"))
			      (choice nil
				      (value nil "enabled")
				      (value nil "disabled"))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:end-angle"))
			      (ref
			       ((name . "angle")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:close-front"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:close-back"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:lighting-mode"))
			      (choice nil
				      (value nil "standard")
				      (value nil "double-sided"))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:normals-kind"))
			      (choice nil
				      (value nil "object")
				      (value nil "flat")
				      (value nil "sphere"))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:normals-direction"))
			      (choice nil
				      (value nil "normal")
				      (value nil "inverse"))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:texture-generation-mode-x"))
			      (choice nil
				      (value nil "object")
				      (value nil "parallel")
				      (value nil "sphere"))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:texture-generation-mode-y"))
			      (choice nil
				      (value nil "object")
				      (value nil "parallel")
				      (value nil "sphere"))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:texture-kind"))
			      (choice nil
				      (value nil "luminance")
				      (value nil "intensity")
				      (value nil "color"))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:texture-filter"))
			      (choice nil
				      (value nil "enabled")
				      (value nil "disabled"))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:texture-mode"))
			      (choice nil
				      (value nil "replace")
				      (value nil "modulate")
				      (value nil "blend"))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:ambient-color"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:emissive-color"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:specular-color"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:diffuse-color"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:shininess"))
			      (ref
			       ((name . "percent")))))
		   (optional nil
			     (attribute
			      ((name . "dr3d:shadow"))
			      (choice nil
				      (value nil "visible")
				      (value nil "hidden"))))
		   (ref
		    ((name . "common-draw-rel-size-attlist")))
		   (optional nil
			     (attribute
			      ((name . "fo:min-width"))
			      (choice nil
				      (ref
				       ((name . "length")))
				      (ref
				       ((name . "percent"))))))
		   (optional nil
			     (attribute
			      ((name . "fo:min-height"))
			      (choice nil
				      (ref
				       ((name . "length")))
				      (ref
				       ((name . "percent"))))))
		   (optional nil
			     (attribute
			      ((name . "fo:max-height"))
			      (choice nil
				      (ref
				       ((name . "length")))
				      (ref
				       ((name . "percent"))))))
		   (optional nil
			     (attribute
			      ((name . "fo:max-width"))
			      (choice nil
				      (ref
				       ((name . "length")))
				      (ref
				       ((name . "percent"))))))
		   (ref
		    ((name . "common-horizontal-margin-attlist")))
		   (ref
		    ((name . "common-vertical-margin-attlist")))
		   (ref
		    ((name . "common-margin-attlist")))
		   (optional nil
			     (attribute
			      ((name . "style:print-content"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "style:protect"))
			      (choice nil
				      (value nil "none")
				      (list nil
					    (oneOrMore nil
						       (choice nil
							       (value nil "content")
							       (value nil "position")
							       (value nil "size")))))))
		   (optional nil
			     (attribute
			      ((name . "style:horizontal-pos"))
			      (choice nil
				      (value nil "left")
				      (value nil "center")
				      (value nil "right")
				      (value nil "from-left")
				      (value nil "inside")
				      (value nil "outside")
				      (value nil "from-inside"))))
		   (optional nil
			     (attribute
			      ((name . "svg:x"))
			      (ref
			       ((name . "coordinate")))))
		   (optional nil
			     (attribute
			      ((name . "style:horizontal-rel"))
			      (choice nil
				      (value nil "page")
				      (value nil "page-content")
				      (value nil "page-start-margin")
				      (value nil "page-end-margin")
				      (value nil "frame")
				      (value nil "frame-content")
				      (value nil "frame-start-margin")
				      (value nil "frame-end-margin")
				      (value nil "paragraph")
				      (value nil "paragraph-content")
				      (value nil "paragraph-start-margin")
				      (value nil "paragraph-end-margin")
				      (value nil "char"))))
		   (ref
		    ((name . "common-vertical-pos-attlist")))
		   (ref
		    ((name . "common-vertical-rel-attlist")))
		   (ref
		    ((name . "common-text-anchor-attlist")))
		   (ref
		    ((name . "common-border-attlist")))
		   (ref
		    ((name . "common-border-line-width-attlist")))
		   (ref
		    ((name . "common-padding-attlist")))
		   (ref
		    ((name . "common-shadow-attlist")))
		   (ref
		    ((name . "common-background-color-attlist")))
		   (ref
		    ((name . "common-background-transparency-attlist")))
		   (ref
		    ((name . "common-editable-attlist")))
		   (optional nil
			     (attribute
			      ((name . "style:wrap"))
			      (choice nil
				      (value nil "none")
				      (value nil "left")
				      (value nil "right")
				      (value nil "parallel")
				      (value nil "dynamic")
				      (value nil "run-through")
				      (value nil "biggest"))))
		   (optional nil
			     (attribute
			      ((name . "style:wrap-dynamic-threshold"))
			      (ref
			       ((name . "nonNegativeLength")))))
		   (optional nil
			     (attribute
			      ((name . "style:number-wrapped-paragraphs"))
			      (choice nil
				      (value nil "no-limit")
				      (ref
				       ((name . "positiveInteger"))))))
		   (optional nil
			     (attribute
			      ((name . "style:wrap-contour"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "style:wrap-contour-mode"))
			      (choice nil
				      (value nil "full")
				      (value nil "outside"))))
		   (optional nil
			     (attribute
			      ((name . "style:run-through"))
			      (choice nil
				      (value nil "foreground")
				      (value nil "background"))))
		   (optional nil
			     (attribute
			      ((name . "style:flow-with-text"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "style:overflow-behavior"))
			      (choice nil
				      (value nil "clip")
				      (value nil "auto-create-new-frame"))))
		   (optional nil
			     (attribute
			      ((name . "style:mirror"))
			      (choice nil
				      (value nil "none")
				      (value nil "vertical")
				      (ref
				       ((name . "horizontal-mirror")))
				      (list nil
					    (value nil "vertical")
					    (ref
					     ((name . "horizontal-mirror"))))
				      (list nil
					    (ref
					     ((name . "horizontal-mirror")))
					    (value nil "vertical")))))
		   (optional nil
			     (attribute
			      ((name . "fo:clip"))
			      (choice nil
				      (value nil "auto")
				      (ref
				       ((name . "clipShape"))))))
		   (optional nil
			     (attribute
			      ((name . "draw:wrap-influence-on-position"))
			      (choice nil
				      (value nil "iterative")
				      (value nil "once-concurrent")
				      (value nil "once-successive"))))
		   (ref
		    ((name . "common-writing-mode-attlist")))
		   (optional nil
			     (attribute
			      ((name . "draw:frame-display-scrollbar"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:frame-display-border"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:frame-margin-horizontal"))
			      (ref
			       ((name . "nonNegativePixelLength")))))
		   (optional nil
			     (attribute
			      ((name . "draw:frame-margin-vertical"))
			      (ref
			       ((name . "nonNegativePixelLength")))))
		   (optional nil
			     (attribute
			      ((name . "draw:visible-area-left"))
			      (ref
			       ((name . "nonNegativeLength")))))
		   (optional nil
			     (attribute
			      ((name . "draw:visible-area-top"))
			      (ref
			       ((name . "nonNegativeLength")))))
		   (optional nil
			     (attribute
			      ((name . "draw:visible-area-width"))
			      (ref
			       ((name . "positiveLength")))))
		   (optional nil
			     (attribute
			      ((name . "draw:visible-area-height"))
			      (ref
			       ((name . "positiveLength")))))
		   (optional nil
			     (attribute
			      ((name . "draw:draw-aspect"))
			      (choice nil
				      (value nil "content")
				      (value nil "thumbnail")
				      (value nil "icon")
				      (value nil "print-view"))))
		   (optional nil
			     (attribute
			      ((name . "draw:ole-draw-aspect"))
			      (ref
			       ((name . "nonNegativeInteger")))))))
      (define
       ((name . "style-graphic-fill-properties-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "draw:fill"))
			      (choice nil
				      (value nil "none")
				      (value nil "solid")
				      (value nil "bitmap")
				      (value nil "gradient")
				      (value nil "hatch"))))
		   (optional nil
			     (attribute
			      ((name . "draw:fill-color"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "draw:secondary-fill-color"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "draw:fill-gradient-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "draw:gradient-step-count"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (optional nil
			     (attribute
			      ((name . "draw:fill-hatch-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "draw:fill-hatch-solid"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "draw:fill-image-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "style:repeat"))
			      (choice nil
				      (value nil "no-repeat")
				      (value nil "repeat")
				      (value nil "stretch"))))
		   (optional nil
			     (attribute
			      ((name . "draw:fill-image-width"))
			      (choice nil
				      (ref
				       ((name . "length")))
				      (ref
				       ((name . "percent"))))))
		   (optional nil
			     (attribute
			      ((name . "draw:fill-image-height"))
			      (choice nil
				      (ref
				       ((name . "length")))
				      (ref
				       ((name . "percent"))))))
		   (optional nil
			     (attribute
			      ((name . "draw:fill-image-ref-point-x"))
			      (ref
			       ((name . "percent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:fill-image-ref-point-y"))
			      (ref
			       ((name . "percent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:fill-image-ref-point"))
			      (choice nil
				      (value nil "top-left")
				      (value nil "top")
				      (value nil "top-right")
				      (value nil "left")
				      (value nil "center")
				      (value nil "right")
				      (value nil "bottom-left")
				      (value nil "bottom")
				      (value nil "bottom-right"))))
		   (optional nil
			     (attribute
			      ((name . "draw:tile-repeat-offset"))
			      (list nil
				    (ref
				     ((name . "zeroToHundredPercent")))
				    (choice nil
					    (value nil "horizontal")
					    (value nil "vertical")))))
		   (optional nil
			     (attribute
			      ((name . "draw:opacity"))
			      (ref
			       ((name . "zeroToHundredPercent")))))
		   (optional nil
			     (attribute
			      ((name . "draw:opacity-name"))
			      (ref
			       ((name . "styleNameRef")))))
		   (optional nil
			     (attribute
			      ((name . "svg:fill-rule"))
			      (choice nil
				      (value nil "nonzero")
				      (value nil "evenodd"))))))
      (define
       ((name . "style-graphic-properties-elements"))
       (interleave nil
		   (optional nil
			     (ref
			      ((name . "text-list-style"))))
		   (ref
		    ((name . "style-background-image")))
		   (ref
		    ((name . "style-columns")))))
      (define
       ((name . "common-vertical-pos-attlist"))
       (optional nil
		 (attribute
		  ((name . "style:vertical-pos"))
		  (choice nil
			  (value nil "top")
			  (value nil "middle")
			  (value nil "bottom")
			  (value nil "from-top")
			  (value nil "below"))))
       (optional nil
		 (attribute
		  ((name . "svg:y"))
		  (ref
		   ((name . "coordinate"))))))
      (define
       ((name . "common-vertical-rel-attlist"))
       (optional nil
		 (attribute
		  ((name . "style:vertical-rel"))
		  (choice nil
			  (value nil "page")
			  (value nil "page-content")
			  (value nil "frame")
			  (value nil "frame-content")
			  (value nil "paragraph")
			  (value nil "paragraph-content")
			  (value nil "char")
			  (value nil "line")
			  (value nil "baseline")
			  (value nil "text")))))
      (define
       ((name . "common-editable-attlist"))
       (optional nil
		 (attribute
		  ((name . "style:editable"))
		  (ref
		   ((name . "boolean"))))))
      (define
       ((name . "horizontal-mirror"))
       (choice nil
	       (value nil "horizontal")
	       (value nil "horizontal-on-odd")
	       (value nil "horizontal-on-even")))
      (define
       ((name . "clipShape"))
       (data
	((type . "string"))
	(param
	 ((name . "pattern"))
	 "rect\\([ ]*((-?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc)))|(auto))([ ]*,[ ]*((-?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc))))|(auto)){3}[ ]*\\)")))
      (define
       ((name . "nonNegativePixelLength"))
       (data
	((type . "string"))
	(param
	 ((name . "pattern"))
	 "([0-9]+(\\.[0-9]*)?|\\.[0-9]+)(px)")))
      (define
       ((name . "style-chart-properties"))
       (element
	((name . "style:chart-properties"))
	(ref
	 ((name . "style-chart-properties-content-strict")))))
      (define
       ((name . "style-chart-properties-content-strict"))
       (ref
	((name . "style-chart-properties-attlist")))
       (ref
	((name . "style-chart-properties-elements"))))
      (define
       ((name . "style-chart-properties-elements"))
       (empty nil))
      (define
       ((name . "style-chart-properties-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "chart:scale-text"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:three-dimensional"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:deep"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:right-angled-axes"))
			      (ref
			       ((name . "boolean")))))
		   (choice nil
			   (attribute
			    ((name . "chart:symbol-type"))
			    (value nil "none"))
			   (attribute
			    ((name . "chart:symbol-type"))
			    (value nil "automatic"))
			   (group nil
				  (attribute
				   ((name . "chart:symbol-type"))
				   (value nil "named-symbol"))
				  (attribute
				   ((name . "chart:symbol-name"))
				   (choice nil
					   (value nil "square")
					   (value nil "diamond")
					   (value nil "arrow-down")
					   (value nil "arrow-up")
					   (value nil "arrow-right")
					   (value nil "arrow-left")
					   (value nil "bow-tie")
					   (value nil "hourglass")
					   (value nil "circle")
					   (value nil "star")
					   (value nil "x")
					   (value nil "plus")
					   (value nil "asterisk")
					   (value nil "horizontal-bar")
					   (value nil "vertical-bar"))))
			   (group nil
				  (attribute
				   ((name . "chart:symbol-type"))
				   (value nil "image"))
				  (element
				   ((name . "chart:symbol-image"))
				   (attribute
				    ((name . "xlink:href"))
				    (ref
				     ((name . "anyIRI"))))))
			   (empty nil))
		   (optional nil
			     (attribute
			      ((name . "chart:symbol-width"))
			      (ref
			       ((name . "nonNegativeLength")))))
		   (optional nil
			     (attribute
			      ((name . "chart:symbol-height"))
			      (ref
			       ((name . "nonNegativeLength")))))
		   (optional nil
			     (attribute
			      ((name . "chart:sort-by-x-values"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:vertical"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:connect-bars"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:gap-width"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "chart:overlap"))
			      (ref
			       ((name . "integer")))))
		   (optional nil
			     (attribute
			      ((name . "chart:group-bars-per-axis"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:japanese-candle-stick"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:interpolation"))
			      (choice nil
				      (value nil "none")
				      (value nil "cubic-spline")
				      (value nil "b-spline"))))
		   (optional nil
			     (attribute
			      ((name . "chart:spline-order"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "chart:spline-resolution"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "chart:pie-offset"))
			      (ref
			       ((name . "nonNegativeInteger")))))
		   (optional nil
			     (attribute
			      ((name . "chart:angle-offset"))
			      (ref
			       ((name . "angle")))))
		   (optional nil
			     (attribute
			      ((name . "chart:hole-size"))
			      (ref
			       ((name . "percent")))))
		   (optional nil
			     (attribute
			      ((name . "chart:lines"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:solid-type"))
			      (choice nil
				      (value nil "cuboid")
				      (value nil "cylinder")
				      (value nil "cone")
				      (value nil "pyramid"))))
		   (optional nil
			     (attribute
			      ((name . "chart:stacked"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:percentage"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:treat-empty-cells"))
			      (choice nil
				      (value nil "use-zero")
				      (value nil "leave-gap")
				      (value nil "ignore"))))
		   (optional nil
			     (attribute
			      ((name . "chart:link-data-style-to-source"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:logarithmic"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:maximum"))
			      (ref
			       ((name . "double")))))
		   (optional nil
			     (attribute
			      ((name . "chart:minimum"))
			      (ref
			       ((name . "double")))))
		   (optional nil
			     (attribute
			      ((name . "chart:origin"))
			      (ref
			       ((name . "double")))))
		   (optional nil
			     (attribute
			      ((name . "chart:interval-major"))
			      (ref
			       ((name . "double")))))
		   (optional nil
			     (attribute
			      ((name . "chart:interval-minor-divisor"))
			      (ref
			       ((name . "positiveInteger")))))
		   (optional nil
			     (attribute
			      ((name . "chart:tick-marks-major-inner"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:tick-marks-major-outer"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:tick-marks-minor-inner"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:tick-marks-minor-outer"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:reverse-direction"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:display-label"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:text-overlap"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "text:line-break"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:label-arrangement"))
			      (choice nil
				      (value nil "side-by-side")
				      (value nil "stagger-even")
				      (value nil "stagger-odd"))))
		   (ref
		    ((name . "common-style-direction-attlist")))
		   (ref
		    ((name . "common-rotation-angle-attlist")))
		   (optional nil
			     (attribute
			      ((name . "chart:data-label-number"))
			      (choice nil
				      (value nil "none")
				      (value nil "value")
				      (value nil "percentage")
				      (value nil "value-and-percentage"))))
		   (optional nil
			     (attribute
			      ((name . "chart:data-label-text"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:data-label-symbol"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (element
			      ((name . "chart:label-separator"))
			      (ref
			       ((name . "text-p")))))
		   (optional nil
			     (attribute
			      ((name . "chart:label-position"))
			      (ref
			       ((name . "labelPositions")))))
		   (optional nil
			     (attribute
			      ((name . "chart:label-position-negative"))
			      (ref
			       ((name . "labelPositions")))))
		   (optional nil
			     (attribute
			      ((name . "chart:visible"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:auto-position"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:auto-size"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:mean-value"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:error-category"))
			      (choice nil
				      (value nil "none")
				      (value nil "variance")
				      (value nil "standard-deviation")
				      (value nil "percentage")
				      (value nil "error-margin")
				      (value nil "constant")
				      (value nil "standard-error")
				      (value nil "cell-range"))))
		   (optional nil
			     (attribute
			      ((name . "chart:error-percentage"))
			      (ref
			       ((name . "double")))))
		   (optional nil
			     (attribute
			      ((name . "chart:error-margin"))
			      (ref
			       ((name . "double")))))
		   (optional nil
			     (attribute
			      ((name . "chart:error-lower-limit"))
			      (ref
			       ((name . "double")))))
		   (optional nil
			     (attribute
			      ((name . "chart:error-upper-limit"))
			      (ref
			       ((name . "double")))))
		   (optional nil
			     (attribute
			      ((name . "chart:error-upper-indicator"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:error-lower-indicator"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "chart:error-lower-range"))
			      (ref
			       ((name . "cellRangeAddressList")))))
		   (optional nil
			     (attribute
			      ((name . "chart:error-upper-range"))
			      (ref
			       ((name . "cellRangeAddressList")))))
		   (optional nil
			     (attribute
			      ((name . "chart:series-source"))
			      (choice nil
				      (value nil "columns")
				      (value nil "rows"))))
		   (optional nil
			     (attribute
			      ((name . "chart:regression-type"))
			      (choice nil
				      (value nil "none")
				      (value nil "linear")
				      (value nil "logarithmic")
				      (value nil "exponential")
				      (value nil "power"))))
		   (optional nil
			     (attribute
			      ((name . "chart:axis-position"))
			      (choice nil
				      (value nil "start")
				      (value nil "end")
				      (ref
				       ((name . "double"))))))
		   (optional nil
			     (attribute
			      ((name . "chart:axis-label-position"))
			      (choice nil
				      (value nil "near-axis")
				      (value nil "near-axis-other-side")
				      (value nil "outside-start")
				      (value nil "outside-end"))))
		   (optional nil
			     (attribute
			      ((name . "chart:tick-mark-position"))
			      (choice nil
				      (value nil "at-labels")
				      (value nil "at-axis")
				      (value nil "at-labels-and-axis"))))
		   (optional nil
			     (attribute
			      ((name . "chart:include-hidden-cells"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "labelPositions"))
       (choice nil
	       (value nil "avoid-overlap")
	       (value nil "center")
	       (value nil "top")
	       (value nil "top-right")
	       (value nil "right")
	       (value nil "bottom-right")
	       (value nil "bottom")
	       (value nil "bottom-left")
	       (value nil "left")
	       (value nil "top-left")
	       (value nil "inside")
	       (value nil "outside")
	       (value nil "near-origin")))
      (define
       ((name . "style-drawing-page-properties-attlist"))
       (interleave nil
		   (optional nil
			     (attribute
			      ((name . "presentation:transition-type"))
			      (choice nil
				      (value nil "manual")
				      (value nil "automatic")
				      (value nil "semi-automatic"))))
		   (optional nil
			     (attribute
			      ((name . "presentation:transition-style"))
			      (choice nil
				      (value nil "none")
				      (value nil "fade-from-left")
				      (value nil "fade-from-top")
				      (value nil "fade-from-right")
				      (value nil "fade-from-bottom")
				      (value nil "fade-from-upperleft")
				      (value nil "fade-from-upperright")
				      (value nil "fade-from-lowerleft")
				      (value nil "fade-from-lowerright")
				      (value nil "move-from-left")
				      (value nil "move-from-top")
				      (value nil "move-from-right")
				      (value nil "move-from-bottom")
				      (value nil "move-from-upperleft")
				      (value nil "move-from-upperright")
				      (value nil "move-from-lowerleft")
				      (value nil "move-from-lowerright")
				      (value nil "uncover-to-left")
				      (value nil "uncover-to-top")
				      (value nil "uncover-to-right")
				      (value nil "uncover-to-bottom")
				      (value nil "uncover-to-upperleft")
				      (value nil "uncover-to-upperright")
				      (value nil "uncover-to-lowerleft")
				      (value nil "uncover-to-lowerright")
				      (value nil "fade-to-center")
				      (value nil "fade-from-center")
				      (value nil "vertical-stripes")
				      (value nil "horizontal-stripes")
				      (value nil "clockwise")
				      (value nil "counterclockwise")
				      (value nil "open-vertical")
				      (value nil "open-horizontal")
				      (value nil "close-vertical")
				      (value nil "close-horizontal")
				      (value nil "wavyline-from-left")
				      (value nil "wavyline-from-top")
				      (value nil "wavyline-from-right")
				      (value nil "wavyline-from-bottom")
				      (value nil "spiralin-left")
				      (value nil "spiralin-right")
				      (value nil "spiralout-left")
				      (value nil "spiralout-right")
				      (value nil "roll-from-top")
				      (value nil "roll-from-left")
				      (value nil "roll-from-right")
				      (value nil "roll-from-bottom")
				      (value nil "stretch-from-left")
				      (value nil "stretch-from-top")
				      (value nil "stretch-from-right")
				      (value nil "stretch-from-bottom")
				      (value nil "vertical-lines")
				      (value nil "horizontal-lines")
				      (value nil "dissolve")
				      (value nil "random")
				      (value nil "vertical-checkerboard")
				      (value nil "horizontal-checkerboard")
				      (value nil "interlocking-horizontal-left")
				      (value nil "interlocking-horizontal-right")
				      (value nil "interlocking-vertical-top")
				      (value nil "interlocking-vertical-bottom")
				      (value nil "fly-away")
				      (value nil "open")
				      (value nil "close")
				      (value nil "melt"))))
		   (optional nil
			     (attribute
			      ((name . "presentation:transition-speed"))
			      (ref
			       ((name . "presentationSpeeds")))))
		   (optional nil
			     (attribute
			      ((name . "smil:type"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "smil:subtype"))
			      (ref
			       ((name . "string")))))
		   (optional nil
			     (attribute
			      ((name . "smil:direction"))
			      (choice nil
				      (value nil "forward")
				      (value nil "reverse"))))
		   (optional nil
			     (attribute
			      ((name . "smil:fadeColor"))
			      (ref
			       ((name . "color")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:duration"))
			      (ref
			       ((name . "duration")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:visibility"))
			      (choice nil
				      (value nil "visible")
				      (value nil "hidden"))))
		   (optional nil
			     (attribute
			      ((name . "draw:background-size"))
			      (choice nil
				      (value nil "full")
				      (value nil "border"))))
		   (optional nil
			     (attribute
			      ((name . "presentation:background-objects-visible"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:background-visible"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:display-header"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:display-footer"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:display-page-number"))
			      (ref
			       ((name . "boolean")))))
		   (optional nil
			     (attribute
			      ((name . "presentation:display-date-time"))
			      (ref
			       ((name . "boolean")))))))
      (define
       ((name . "style-drawing-page-properties-elements"))
       (optional nil
		 (ref
		  ((name . "presentation-sound")))))
      (define
       ((name . "string"))
       (data
	((type . "string"))))
      (define
       ((name . "date"))
       (data
	((type . "date"))))
      (define
       ((name . "time"))
       (data
	((type . "time"))))
      (define
       ((name . "dateTime"))
       (data
	((type . "dateTime"))))
      (define
       ((name . "duration"))
       (data
	((type . "duration"))))
      (define
       ((name . "integer"))
       (data
	((type . "integer"))))
      (define
       ((name . "nonNegativeInteger"))
       (data
	((type . "nonNegativeInteger"))))
      (define
       ((name . "positiveInteger"))
       (data
	((type . "positiveInteger"))))
      (define
       ((name . "double"))
       (data
	((type . "double"))))
      (define
       ((name . "anyURI"))
       (data
	((type . "anyURI"))))
      (define
       ((name . "base64Binary"))
       (data
	((type . "base64Binary"))))
      (define
       ((name . "ID"))
       (data
	((type . "ID"))))
      (define
       ((name . "IDREF"))
       (data
	((type . "IDREF"))))
      (define
       ((name . "IDREFS"))
       (data
	((type . "IDREFS"))))
      (define
       ((name . "NCName"))
       (data
	((type . "NCName"))))
      (define
       ((name . "boolean"))
       (choice nil
	       (value nil "true")
	       (value nil "false")))
      (define
       ((name . "dateOrDateTime"))
       (choice nil
	       (data
		((type . "date")))
	       (data
		((type . "dateTime")))))
      (define
       ((name . "timeOrDateTime"))
       (choice nil
	       (data
		((type . "time")))
	       (data
		((type . "dateTime")))))
      (define
       ((name . "language"))
       (data
	((type . "language"))))
      (define
       ((name . "countryCode"))
       (data
	((type . "token"))
	(param
	 ((name . "pattern"))
	 "[A-Za-z0-9]{1,8}")))
      (define
       ((name . "languageCode"))
       (data
	((type . "token"))
	(param
	 ((name . "pattern"))
	 "[A-Za-z]{1,8}")))
      (define
       ((name . "scriptCode"))
       (data
	((type . "token"))
	(param
	 ((name . "pattern"))
	 "[A-Za-z0-9]{1,8}")))
      (define
       ((name . "character"))
       (data
	((type . "string"))
	(param
	 ((name . "length"))
	 "1")))
      (define
       ((name . "length"))
       (data
	((type . "string"))
	(param
	 ((name . "pattern"))
	 "-?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc)|(px))")))
      (define
       ((name . "nonNegativeLength"))
       (data
	((type . "string"))
	(param
	 ((name . "pattern"))
	 "([0-9]+(\\.[0-9]*)?|\\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc)|(px))")))
      (define
       ((name . "positiveLength"))
       (data
	((type . "string"))
	(param
	 ((name . "pattern"))
	 "([0-9]*[1-9][0-9]*(\\.[0-9]*)?|0+\\.[0-9]*[1-9][0-9]*|\\.[0-9]*[1-9][0-9]*)((cm)|(mm)|(in)|(pt)|(pc)|(px))")))
      (define
       ((name . "percent"))
       (data
	((type . "string"))
	(param
	 ((name . "pattern"))
	 "-?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)%")))
      (define
       ((name . "zeroToHundredPercent"))
       (data
	((type . "string"))
	(param
	 ((name . "pattern"))
	 "([0-9]?[0-9](\\.[0-9]*)?|100(\\.0*)?|\\.[0-9]+)%")))
      (define
       ((name . "signedZeroToHundredPercent"))
       (data
	((type . "string"))
	(param
	 ((name . "pattern"))
	 "-?([0-9]?[0-9](\\.[0-9]*)?|100(\\.0*)?|\\.[0-9]+)%")))
      (define
       ((name . "relativeLength"))
       (data
	((type . "string"))
	(param
	 ((name . "pattern"))
	 "[0-9]+\\*")))
      (define
       ((name . "coordinate"))
       (ref
	((name . "length"))))
      (define
       ((name . "distance"))
       (ref
	((name . "length"))))
      (define
       ((name . "color"))
       (data
	((type . "string"))
	(param
	 ((name . "pattern"))
	 "#[0-9a-fA-F]{6}")))
      (define
       ((name . "angle"))
       (data
	((type . "string"))))
      (define
       ((name . "CURIE"))
       (data
	((type . "string"))
	(param
	 ((name . "pattern"))
	 "(([\\i-[:]][\\c-[:]]*)?:)?.+")
	(param
	 ((name . "minLength"))
	 "1")))
      (define
       ((name . "CURIEs"))
       (list nil
	     (oneOrMore nil
			(ref
			 ((name . "CURIE"))))))
      (define
       ((name . "SafeCURIE"))
       (data
	((type . "string"))
	(param
	 ((name . "pattern"))
	 "\\[(([\\i-[:]][\\c-[:]]*)?:)?.+\\]")
	(param
	 ((name . "minLength"))
	 "3")))
      (define
       ((name . "URIorSafeCURIE"))
       (choice nil
	       (ref
		((name . "anyURI")))
	       (ref
		((name . "SafeCURIE")))))
      (define
       ((name . "styleName"))
       (data
	((type . "NCName"))))
      (define
       ((name . "styleNameRef"))
       (choice nil
	       (data
		((type . "NCName")))
	       (empty nil)))
      (define
       ((name . "styleNameRefs"))
       (list nil
	     (zeroOrMore nil
			 (data
			  ((type . "NCName"))))))
      (define
       ((name . "variableName"))
       (data
	((type . "string"))))
      (define
       ((name . "targetFrameName"))
       (choice nil
	       (value nil "_self")
	       (value nil "_blank")
	       (value nil "_parent")
	       (value nil "_top")
	       (ref
		((name . "string")))))
      (define
       ((name . "valueType"))
       (choice nil
	       (value nil "float")
	       (value nil "time")
	       (value nil "date")
	       (value nil "percentage")
	       (value nil "currency")
	       (value nil "boolean")
	       (value nil "string")))
      (define
       ((name . "points"))
       (data
	((type . "string"))
	(param
	 ((name . "pattern"))
	 "-?[0-9]+,-?[0-9]+([ ]+-?[0-9]+,-?[0-9]+)*")))
      (define
       ((name . "pathData"))
       (data
	((type . "string"))))
      (define
       ((name . "vector3D"))
       (data
	((type . "string"))
	(param
	 ((name . "pattern"))
	 "\\([ ]*-?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([ ]+-?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)){2}[ ]*\\)")))
      (define
       ((name . "namespacedToken"))
       (data
	((type . "QName"))
	(param
	 ((name . "pattern"))
	 "[^:]+:[^:]+")))
      (define
       ((name . "anyIRI"))
       (data
	((type . "anyURI")))
       (description nil "An IRI-reference as defined in [RFC3987]. See ODF 1.2 Part 1 section 18.3."))
      (define
       ((name . "anyAttListOrElements"))
       (zeroOrMore nil
		   (attribute nil
			      (anyName nil)
			      (text nil)))
       (ref
	((name . "anyElements"))))
      (define
       ((name . "anyElements"))
       (zeroOrMore nil
		   (element nil
			    (anyName nil)
			    (mixed nil
				   (ref
				    ((name . "anyAttListOrElements")))))))))
