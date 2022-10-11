

;;;; style:tab-stop

(defclass style:tab-stop
  (odt-element)
  ((style:leader-style :initarg :leader-style :initform nil :type
		       (or null string))
   (style:leader-text :initarg :leader-text :initform nil :type
		      (or null string))
   (style:position :initarg :position :initform nil :type
		   (or null string))
   (style:type :initarg :type :initform nil :type
	       (or null string))))


;;;; style:tab-stops

(defclass style:tab-stops
  (odt-element)
  ((style:tab-stop :initarg :tab-stop :initform nil :type
		   (or null odt-element))))


;;;; style:text-properties

(defclass style:text-properties
  (odt-element)
  ((fo:background-color :initarg :background-color :initform nil :type
			(or null string))
   (fo:color :initarg :color :initform nil :type
	     (or null string))
   (fo:country :initarg :country :initform nil :type
	       (or null string))
   (fo:font-family :initarg :font-family :initform nil :type
		   (or null string))
   (fo:font-size :initarg :font-size :initform nil :type
		 (or null string))
   (fo:font-style :initarg :font-style :initform nil :type
		  (or null string))
   (fo:font-variant :initarg :font-variant :initform nil :type
		    (or null string))
   (fo:font-weight :initarg :font-weight :initform nil :type
		   (or null string))
   (fo:hyphenate :initarg :hyphenate :initform nil :type
		 (or null string))
   (fo:hyphenation-push-char-count :initarg :hyphenation-push-char-count :initform nil :type
				   (or null string))
   (fo:hyphenation-remain-char-count :initarg :hyphenation-remain-char-count :initform nil :type
				     (or null string))
   (fo:language :initarg :language :initform nil :type
		(or null string))
   (style:country-asian :initarg :country-asian :initform nil :type
			(or null string))
   (style:country-complex :initarg :country-complex :initform nil :type
			  (or null string))
   (style:font-charset :initarg :font-charset :initform nil :type
		       (or null string))
   (style:font-name :initarg :font-name :initform nil :type
		    (or null string))
   (style:font-name-asian :initarg :font-name-asian :initform nil :type
			  (or null string))
   (style:font-name-complex :initarg :font-name-complex :initform nil :type
			    (or null string))
   (style:font-size-asian :initarg :font-size-asian :initform nil :type
			  (or null string))
   (style:font-size-complex :initarg :font-size-complex :initform nil :type
			    (or null string))
   (style:font-style-asian :initarg :font-style-asian :initform nil :type
			   (or null string))
   (style:font-style-complex :initarg :font-style-complex :initform nil :type
			     (or null string))
   (style:font-weight-asian :initarg :font-weight-asian :initform nil :type
			    (or null string))
   (style:font-weight-complex :initarg :font-weight-complex :initform nil :type
			      (or null string))
   (style:language-asian :initarg :language-asian :initform nil :type
			 (or null string))
   (style:language-complex :initarg :language-complex :initform nil :type
			   (or null string))
   (style:letter-kerning :initarg :letter-kerning :initform nil :type
			 (or null string))
   (style:text-line-through-style :initarg :text-line-through-style :initform nil :type
				  (or null string))
   (style:text-position :initarg :text-position :initform nil :type
			(or null string))
   (style:text-underline-color :initarg :text-underline-color :initform nil :type
			       (or null string))
   (style:text-underline-style :initarg :text-underline-style :initform nil :type
			       (or null string))
   (style:text-underline-width :initarg :text-underline-width :initform nil :type
			       (or null string))
   (style:use-window-font-color :initarg :use-window-font-color :initform nil :type
				(or null string))))


;;;; style:paragraph-properties

(defclass style:paragraph-properties
  (odt-element)
  ((fo:background-color :initarg :background-color :initform nil :type
			(or null string))
   (fo:border :initarg :border :initform nil :type
	      (or null string))
   (fo:border-bottom :initarg :border-bottom :initform nil :type
		     (or null string))
   (fo:border-left :initarg :border-left :initform nil :type
		   (or null string))
   (fo:border-right :initarg :border-right :initform nil :type
		    (or null string))
   (fo:border-top :initarg :border-top :initform nil :type
		  (or null string))
   (fo:hyphenation-ladder-count :initarg :hyphenation-ladder-count :initform nil :type
				(or null string))
   (fo:keep-with-next :initarg :keep-with-next :initform nil :type
		      (or null string))
   (fo:margin-bottom :initarg :margin-bottom :initform nil :type
		     (or null string))
   (fo:margin-left :initarg :margin-left :initform nil :type
		   (or null string))
   (fo:margin-right :initarg :margin-right :initform nil :type
		    (or null string))
   (fo:margin-top :initarg :margin-top :initform nil :type
		  (or null string))
   (fo:padding :initarg :padding :initform nil :type
	       (or null string))
   (fo:text-align :initarg :text-align :initform nil :type
		  (or null string))
   (fo:text-indent :initarg :text-indent :initform nil :type
		   (or null string))
   (style:auto-text-indent :initarg :auto-text-indent :initform nil :type
			   (or null string))
   (style:font-independent-line-spacing :initarg :font-independent-line-spacing :initform nil :type
					(or null string))
   (style:join-border :initarg :join-border :initform nil :type
		      (or null string))
   (style:justify-single-word :initarg :justify-single-word :initform nil :type
			      (or null string))
   (style:line-break :initarg :line-break :initform nil :type
		     (or null string))
   (style:page-number :initarg :page-number :initform nil :type
		      (or null string))
   (style:punctuation-wrap :initarg :punctuation-wrap :initform nil :type
			   (or null string))
   (style:shadow :initarg :shadow :initform nil :type
		 (or null string))
   (style:tab-stop-distance :initarg :tab-stop-distance :initform nil :type
			    (or null string))
   (style:text-autospace :initarg :text-autospace :initform nil :type
			 (or null string))
   (style:writing-mode :initarg :writing-mode :initform nil :type
		       (or null string))
   (text:line-number :initarg :line-number :initform nil :type
		     (or null string))
   (text:number-lines :initarg :number-lines :initform nil :type
		      (or null string))
   (style:background-image :initarg :background-image :initform nil :type
			   (or null odt-element))
   (style:tab-stops :initarg :tab-stops :initform nil :type
		    (or null odt-element))))


;;;; style:graphic-properties

(defclass style:graphic-properties
  (odt-element)
  ((draw:blue :initarg :blue :initform nil :type
	      (or null string))
   (draw:color-inversion :initarg :color-inversion :initform nil :type
			 (or null string))
   (draw:color-mode :initarg :color-mode :initform nil :type
		    (or null string))
   (draw:contrast :initarg :contrast :initform nil :type
		  (or null string))
   (draw:end-line-spacing-horizontal :initarg :end-line-spacing-horizontal :initform nil :type
				     (or null string))
   (draw:end-line-spacing-vertical :initarg :end-line-spacing-vertical :initform nil :type
				   (or null string))
   (draw:fill :initarg :fill :initform nil :type
	      (or null string))
   (draw:fill-color :initarg :fill-color :initform nil :type
		    (or null string))
   (draw:gamma :initarg :gamma :initform nil :type
	       (or null string))
   (draw:green :initarg :green :initform nil :type
	       (or null string))
   (draw:image-opacity :initarg :image-opacity :initform nil :type
		       (or null string))
   (draw:luminance :initarg :luminance :initform nil :type
		   (or null string))
   (draw:ole-draw-aspect :initarg :ole-draw-aspect :initform nil :type
			 (or null string))
   (draw:red :initarg :red :initform nil :type
	     (or null string))
   (draw:shadow-offset-x :initarg :shadow-offset-x :initform nil :type
			 (or null string))
   (draw:shadow-offset-y :initarg :shadow-offset-y :initform nil :type
			 (or null string))
   (draw:shadow-opacity :initarg :shadow-opacity :initform nil :type
			(or null string))
   (draw:start-line-spacing-horizontal :initarg :start-line-spacing-horizontal :initform nil :type
				       (or null string))
   (draw:start-line-spacing-vertical :initarg :start-line-spacing-vertical :initform nil :type
				     (or null string))
   (draw:wrap-influence-on-position :initarg :wrap-influence-on-position :initform nil :type
				    (or null string))
   (fo:background-color :initarg :background-color :initform nil :type
			(or null string))
   (fo:border :initarg :border :initform nil :type
	      (or null string))
   (fo:clip :initarg :clip :initform nil :type
	    (or null string))
   (fo:margin-bottom :initarg :margin-bottom :initform nil :type
		     (or null string))
   (fo:margin-left :initarg :margin-left :initform nil :type
		   (or null string))
   (fo:margin-right :initarg :margin-right :initform nil :type
		    (or null string))
   (fo:margin-top :initarg :margin-top :initform nil :type
		  (or null string))
   (fo:min-height :initarg :min-height :initform nil :type
		  (or null string))
   (fo:padding :initarg :padding :initform nil :type
	       (or null string))
   (style:background-transparency :initarg :background-transparency :initform nil :type
				  (or null string))
   (style:flow-with-text :initarg :flow-with-text :initform nil :type
			 (or null string))
   (style:horizontal-pos :initarg :horizontal-pos :initform nil :type
			 (or null string))
   (style:horizontal-rel :initarg :horizontal-rel :initform nil :type
			 (or null string))
   (style:mirror :initarg :mirror :initform nil :type
		 (or null string))
   (style:number-wrapped-paragraphs :initarg :number-wrapped-paragraphs :initform nil :type
				    (or null string))
   (style:rel-width :initarg :rel-width :initform nil :type
		    (or null string))
   (style:run-through :initarg :run-through :initform nil :type
		      (or null string))
   (style:shadow :initarg :shadow :initform nil :type
		 (or null string))
   (style:vertical-pos :initarg :vertical-pos :initform nil :type
		       (or null string))
   (style:vertical-rel :initarg :vertical-rel :initform nil :type
		       (or null string))
   (style:wrap :initarg :wrap :initform nil :type
	       (or null string))
   (style:wrap-contour :initarg :wrap-contour :initform nil :type
		       (or null string))
   (svg:width :initarg :width :initform nil :type
	      (or null string))
   (svg:x :initarg :x :initform nil :type
	  (or null string))
   (svg:y :initarg :y :initform nil :type
	  (or null string))
   (text:anchor-type :initarg :anchor-type :initform nil :type
		     (or null string))
   (style:background-image :initarg :background-image :initform nil :type
			   (or null odt-element))))


;;;; LITERAL

(defclass LITERAL
  (odt-element)
  nil)


;;;; style:header-footer-properties

(defclass style:header-footer-properties
  (odt-element)
  ((fo:margin-left :initarg :margin-left :initform nil :type
		   (or null string))
   (fo:margin-right :initarg :margin-right :initform nil :type
		    (or null string))
   (fo:margin-top :initarg :margin-top :initform nil :type
		  (or null string))
   (fo:min-height :initarg :min-height :initform nil :type
		  (or null string))
   (style:dynamic-spacing :initarg :dynamic-spacing :initform nil :type
			  (or null string))))


;;;; style:footer-style

(defclass style:footer-style
  (odt-element)
  ((style:header-footer-properties :initarg :header-footer-properties :initform nil :type
				   (or null odt-element))))


;;;; style:header-style

(defclass style:header-style
  (odt-element)
  nil)


;;;; style:background-image

(defclass style:background-image
  (odt-element)
  nil)


;;;; style:footnote-sep

(defclass style:footnote-sep
  (odt-element)
  ((style:adjustment :initarg :adjustment :initform nil :type
		     (or null string))
   (style:color :initarg :color :initform nil :type
		(or null string))
   (style:distance-after-sep :initarg :distance-after-sep :initform nil :type
			     (or null string))
   (style:distance-before-sep :initarg :distance-before-sep :initform nil :type
			      (or null string))
   (style:line-style :initarg :line-style :initform nil :type
		     (or null string))
   (style:rel-width :initarg :rel-width :initform nil :type
		    (or null string))
   (style:width :initarg :width :initform nil :type
		(or null string))))


;;;; style:page-layout-properties

(defclass style:page-layout-properties
  (odt-element)
  ((fo:background-color :initarg :background-color :initform nil :type
			(or null string))
   (fo:margin-bottom :initarg :margin-bottom :initform nil :type
		     (or null string))
   (fo:margin-left :initarg :margin-left :initform nil :type
		   (or null string))
   (fo:margin-right :initarg :margin-right :initform nil :type
		    (or null string))
   (fo:margin-top :initarg :margin-top :initform nil :type
		  (or null string))
   (fo:page-height :initarg :page-height :initform nil :type
		   (or null string))
   (fo:page-width :initarg :page-width :initform nil :type
		  (or null string))
   (style:footnote-max-height :initarg :footnote-max-height :initform nil :type
			      (or null string))
   (style:num-format :initarg :num-format :initform nil :type
		     (or null string))
   (style:print-orientation :initarg :print-orientation :initform nil :type
			    (or null string))
   (style:writing-mode :initarg :writing-mode :initform nil :type
		       (or null string))
   (style:background-image :initarg :background-image :initform nil :type
			   (or null odt-element))
   (style:footnote-sep :initarg :footnote-sep :initform nil :type
		       (or null odt-element))))


;;;; style:page-layout

(defclass style:page-layout
  (odt-element)
  ((style:name :initarg :name :initform nil :type
	       (or null string))
   (style:page-usage :initarg :page-usage :initform nil :type
		     (or null string))
   (style:footer-style :initarg :footer-style :initform nil :type
		       (or null odt-element))
   (style:header-style :initarg :header-style :initform nil :type
		       (or null odt-element))
   (style:page-layout-properties :initarg :page-layout-properties :initform nil :type
				 (or null odt-element))))


;;;;; `style:page-layout' Usage

(style:page-layout :name "A4PortraitLayout" :page-usage "mirrored" :page-layout-properties
		   (style:page-layout-properties :page-width "21.001cm" :page-height "29.7cm" :num-format "1" :print-orientation "portrait" :margin-top "2cm" :margin-bottom "2cm" :margin-left "2cm" :margin-right "2cm" :writing-mode "lr-tb" :footnote-max-height "0cm" :footnote-sep
						 (style:footnote-sep :width "0.018cm" :distance-before-sep "0.101cm" :distance-after-sep "0.101cm" :line-style "solid" :adjustment "left" :rel-width "25%" :color "#000000"))
		   :header-style
		   (style:header-style)
		   :footer-style
		   (style:footer-style :header-footer-properties
				       (style:header-footer-properties :min-height "0.6cm" :margin-left "0cm" :margin-right "0cm" :margin-top "0.499cm" :dynamic-spacing "false")))


;;;; style:style

(defclass style:style
  (odt-element)
  ((style:class :initarg :class :initform nil :type
		(or null string))
   (style:default-outline-level :initarg :default-outline-level :initform nil :type
				(or null string))
   (style:display-name :initarg :display-name :initform nil :type
		       (or null string))
   (style:family :initarg :family :initform nil :type
		 (or null string))
   (style:list-style-name :initarg :list-style-name :initform nil :type
			  (or null string))
   (style:name :initarg :name :initform nil :type
	       (or null string))
   (style:next-style-name :initarg :next-style-name :initform nil :type
			  (or null string))
   (style:parent-style-name :initarg :parent-style-name :initform nil :type
			    (or null string))
   (LITERAL :initarg :LITERAL :initform nil :type
	    (or null odt-element))
   (style:graphic-properties :initarg :graphic-properties :initform nil :type
			     (or null odt-element))
   (style:paragraph-properties :initarg :paragraph-properties :initform nil :type
			       (or null odt-element))
   (style:text-properties :initarg :text-properties :initform nil :type
			  (or null odt-element))))


;;;;; `style:style' Usage

(style:style :name "Bold" :family "text" :text-properties
	     (style:text-properties :font-weight "bold"))


(style:style :name "Text_20_body" :display-name "Text body" :family "paragraph" :parent-style-name "Standard" :class "text" :paragraph-properties
	     (style:paragraph-properties :margin-top "0cm" :margin-bottom "0.212cm"))
