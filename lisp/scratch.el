(require 'odt)

(odt-stylesdom:style-name->node org-odt-styles-dom "Text_20_body")

(style:style
 ((style:name . "Text_20_body")
  (style:display-name . "Text body")
  (style:family . "paragraph")
  (style:parent-style-name . "Standard")
  (style:class . "text"))
 (style:paragraph-properties
  ((fo:margin-top . "0cm")
   (fo:margin-bottom . "0.212cm"))))

(odt-stylesdom:inspect-type  'style:style org-odt-styles-dom)

(:parent-tags
 (office:styles office:automatic-styles)
 :attributes
 (style:class style:default-outline-level style:display-name style:family style:list-style-name style:name style:next-style-name style:parent-style-name)
 :child-tags
 (LITERAL style:graphic-properties style:paragraph-properties style:text-properties))

 ;; (odt-rngdom:name->define-node "style-style-attlist")

;; (odt-rngdom:do-referenced-ref-names
;;  (odt-rngdom:name->define-node "style-style-attlist")
;;  odt-rngdom:odf-v1.2-os-schema)

;; (odt-rngdom:name->define-node "style:graphic-properties")

;; (setq target
;;       (odt-rngdom:ref-name->ref-names
;;        (odt-rngdom:do-collect-names-of-ref-nodes
;; 	(odt-rngdom:name->element-node "style:graphic-properties"))))

;; (setq rng (odt-rngdom:ref-name->ref-names
;;            "style:graphic-properties"
;;            odt-rngdom:odf-v1.2-os-schema))



(setq rng-target
      (odt-rngdom:ref-name->ref-names "style-graphic-properties-content-strict"
                                      (odt-rngdom:remove-define-data
                                       (odt-rngdom:remove-nodes-of-type 'data odt-rngdom:odf-v1.2-os-schema))))

((define
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
  ((name . "horizontal-mirror"))
  (choice nil
          (value nil "horizontal")
          (value nil "horizontal-on-odd")
          (value nil "horizontal-on-even")))
 (define
  ((name . "common-editable-attlist"))
  (optional nil
            (attribute
             ((name . "style:editable"))
             (ref
              ((name . "boolean"))))))
 (define
  ((name . "common-background-transparency-attlist"))
  (optional nil
            (attribute
             ((name . "style:background-transparency"))
             (ref
              ((name . "zeroToHundredPercent"))))))
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
  ((name . "common-shadow-attlist"))
  (optional nil
            (attribute
             ((name . "style:shadow"))
             (ref
              ((name . "shadowType"))))))
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
  ((name . "borderWidths"))
  (list nil
        (ref
         ((name . "positiveLength")))
        (ref
         ((name . "positiveLength")))
        (ref
         ((name . "positiveLength")))))
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
  ((name . "coordinate"))
  (ref
   ((name . "length"))))
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
  ((name . "distance"))
  (ref
   ((name . "length"))))
 (define
  ((name . "boolean"))
  (choice nil
          (value nil "true")
          (value nil "false")))
 (define
  ((name . "styleNameRefs"))
  (list nil
        (zeroOrMore nil)))
 (define
  ((name . "styleNameRef"))
  (choice nil
          (empty nil)))
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
  ((name . "text-list-level-style-attr"))
  (attribute
   ((name . "text:level"))
   (ref
    ((name . "positiveInteger")))))
 (define
  ((name . "style-num-letter-sync-attlist"))
  (optional nil
            (attribute
             ((name . "style:num-letter-sync"))
             (ref
              ((name . "boolean"))))))
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
  ((name . "style-list-level-label-alignment"))
  (optional nil
            (element
             ((name . "style:list-level-label-alignment"))
             (ref
              ((name . "style-list-level-label-alignment-attlist")))
             (empty nil))))
 (define
  ((name . "style-list-level-properties-elements"))
  (ref
   ((name . "style-list-level-label-alignment"))))
 (define
  ((name . "style-list-level-properties-content-strict"))
  (ref
   ((name . "style-list-level-properties-attlist")))
  (ref
   ((name . "style-list-level-properties-elements"))))
 (define
  ((name . "style-list-level-properties"))
  (element
   ((name . "style:list-level-properties"))
   (ref
    ((name . "style-list-level-properties-content-strict")))))
 (define
  ((name . "fontVariant"))
  (choice nil
          (value nil "normal")
          (value nil "small-caps")))
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
  ((name . "lineMode"))
  (choice nil
          (value nil "continuous")
          (value nil "skip-white-space")))
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
  ((name . "style-text-properties-elements"))
  (empty nil))
 (define
  ((name . "style-text-properties-content-strict"))
  (ref
   ((name . "style-text-properties-attlist")))
  (ref
   ((name . "style-text-properties-elements"))))
 (define
  ((name . "style-text-properties"))
  (element
   ((name . "style:text-properties"))
   (ref
    ((name . "style-text-properties-content-strict")))))
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
  ((name . "text-list-style"))
  (element
   ((name . "text:list-style"))
   (ref
    ((name . "text-list-style-attr")))
   (zeroOrMore nil
               (ref
                ((name . "text-list-style-content"))))))
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
  ((name . "style-column-sep"))
  (element
   ((name . "style:column-sep"))
   (ref
    ((name . "style-column-sep-attlist")))))
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
  ((name . "style-column"))
  (element
   ((name . "style:column"))
   (ref
    ((name . "style-column-attlist")))))
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
  ((name . "style-graphic-properties-content-strict"))
  (ref
   ((name . "style-graphic-properties-attlist")))
  (ref
   ((name . "style-graphic-fill-properties-attlist")))
  (ref
   ((name . "style-graphic-properties-elements")))))

(nil nil
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
     nil
     (define
      ((name . "horizontal-mirror"))
      (choice nil
              (value nil "horizontal")
              (value nil "horizontal-on-odd")
              (value nil "horizontal-on-even")))
     nil nil
     (define
      ((name . "common-editable-attlist"))
      (optional nil
                (attribute
                 ((name . "style:editable"))
                 (ref
                  ((name . "boolean"))))))
     (define
      ((name . "common-background-transparency-attlist"))
      (optional nil
                (attribute
                 ((name . "style:background-transparency"))
                 (ref
                  ((name . "zeroToHundredPercent"))))))
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
      ((name . "common-shadow-attlist"))
      (optional nil
                (attribute
                 ((name . "style:shadow"))
                 (ref
                  ((name . "shadowType"))))))
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
      ((name . "borderWidths"))
      (list nil
            (ref
             ((name . "positiveLength")))
            (ref
             ((name . "positiveLength")))
            (ref
             ((name . "positiveLength")))))
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
      ((name . "coordinate"))
      (ref
       ((name . "length"))))
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
     nil
     (define
      ((name . "distance"))
      (ref
       ((name . "length"))))
     nil nil nil nil nil
     (define
      ((name . "boolean"))
      (choice nil
              (value nil "true")
              (value nil "false")))
     nil nil
     (define
      ((name . "styleNameRefs"))
      (list nil
            (zeroOrMore nil)))
     (define
      ((name . "styleNameRef"))
      (choice nil
              (empty nil)))
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
     nil
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
      ((name . "text-list-level-style-attr"))
      (attribute
       ((name . "text:level"))
       (ref
        ((name . "positiveInteger")))))
     (define
      ((name . "style-num-letter-sync-attlist"))
      (optional nil
                (attribute
                 ((name . "style:num-letter-sync"))
                 (ref
                  ((name . "boolean"))))))
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
      ((name . "style-list-level-label-alignment"))
      (optional nil
                (element
                 ((name . "style:list-level-label-alignment"))
                 (ref
                  ((name . "style-list-level-label-alignment-attlist")))
                 (empty nil))))
     (define
      ((name . "style-list-level-properties-elements"))
      (ref
       ((name . "style-list-level-label-alignment"))))
     (define
      ((name . "style-list-level-properties-content-strict"))
      (ref
       ((name . "style-list-level-properties-attlist")))
      (ref
       ((name . "style-list-level-properties-elements"))))
     (define
      ((name . "style-list-level-properties"))
      (element
       ((name . "style:list-level-properties"))
       (ref
        ((name . "style-list-level-properties-content-strict")))))
     (define
      ((name . "fontVariant"))
      (choice nil
              (value nil "normal")
              (value nil "small-caps")))
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
     nil nil nil nil nil
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
      ((name . "lineMode"))
      (choice nil
              (value nil "continuous")
              (value nil "skip-white-space")))
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
      ((name . "style-text-properties-elements"))
      (empty nil))
     (define
      ((name . "style-text-properties-content-strict"))
      (ref
       ((name . "style-text-properties-attlist")))
      (ref
       ((name . "style-text-properties-elements"))))
     (define
      ((name . "style-text-properties"))
      (element
       ((name . "style:text-properties"))
       (ref
        ((name . "style-text-properties-content-strict")))))
     nil
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
      ((name . "text-list-style"))
      (element
       ((name . "text:list-style"))
       (ref
        ((name . "text-list-style-attr")))
       (zeroOrMore nil
                   (ref
                    ((name . "text-list-style-content"))))))
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
     nil
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
     nil
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
     nil
     (define
      ((name . "office-binary-data"))
      (element
       ((name . "office:binary-data"))
       (ref
        ((name . "base64Binary")))))
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
      ((name . "style-column-sep"))
      (element
       ((name . "style:column-sep"))
       (ref
        ((name . "style-column-sep-attlist")))))
     nil
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
      ((name . "style-column"))
      (element
       ((name . "style:column"))
       (ref
        ((name . "style-column-attlist")))))
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
      ((name . "style-graphic-properties-content-strict"))
      (ref
       ((name . "style-graphic-properties-attlist")))
      (ref
       ((name . "style-graphic-fill-properties-attlist")))
      (ref
       ((name . "style-graphic-properties-elements")))))


((define
  ((name . "positiveLength"))
  (data
   ((type . "string"))
   (param
    ((name . "pattern"))
    "([0-9]*[1-9][0-9]*(\\.[0-9]*)?|0+\\.[0-9]*[1-9][0-9]*|\\.[0-9]*[1-9][0-9]*)((cm)|(mm)|(in)|(pt)|(pc)|(px))")))
 (define
  ((name . "nonNegativePixelLength"))
  (data
   ((type . "string"))
   (param
    ((name . "pattern"))
    "([0-9]+(\\.[0-9]*)?|\\.[0-9]+)(px)")))
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
  ((name . "clipShape"))
  (data
   ((type . "string"))
   (param
    ((name . "pattern"))
    "rect\\([ ]*((-?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc)))|(auto))([ ]*,[ ]*((-?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc))))|(auto)){3}[ ]*\\)")))
 (define
  ((name . "horizontal-mirror"))
  (choice nil
          (value nil "horizontal")
          (value nil "horizontal-on-odd")
          (value nil "horizontal-on-even")))
 (define
  ((name . "positiveInteger"))
  (data
   ((type . "positiveInteger"))))
 (define
  ((name . "nonNegativeLength"))
  (data
   ((type . "string"))
   (param
    ((name . "pattern"))
    "([0-9]+(\\.[0-9]*)?|\\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc)|(px))")))
 (define
  ((name . "common-editable-attlist"))
  (optional nil
            (attribute
             ((name . "style:editable"))
             (ref
              ((name . "boolean"))))))
 (define
  ((name . "common-background-transparency-attlist"))
  (optional nil
            (attribute
             ((name . "style:background-transparency"))
             (ref
              ((name . "zeroToHundredPercent"))))))
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
  ((name . "common-shadow-attlist"))
  (optional nil
            (attribute
             ((name . "style:shadow"))
             (ref
              ((name . "shadowType"))))))
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
  ((name . "borderWidths"))
  (list nil
        (ref
         ((name . "positiveLength")))
        (ref
         ((name . "positiveLength")))
        (ref
         ((name . "positiveLength")))))
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
  ((name . "coordinate"))
  (ref
   ((name . "length"))))
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
  ((name . "angle"))
  (data
   ((type . "string"))))
 (define
  ((name . "distance"))
  (ref
   ((name . "length"))))
 (define
  ((name . "signedZeroToHundredPercent"))
  (data
   ((type . "string"))
   (param
    ((name . "pattern"))
    "-?([0-9]?[0-9](\\.[0-9]*)?|100(\\.0*)?|\\.[0-9]+)%")))
 (define
  ((name . "percent"))
  (data
   ((type . "string"))
   (param
    ((name . "pattern"))
    "-?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)%")))
 (define
  ((name . "duration"))
  (data
   ((type . "duration"))))
 (define
  ((name . "nonNegativeInteger"))
  (data
   ((type . "nonNegativeInteger"))))
 (define
  ((name . "zeroToHundredPercent"))
  (data
   ((type . "string"))
   (param
    ((name . "pattern"))
    "([0-9]?[0-9](\\.[0-9]*)?|100(\\.0*)?|\\.[0-9]+)%")))
 (define
  ((name . "boolean"))
  (choice nil
          (value nil "true")
          (value nil "false")))
 (define
  ((name . "color"))
  (data
   ((type . "string"))
   (param
    ((name . "pattern"))
    "#[0-9a-fA-F]{6}")))
 (define
  ((name . "length"))
  (data
   ((type . "string"))
   (param
    ((name . "pattern"))
    "-?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc)|(px))")))
 (define
  ((name . "styleNameRefs"))
  (list nil
        (zeroOrMore nil
                    (data
                     ((type . "NCName"))))))
 (define
  ((name . "styleNameRef"))
  (choice nil
          (data
           ((type . "NCName")))
          (empty nil)))
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
  ((name . "styleName"))
  (data
   ((type . "NCName"))))
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
  ((name . "text-list-level-style-attr"))
  (attribute
   ((name . "text:level"))
   (ref
    ((name . "positiveInteger")))))
 (define
  ((name . "style-num-letter-sync-attlist"))
  (optional nil
            (attribute
             ((name . "style:num-letter-sync"))
             (ref
              ((name . "boolean"))))))
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
  ((name . "style-list-level-label-alignment"))
  (optional nil
            (element
             ((name . "style:list-level-label-alignment"))
             (ref
              ((name . "style-list-level-label-alignment-attlist")))
             (empty nil))))
 (define
  ((name . "style-list-level-properties-elements"))
  (ref
   ((name . "style-list-level-label-alignment"))))
 (define
  ((name . "style-list-level-properties-content-strict"))
  (ref
   ((name . "style-list-level-properties-attlist")))
  (ref
   ((name . "style-list-level-properties-elements"))))
 (define
  ((name . "style-list-level-properties"))
  (element
   ((name . "style:list-level-properties"))
   (ref
    ((name . "style-list-level-properties-content-strict")))))
 (define
  ((name . "fontVariant"))
  (choice nil
          (value nil "normal")
          (value nil "small-caps")))
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
  ((name . "languageCode"))
  (data
   ((type . "token"))
   (param
    ((name . "pattern"))
    "[A-Za-z]{1,8}")))
 (define
  ((name . "countryCode"))
  (data
   ((type . "token"))
   (param
    ((name . "pattern"))
    "[A-Za-z0-9]{1,8}")))
 (define
  ((name . "scriptCode"))
  (data
   ((type . "token"))
   (param
    ((name . "pattern"))
    "[A-Za-z0-9]{1,8}")))
 (define
  ((name . "language"))
  (data
   ((type . "language"))))
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
  ((name . "lineMode"))
  (choice nil
          (value nil "continuous")
          (value nil "skip-white-space")))
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
  ((name . "style-text-properties-elements"))
  (empty nil))
 (define
  ((name . "style-text-properties-content-strict"))
  (ref
   ((name . "style-text-properties-attlist")))
  (ref
   ((name . "style-text-properties-elements"))))
 (define
  ((name . "style-text-properties"))
  (element
   ((name . "style:text-properties"))
   (ref
    ((name . "style-text-properties-content-strict")))))
 (define
  ((name . "character"))
  (data
   ((type . "string"))
   (param
    ((name . "length"))
    "1")))
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
  ((name . "text-list-style"))
  (element
   ((name . "text:list-style"))
   (ref
    ((name . "text-list-style-attr")))
   (zeroOrMore nil
               (ref
                ((name . "text-list-style-content"))))))
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
  ((name . "string"))
  (data
   ((type . "string"))))
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
  ((name . "anyIRI"))
  (data
   ((type . "anyURI"))))
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
  ((name . "base64Binary"))
  (data
   ((type . "base64Binary"))))
 (define
  ((name . "office-binary-data"))
  (element
   ((name . "office:binary-data"))
   (ref
    ((name . "base64Binary")))))
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
  ((name . "style-column-sep"))
  (element
   ((name . "style:column-sep"))
   (ref
    ((name . "style-column-sep-attlist")))))
 (define
  ((name . "relativeLength"))
  (data
   ((type . "string"))
   (param
    ((name . "pattern"))
    "[0-9]+\\*")))
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
  ((name . "style-column"))
  (element
   ((name . "style:column"))
   (ref
    ((name . "style-column-attlist")))))
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
  ((name . "style-graphic-properties-content-strict"))
  (ref
   ((name . "style-graphic-properties-attlist")))
  (ref
   ((name . "style-graphic-fill-properties-attlist")))
  (ref
   ((name . "style-graphic-properties-elements")))))


(setq rng

      '((define
        ((name . "positiveLength"))
        (data
         ((type . "string"))
         (param
          ((name . "pattern"))
          "([0-9]*[1-9][0-9]*(\\.[0-9]*)?|0+\\.[0-9]*[1-9][0-9]*|\\.[0-9]*[1-9][0-9]*)((cm)|(mm)|(in)|(pt)|(pc)|(px))")))
       (define
        ((name . "nonNegativePixelLength"))
        (data
         ((type . "string"))
         (param
          ((name . "pattern"))
          "([0-9]+(\\.[0-9]*)?|\\.[0-9]+)(px)")))
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
        ((name . "clipShape"))
        (data
         ((type . "string"))
         (param
          ((name . "pattern"))
          "rect\\([ ]*((-?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc)))|(auto))([ ]*,[ ]*((-?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc))))|(auto)){3}[ ]*\\)")))
       (define
        ((name . "horizontal-mirror"))
        (choice nil
                (value nil "horizontal")
                (value nil "horizontal-on-odd")
                (value nil "horizontal-on-even")))
       (define
        ((name . "positiveInteger"))
        (data
         ((type . "positiveInteger"))))
       (define
        ((name . "nonNegativeLength"))
        (data
         ((type . "string"))
         (param
          ((name . "pattern"))
          "([0-9]+(\\.[0-9]*)?|\\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc)|(px))")))
       (define
        ((name . "common-editable-attlist"))
        (optional nil
                  (attribute
                   ((name . "style:editable"))
                   (ref
                    ((name . "boolean"))))))
       (define
        ((name . "common-background-transparency-attlist"))
        (optional nil
                  (attribute
                   ((name . "style:background-transparency"))
                   (ref
                    ((name . "zeroToHundredPercent"))))))
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
        ((name . "common-shadow-attlist"))
        (optional nil
                  (attribute
                   ((name . "style:shadow"))
                   (ref
                    ((name . "shadowType"))))))
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
        ((name . "borderWidths"))
        (list nil
              (ref
               ((name . "positiveLength")))
              (ref
               ((name . "positiveLength")))
              (ref
               ((name . "positiveLength")))))
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
        ((name . "coordinate"))
        (ref
         ((name . "length"))))
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
        ((name . "angle"))
        (data
         ((type . "string"))))
       (define
        ((name . "distance"))
        (ref
         ((name . "length"))))
       (define
        ((name . "signedZeroToHundredPercent"))
        (data
         ((type . "string"))
         (param
          ((name . "pattern"))
          "-?([0-9]?[0-9](\\.[0-9]*)?|100(\\.0*)?|\\.[0-9]+)%")))
       (define
        ((name . "percent"))
        (data
         ((type . "string"))
         (param
          ((name . "pattern"))
          "-?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)%")))
       (define
        ((name . "duration"))
        (data
         ((type . "duration"))))
       (define
        ((name . "nonNegativeInteger"))
        (data
         ((type . "nonNegativeInteger"))))
       (define
        ((name . "zeroToHundredPercent"))
        (data
         ((type . "string"))
         (param
          ((name . "pattern"))
          "([0-9]?[0-9](\\.[0-9]*)?|100(\\.0*)?|\\.[0-9]+)%")))
       (define
        ((name . "boolean"))
        (choice nil
                (value nil "true")
                (value nil "false")))
       (define
        ((name . "color"))
        (data
         ((type . "string"))
         (param
          ((name . "pattern"))
          "#[0-9a-fA-F]{6}")))
       (define
        ((name . "length"))
        (data
         ((type . "string"))
         (param
          ((name . "pattern"))
          "-?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc)|(px))")))
       (define
        ((name . "styleNameRefs"))
        (list nil
              (zeroOrMore nil
                          (data
                           ((type . "NCName"))))))
       (define
        ((name . "styleNameRef"))
        (choice nil
                (data
                 ((type . "NCName")))
                (empty nil)))
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
        ((name . "styleName"))
        (data
         ((type . "NCName"))))
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
        ((name . "text-list-level-style-attr"))
        (attribute
         ((name . "text:level"))
         (ref
          ((name . "positiveInteger")))))
       (define
        ((name . "style-num-letter-sync-attlist"))
        (optional nil
                  (attribute
                   ((name . "style:num-letter-sync"))
                   (ref
                    ((name . "boolean"))))))
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
        ((name . "style-list-level-label-alignment"))
        (optional nil
                  (element
                   ((name . "style:list-level-label-alignment"))
                   (ref
                    ((name . "style-list-level-label-alignment-attlist")))
                   (empty nil))))
       (define
        ((name . "style-list-level-properties-elements"))
        (ref
         ((name . "style-list-level-label-alignment"))))
       (define
        ((name . "style-list-level-properties-content-strict"))
        (ref
         ((name . "style-list-level-properties-attlist")))
        (ref
         ((name . "style-list-level-properties-elements"))))
       (define
        ((name . "style-list-level-properties"))
        (element
         ((name . "style:list-level-properties"))
         (ref
          ((name . "style-list-level-properties-content-strict")))))
       (define
        ((name . "fontVariant"))
        (choice nil
                (value nil "normal")
                (value nil "small-caps")))
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
        ((name . "languageCode"))
        (data
         ((type . "token"))
         (param
          ((name . "pattern"))
          "[A-Za-z]{1,8}")))
       (define
        ((name . "countryCode"))
        (data
         ((type . "token"))
         (param
          ((name . "pattern"))
          "[A-Za-z0-9]{1,8}")))
       (define
        ((name . "scriptCode"))
        (data
         ((type . "token"))
         (param
          ((name . "pattern"))
          "[A-Za-z0-9]{1,8}")))
       (define
        ((name . "language"))
        (data
         ((type . "language"))))
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
        ((name . "lineMode"))
        (choice nil
                (value nil "continuous")
                (value nil "skip-white-space")))
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
        ((name . "style-text-properties-elements"))
        (empty nil))
       (define
        ((name . "style-text-properties-content-strict"))
        (ref
         ((name . "style-text-properties-attlist")))
        (ref
         ((name . "style-text-properties-elements"))))
       (define
        ((name . "style-text-properties"))
        (element
         ((name . "style:text-properties"))
         (ref
          ((name . "style-text-properties-content-strict")))))
       (define
        ((name . "character"))
        (data
         ((type . "string"))
         (param
          ((name . "length"))
          "1")))
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
        ((name . "text-list-style"))
        (element
         ((name . "text:list-style"))
         (ref
          ((name . "text-list-style-attr")))
         (zeroOrMore nil
                     (ref
                      ((name . "text-list-style-content"))))))
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
        ((name . "string"))
        (data
         ((type . "string"))))
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
        ((name . "anyIRI"))
        (data
         ((type . "anyURI")))
        (description nil "An IRI-reference as defined in [RFC3987]. See ODF 1.2 Part 1 section 18.3."))
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
        ((name . "base64Binary"))
        (data
         ((type . "base64Binary"))))
       (define
        ((name . "office-binary-data"))
        (element
         ((name . "office:binary-data"))
         (ref
          ((name . "base64Binary")))))
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
        ((name . "style-column-sep"))
        (element
         ((name . "style:column-sep"))
         (ref
          ((name . "style-column-sep-attlist")))))
       (define
        ((name . "relativeLength"))
        (data
         ((type . "string"))
         (param
          ((name . "pattern"))
          "[0-9]+\\*")))
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
        ((name . "style-column"))
        (element
         ((name . "style:column"))
         (ref
          ((name . "style-column-attlist")))))
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
        ((name . "style-graphic-properties-content-strict"))
        (ref
         ((name . "style-graphic-properties-attlist")))
        (ref
         ((name . "style-graphic-fill-properties-attlist")))
        (ref
         ((name . "style-graphic-properties-elements"))))))



(insert "\n\n"
	(with-temp-buffer
	  (rnc-mode)
	  (insert (odt-rngdom:do-rng->rnc odt-rngdom:odf-v1.2-os-schema rng t))
	  (indent-region (point-min) (point-max))
	  (buffer-substring-no-properties (point-min) (point-max)))
        "\n\n")




positiveLength =
    xsd:string {

		pattern = "([0-9]*[1-9][0-9]*(\.[0-9]*)?|0+\.[0-9]*[1-9][0-9]*|\.[0-9]*[1-9][0-9]*)((cm)|(mm)|(in)|(pt)|(pc)|(px))"
}

nonNegativePixelLength =
    xsd:string {

		pattern = "([0-9]+(\.[0-9]*)?|\.[0-9]+)(px)"
}

common-writing-mode-attlist =
    attribute style:writing-mode { ("lr-tb"
				   | "rl-tb"
				   | "tb-rl"
				   | "tb-lr"
				   | "lr"
				   | "rl"
				   | "tb"
				   | "page") }?

clipShape =
    xsd:string {

		pattern = "rect\([ ]*((-?([0-9]+(\.[0-9]*)?|\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc)))|(auto))([ ]*,[ ]*((-?([0-9]+(\.[0-9]*)?|\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc))))|(auto)){3}[ ]*\)"
}

horizontal-mirror =
    ("horizontal"
    | "horizontal-on-odd"
    | "horizontal-on-even")

positiveInteger =
    xsd:positiveInteger

nonNegativeLength =
    xsd:string {

		pattern = "([0-9]+(\.[0-9]*)?|\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc)|(px))"
}

common-editable-attlist =
    attribute style:editable { boolean }?

common-background-transparency-attlist =
    attribute style:background-transparency { zeroToHundredPercent }?

common-background-color-attlist =
    attribute fo:background-color { ("transparent"
				    | color) }?

common-shadow-attlist =
    attribute style:shadow { shadowType }?

common-padding-attlist =
    attribute fo:padding { nonNegativeLength }?,
    attribute fo:padding-top { nonNegativeLength }?,
    attribute fo:padding-bottom { nonNegativeLength }?,
    attribute fo:padding-left { nonNegativeLength }?,
    attribute fo:padding-right { nonNegativeLength }?

borderWidths =
    list { positiveLength,
	   positiveLength,
	   positiveLength }

common-border-line-width-attlist =
    attribute style:border-line-width { borderWidths }?,
    attribute style:border-line-width-top { borderWidths }?,
    attribute style:border-line-width-bottom { borderWidths }?,
    attribute style:border-line-width-left { borderWidths }?,
    attribute style:border-line-width-right { borderWidths }?

common-border-attlist =
    attribute fo:border { \string }?,
    attribute fo:border-top { \string }?,
    attribute fo:border-bottom { \string }?,
    attribute fo:border-left { \string }?,
    attribute fo:border-right { \string }?

common-text-anchor-attlist =
    (attribute text:anchor-type { ("page"
				  | "frame"
				  | "paragraph"
				  | "char"
				  | "as-char") }?
    & attribute text:anchor-page-number { positiveInteger }?)

common-vertical-rel-attlist =
    attribute style:vertical-rel { ("page"
				   | "page-content"
				   | "frame"
				   | "frame-content"
				   | "paragraph"
				   | "paragraph-content"
				   | "char"
				   | "line"
				   | "baseline"
				   | "text") }?

common-vertical-pos-attlist =
    attribute style:vertical-pos { ("top"
				   | "middle"
				   | "bottom"
				   | "from-top"
				   | "below") }?,
    attribute svg:y { coordinate }?

coordinate =
    length

common-margin-attlist =
    attribute fo:margin { (nonNegativeLength
			  | percent) }?

common-vertical-margin-attlist =
    attribute fo:margin-top { (nonNegativeLength
			      | percent) }?,
    attribute fo:margin-bottom { (nonNegativeLength
				 | percent) }?

common-horizontal-margin-attlist =
    attribute fo:margin-left { (length
			       | percent) }?,
    attribute fo:margin-right { (length
				| percent) }?

common-draw-size-attlist =
    attribute svg:width { length }?,
    attribute svg:height { length }?

common-draw-rel-size-attlist =
    common-draw-size-attlist,
    attribute style:rel-width { (percent
				| "scale"
				| "scale-min") }?,
    attribute style:rel-height { (percent
				 | "scale"
				 | "scale-min") }?

angle =
    xsd:string

distance =
    length

signedZeroToHundredPercent =
    xsd:string {

		pattern = "-?([0-9]?[0-9](\.[0-9]*)?|100(\.0*)?|\.[0-9]+)%"
}

percent =
    xsd:string {

		pattern = "-?([0-9]+(\.[0-9]*)?|\.[0-9]+)%"
}

duration =
    xsd:duration

nonNegativeInteger =
    xsd:nonNegativeInteger

zeroToHundredPercent =
    xsd:string {

		pattern = "([0-9]?[0-9](\.[0-9]*)?|100(\.0*)?|\.[0-9]+)%"
}

boolean =
    ("true"
    | "false")

color =
    xsd:string {

		pattern = "#[0-9a-fA-F]{6}"
}

length =
    xsd:string {

		pattern = "-?([0-9]+(\.[0-9]*)?|\.[0-9]+)((cm)|(mm)|(in)|(pt)|(pc)|(px))"
}

styleNameRefs =
    list { xsd:NCName* }

styleNameRef =
    (xsd:NCName
    | empty)

style-graphic-fill-properties-attlist =
    (attribute draw:fill { ("none"
			   | "solid"
			   | "bitmap"
			   | "gradient"
			   | "hatch") }?
    & attribute draw:fill-color { color }?
    & attribute draw:secondary-fill-color { color }?
    & attribute draw:fill-gradient-name { styleNameRef }?
    & attribute draw:gradient-step-count { nonNegativeInteger }?
    & attribute draw:fill-hatch-name { styleNameRef }?
    & attribute draw:fill-hatch-solid { boolean }?
    & attribute draw:fill-image-name { styleNameRef }?
    & attribute style:repeat { ("no-repeat"
			       | "repeat"
			       | "stretch") }?
    & attribute draw:fill-image-width { (length
					| percent) }?
    & attribute draw:fill-image-height { (length
					 | percent) }?
    & attribute draw:fill-image-ref-point-x { percent }?
    & attribute draw:fill-image-ref-point-y { percent }?
    & attribute draw:fill-image-ref-point { ("top-left"
					    | "top"
					    | "top-right"
					    | "left"
					    | "center"
					    | "right"
					    | "bottom-left"
					    | "bottom"
					    | "bottom-right") }?
    & attribute draw:tile-repeat-offset { list { zeroToHundredPercent,
						 ("horizontal"
						 | "vertical") } }?
    & attribute draw:opacity { zeroToHundredPercent }?
    & attribute draw:opacity-name { styleNameRef }?
    & attribute svg:fill-rule { ("nonzero"
				| "evenodd") }?)

styleName =
    xsd:NCName

text-list-style-attr =
    (attribute style:name { styleName }
    & attribute style:display-name { \string }?
    & attribute text:consecutive-numbering { boolean }?)

text-list-level-style-attr =
    attribute text:level { positiveInteger }

style-num-letter-sync-attlist =
    attribute style:num-letter-sync { boolean }?

common-num-format-attlist =
    (attribute style:num-format { ("1"
				  | "i"
				  | "I"
				  | \string
				  | empty) }
    | (attribute style:num-format { ("a"
				    | "A") },
       style-num-letter-sync-attlist)
    | empty)

text-list-level-style-number-attr =
    (attribute text:style-name { styleNameRef }?
    & common-num-format-attlist
    & common-num-format-prefix-suffix-attlist
    & attribute text:display-levels { positiveInteger }?
    & attribute text:start-value { positiveInteger }?)

common-text-align =
    attribute fo:text-align { ("start"
			      | "end"
			      | "left"
			      | "right"
			      | "center"
			      | "justify") }?

style-list-level-properties-attlist =
    (common-text-align
    & attribute text:space-before { length }?
    & attribute text:min-label-width { nonNegativeLength }?
    & attribute text:min-label-distance { nonNegativeLength }?
    & attribute style:font-name { \string }?
    & attribute fo:width { positiveLength }?
    & attribute fo:height { positiveLength }?
    & common-vertical-rel-attlist
    & common-vertical-pos-attlist
    & attribute text:list-level-position-and-space-mode { ("label-width-and-position"
							  | "label-alignment") }?)

style-list-level-label-alignment-attlist =
    (attribute text:label-followed-by { ("listtab"
					| "space"
					| "nothing") }
    & attribute text:list-tab-stop-position { length }?
    & attribute fo:text-indent { length }?
    & attribute fo:margin-left { length }?)

style-list-level-label-alignment =
    element style:list-level-label-alignment { style-list-level-label-alignment-attlist,
					       empty }?

style-list-level-properties-elements =
    style-list-level-label-alignment

style-list-level-properties-content-strict =
    style-list-level-properties-attlist,
    style-list-level-properties-elements

style-list-level-properties =
    element style:list-level-properties { style-list-level-properties-content-strict }

fontVariant =
    ("normal"
    | "small-caps")

lineType =
    ("none"
    | "single"
    | "double")

lineStyle =
    ("none"
    | "solid"
    | "dotted"
    | "dash"
    | "long-dash"
    | "dot-dash"
    | "dot-dot-dash"
    | "wave")

lineWidth =
    ("auto"
    | "normal"
    | "bold"
    | "thin"
    | "medium"
    | "thick"
    | positiveInteger
    | percent
    | positiveLength)

fontFamilyGeneric =
    ("roman"
    | "swiss"
    | "modern"
    | "decorative"
    | "script"
    | "system")

fontPitch =
    ("fixed"
    | "variable")

textEncoding =
    xsd:string {

		pattern = "[A-Za-z][A-Za-z0-9._\-]*"
}

languageCode =
    xsd:token {

	       pattern = "[A-Za-z]{1,8}"
}

countryCode =
    xsd:token {

	       pattern = "[A-Za-z0-9]{1,8}"
}

scriptCode =
    xsd:token {

	       pattern = "[A-Za-z0-9]{1,8}"
}

language =
    xsd:language

fontStyle =
    ("normal"
    | "italic"
    | "oblique")

shadowType =
    ("none"
    | \string)

lineMode =
    ("continuous"
    | "skip-white-space")

fontWeight =
    ("normal"
    | "bold"
    | "100"
    | "200"
    | "300"
    | "400"
    | "500"
    | "600"
    | "700"
    | "800"
    | "900")

style-text-properties-attlist =
    (attribute fo:font-variant { fontVariant }?
    & attribute fo:text-transform { ("none"
				    | "lowercase"
				    | "uppercase"
				    | "capitalize") }?
    & attribute fo:color { color }?
    & attribute style:use-window-font-color { boolean }?
    & attribute style:text-outline { boolean }?
    & attribute style:text-line-through-type { lineType }?
    & attribute style:text-line-through-style { lineStyle }?
    & attribute style:text-line-through-width { lineWidth }?
    & attribute style:text-line-through-color { ("font-color"
						| color) }?
    & attribute style:text-line-through-text { \string }?
    & attribute style:text-line-through-text-style { styleNameRef }?
    & attribute style:text-position { list { (percent
					     | "super"
					     | "sub"),
					     percent? } }?
    & attribute style:font-name { \string }?
    & attribute style:font-name-asian { \string }?
    & attribute style:font-name-complex { \string }?
    & attribute fo:font-family { \string }?
    & attribute style:font-family-asian { \string }?
    & attribute style:font-family-complex { \string }?
    & attribute style:font-family-generic { fontFamilyGeneric }?
    & attribute style:font-family-generic-asian { fontFamilyGeneric }?
    & attribute style:font-family-generic-complex { fontFamilyGeneric }?
    & attribute style:font-style-name { \string }?
    & attribute style:font-style-name-asian { \string }?
    & attribute style:font-style-name-complex { \string }?
    & attribute style:font-pitch { fontPitch }?
    & attribute style:font-pitch-asian { fontPitch }?
    & attribute style:font-pitch-complex { fontPitch }?
    & attribute style:font-charset { textEncoding }?
    & attribute style:font-charset-asian { textEncoding }?
    & attribute style:font-charset-complex { textEncoding }?
    & attribute fo:font-size { (positiveLength
			       | percent) }?
    & attribute style:font-size-asian { (positiveLength
					| percent) }?
    & attribute style:font-size-complex { (positiveLength
					  | percent) }?
    & attribute style:font-size-rel { length }?
    & attribute style:font-size-rel-asian { length }?
    & attribute style:font-size-rel-complex { length }?
    & attribute style:script-type { ("latin"
				    | "asian"
				    | "complex"
				    | "ignore") }?
    & attribute fo:letter-spacing { (length
				    | "normal") }?
    & attribute fo:language { languageCode }?
    & attribute style:language-asian { languageCode }?
    & attribute style:language-complex { languageCode }?
    & attribute fo:country { countryCode }?
    & attribute style:country-asian { countryCode }?
    & attribute style:country-complex { countryCode }?
    & attribute fo:script { scriptCode }?
    & attribute style:script-asian { scriptCode }?
    & attribute style:script-complex { scriptCode }?
    & attribute style:rfc-language-tag { language }?
    & attribute style:rfc-language-tag-asian { language }?
    & attribute style:rfc-language-tag-complex { language }?
    & attribute fo:font-style { fontStyle }?
    & attribute style:font-style-asian { fontStyle }?
    & attribute style:font-style-complex { fontStyle }?
    & attribute style:font-relief { ("none"
				    | "embossed"
				    | "engraved") }?
    & attribute fo:text-shadow { shadowType }?
    & attribute style:text-underline-type { lineType }?
    & attribute style:text-underline-style { lineStyle }?
    & attribute style:text-underline-width { lineWidth }?
    & attribute style:text-underline-color { ("font-color"
					     | color) }?
    & attribute style:text-overline-type { lineType }?
    & attribute style:text-overline-style { lineStyle }?
    & attribute style:text-overline-width { lineWidth }?
    & attribute style:text-overline-color { ("font-color"
					    | color) }?
    & attribute style:text-overline-mode { lineMode }?
    & attribute fo:font-weight { fontWeight }?
    & attribute style:font-weight-asian { fontWeight }?
    & attribute style:font-weight-complex { fontWeight }?
    & attribute style:text-underline-mode { lineMode }?
    & attribute style:text-line-through-mode { lineMode }?
    & attribute style:letter-kerning { boolean }?
    & attribute style:text-blinking { boolean }?
    & common-background-color-attlist
    & attribute style:text-combine { ("none"
				     | "letters"
				     | "lines") }?
    & attribute style:text-combine-start-char { character }?
    & attribute style:text-combine-end-char { character }?
    & attribute style:text-emphasize { ("none"
				       | list { ("none"
						| "accent"
						| "dot"
						| "circle"
						| "disc"),
						("above"
						| "below") }) }?
    & attribute style:text-scale { percent }?
    & attribute style:text-rotation-angle { angle }?
    & attribute style:text-rotation-scale { ("fixed"
					    | "line-height") }?
    & attribute fo:hyphenate { boolean }?
    & attribute fo:hyphenation-remain-char-count { positiveInteger }?
    & attribute fo:hyphenation-push-char-count { positiveInteger }?
    & (attribute text:display { "true" }
      | attribute text:display { "none" }
      | (attribute text:display { "condition" },
	 attribute text:condition { "none" })
      | empty))

style-text-properties-elements =
    empty

style-text-properties-content-strict =
    style-text-properties-attlist,
    style-text-properties-elements

style-text-properties =
    element style:text-properties { style-text-properties-content-strict }

character =
    xsd:string {

		length = "1"
}

common-num-format-prefix-suffix-attlist =
    attribute style:num-prefix { \string }?,
    attribute style:num-suffix { \string }?

text-list-level-style-bullet-attr =
    (attribute text:style-name { styleNameRef }?
    & attribute text:bullet-char { character }
    & common-num-format-prefix-suffix-attlist
    & attribute text:bullet-relative-size { percent }?)

text-list-level-style-image-attr =
    (common-draw-data-attlist
    | office-binary-data)

text-list-style-content =
    (element text:list-level-style-number { text-list-level-style-attr,
					    text-list-level-style-number-attr,
					    style-list-level-properties?,
					    style-text-properties? }
    | element text:list-level-style-bullet { text-list-level-style-attr,
					     text-list-level-style-bullet-attr,
					     style-list-level-properties?,
					     style-text-properties? }
    | element text:list-level-style-image { text-list-level-style-attr,
					    text-list-level-style-image-attr,
					    style-list-level-properties? })

text-list-style =
    element text:list-style { text-list-style-attr,
			      text-list-style-content* }

horiBackPos =
    ("left"
    | "center"
    | "right")

vertBackPos =
    ("top"
    | "center"
    | "bottom")

string =
    xsd:string

style-background-image-attlist =
    (attribute style:repeat { ("no-repeat"
			      | "repeat"
			      | "stretch") }?
    & attribute style:position { ("left"
				 | "center"
				 | "right"
				 | "top"
				 | "bottom"
				 | list { horiBackPos,
					  vertBackPos }
				 | list { vertBackPos,
					  horiBackPos }) }?
    & attribute style:filter-name { \string }?
    & attribute draw:opacity { zeroToHundredPercent }?)

anyIRI =
    xsd:anyURI,


common-draw-data-attlist =
    (attribute xlink:type { "simple" },
     attribute xlink:href { anyIRI },
     attribute xlink:show { "embed" }?,
     attribute xlink:actuate { "onLoad" }?)

base64Binary =
    xsd:base64Binary

office-binary-data =
    element office:binary-data { base64Binary }

style-background-image =
    element style:background-image { style-background-image-attlist,
				     (common-draw-data-attlist
				     | office-binary-data
				     | empty) }?

style-columns-attlist =
    (attribute fo:column-count { positiveInteger }
    & attribute fo:column-gap { length }?)

style-column-sep-attlist =
    (attribute style:style { ("none"
			     | "solid"
			     | "dotted"
			     | "dashed"
			     | "dot-dashed") }?
    & attribute style:width { length }
    & attribute style:height { zeroToHundredPercent }?
    & attribute style:vertical-align { ("top"
				       | "middle"
				       | "bottom") }?
    & attribute style:color { color }?)

style-column-sep =
    element style:column-sep { style-column-sep-attlist }

relativeLength =
    xsd:string {

		pattern = "[0-9]+\*"
}

style-column-attlist =
    (attribute style:rel-width { relativeLength }
    & attribute fo:start-indent { length }?
    & attribute fo:end-indent { length }?
    & attribute fo:space-before { length }?
    & attribute fo:space-after { length }?)

style-column =
    element style:column { style-column-attlist }

style-columns =
    element style:columns { style-columns-attlist,
			    style-column-sep?,
			    style-column* }?

style-graphic-properties-elements =
    (text-list-style?
    & style-background-image
    & style-columns)

style-graphic-properties-attlist =
    (attribute draw:stroke { ("none"
			     | "dash"
			     | "solid") }?
    & attribute draw:stroke-dash { styleNameRef }?
    & attribute draw:stroke-dash-names { styleNameRefs }?
    & attribute svg:stroke-width { length }?
    & attribute svg:stroke-color { color }?
    & attribute draw:marker-start { styleNameRef }?
    & attribute draw:marker-end { styleNameRef }?
    & attribute draw:marker-start-width { length }?
    & attribute draw:marker-end-width { length }?
    & attribute draw:marker-start-center { boolean }?
    & attribute draw:marker-end-center { boolean }?
    & attribute svg:stroke-opacity { (xsd:double {

						  minInclusive = "0"
						  maxInclusive = "1"
				     }
				     | zeroToHundredPercent) }?
    & attribute draw:stroke-linejoin { ("miter"
				       | "round"
				       | "bevel"
				       | "middle"
				       | "none") }?
    & attribute svg:stroke-linecap { ("butt"
				     | "square"
				     | "round") }?
    & attribute draw:symbol-color { color }?
    & attribute text:animation { ("none"
				 | "scroll"
				 | "alternate"
				 | "slide") }?
    & attribute text:animation-direction { ("left"
					   | "right"
					   | "up"
					   | "down") }?
    & attribute text:animation-start-inside { boolean }?
    & attribute text:animation-stop-inside { boolean }?
    & attribute text:animation-repeat { nonNegativeInteger }?
    & attribute text:animation-delay { duration }?
    & attribute text:animation-steps { length }?
    & attribute draw:auto-grow-width { boolean }?
    & attribute draw:auto-grow-height { boolean }?
    & attribute draw:fit-to-size { boolean }?
    & attribute draw:fit-to-contour { boolean }?
    & attribute draw:textarea-vertical-align { ("top"
					       | "middle"
					       | "bottom"
					       | "justify") }?
    & attribute draw:textarea-horizontal-align { ("left"
						 | "center"
						 | "right"
						 | "justify") }?
    & attribute fo:wrap-option { ("no-wrap"
				 | "wrap") }?
    & attribute style:shrink-to-fit { boolean }?
    & attribute draw:color-mode { ("greyscale"
				  | "mono"
				  | "watermark"
				  | "standard") }?
    & attribute draw:color-inversion { boolean }?
    & attribute draw:luminance { zeroToHundredPercent }?
    & attribute draw:contrast { percent }?
    & attribute draw:gamma { percent }?
    & attribute draw:red { signedZeroToHundredPercent }?
    & attribute draw:green { signedZeroToHundredPercent }?
    & attribute draw:blue { signedZeroToHundredPercent }?
    & attribute draw:image-opacity { zeroToHundredPercent }?
    & attribute draw:shadow { ("visible"
			      | "hidden") }?
    & attribute draw:shadow-offset-x { length }?
    & attribute draw:shadow-offset-y { length }?
    & attribute draw:shadow-color { color }?
    & attribute draw:shadow-opacity { zeroToHundredPercent }?
    & attribute draw:start-line-spacing-horizontal { distance }?
    & attribute draw:start-line-spacing-vertical { distance }?
    & attribute draw:end-line-spacing-horizontal { distance }?
    & attribute draw:end-line-spacing-vertical { distance }?
    & attribute draw:line-distance { distance }?
    & attribute draw:guide-overhang { length }?
    & attribute draw:guide-distance { distance }?
    & attribute draw:start-guide { length }?
    & attribute draw:end-guide { length }?
    & attribute draw:placing { ("below"
			       | "above") }?
    & attribute draw:parallel { boolean }?
    & attribute draw:measure-align { ("automatic"
				     | "left-outside"
				     | "inside"
				     | "right-outside") }?
    & attribute draw:measure-vertical-align { ("automatic"
					      | "above"
					      | "below"
					      | "center") }?
    & attribute draw:unit { ("automatic"
			    | "mm"
			    | "cm"
			    | "m"
			    | "km"
			    | "pt"
			    | "pc"
			    | "inch"
			    | "ft"
			    | "mi") }?
    & attribute draw:show-unit { boolean }?
    & attribute draw:decimal-places { nonNegativeInteger }?
    & attribute draw:caption-type { ("straight-line"
				    | "angled-line"
				    | "angled-connector-line") }?
    & attribute draw:caption-angle-type { ("fixed"
					  | "free") }?
    & attribute draw:caption-angle { angle }?
    & attribute draw:caption-gap { distance }?
    & attribute draw:caption-escape-direction { ("horizontal"
						| "vertical"
						| "auto") }?
    & attribute draw:caption-escape { (length
				      | percent) }?
    & attribute draw:caption-line-length { length }?
    & attribute draw:caption-fit-line-length { boolean }?
    & attribute dr3d:horizontal-segments { nonNegativeInteger }?
    & attribute dr3d:vertical-segments { nonNegativeInteger }?
    & attribute dr3d:edge-rounding { percent }?
    & attribute dr3d:edge-rounding-mode { ("correct"
					  | "attractive") }?
    & attribute dr3d:back-scale { percent }?
    & attribute dr3d:depth { length }?
    & attribute dr3d:backface-culling { ("enabled"
					| "disabled") }?
    & attribute dr3d:end-angle { angle }?
    & attribute dr3d:close-front { boolean }?
    & attribute dr3d:close-back { boolean }?
    & attribute dr3d:lighting-mode { ("standard"
				     | "double-sided") }?
    & attribute dr3d:normals-kind { ("object"
				    | "flat"
				    | "sphere") }?
    & attribute dr3d:normals-direction { ("normal"
					 | "inverse") }?
    & attribute dr3d:texture-generation-mode-x { ("object"
						 | "parallel"
						 | "sphere") }?
    & attribute dr3d:texture-generation-mode-y { ("object"
						 | "parallel"
						 | "sphere") }?
    & attribute dr3d:texture-kind { ("luminance"
				    | "intensity"
				    | "color") }?
    & attribute dr3d:texture-filter { ("enabled"
				      | "disabled") }?
    & attribute dr3d:texture-mode { ("replace"
				    | "modulate"
				    | "blend") }?
    & attribute dr3d:ambient-color { color }?
    & attribute dr3d:emissive-color { color }?
    & attribute dr3d:specular-color { color }?
    & attribute dr3d:diffuse-color { color }?
    & attribute dr3d:shininess { percent }?
    & attribute dr3d:shadow { ("visible"
			      | "hidden") }?
    & common-draw-rel-size-attlist
    & attribute fo:min-width { (length
			       | percent) }?
    & attribute fo:min-height { (length
				| percent) }?
    & attribute fo:max-height { (length
				| percent) }?
    & attribute fo:max-width { (length
			       | percent) }?
    & common-horizontal-margin-attlist
    & common-vertical-margin-attlist
    & common-margin-attlist
    & attribute style:print-content { boolean }?
    & attribute style:protect { ("none"
				| list { 
				      ("content"
				      | "position"
				      | "size")+
				  }) }?
    & attribute style:horizontal-pos { ("left"
				       | "center"
				       | "right"
				       | "from-left"
				       | "inside"
				       | "outside"
				       | "from-inside") }?
    & attribute svg:x { coordinate }?
    & attribute style:horizontal-rel { ("page"
				       | "page-content"
				       | "page-start-margin"
				       | "page-end-margin"
				       | "frame"
				       | "frame-content"
				       | "frame-start-margin"
				       | "frame-end-margin"
				       | "paragraph"
				       | "paragraph-content"
				       | "paragraph-start-margin"
				       | "paragraph-end-margin"
				       | "char") }?
    & common-vertical-pos-attlist
    & common-vertical-rel-attlist
    & common-text-anchor-attlist
    & common-border-attlist
    & common-border-line-width-attlist
    & common-padding-attlist
    & common-shadow-attlist
    & common-background-color-attlist
    & common-background-transparency-attlist
    & common-editable-attlist
    & attribute style:wrap { ("none"
			     | "left"
			     | "right"
			     | "parallel"
			     | "dynamic"
			     | "run-through"
			     | "biggest") }?
    & attribute style:wrap-dynamic-threshold { nonNegativeLength }?
    & attribute style:number-wrapped-paragraphs { ("no-limit"
						  | positiveInteger) }?
    & attribute style:wrap-contour { boolean }?
    & attribute style:wrap-contour-mode { ("full"
					  | "outside") }?
    & attribute style:run-through { ("foreground"
				    | "background") }?
    & attribute style:flow-with-text { boolean }?
    & attribute style:overflow-behavior { ("clip"
					  | "auto-create-new-frame") }?
    & attribute style:mirror { ("none"
			       | "vertical"
			       | horizontal-mirror
			       | list { "vertical",
					horizontal-mirror }
			       | list { horizontal-mirror,
					"vertical" }) }?
    & attribute fo:clip { ("auto"
			  | clipShape) }?
    & attribute draw:wrap-influence-on-position { ("iterative"
						  | "once-concurrent"
						  | "once-successive") }?
    & common-writing-mode-attlist
    & attribute draw:frame-display-scrollbar { boolean }?
    & attribute draw:frame-display-border { boolean }?
    & attribute draw:frame-margin-horizontal { nonNegativePixelLength }?
    & attribute draw:frame-margin-vertical { nonNegativePixelLength }?
    & attribute draw:visible-area-left { nonNegativeLength }?
    & attribute draw:visible-area-top { nonNegativeLength }?
    & attribute draw:visible-area-width { positiveLength }?
    & attribute draw:visible-area-height { positiveLength }?
    & attribute draw:draw-aspect { ("content"
				   | "thumbnail"
				   | "icon"
				   | "print-view") }?
    & attribute draw:ole-draw-aspect { nonNegativeInteger }?)

style-graphic-properties-content-strict =
    style-graphic-properties-attlist,
    style-graphic-fill-properties-attlist,
    style-graphic-properties-elements



(setq rng rng-target)

(setq rng ;; (seq-take rng 10)
      rng-target)

(let ((print-circle nil))
  (odt-rngdom-new:rng->rnc nil rng ))

(let* ((print-circle nil)
       ;; (rng (odt-rngdom:ref-name->ref-names "percent" odt-rngdom:odf-v1.2-os-schema))
       ;; (rng (odt-rngdom:ref-name->ref-names "style-graphic-properties-content-strict"
       ;;                                  odt-rngdom:odf-v1.2-os-schema))
       (rng rng-target))
  (odt-rngdom-new:rng->rnc nil
                           rng
                           ;; (odt-rngdom:remove-define-data
                           ;;  (odt-rngdom:remove-nodes-of-type 'data rng))
                           ))


(mapcar )

(odt-stylesdom:dom->style-nodes org-odt-styles-dom)

;; (let ((style-names '("OpenSymbol")))
;;   (odt-dom-map
;;    (lambda (node)
;;      (let ((style-name (odt-dom-property node 'style:name)))
;;        (when (member style-name style-names)
;; 	 node)))
;;    (odt-dom:file-name->dom
;;     (expand-file-name "OrgOdtStyles.xml" org-odt-styles-dir)
;;     'remove-xmlns-attributes)))

;; (with-current-buffer (find-file-noselect "/tmp/styles.xml")
;;   (erase-buffer)
;;   (pop-to-buffer (current-buffer))
;;   (insert (org-odt--lisp-to-xml org-odt-styles-dom nil t))
;;   (save-buffer 0))

(setq styles-dom (odt-dom:file-name->dom
		  (expand-file-name "OrgOdtStyles.xml" org-odt-styles-dir)
		  ;; 'remove-xmlns-attributes
                  ))

(car (odt-dom:type->nodes 'office:document-styles styles-dom))

nil



(let* ((styles-file (expand-file-name "OrgOdtStyles.xml" org-odt-styles-dir))
       (styles-dom (odt-stylesdom:styles-file->dom styles-file)))
  ;; (message "attrs: %S" attrs)
  (with-current-buffer (find-file-noselect "/tmp/styles.xml")
    (erase-buffer)
    (pop-to-buffer (current-buffer))
    (insert (org-odt--lisp-to-xml styles-dom nil t))
    (save-buffer 0))
  (with-current-buffer (find-file-noselect "/tmp/pruned-styles.xml")
    (erase-buffer)
    (pop-to-buffer (current-buffer))
    (insert (org-odt--lisp-to-xml (odt-stylesdom:remove-style-names
                                   '("OrgOutline" "Text_20_body")
                                   styles-dom)
                                  nil t))
    (save-buffer 0)))

nil
