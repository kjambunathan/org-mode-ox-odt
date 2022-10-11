;;;; odt-structs --- Odt Structs -*- lexical-binding: t; coding: utf-8-emacs; -*-

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



;;;; style:style

(cl-defstruct
    (odt-style:style-attributes
     (:constructor odt-make-style:style-attributes
		   (&key class default-outline-level display-name family list-style-name name next-style-name parent-style-name &aux
			 (style:class class)
			 (style:default-outline-level default-outline-level)
			 (style:display-name display-name)
			 (style:family family)
			 (style:list-style-name list-style-name)
			 (style:name name)
			 (style:next-style-name next-style-name)
			 (style:parent-style-name parent-style-name))))
  style:class style:default-outline-level style:display-name style:family style:list-style-name style:name style:next-style-name style:parent-style-name)


(defvar odt-make-style:style-attributes--usage
  '(odt-make-style:style-attributes :name "Text_20_body" :display-name "Text body" :family "paragraph" :parent-style-name "Standard" :class "text"))


(put 'odt-make-style:style-attributes 'function-documentation
"
Constructor for objects of type `odt-style:style-attributes'.

(odt-make-style:style-attributes &key CLASS DEFAULT-OUTLINE-LEVEL
				 DISPLAY-NAME FAMILY LIST-STYLE-NAME NAME NEXT-STYLE-NAME
				 PARENT-STYLE-NAME)


Usage:

(odt-make-style:style-attributes
 :name \"Text_20_body\"
 :display-name \"Text body\"
 :family \"paragraph\"
 :parent-style-name \"Standard\"
 :class \"text\")

")

;;;; style:page-layout-properties

(cl-defstruct
    (odt-style:page-layout-properties-attributes
     (:constructor odt-make-style:page-layout-properties-attributes
		   (&key background-color margin-bottom margin-left margin-right margin-top page-height page-width footnote-max-height num-format print-orientation writing-mode &aux
			 (fo:background-color background-color)
			 (fo:margin-bottom margin-bottom)
			 (fo:margin-left margin-left)
			 (fo:margin-right margin-right)
			 (fo:margin-top margin-top)
			 (fo:page-height page-height)
			 (fo:page-width page-width)
			 (style:footnote-max-height footnote-max-height)
			 (style:num-format num-format)
			 (style:print-orientation print-orientation)
			 (style:writing-mode writing-mode))))
  fo:background-color fo:margin-bottom fo:margin-left fo:margin-right fo:margin-top fo:page-height fo:page-width style:footnote-max-height style:num-format style:print-orientation style:writing-mode)


(defvar odt-make-style:page-layout-properties-attributes--usage
  '(odt-make-style:page-layout-properties-attributes :page-width "21.001cm" :page-height "29.7cm" :num-format "1" :print-orientation "portrait" :margin-top "2cm" :margin-bottom "2cm" :margin-left "2cm" :margin-right "2cm" :writing-mode "lr-tb" :footnote-max-height "0cm"))


(put 'odt-make-style:page-layout-properties-attributes 'function-documentation
"
Constructor for objects of type `odt-style:page-layout-properties-attributes'.

(odt-make-style:page-layout-properties-attributes &key
						  BACKGROUND-COLOR MARGIN-BOTTOM MARGIN-LEFT MARGIN-RIGHT MARGIN-TOP
						  PAGE-HEIGHT PAGE-WIDTH FOOTNOTE-MAX-HEIGHT NUM-FORMAT
						  PRINT-ORIENTATION WRITING-MODE)


Usage:

(odt-make-style:page-layout-properties-attributes
 :page-width \"21.001cm\"
 :page-height \"29.7cm\"
 :num-format \"1\"
 :print-orientation \"portrait\"
 :margin-top \"2cm\"
 :margin-bottom \"2cm\"
 :margin-left \"2cm\"
 :margin-right \"2cm\"
 :writing-mode \"lr-tb\"
 :footnote-max-height \"0cm\")

")



(provide 'odt-structs)

;;; odt-structs.el ends here

;; Local Variables:
;; fill-column: 1000
;; End:
