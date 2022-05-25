<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [The Authoritative fork of Org mode's ODT exporter](#the-authoritative-fork-of-org-modes-odt-exporter)
    - [Installation](#installation)
        - [Ensure that you are using the *enhanced `ox-odt`* and *not* the `ox-odt` that comes with *upstream* `emacs` or `org-mode`](#ensure-that-you-are-using-the-enhanced-ox-odt-and-not-the-ox-odt-that-comes-with-upstream-emacs-or-org-mode)
        - [What to do if the `ox-odt` from previous step is *not* the enhanced `ox-odt`](#what-to-do-if-the-ox-odt-from-previous-step-is-not-the-enhanced-ox-odt)
            - [If you use Emacs' `package.el` ...](#if-you-use-emacs-packageel-)
            - [If you use `straight.el` ...](#if-you-use-straightel-)
            - [If you use Doom Emacs' `straight.el` ...](#if-you-use-doom-emacs-straightel-)
            - [If you use a `git` checkout of this repo ...](#if-you-use-a-git-checkout-of-this-repo-)
            - [If you have lost all hope of using this exporter ...](#if-you-have-lost-all-hope-of-using-this-exporter-)
    - [User Manual](#user-manual)
    - [Blog-style articles](#blog-style-articles)
    - [Getting Help](#getting-help)
    - [FAQs](#faqs)
- [Miscellaneous](#miscellaneous)
    - [Good Resources for learning `LibreOffice`](#good-resources-for-learning-libreoffice)

<!-- markdown-toc end -->

# The Authoritative fork of Org mode's ODT exporter

This is *a fork* of Org's ODT backend and adds many useful improvements to that backend.  This fork is *authoritative* because it is maintained by the *original* author of that backend.

## Installation

Add `https://kjambunathan.github.io/elpa/` to `package-archives`.  Once that is done, you can install the exporter with`M-x list-packages`.

```elisp
(custom-set-variables
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("ox-odt" . "https://kjambunathan.github.io/elpa/")))))
```

 In the Emacs `*Packages*` menu, the entry for the ODT backend looks like

```
ox-odt             9.2.1.205     available  ox-odt     OpenDocument Text Exporter for Org Mode
```

### Ensure that you are using the *enhanced `ox-odt`* and *not* the `ox-odt` that comes with *upstream* `emacs` or `org-mode`

1. Visit an `org` file: `C-x C-f somefie.org`
2. Export it to `odt`: `C-c C-e o o`
3. Switch to  `*Messages*` buffer: `C-x b *Messages*`. Inspect the path of `styles` file.

   Here is a sample run on my setup.

      ```
      ODT Zip Dir is /tmp/odt-fLRSbZ/
      Formatting LaTeX using mathml
      Embedding /home/kjambunathan/src/org-mode-ox-odt/testing/examples/odt/publish/org-mode-unicorn.png as Images/0001.png...
      ----> ox-odt: Content template file is /home/kjambunathan/.emacs.d/elpa/ox-odt-9.5.3.437/etc/styles/OrgOdtContentTemplate.xml**
      Wrote /tmp/odt-fLRSbZ/content.xml
      ----> ox-odt: Styles file is /home/kjambunathan/.emacs.d/elpa/ox-odt-9.5.3.437/etc/styles/OrgOdtStyles.xml**
      Wrote /tmp/odt-fLRSbZ/styles.xml [2 times]
      Wrote /tmp/odt-fLRSbZ/meta.xml
      Wrote /tmp/odt-fLRSbZ/mimetype
      Wrote /tmp/odt-fLRSbZ/META-INF/manifest.xml
      Create OpenDocument file ‘3jacmbl0nfj0.odt’...
      Using schema ~/.emacs.d/elpa/ox-odt-9.5.3.437/etc/schema/od-schema.rnc
      Running zip -mX0 3jacmbl0nfj0.odt mimetype
      Running zip -rmTq 3jacmbl0nfj0.odt .
      Created /home/kjambunathan/src/org-mode-ox-odt/testing/examples/odt/publish/3jacmbl0nfj0.odt

      ```

   The path of the `styles` files suggests that `ox-odt` is coming from `/home/kjambunathan/.emacs.d/elpa/ox-odt-9.5.3.437/` which is exactly what you want.

### What to do if the `ox-odt` from previous step is *not* the enhanced `ox-odt`

#### If you use Emacs' `package.el` ...

1. Add the following to the very end of your init file

    ```elisp
    (progn
      (require 'package)
      (setq load-path
	    (cons
	     (package-desc-dir (package--get-activatable-pkg 'ox-odt))
	     load-path))
      (load-library "ox-odt"))
    ```
2. Re-start Emacs
3. [Ensure that you are using the *enhanced `ox-odt`* and *not* the `ox-odt` that comes with *upstream* `emacs` or `org-mode`](#ensure-that-you-are-using-the-enhanced-ox-odt-and-not-the-ox-odt-that-comes-with-upstream-emacs-or-org-mode)

#### If you use `straight.el` ...

``` elisp
(use-package ox-odt
  :straight (org-mode-ox-odt
	     :host github
	     :repo "kjambunathan/org-mode-ox-odt"
	     :files ("lisp/ox-odt.el"
		     "etc"
		     "docs"
		     "contrib/odt/LibreOffice")))
```

#### If you use Doom Emacs' `straight.el` ...

1. Add the snippet you see below to `~/.doom.d/packages.el`,

``` elisp
      (package! ox-odt
		:recipe
		(
		 :host github
		 :repo "kjambunathan/org-mode-ox-odt"
		 :files ("lisp/ox-odt.el"
			 "etc"
			 "docs"
			 "contrib/odt/LibreOffice")))
```

2. Do `~/.emacs.d/bin/doom sync`.

Doom-emacs uses straight.el, so it may be like this in the straight.el way.

#### If you use a `git` checkout of this repo ...

Assuming that you have checked out the source under `~/src/` directory, add the following at ***THE VERY BOTTOM*** of your `user-init-file`

``` elisp
(setq load-path (cons "~/src/org-mode-ox-odt/lisp/" load-path))
(load-library "ox-odt")
```

#### If you have lost all hope of using this exporter ...

1. Identify the most recent version of `ox-odt` from the [`org-mode-ox-odt` ELPA]( https://github.com/kjambunathan/kjambunathan.github.io/tree/master/elpa).  At the time of writing this note, it is `ox-odt-9.5.3.456.tar`.

2. Download `ox-odt-9.5.3.456.tar` and `untar` it

``` shell
~$ cd ~/Downloads/

~/Downloads$ wget https://raw.githubusercontent.com/kjambunathan/kjambunathan.github.io/master/elpa/ox-odt-9.5.3.456.tar

~/Downloads$ tar xvf ox-odt-9.5.3.456.tar 
```
3. Add the following at ***THE VERY BOTTOM*** of your `user-init-file`

``` elisp
(progn
  (setq load-path
	(cons
	 ;; Replace this path with where `ox-odt.el' can be found
	 "~/Downloads/ox-odt-9.5.3.456/"
	 load-path))
  (load-library "ox-odt"))
```
4. Restart Emacs

## User Manual

See [Top (OpenDocument Text Exporter for Emacs’ Org Mode)](https://kjambunathan.gitlab.io/org-mode-ox-odt/).

If you are an existing user of OpenDocument exporter, start with [What is New (OpenDocument Text Exporter for Emacs’ Org Mode)](https://kjambunathan.gitlab.io/org-mode-ox-odt/What-is-New.html).

If you want a /near complete/, but a /crude/ list of features start with [List of features that are exclusive to The Enhanced OpenDocument Exporter for Org mode](https://github.com/kjambunathan/org-mode-ox-odt/blob/master/notes/SNIPPETS.org).

## Blog-style articles

If you want a blog-style, informal introduction to some of the features of this exporter, you may want to look at the following articles:

- [Create a stylesheet for your OpenDocument files, and inline the XML definitions right in your Org file–Think HTML_HEAD, or HTML_HEAD_EXTRA but for ODT / DOCX files](https://emacsnotes.wordpress.com/2020/06/21/create-a-stylesheet-for-your-opendocument-files-and-inline-the-xml-definitions-right-in-your-org-file-think-html_head-or-html_head_extra-but-for-odt-docx-files/)
- [Create tables with paragraph-like content in Org mode, with the least amount of hassle](https://emacsnotes.wordpress.com/2020/04/26/create-tables-with-paragraph-like-content-in-org-mode-with-the-least-amount-of-hassle/)
- [Use Starmath—NOT LaTeX, NOT MathML—when exporting Org mode files to LibreOffice](https://emacsnotes.wordpress.com/2021/12/22/use-starmath-not-latex-not-mathml-when-exporting-org-mode-files-to-libreoffice/)
- [Mix Starmath and LaTeX / Mix English and Tamil in same Org file (or) How to conditionally export text using Macros and Drawers in Org mode](https://emacsnotes.wordpress.com/2021/12/25/mix-starmath-and-latex-mix-english-and-tamil-in-same-org-file-or-how-to-conditionally-export-text-using-macros-and-drawers-in-org-mode/)

## Getting Help

1. Consult the manual [Top (OpenDocument Text Exporter for Emacs’ Org
   Mode)](https://kjambunathan.gitlab.io/org-mode-ox-odt/).
2. Search [`Issues` page](https://github.com/kjambunathan/org-mode-ox-odt/issues).  Whenever a new feature is added, I add a very user-friendly note to how that feature may be put to use.
3. If you have an issue or feature request, open an issue at [`Issues` page](https://github.com/kjambunathan/org-mode-ox-odt/issues)
4. If you have a question, you can visit [`Discussions` page](https://github.com/kjambunathan/org-mode-ox-odt/discussions)

## FAQs

1. *Will you merge this repo to upstream `Orgmode` or `GNU Emacs`?*

    _Never_

2. *Will you put this repo on `MELPA` or `GNU ELPA`?*

    _Never_

3. *Do you accept `Pull Requests`?*

   _I prefer bug reports  and feature requests_.

# Miscellaneous

## Good Resources for learning `LibreOffice`

If you would like to customize the look of a document produced by this exporter, a good familiarity with `LibreOffice`—this means  working with `styles`—is a must.

`LibreOffice` comes with good set of [User Guides](https://documentation.libreoffice.org/en/english-documentation/).  If you find that these guides are too detailed for your  immediate needs, you may want to start with one of these:

1. [Students' Guide to OpenOffice Writer](https://web.archive.org/web/20220121040747/https://wiki.documentfoundation.org/images/0/01/Ooo_for_students.pdf) 
2. [Writing a Thesis in OpenOffice.org](https://web.archive.org/web/20220525051011/http://www.openoffice.org/documentation/HOW_TO/word_processing/How_to_Write_a_Thesis_in_OOo.pdf)
3. [Self-Publishing using LibreOffice Writer 6: How to use free software to write, design, and create ebooks and PDFs for print-on-demand books](https://web.archive.org/web/20220525050638/https://www.taming-libreoffice.com/wp-content/uploads/2019/10/SelfPublishWithLibreOffice6.pdf).  For more recent versions, see [Taming LibreOffice Resources for intermediate & advanced users](https://taming-libreoffice.com/my-books/).
