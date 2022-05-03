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

## User Manual

See [Top (OpenDocument Text Exporter for Emacs’ Org Mode)](https://kjambunathan.gitlab.io/org-mode-ox-odt/).

If you are an existing user of OpenDocument exporter, start with [What is New (OpenDocument Text Exporter for Emacs’ Org Mode)](https://kjambunathan.gitlab.io/org-mode-ox-odt/What-is-New.html).

If you want a /near complete/, but a /crude/ list of features start with [List of features that are exclusive to The Enhanced OpenDocument Exporter for Org mode](https://github.com/kjambunathan/org-mode-ox-odt/blob/master/notes/SNIPPETS.org).

## Ensure that you are using the enhanced `ox-odt` and not the `ox-odt` that comes with upstream `emacs` or `org-mode`

1. Visit an `org` file: `C-x C-f somefie.org`
2. Export it to `odt`: `C-c C-e o o`
3. Switch to  `*Messages*` buffer: `C-x b *Messages*`. Inspect the
   path of `styles` file.

   Here is a sample run on my setup.

    > ODT Zip Dir is /tmp/odt-fLRSbZ/
    > Formatting LaTeX using mathml
    > Embedding /home/kjambunathan/src/org-mode-ox-odt/testing/examples/odt/publish/org-mode-unicorn.png as Images/0001.png...
    > **ox-odt: Content template file is /home/kjambunathan/.emacs.d/elpa/ox-odt-9.5.3.437/etc/styles/OrgOdtContentTemplate.xml**
    > Wrote /tmp/odt-fLRSbZ/content.xml
    > **ox-odt: Styles file is /home/kjambunathan/.emacs.d/elpa/ox-odt-9.5.3.437/etc/styles/OrgOdtStyles.xml**
    > Wrote /tmp/odt-fLRSbZ/styles.xml [2 times]
    > Wrote /tmp/odt-fLRSbZ/meta.xml
    > Wrote /tmp/odt-fLRSbZ/mimetype
    > Wrote /tmp/odt-fLRSbZ/META-INF/manifest.xml
    > Create OpenDocument file ‘3jacmbl0nfj0.odt’...
    > Using schema ~/.emacs.d/elpa/ox-odt-9.5.3.437/etc/schema/od-schema.rnc
    > Running zip -mX0 3jacmbl0nfj0.odt mimetype
    > Running zip -rmTq 3jacmbl0nfj0.odt .
    > Created /home/kjambunathan/src/org-mode-ox-odt/testing/examples/odt/publish/3jacmbl0nfj0.odt

   The path of the `styles` file suggests that `ox-odt` is coming from `/home/kjambunathan/.emacs.d/elpa/ox-odt-9.5.3.437/` which is exactly what you want.

## What to do if the `ox-odt` from previous step is not the enhanced `ox-odt`

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
3. Ensure that you are using the enhanced `ox-odt`.

## Blog-style artciles

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
