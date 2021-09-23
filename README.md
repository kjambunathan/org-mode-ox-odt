# The Authoritative fork of Org mode's ODT exporter 

This is *a fork* of Org's ODT backend and adds many useful improvements to that backend.  This fork is *authoritative* because it is maintained by the *original* author of that backend.  

## Installation

Add `https://kjambunathan.github.io/elpa/` to `package-archives`.  Once that is done, you can install the exporter with`M-x list-packages`. 

```
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

See [Top (OpenDocument Text Exporter for Emacs’ Org Mode)](https://kjambunathan.github.io/org-mode-ox-odt/).  

If you are an existing user of OpenDocument exporter, start with [What is New (OpenDocument Text Exporter for Emacs’ Org Mode)](https://kjambunathan.github.io/org-mode-ox-odt/What-is-New.html#What-is-New)


## Getting Help

1. Consult the manual [Top (OpenDocument Text Exporter for Emacs’ Org Mode)](https://kjambunathan.github.io/org-mode-ox-odt/).
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
