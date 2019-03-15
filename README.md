# The Authoritative fork of Org mode's ODT exporter 

This is *a fork* of Org's ODT backend and adds many useful improvements to that backend.  This fork is *authoritative* because it is maintained by the *original* author of that backend.

## Installation

Add `https://kjambunathan.github.io/elpa/` to `pacakage-archives`.  Once that is done, you can install the exporter`M-x list-packages`. 

(custom-set-variables
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("ox-odt" . "https://kjambunathan.github.io/elpa/")))))
```
 
 In the Emacs `*Packages* menu, the entry for the ODT backend looks like
 
 
```
ox-odt             9.2.1.205     available  ox-odt     OpenDocument Text Exporter for Org Mode
```
