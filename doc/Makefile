.SUFFIXES:	# we don't need default suffix rules
ifeq ($(MAKELEVEL), 0)
  $(error This make needs to be started as a sub-make from the toplevel directory.)
endif
.PHONY:		all info html pdf card manual guide install \
		clean cleanall clean-install

all:		$(ORG_MAKE_DOC)

info:		org orgguide org-odt

html:		org.html orgguide.html

pdf:		org.pdf orgguide.pdf

card:		orgcard.pdf orgcard_letter.pdf orgguide.pdf

ifneq ($(SERVERMK),)
manual guide::
	-$(RMR) $@
	$(MKDIR) $@
manual::	org.texi org-version.inc
	$(TEXI2HTML) -o $@ $<
	../mk/mansplit.pl $@/*
guide::		orgguide.texi org-version.inc
	$(TEXI2HTML) -o $@ $<
	../mk/guidesplit.pl $@/*
endif

# We increase GC threshold when generating documentation.  This
# imrpoves the performance significantly.  See
# https://yhetil.org/emacs-devel/9778F176-E724-4E61-B0FB-327BCDD316C0@acm.org
# Too high number may cause memory overflow (for example, it has
# happened in CI during automated build).  We choose 50Mb threshold,
# based on the above discussion.
org.texi:	org-manual.org
	$(BATCH) 				      \
	  --eval '(add-to-list `load-path "../lisp")' \
	  --eval '(load "../mk/org-fixup.el")' 	      \
	  --eval '(setq gc-cons-threshold (* 50 1000 1000))' \
	  --eval '(org-make-manual)'

orgguide.texi:	org-guide.org
	$(BATCH) 				      \
	  --eval '(add-to-list `load-path "../lisp")' \
	  --eval '(load "../mk/org-fixup.el")' 	      \
	  --eval '(setq gc-cons-threshold (* 50 1000 1000))' \
	  --eval '(org-make-guide)'

org-version.inc:	org.texi
	@echo "org-version: $(ORGVERSION) ($(GITVERSION))"
	@echo "@c automatically generated, do not edit"  > org-version.inc
	@echo "@set VERSION $(ORGVERSION) ($(GITVERSION))" >> org-version.inc
	@echo "@set DATE $(DATE)" >> org-version.inc

org-version.tex:	orgcard.tex
	@printf "org-version: $(ORGVERSION) ($(GITVERSION))\n"
	@printf "%% automatically generated, do not edit\n" > org-version.tex
	@printf "\def\orgversionnumber{$(ORGVERSION)}\n" >> org-version.tex
	@printf "\def\\\\versionyear{$(YEAR)}\n" >> org-version.tex
	@printf "\def\year{$(YEAR)}\n" >> org-version.tex

imagelinks:
	$(RM) *.png
	 $(foreach f, $(wildcard org-odt-manual/*.png), ln -s $f  $(notdir $f);)

org-odt.texi: org-odt-manual.org imagelinks
	$(BATCH) \
	  --eval '(add-to-list '"'"'load-path "../lisp")' \
	  --eval '(load "../mk/org-fixup.el")' \
	  --eval '(org-make-manuals (list "$<"))'

org-odt.html: TEXI2HTMLEXTRA=--css-ref=style.css --split=chapter --no-headers  -o org-odt-manual
org-odt.html:		org-odt.texi
	$(TEXI2HTML) $(TEXI2HTMLEXTRA) $<

org-odt.pdf: org-odt.texi
	texi2pdf $<

install:	org orgguide
	if [ ! -d $(DESTDIR)$(infodir) ]; then $(MKDIR) $(DESTDIR)$(infodir); else true; fi ;
	$(CP) org.info $(DESTDIR)$(infodir)
	$(CP) orgguide.info $(DESTDIR)$(infodir)
	$(INSTALL_INFO) --infodir=$(DESTDIR)$(infodir) org.info
	$(INSTALL_INFO) --infodir=$(DESTDIR)$(infodir) orgguide.info

clean:
	$(RM) org orgguide org-odt *.pdf *.html *.info *_letter.tex org-version.inc \
	      org-version.tex *.aux *.cp *.cps *.dvi *.fn *.fns *.ky *.kys \
              *.pg *.pgs *.toc *.tp *.tps *.vr *.vrs *.log *.html *.ps
	$(RMR) org-odt-manual/*.html
	$(RM) *.png
cleanall:	clean
	$(RM) org.texi orgguide.texi
	$(RMR) guide manual

clean-install:
	$(RM) $(DESTDIR)$(infodir)/org*
	$(INSTALL_INFO) --infodir=$(DESTDIR)$(infodir) --remove org
	$(INSTALL_INFO) --infodir=$(DESTDIR)$(infodir) --remove orgguide

.SUFFIXES:	.texi .tex .txt _letter.tex

%:		%.texi org-version.inc
	$(MAKEINFO) --no-split $< -o $@.info

# the following two lines work around a bug in some versions of texi2dvi
%.pdf:		LC_ALL=C
%.pdf:		LANG=C
%.pdf:		%.texi org-version.inc
	$(TEXI2PDF) $<
%.pdf:		%.tex org-version.tex
	PDFLATEX=$(PDFTEX) $(TEXI2PDF) $<

%.html:		%.texi org-version.inc
	$(TEXI2HTML) $(TEXI2HTMLEXTRA) $<
ifneq ($(SERVERMK),)
	../mk/manfull.pl $@

%.txt:		%.tex
	perl ../mk/orgcard2txt.pl $< > $@
endif

%_letter.tex:	%.tex
	$(BATCH) 				      \
	  --eval '(add-to-list `load-path "../lisp")' \
	  --eval '(load "org-compat.el")' 	      \
	  --eval '(load "../mk/org-fixup.el")' 	      \
	  --eval '(org-make-letterformat "$(<F)" "$(@F)")'
