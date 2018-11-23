include mk/server.mk		# for version

.PHONY:	version odtpkg

help helpall helpserver::
	$(info )
	$(info Maintenance)
	$(info ===========)
	$(info odtpkg             - create ODT package)

helpserver::
	@echo ""

SERVROOT		= .

MIN_ORG_VERSION		= $(ORGVERSION)
MIN_ORG_VERSION_L	= $(shell $(BATCH) --eval '(prin1 (version-to-list "$(MIN_ORG_VERSION)"))')

ODT_PKG_NAME		= ox-odt
ODT_PKG_TAG		= $(ORGVERSION).$(shell git log --format=oneline release_$(ORGVERSION).. | wc -l)
ODT_PKG_TAG_L		= $(shell $(BATCH) --eval '(prin1 (version-to-list "$(ODT_PKG_TAG)"))')
ODT_PKG_DOC		= "OpenDocument Text Exporter for Org Mode"

ODT_PKG_REQ		= (org \"$(MIN_ORG_VERSION)\")
ODT_PKG_REQ_L		= (org $(MIN_ORG_VERSION_L))

ODTDIR			= $(ODT_PKG_NAME)-$(ODT_PKG_TAG)

ODTELPA			= lisp/ox-odt.el				\
				 etc/styles/				\
				 etc/schema/				\
				 contrib/odt/OrgModeUtilities.oxt	\
				 $(ODT_PKG_NAME)-pkg.el

ODTDESC			= (quote ($(ODT_PKG_NAME) . [$(ODT_PKG_TAG_L) ($(ODT_PKG_REQ_L)) $(ODT_PKG_DOC) tar]))

odtpkg: 
	@$(MAKE) GITVERSION=$(GITVERSION:release_%=%)-elpa version autoloads
	-@$(RM) $(ODTDIR) $(ORGTAR) $(ORGZIP)
	-@$(RM) $(SERVROOT)/elpa/$(ODT_PKG_NAME)-*.tar
	ln -s . $(ODTDIR)
	echo "(define-package \"$(ODT_PKG_NAME)\""				>  $(ODT_PKG_NAME)-pkg.el
	echo "  \"$(ODT_PKG_TAG)\" \"$(ODT_PKG_DOC)\" '($(ODT_PKG_REQ)))"	>> $(ODT_PKG_NAME)-pkg.el
	echo ";; Local Variables:"						>> $(ODT_PKG_NAME)-pkg.el
	echo ";; no-byte-compile: t"						>> $(ODT_PKG_NAME)-pkg.el
	echo ";; End:"								>> $(ODT_PKG_NAME)-pkg.el
	tar	--exclude=Makefile						\
		--transform='s|lisp/||'						\
		--transform='s|contrib/odt/|libreoffice/|'			\
		-cf $(SERVROOT)/elpa/$(ODTDIR).tar				\
	  $(foreach dist, $(ODTELPA), $(ODTDIR)/$(dist))
	-@$(RM) $(ODTDIR) $(ODT_PKG_NAME)-pkg.el
	$(BATCH) -l mk/odtpkg.el --eval '(odtpkg-update-archive-contents $(ODTDESC))'

JABREF_PKG_NAME		= ox-jabref
JABREF_PKG_TAG		= $(ORGVERSION).$(shell git log --format=oneline release_$(ORGVERSION).. | wc -l)
JABREF_PKG_TAG_L	= $(shell $(BATCH) --eval '(prin1 (version-to-list "$(JABREF_PKG_TAG)"))')
JABREF_PKG_DOC		= "JabRef Citation Processor for Orgmode"

JABREF_PKG_REQ		= (org \"$(ODT_PKG_TAG)\")
JABREF_PKG_REQ_L	= (ox-odt $(ODT_PKG_TAG_L))

JABREFDIR		= $(JABREF_PKG_NAME)-$(JABREF_PKG_TAG)

JABREFELPA		= contrib/lisp/ox-jabref.el			\
				contrib/odt/JabRefChicagoForOrgmode/	\
				$(JABREF_PKG_NAME)-pkg.el

JABREFDESC		= (quote ($(JABREF_PKG_NAME) . [$(JABREF_PKG_TAG_L) ($(JABREF_PKG_REQ_L)) $(JABREF_PKG_DOC) tar]))

jabrefpkg: 
	@$(MAKE) GITVERSION=$(GITVERSION:release_%=%)-elpa version autoloads
	-@$(RM) $(JABREFDIR) $(ORGTAR) $(ORGZIP)
	-@$(RM) $(SERVROOT)/elpa/$(JABREF_PKG_NAME)-*.tar
	ln -s . $(JABREFDIR)
	echo "(define-package \"$(JABREF_PKG_NAME)\""					>  $(JABREF_PKG_NAME)-pkg.el
	echo "  \"$(JABREF_PKG_TAG)\" \"$(JABREF_PKG_DOC)\" '($(JABREF_PKG_REQ)))"	>> $(JABREF_PKG_NAME)-pkg.el
	echo ";; Local Variables:"							>> $(JABREF_PKG_NAME)-pkg.el
	echo ";; no-byte-compile: t"							>> $(JABREF_PKG_NAME)-pkg.el
	echo ";; End:"									>> $(JABREF_PKG_NAME)-pkg.el
	tar	--exclude=Makefile						\
		--transform='s|contrib/lisp/||'					\
		--transform='s|contrib/odt/JabRefChicagoForOrgmode|jabref|'	\
		-cf $(SERVROOT)/elpa/$(JABREFDIR).tar				\
	  $(foreach dist, $(JABREFELPA), $(JABREFDIR)/$(dist))
	-@$(RM) $(JABREFDIR) $(JABREF_PKG_NAME)-pkg.el
	$(BATCH) --eval '(setq debug-on-error t)' -l mk/odtpkg.el --eval '(odtpkg-update-archive-contents $(JABREFDESC))'
