include mk/server.mk		# for version

.PHONY:	version odtpkg

help helpall helpserver::
	$(info )
	$(info Maintenance)
	$(info ===========)
	$(info odtpkg             - create ODT package)

helpserver::
	@echo ""

SERVROOT = .

MIN_ORG_VERSION		= $(ORGVERSION)
MIN_ORG_VERSION_L	= $(shell $(BATCH) --eval '(prin1 (version-to-list "$(MIN_ORG_VERSION)"))')

ODT_PKG_NAME		= ox-odt
ODT_PKG_TAG		= $(ORGVERSION).$(shell git log --format=oneline release_9.1.14.. | wc -l)
ODT_PKG_DOC		= "OpenDocument Text Exporter for Org Mode"

ODT_PKG_REQ		= (org \"$(MIN_ORG_VERSION)\")
ODT_PKG_REQ_L		= (org $(MIN_ORG_VERSION_L))

ODTDIR			=$(ODT_PKG_NAME)-$(ODT_PKG_TAG)

ODTELPA			= lisp/ox-odt.el			\
				 contrib/lisp/ox-jabref.el	\
				 etc/styles/			\
				 etc/schema/			\
				 $(ODT_PKG_NAME)-pkg.el

odtpkg: 
	@$(MAKE) GITVERSION=$(GITVERSION:release_%=%)-elpa version autoloads
	-@$(RM) $(ODTDIR) $(ORGTAR) $(ORGZIP)
	-@$(RM) $(SERVROOT)/elpa/*
	ln -s . $(ODTDIR)
	echo "(define-package \"$(ODT_PKG_NAME)\""				>  $(ODT_PKG_NAME)-pkg.el
	echo "  \"$(ODT_PKG_TAG)\" \"$(ODT_PKG_DOC)\" '($(ODT_PKG_REQ)))"	>> $(ODT_PKG_NAME)-pkg.el
	echo ";; Local Variables:"						>> $(ODT_PKG_NAME)-pkg.el
	echo ";; no-byte-compile: t"						>> $(ODT_PKG_NAME)-pkg.el
	echo ";; End:"								>> $(ODT_PKG_NAME)-pkg.el
	tar --exclude=Makefile \
	  --transform='s:\(contrib/lisp\|lisp\|doc\)/::' \
	  -cf $(SERVROOT)/elpa/$(ODTDIR).tar \
	  $(foreach dist, $(ODTELPA), $(ODTDIR)/$(dist))
	-@$(RM) $(ODTDIR) $(ODT_PKG_NAME)-pkg.el
	echo "(1 ($(ODT_PKG_NAME) . [($(ODT_PKG_TAG)) ($(ODT_PKG_REQ_L)) \"$(ODT_PKG_DOC)\" tar]))" \
		> $(SERVROOT)/elpa/archive-contents
