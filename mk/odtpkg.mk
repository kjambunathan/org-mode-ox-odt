include mk/server.mk		# for version

.PHONY:	version odtpkg jabrefpkg

help helpall helpserver::
	$(info )
	$(info Maintenance)
	$(info ===========)
	$(info odtpkg             - create ODT package)

helpserver::
	@echo ""

SERVROOT			= ../kjambunathan.github.io/elpa

ORG_GIT_DIR			= .
JABREF_GIT_DIR		= ./contrib/odt/JabRefChicagoForOrgmode

JABREF_VERSION0		= $(shell git --git-dir=$(JABREF_GIT_DIR)/.git describe --abbrev=0)
JABREF_VERSION		= $(JABREF_VERSION0).$(shell git --git-dir=$(JABREF_GIT_DIR)/.git log --format=oneline $(JABREF_VERSION0).. | wc -l)
JABREF_VERSION_L	= $(shell $(BATCH) --eval '(prin1 (version-to-list "$(JABREF_VERSION)"))')



pkg:
	@$(MAKE) GITVERSION=$(GITVERSION:release_%=%)-elpa version autoloads
	-@$(RM) $(ELPA_PKG_DIR) $(ELPA_PKG_DIR).tar $(ELPA_PKG_NAME)-pkg.el
	-@$(RM) $(SERVROOT)/$(ELPA_PKG_NAME)-*.tar
	ln -s . $(ELPA_PKG_DIR)
	echo "(define-package \"$(ELPA_PKG_NAME)\""								> $(ELPA_PKG_NAME)-pkg.el
	echo " \"$(ELPA_PKG_VERSION)\" \"$(ELPA_PKG_DOC)\" '($(ELPA_PKG_REQ)))" >> $(ELPA_PKG_NAME)-pkg.el
	echo ";; Local Variables:"												>> $(ELPA_PKG_NAME)-pkg.el
	echo ";; tab-width: 4"													>> $(ELPA_PKG_NAME)-pkg.el
	echo ";; End:"															>> $(ELPA_PKG_NAME)-pkg.el
	echo ";; no-byte-compile: t"											>> $(ELPA_PKG_NAME)-pkg.el
	echo ";; End:"															>> $(ELPA_PKG_NAME)-pkg.el
	tar $(ELPA_PKG_TAR_ARGS) -cf $(ELPA_PKG_DIR).tar $(foreach file, $(ELPA_PKG_FILES), $(ELPA_PKG_DIR)/$(file))
	$(BATCH) -l package-x --eval '(let ((package-archive-upload-base "$(SERVROOT)")) (with-demoted-errors (package-upload-file "$(ELPA_PKG_DIR).tar")))'
	-@$(RM) $(ELPA_PKG_DIR) $(ELPA_PKG_DIR).tar $(ELPA_PKG_NAME)-pkg.el



odtpkg: ELPA_PKG_NAME				= ox-odt
odtpkg: ELPA_PKG_GIT_DIR			= $(ORG_GIT_DIR)

odtpkg: ELPA_PKG_VERSION0			= $(ORGVERSION)
odtpkg: ELPA_PKG_VERSION			= $(ELPA_PKG_VERSION0).$(shell git --git-dir=$(ELPA_PKG_GIT_DIR)/.git \
										log --format=oneline release_$(ELPA_PKG_VERSION0).. | wc -l)

odtpkg: ELPA_PKG_VERSION0_L			= $(shell $(BATCH) --eval '(prin1 (version-to-list "$(ELPA_PKG_VERSION0)"))')
odtpkg: ELPA_PKG_VERSION_L			= $(shell $(BATCH) --eval '(prin1 (version-to-list "$(ELPA_PKG_VERSION)"))')

odtpkg: ELPA_PKG_DOC				= "OpenDocument Text Exporter for Org Mode"

odtpkg: ELPA_PKG_REQ				= (org \"$(ELPA_PKG_VERSION0)\") (JabrefExportChicagoODF \"$(JABREF_VERSION)\")
odtpkg: ELPA_PKG_REQ_L				= (org $(ELPA_PKG_VERSION0_L)) (JabrefExportChicagoODF $(JABREF_VERSION_L))

odtpkg: ELPA_PKG_DIR				= $(ELPA_PKG_NAME)-$(ELPA_PKG_VERSION)

odtpkg: ELPA_PKG_FILES				= lisp/ox-odt.el									\
										etc/styles/										\
										etc/schema/										\
										contrib/lisp/ox-jabref.el						\
										contrib/odt/OrgModeUtilities.oxt				\
										testing/examples/odt/							\
										$(ELPA_PKG_NAME)-pkg.el

odtpkg: ELPA_PKG_TAR_ARGS			= --exclude=test-new.odt							\
										--exclude=subdocument2.*						\
										--transform='s|contrib/lisp/||'					\
										--transform='s|lisp/||'							\
										--transform='s|contrib/odt/|libreoffice/|'		\
										--transform='s|testing/examples/odt/|samples/|'	\

odtpkg: ELPA_PKG_DESC				= (quote ($(ELPA_PKG_NAME) . [$(ELPA_PKG_VERSION_L) ($(ELPA_PKG_REQ_L)) $(ELPA_PKG_DOC) tar]))



jabrefpkg: ELPA_PKG_NAME		= JabrefExportChicagoODF
jabrefpkg: ELPA_PKG_GIT_DIR		= $(JABREF_GIT_DIR)

jabrefpkg: ELPA_PKG_VERSION0	= $(shell git --git-dir=$(ELPA_PKG_GIT_DIR)/.git describe --abbrev=0)
jabrefpkg: ELPA_PKG_VERSION		= $(ELPA_PKG_VERSION0).$(shell git --git-dir=$(ELPA_PKG_GIT_DIR)/.git \
									log --format=oneline $(ELPA_PKG_VERSION0).. | wc -l)
jabrefpkg: ELPA_PKG_VERSION_L	= $(shell $(BATCH) --eval '(prin1 (version-to-list "$(ELPA_PKG_VERSION)"))')

jabrefpkg: ELPA_PKG_REQ			= ""
jabrefpkg: ELPA_PKG_REQ_L		= ()

jabrefpkg: ELPA_PKG_DOC			= "Jabref Plugin for export to Chicago Manual of Style in OpenDocumentFormat"

jabrefpkg: ELPA_PKG_DESC		= (quote ($(ELPA_PKG_NAME) . [$(ELPA_PKG_VERSION_L) ($(ELPA_PKG_REQ_L)) $(ELPA_PKG_DOC) tar]))

jabrefpkg: ELPA_PKG_DIR			= $(ELPA_PKG_NAME)-$(ELPA_PKG_VERSION)
jabrefpkg: ELPA_PKG_FILES		= contrib/odt/JabRefChicagoForOrgmode/* $(ELPA_PKG_NAME)-pkg.el
jabrefpkg: ELPA_PKG_TAR_ARGS	= --exclude=*.jar											\
									--exclude=*.xml											\
									--transform='s|contrib/odt/JabRefChicagoForOrgmode/||'	\



odtpkg jabrefpkg: pkg

# Local Variables:
# tab-width: 4
# End:
