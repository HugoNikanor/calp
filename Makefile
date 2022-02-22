.PHONY: all clean test \
	static coverage

GUILE_SITE_DIR=$(shell guile -c "(display (%site-dir))")
GUILE_CCACHE_DIR=$(shell guile -c "(display (%site-ccache-dir))")

SCM_FILES = $(shell find module/ -type f -name \*.scm)
GO_FILES = $(SCM_FILES:module/%.scm=obj/%.go)

GUILE_C_FLAGS = -Lmodule \
				-Wunused-toplevel \
				-Wshadowed-toplevel -Wunbound-variable \
				-Wmacro-use-before-definition -Warity-mismatch \
				-Wduplicate-case-datum -Wbad-case-datum

# All po-files inside po/, except new.po, and hidden files
PO_FILES = $(shell find po -type f -name \*.po -and -not -name new.po -and -not -name .\*)
LOCALIZATIONS = $(PO_FILES:po/%.po=localization/%/LC_MESSAGES/calp.mo)

all: $(GO_FILES) README static $(LOCALIZATIONS)

XGETTEXT_FLAGS = --from-code=UTF-8 --add-comments --indent -k_

static:
	$(MAKE) -C static

po/%.po: $(SCM_FILES)
	xgettext $(XGETTEXT_FLAGS) --output $@ -L scheme $^ --join-existing --omit-header

po/new.po: $(SCM_FILES)
	xgettext $(XGETTEXT_FLAGS) --output $@ -L scheme $^

localization/%/LC_MESSAGES/calp.mo: po/%.po
	-@mkdir -p $(shell dirname $@)
	msgfmt --check -o $@ $<

obj/%.go: module/%.scm
	@mkdir -p obj
	@echo guild compile $<
	@guild compile $(GUILE_C_FLAGS) -o $@ $<

clean:
	$(MAKE) -C static clean
	-rm -r obj

install: all
	install -d $(DESTDIR)$(GUILE_SITE_DIR)  $(DESTDIR)$(GUILE_CCACHE_DIR)
	rsync -a module/ $(DESTDIR)$(GUILE_SITE_DIR)
	rsync -a obj/ $(DESTDIR)$(GUILE_CCACHE_DIR)
	install -d $(DESTDIR)/usr/share/calp/www
	rsync -a static $(DESTDIR)/usr/share/calp/www
	install -m 644 -D -t $(DESTDIR)/usr/share/doc/calp README
	install -m 755 -D -t $(DESTDIR)/usr/lib/calp/ scripts/tzget
	install -D production-main $(DESTDIR)/usr/bin/calp

README: README.in
	./main text < README.in | sed "s/<<today>>/`date -I`/" > README

test:
	tests/run-tests.scm
	$(MAKE) coverage

coverage:
	genhtml \
		--show-details \
		--output-directory coverage \
		--prefix $(shell pwd) \
		--no-function-coverage \
		lcov.info
