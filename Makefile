.PHONY: all clean test \
	static \
	go_files

GUILE := guile
export GUILE

GUILD := guild

GUILE_VERSION=$(shell $(GUILE) -c '(display (version))')

GUILE_SITE_DIR=$(shell $(GUILE) -c "(display (%site-dir))")
GUILE_CCACHE_DIR=$(shell $(GUILE) -c "(display (%site-ccache-dir))")

SCM_FILES = $(shell find module/ -type f -name \*.scm)
GO_FILES = $(SCM_FILES:module/%.scm=obj-$(GUILE_VERSION)/%.go)

GUILE_C_FLAGS = -Lmodule \
				-Wshadowed-toplevel -Wunbound-variable \
				-Wmacro-use-before-definition -Warity-mismatch \
				-Wduplicate-case-datum -Wbad-case-datum

# All po-files inside po/, except new.po, and hidden files
PO_FILES = $(shell find po -type f -name \*.po -and -not -name new.po -and -not -name .\*)
LOCALIZATIONS = $(PO_FILES:po/%.po=localization/%/LC_MESSAGES/calp.mo)

all: go_files README static $(LOCALIZATIONS)
	$(MAKE) -C doc/ref

XGETTEXT_FLAGS = --from-code=UTF-8 --add-comments --indent -k_

static:
	$(MAKE) -C static

obj-$(GUILE_VERSION)/%.go: module/%.scm
	@echo $(GUILD) $(GUILE_VERSION) compile $<
	@$(GUILD) compile $(GUILE_C_FLAGS) -o $@ $< >/dev/null

# Phony target used by test/run-tests.scm and main to
# automatically compile everything before they run.
go_files: $(GO_FILES)

po/%.po: $(SCM_FILES)
	xgettext $(XGETTEXT_FLAGS) --output $@ -L scheme $^ --join-existing --omit-header --no-location

po/new.po: $(SCM_FILES)
	xgettext $(XGETTEXT_FLAGS) --output $@ -L scheme $^

localization/%/LC_MESSAGES/calp.mo: po/%.po
	-@mkdir -p $(shell dirname $@)
	msgfmt --check -o $@ $<

clean:
	-$(MAKE) -C static clean
	-rm -r obj-*

install: all
	install -d $(DESTDIR)$(GUILE_SITE_DIR)  $(DESTDIR)$(GUILE_CCACHE_DIR)
	rsync -a module/ $(DESTDIR)$(GUILE_SITE_DIR)
	rsync -a obj-$(GUILE_VERSION)/ $(DESTDIR)$(GUILE_CCACHE_DIR)
	install -d $(DESTDIR)/usr/share/calp/www
	$(MAKE) -C static install
	$(MAKE) -C doc/ref install
	install -m 644 -D -t $(DESTDIR)/usr/share/doc/calp README
	install -m 755 -D -t $(DESTDIR)/usr/lib/calp/ scripts/tzget
	install -m755 -D production-main $(DESTDIR)/usr/bin/calp

README: README.in
	./main text < README.in | sed "s/<<today>>/`date -I`/" > README

lcov.info: $(GO_FILES)
	env DEBUG=1 tests/run-tests.scm --coverage=$@

test: coverage

GENHTML_FLAGS=--show-details \
			  --prefix $(shell pwd)/module \
			  --no-function-coverage \
			  --quiet

coverage: lcov.info
	genhtml $(GENHTML_FLAGS) --output-directory $@ $<
