.PHONY: all clean test \
	check \
	static \
	go_files \
	lcov.info

GUILE := guile
export GUILE

GUILD := guild

GUILE_VERSION=$(shell $(GUILE) -c '(display (version))')

GUILE_SITE_DIR=$(shell $(GUILE) -c "(display (%site-dir))")
GUILE_CCACHE_DIR=$(shell $(GUILE) -c "(display (%site-ccache-dir))")

SCM_FILES = $(shell find module/ -type f -name \*.scm)
GO_FILES = $(SCM_FILES:module/%.scm=obj-$(GUILE_VERSION)/%.go)

GUILE_ENV = GUILE_LOAD_PATH=$(PWD)/module \
			GUILE_LOAD_COMPILED_PATH=$(PWD)/obj-$(GUILE_VERSION)

GUILE_C_FLAGS = -Lmodule \
				-Wshadowed-toplevel -Wunbound-variable \
				-Wmacro-use-before-definition -Warity-mismatch \
				-Wduplicate-case-datum -Wbad-case-datum

CLIBS = guile-3.0
CFLAGS = -Wall -pedantic -std=c11 $(shell pkg-config --cflags $(CLIBS))
LDLIBS = $(shell pkg-config --libs $(CLIBS))
LDFLAGS = -lrt

# All po-files inside po/, except new.po, and hidden files
PO_FILES = $(shell find po -type f -name \*.po -and -not -name new.po -and -not -name .\*)
LOCALIZATIONS = $(PO_FILES:po/%.po=localization/%/LC_MESSAGES/calp.mo)

# Limit test to these files
LIMIT_FILES=$(LIMIT:%=--only %)
# Skip these files when testing
SKIP=--skip $(PWD)/tests/test/web-server.scm

all: calp go_files static $(LOCALIZATIONS)
	$(MAKE) -C doc/ref

calp: calp.c
	$(CC) -ggdb $(CFLAGS) -DBUILD_ENV $(LDFLAGS) -o $@ $< $(LDLIBS)

calp-release: calp.c
	$(CC) -O2 $(CFLAGS) $(LDFLAGS) -o $@ $< $(LDLIBS)

XGETTEXT_FLAGS = --from-code=UTF-8 --add-comments --indent -kG_

static:
	$(MAKE) -C static

obj-$(GUILE_VERSION)/%.go: module/%.scm
	@echo $(GUILD) $(GUILE_VERSION) compile $<
	@env $(GUILE_ENV) $(GUILD) compile $(GUILE_C_FLAGS) -o $@ $< >/dev/null

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
	-rm calp
	-rm calp-release

install: all calp-release
	install -d $(DESTDIR)$(GUILE_SITE_DIR)  $(DESTDIR)$(GUILE_CCACHE_DIR)
	rsync -a module/ $(DESTDIR)$(GUILE_SITE_DIR)
	rsync -a obj-$(GUILE_VERSION)/ $(DESTDIR)$(GUILE_CCACHE_DIR)
	$(MAKE) -C static install
	$(MAKE) -C system install
	$(MAKE) -C doc/ref install
	install -m 644 -D -t $(DESTDIR)/usr/share/doc/calp README.md
	install -m 755 -D -t $(DESTDIR)/usr/lib/calp/ scripts/tzget
	install -m755 -D calp-release $(DESTDIR)/usr/bin/calp

lcov.info: $(GO_FILES)
	env DEBUG=0 tests/run-tests.scm --coverage=$@ $(if $(VERBOSE),--verbose) $(SKIP) $(LIMIT_FILES)

test: coverage

GENHTML_FLAGS=--show-details \
			  --prefix $(shell pwd)/module \
			  --no-function-coverage \
			  --quiet

coverage: lcov.info
	genhtml $(GENHTML_FLAGS) --output-directory $@ $<

check:
	tests/run-tests.scm $(if $(VERBOSE),--verbose) $(SKIP) $(LIMIT_FILES)
