COV_FILE=coverage.info

.PHONY: all clean test \
	unit-test-with-cov \
	litmus \
	static \
	$(COV_FILE) \
	genhtml \
	lcov.info \
	unit-test-deps

GUILE := guile
export GUILE

GUILD := guild

GUILE_VERSION=$(shell $(GUILE) -c '(display (version))')

GUILE_SITE_DIR=$(shell $(GUILE) -c "(display (%site-dir))")
GUILE_CCACHE_DIR=$(shell $(GUILE) -c "(display (%site-ccache-dir))")

SCM_FILES = $(shell find module/ -type f -name \*.scm)
GO_FILES = $(SCM_FILES:%.scm=obj-$(GUILE_VERSION)/%.go)
SCM_UNIT_TESTS = $(shell find tests/unit/ -type f -name \*.scm)
GO_UNIT_TESTS = $(SCM_UNIT_TESTS:%.scm=obj-$(GUILE_VERSION)/%.go)

TEST_FILES = $(shell find tests/unit/util/ -type f -name \*.scm)

GUILE_ENV = GUILE_LOAD_PATH=$(PWD)/module:$(PWD)/tests/unit \
	GUILE_LOAD_COMPILED_PATH=$(PWD)/obj-$(GUILE_VERSION)/module:$(PWD)/obj-$(GUILE_VERSION)/obj-3.0.9/tests/util \
	GUILE_AUTO_COMPILE=0

GUILE_C_FLAGS = -Lmodule -Ltests/util \
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

all: calp $(GO_FILES) static $(LOCALIZATIONS)
	$(MAKE) -C doc/ref

calp: calp.c
	$(CC) -ggdb $(CFLAGS) -DBUILD_ENV $(LDFLAGS) -o $@ $< $(LDLIBS)

calp-release: calp.c
	$(CC) -O2 $(CFLAGS) $(LDFLAGS) -o $@ $< $(LDLIBS)

cpucount: cpucount.c
	$(CC) -o $@ $<

XGETTEXT_FLAGS = --from-code=UTF-8 --add-comments --indent -kG_

static:
	$(MAKE) -C static

obj-$(GUILE_VERSION)/%.go: %.scm
	@echo $(GUILD) $(GUILE_VERSION) compile $<
	@env $(GUILE_ENV) $(GUILD) compile $(GUILE_C_FLAGS) -o $@ $< >/dev/null

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
	rsync -a obj-$(GUILE_VERSION)/module $(DESTDIR)$(GUILE_CCACHE_DIR)
	$(MAKE) -C static install
	$(MAKE) -C system install
	$(MAKE) -C doc/ref install
	install -m 644 -D -t $(DESTDIR)/usr/share/doc/calp README.md
	install -m 755 -D -t $(DESTDIR)/usr/lib/calp/ scripts/tzget
	install -m755 -D calp-release $(DESTDIR)/usr/bin/calp

# TODO a test completed with no errors should emit a .test-accepted
# file, which make can then use as a target.

unit-test-deps: calp $(GO_UNIT_TESTS) $(GO_FILES) $(TEST_FILES)

THREADS = $(shell echo $$(( $(shell ./cpucount) / 2 )))

# TODO (current-processor-count)
$(COV_FILE): cpucount unit-test-deps
	./testrunner.scm --threads $(THREADS)  --coverage $@ --coverage-supplement tests/unit/coverage-supplement.scm

GENHTML_FLAGS=--show-details \
			  --hierarchical \
			  --prefix $(shell pwd) \
			  --no-function-coverage \
			  --quiet

# TODO order between these?
# Is it guaranteed that COV_FILE will be made before genhtml?
# Run full coverage test, then generate resulting HTML directory
coverage: $(COV_FILE) genhtml

# Only generate HTML output from coverage info.
# Allows manually running
#     ./testrunner.scm --file <indiviual-file> -coverage coverage.info
# To quickly get coverage data for a specific file.
genhtml:
	genhtml $(GENHTML_FLAGS) --output-directory coverage $(COV_FILE)


CHECK_FLAGS = \
	$(if $(CATCH),--catch) \
	$(if $(VERBOSE),--verbose) \
	$(SKIP) $(LIMIT_FILES)

litmus:
	tests/litmus.scm $(path)
