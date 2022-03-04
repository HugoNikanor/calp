.PHONY: all clean test \
	static coverage \
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
				-Wunused-toplevel \
				-Wshadowed-toplevel -Wunbound-variable \
				-Wmacro-use-before-definition -Warity-mismatch \
				-Wduplicate-case-datum -Wbad-case-datum

all: go_files README static

static:
	$(MAKE) -C static

obj-$(GUILE_VERSION)/%.go: module/%.scm
	@echo $(GUILD) compile $<
	@$(GUILD) compile $(GUILE_C_FLAGS) -o $@ $<

# Phony target used by test/run-tests.scm and main to
# automatically compile everything before they run.
go_files: $(GO_FILES)

clean:
	-$(MAKE) -C static clean
	-rm -r obj-*

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

test: go_files
	tests/run-tests.scm
	$(MAKE) coverage

coverage:
	genhtml \
		--show-details \
		--output-directory coverage \
		--prefix $(shell pwd) \
		--no-function-coverage \
		--quiet \
		lcov.info
