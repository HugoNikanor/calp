.PHONY: all clean test

GUILE_SITE_DIR=$(shell guile -c "(display (%site-dir))")
GUILE_CCACHE_DIR=$(shell guile -c "(display (%site-ccache-dir))")

SCM_FILES = $(shell find module/ -type f -name \*.scm)
GO_FILES = $(SCM_FILES:%=obj/%.go)

GUILE_C_FLAGS = -Lmodule \
				-Wunused-toplevel \
				-Wshadowed-toplevel -Wunbound-variable \
				-Wmacro-use-before-definition -Warity-mismatch \
				-Wduplicate-case-datum -Wbad-case-datum

all: $(GO_FILES) README

obj/%.scm.go: %.scm
	@mkdir -p obj
	guild compile $(GUILE_C_FLAGS) -o $@ $<

clean:
	-rm -r obj

install:
	install -d $(DESTDIR)$(GUILE_SITE_DIR)  $(DESTDIR)$(GUILE_CCACHE_DIR)
	rsync -a module/ $(DESTDIR)$(GUILE_SITE_DIR)
	rsync -a obj/ $(DESTDIR)$(GUILE_CCACHE_DIR)
	install -d $(DESTDIR)/usr/share/calp/www
	rsync -a static $(DESTDIR)/usr/share/calp/www
	# TODO main, tzget

README: README.in
	./main text < README.in | sed "s/<<today>>/`date -I`/" > README

test:
	tests/run-tests.scm
