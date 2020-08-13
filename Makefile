.PHONY: all clean test \
	static

GUILE_SITE_DIR=$(shell guile -c "(display (%site-dir))")
GUILE_CCACHE_DIR=$(shell guile -c "(display (%site-ccache-dir))")

SCM_FILES = $(shell find module/ -type f -name \*.scm)
GO_FILES = $(SCM_FILES:%=obj/%.go)

GUILE_C_FLAGS = -Lmodule \
				-Wunused-toplevel \
				-Wshadowed-toplevel -Wunbound-variable \
				-Wmacro-use-before-definition -Warity-mismatch \
				-Wduplicate-case-datum -Wbad-case-datum

all: $(GO_FILES) README static

static:
	$(MAKE) -C static

obj/%.scm.go: %.scm
	@mkdir -p obj
	guild compile $(GUILE_C_FLAGS) -o $@ $<

clean:
	$(MAKE) -C static clean
	-rm -r obj

install:
	install -d $(DESTDIR)$(GUILE_SITE_DIR)  $(DESTDIR)$(GUILE_CCACHE_DIR)
	rsync -a module/ $(DESTDIR)$(GUILE_SITE_DIR)
	rsync -a obj/ $(DESTDIR)$(GUILE_CCACHE_DIR)
	install -d $(DESTDIR)/usr/share/calp/www
	rsync -a static $(DESTDIR)/usr/share/calp/www
	install -m 644 -D -t $(DESTDIR)/usr/share/doc/calp README
	install -m 755 -D -t $(DESTDIR)/usr/lib/calp/ main
	install -m 755 -D -t $(DESTDIR)/usr/lib/calp/ tzget
	install -d $(DESTDIR)/usr/bin
	ln -s -f /usr/lib/calp/main $(DESTDIR)/usr/bin/calp
	@env CACHE_DIR=$(DESTDIR)/var/cache/calp/ ./tzget
	# TODO zoneinfo files

README: README.in
	./main text < README.in | sed "s/<<today>>/`date -I`/" > README

test:
	tests/run-tests.scm
