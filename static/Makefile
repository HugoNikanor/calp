.PHONY: all install clean watch watch-esbuild

TARGETS := style.css smallcal.css script.js directory-listing.css
WATCH=

TS_FILES = $(shell find . -type f -name \*.ts -not -path */node_modules/*)
JS_FILES = $(TS_FILES:%.ts=%.js)

ESBUILD_LOGLEVEL=warning
# Variable for adding extra flags
ESBUILD_FLAGS =
# Used flags
__ESBUILD_FLAGS = --log-level=$(ESBUILD_LOGLEVEL) \
				  --sourcemap --bundle --outdir=$(CURDIR) \
				  $(ESBUILD_FLAGS)

export PATH := $(shell npm bin):$(PATH)

all: $(TARGETS)

# script explicitly named, since that is our entry point
script.js: script.ts $(TS_FILES)
	esbuild $< $(__ESBUILD_FLAGS)

watch-esbuild:
	$(MAKE) ESBUILD_FLAGS+='--watch' ESBUILD_LOGLEVEL=info -B script.js

deps.svg: $(TS_FILES)
	madge --image $@ $^

watch:
	./make-watch

install: all
	install -d $(DESTDIR)/usr/share/calp/www
	install -m644 -t $(DESTDIR)/usr/share/calp/www/ $(TARGETS)

clean:
	-rm $(TARGETS)

%.css: %.scss
	scss $(WATCH) -I. $< $@
