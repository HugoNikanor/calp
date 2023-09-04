.PHONY: all install clean watch watch-esbuild doc

_TARGETS := style.css smallcal.css script.js directory-listing.css
TARGETS = $(addprefix out/,$(_TARGETS))
WATCH=

TS_FILES = $(shell find ts -type f -name \*.ts)
JS_FILES = $(TS_FILES:ts/%.ts=out/%.js)

ESBUILD_LOGLEVEL=warning
# Variable for adding extra flags
ESBUILD_FLAGS =
# Used flags
__ESBUILD_FLAGS = --log-level=$(ESBUILD_LOGLEVEL) \
				  --sourcemap --bundle --outdir=$(CURDIR)/out \
				  $(ESBUILD_FLAGS)

export PATH := $(shell npm bin):$(PATH)

all: $(TARGETS)

# script explicitly named, since that is our entry point
out/script.js: ts/script.ts $(TS_FILES)
	esbuild $< $(__ESBUILD_FLAGS)

watch-esbuild:
	$(MAKE) ESBUILD_FLAGS+='--watch' ESBUILD_LOGLEVEL=info -B out/script.js

deps.svg: $(TS_FILES)
	madge --image $@ $^

watch:
	./make-watch

install: all
	install -d $(DESTDIR)/usr/share/calp/www
	install -m644 -t $(DESTDIR)/usr/share/calp/www/ $(TARGETS)

clean:
	-rm $(TARGETS)

out/%.css: scss/%.scss
	scss -E UTF-8 $(WATCH) -I. $< $@

doc:
	typedoc --logLevel Verbose --excludeExternals
