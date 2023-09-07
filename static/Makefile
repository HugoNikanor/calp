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

export PATH := $(CURDIR)/node_modules/.bin/:$(PATH)

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

# The grep expression is to supress irrelevant warning messages.
# - __type since the extensions to base classes propagate to many
#   events, but typedoc only documents them if in the entry point
# - [.]# since it's sometimes sensible to not document
#   implementation details.
# - connectedCallback, attributeChangedCallabck: These are part
#   of the standard API for Web Components, and usually have nothing
#   interesting to note.
doc:
	typedoc --excludeExternals |& grep -vE '(__type|[.]#|connectedCallback|attributeChangedCallback)'
