.PHONY: all clean watch

TARGETS := style.css smallcal.css script.out.js
WATCH=

export PATH := $(shell npm bin):$(PATH)

all: $(TARGETS)

%.map.json: %.out.js
	tail -n1 $< | tail -c+65 | base64 --decode | jq '.' > $@

# r!browserify --list script.ts -p tsify | xargs -L1 basename | tac
script.out.js: script.ts popup.ts globals.ts vevent.ts types.ts dragable.ts clock.ts lib.ts
	browserify $< -p tsify --noImplicitAny --debug -o $@

watch:
	$(MAKE) WATCH=--watch all

clean:
	rm $(TARGETS)

%.css: %.scss
	scss $(WATCH) -I. $< $@
