.PHONY: all clean watch

TARGETS := style.css smallcal.css script.out.js
WATCH=

# script explicitly named, since that is our entry point
TS_FILES = script.ts $(shell find . -type f -name \*.ts -not -path */node_modules/*)

export PATH := $(shell npm bin):$(PATH)

all: $(TARGETS)

%.map.json: %.out.js
	tail -n1 $< | tail -c+65 | base64 --decode | jq '.' > $@

# r!browserify --list script.ts -p tsify | xargs -L1 basename | tac
script.out.js: $(TS_FILES)
	browserify $< -p tsify --noImplicitAny --debug -o $@

deps.svg: $(TS_FILES)
	madge --image $@ $^

# Note that 'tsc --watch' doesn't provide the files we are using. It's
# just here for debug.
watch:
	tmux \
		new-session "scss --watch -I. style.scss:style.css" \; \
		split-window "tsc --watch" \; \
		rename-session "calp watch" \; \
		select-layout even-vertical


clean:
	rm $(TARGETS)

%.css: %.scss
	scss $(WATCH) -I. $< $@
