.PHONY: all clean watch

TARGETS := style.css smallcal.css
WATCH=

all: $(TARGETS)

watch:
	$(MAKE) WATCH=--watch all

clean:
	rm $(TARGETS)

%.css: %.scss
	scss $(WATCH) -I. $< $@
