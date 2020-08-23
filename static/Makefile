.PHONY: all clean

TARGETS := style.css smallcal.css

all: $(TARGETS)

clean:
	rm $(TARGETS)

%.css: %.scss
	scss -I. $< $@
