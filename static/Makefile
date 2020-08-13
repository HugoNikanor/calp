.PHONY: all clean

TARGETS := style.css

all: $(TARGETS)

clean:
	rm $(TARGETS)

%.css: %.scss
	scss $< $@
