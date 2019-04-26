.PHONY: all clean tests guild-stuff guile-none

CC  := gcc

OBJDIR = obj
SRCDIR = src
LIBDIR = lib

CFLAGS  = -std=gnu11 -Wall -Wextra \
		  -ggdb -fPIC \
		  $(shell guile-config compile)
LDFLAGS = -fPIC $(shell guile-config link)

LIBS = libguile-calendar.so
SO_FILES = $(addprefix $(LIBDIR)/, $(LIBS))

H_FILES = $(wildcard src/*.h)
C_FILES = $(wildcard src/*.c)

SCM_C_FILES = $(wildcard src/*.scm.c)
X_FILES = $(SCM_C_FILES:.scm.c=.x)

.SECONDARY: $(X_FILES)

O_FILES = $(C_FILES:src/%.c=obj/%.o)

all: parse $(SO_FILES) guild-stuff guile-none

guild-stuff:
	guild compile module/vcomponent/primitive.scm

guile-none:
	module/main.scm none

parse: $(O_FILES)
	$(CC) -o $@ $^ $(LDFLAGS)

$(O_FILES): | $(OBJDIR)

$(SO_FILES): | $(LIBDIR)

src/%.x : src/%.scm.c
	guile-snarf -o $@ $< $(CFLAGS)

$(OBJDIR)/%.scm.o : src/%.scm.c src/%.x
	$(CC) -c $(CFLAGS) -o $@ $<

$(OBJDIR)/%.o : src/%.c # $(H_FILES) $(X_FILES)
	$(CC) -c $(CFLAGS) -o $@ $<

$(OBJDIR):
	mkdir -p $(OBJDIR)

$(LIBDIR):
	mkdir -p $(LIBDIR)

$(LIBDIR)/%.so: $(O_FILES)
	$(CC) -shared -o $@ $^ $(LDFLAGS)

.SECONDARY += %.dot
%.dot: testcal/%.ics parse
	./parse $< -g $@

%.pdf: %.dot
	dot -Tpdf -o $@ $<

tags: $(C_FILES) $(H_FILES)
	ctags -R
	./rfc-tags rfc5545.txt >> tags

clean:
	-rm parse
	-rm $(OBJDIR)/*.o
	-rmdir $(OBJDIR)
	-rm $(LIBDIR)/*.so
	-rm $(SRCDIR)/*.x
	-rm -r $$HOME/.cache/guile/ccache/2.2-LE-8-3.A/$$PWD

tests:
	tests/run-tests.scm
