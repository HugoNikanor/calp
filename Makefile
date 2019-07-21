.PHONY: all clean tests html

CC  := gcc

OBJDIR = obj
SRCDIR = src
LIBDIR = lib

export LD_LIBRARY_PATH=$(PWD)/$(LIBDIR)

CFLAGS  = -std=gnu11 -Wall -Wextra -ggdb -fPIC $(shell guile-config compile)
LDFLAGS = $(shell guile-config link)

LIBS = libguile-calendar.so
SO_FILES = $(addprefix $(LIBDIR)/, $(LIBS))

H_FILES = $(wildcard src/*.h)
C_FILES = $(wildcard src/*.c)

SCM_C_FILES = $(wildcard src/*.scm.c)
X_FILES = $(SCM_C_FILES:.scm.c=.x)

.SECONDARY: $(X_FILES)

O_FILES = $(C_FILES:src/%.c=obj/%.o)

SCM_FILES = $(shell find module/ -type f -name \*.scm)
GO_FILES = $(addprefix obj/,$(addsuffix .go,$(SCM_FILES)))

GUILE_C_FLAGS = -Lmodule \
				-Wunused-variable -Wunused-toplevel \
				-Wshadowed-toplevel -Wunbound-variable \
				-Wmacro-use-before-definition -Warity-mismatch \
				-Wduplicate-case-datum -Wbad-case-datum


all: $(SO_FILES) $(GO_FILES)

# Old C main
parse: $(O_FILES)
	$(CC) -o $@ $^ $(LDFLAGS)

src/%.x : src/%.scm.c
	guile-snarf -o $@ $< $(CFLAGS)

$(OBJDIR)/%.scm.o : src/%.scm.c src/%.x
	@mkdir -p $(OBJDIR)
	$(CC) -c $(CFLAGS) -o $@ $<

$(OBJDIR)/%.o : src/%.c # $(H_FILES) $(X_FILES)
	@mkdir -p $(OBJDIR)
	$(CC) -c $(CFLAGS) -o $@ $<

$(LIBDIR)/%.so: $(O_FILES)
	@mkdir -p $(LIBDIR)
	$(CC) -shared -o $@ $^ $(LDFLAGS)

$(OBJDIR)/%.scm.go: %.scm $(SO_FILES)
	@mkdir -p $(OBJDIR)
	guild compile $(GUILE_C_FLAGS) -o $@ $<

.SECONDARY += %.dot
%.dot: testcal/%.ics parse
	./parse $< -g $@

%.pdf: %.dot
	dot -Tpdf -o $@ $<

html: $(GO_FILES)
	mkdir -p html
	ln -sf ../static html
	module/main.scm html -f 2019-07-01 -t 2019-08-30 > html/index.html

tags: $(C_FILES) $(H_FILES)
	ctags -R
	./rfc-tags rfc5545.txt >> tags

clean:
	-rm parse
	-rm -r $(OBJDIR)/*
	-rm $(LIBDIR)/*.so
	-rm $(SRCDIR)/*.x


tests:
	tests/run-tests.scm
