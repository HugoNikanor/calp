.PHONY: all clean tests html

CC  := gcc

export LD_LIBRARY_PATH=$(PWD)/lib
export GUILE_AUTO_COMPILE=0

CFLAGS  = -std=gnu11 -Wall -Wextra -ggdb -fPIC $(shell guile-config compile)
LDFLAGS = $(shell guile-config link)

LIBS = libguile-calendar.so
SO_FILES = $(addprefix lib/, $(LIBS))

H_FILES = $(wildcard src/*.h)
C_FILES = $(wildcard src/*.c)

SCM_C_FILES = $(wildcard src/*.scm.c)
X_FILES = $(SCM_C_FILES:.scm.c=.x)

O_FILES = $(C_FILES:src/%.c=obj/%.o)

SCM_FILES = $(shell find module/ -type f -name \*.scm)
GO_FILES = $(SCM_FILES:%=obj/%.go)

GUILE_C_FLAGS = -Lmodule \
				-Wunused-variable -Wunused-toplevel \
				-Wshadowed-toplevel -Wunbound-variable \
				-Wmacro-use-before-definition -Warity-mismatch \
				-Wduplicate-case-datum -Wbad-case-datum

.SECONDARY: $(X_FILES) $(O_FILES)



all: $(SO_FILES) $(GO_FILES)

src/%.x : src/%.scm.c
	guile-snarf -o $@ $< $(CFLAGS)

obj/%.scm.o : src/%.scm.c src/%.x
	@mkdir -p obj
	$(CC) -c $(CFLAGS) -o $@ $<

obj/%.o : src/%.c # $(H_FILES) $(X_FILES)
	@mkdir -p obj
	$(CC) -c $(CFLAGS) -o $@ $<

lib/%.so: $(O_FILES)
	@mkdir -p lib
	$(CC) -shared -o $@ $^ $(LDFLAGS)

obj/module/vcomponent/primitive.scm.go: module/vcomponent/primitive.scm $(SO_FILES)
	@mkdir -p obj
	guild compile $(GUILE_C_FLAGS) -o $@ $<

obj/%.scm.go: %.scm 
	@mkdir -p obj
	guild compile $(GUILE_C_FLAGS) -o $@ $<

html: $(GO_FILES)
	mkdir -p html
	ln -sf ../static html
	module/main.scm html -f 2019-10-01 -t 2019-12-31 > html/index.html

tags: $(C_FILES) $(H_FILES)
	ctags -R
	./rfc-tags rfc5545.txt >> tags

clean:
	-rm -r html
	-rm -r obj
	-rm -r lib
	-rm src/*.x


tests:
	tests/run-tests.scm
