.PHONY: all clean

CC  := gcc

OBJDIR = obj

# CPPFLAGS =
CFLAGS  = $(CPPFLAGS) \
		  -std=gnu11 -Wall -Wextra \
		  -ggdb -fPIC \
		  $(shell guile-config compile)
LDFLAGS = -fPIC $(shell guile-config link)

H_FILES = $(wildcard *.h)
C_FILES = $(wildcard *.c)

SCM_C_FILES = $(wildcard *.scm.c)
X_FILES = $(SCM_C_FILES:.scm.c=.x)

.SECONDARY: $(X_FILES)

O_FILES = $(addprefix obj/,$(C_FILES:.c=.o))

all: parse libguile-calendar.so

parse: $(O_FILES)
	$(CC) -o $@ $^ $(LDFLAGS)

$(O_FILES): | $(OBJDIR)

$(OBJDIR)/%.o : %.c $(H_FILES) $(X_FILES)
	$(CC) -c -o $@ $< $(CFLAGS)

$(OBJDIR):
	mkdir -p $(OBJDIR)

%.x : %.scm.c
	guile-snarf -o $@ $< $(CFLAGS)

libguile-calendar.so: $(O_FILES)
	$(CC) -shared -o $@ $^ $(LDFLAGS)

CALDIR = test-cal/cal2/0c0f4d7090bacd99abe9ccca2d5baadf2afac709f7cb66454ef913c52a5ef096.ics
.SECONDARY += %.dot
%.dot: parse
	./parse $(CALDIR) -g $@

%.pdf: %.dot
	dot -Tpdf -o $@ $<

tags: $(C_FILES) $(H_FILES)
	ctags -R

clean:
	-rm parse
	-rm $(OBJDIR)/*.o
	-rmdir $(OBJDIR)
	-rm *.so
	-rm *.x
