.PHONY: all clean

CC  := g++

OBJDIR = obj

CFLAGS  = -std=gnu++11 -Wall -Wextra \
		  -ggdb -fPIC \
		  $(shell guile-config compile)
LDFLAGS = -fPIC $(shell guile-config link)

H_FILES = $(wildcard *.h)
C_FILES = $(wildcard *.cpp)

SCM_C_FILES = $(wildcard *.scm.c)
X_FILES = $(SCM_C_FILES:.scm.cpp=.x)

.SECONDARY: $(X_FILES)

O_FILES = $(addprefix obj/,$(C_FILES:.cpp=.o))

all: parse libguile-calendar.so

parse: $(O_FILES)
	$(CC) -o $@ $^ $(LDFLAGS)

$(O_FILES): | $(OBJDIR)

%.x : %.scm.c
	guile-snarf -o $@ $< $(CFLAGS)

$(OBJDIR)/%.scm.o : %.scm.c %.x
	$(CC) -c $(CFLAGS) -o $@ $<

$(OBJDIR)/%.o : %.cpp # $(H_FILES) $(X_FILES)
	$(CC) -c $(CFLAGS) -o $@ $<

$(OBJDIR):
	mkdir -p $(OBJDIR)

libguile-calendar.so: $(O_FILES)
	$(CC) -shared -o $@ $^ $(LDFLAGS)

.SECONDARY += %.dot
%.dot: testcal/%.ics parse
	./parse $< -g $@

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
