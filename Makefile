.PHONY: all clean

CC  := gcc

OBJDIR = obj

CPPFLAGS = -DSAFE_STR -DSAFE_HASH 
CFLAGS  = $(CPPFLAGS) \
		  -std=gnu99 -Wall -Wextra -pedantic \
		  -ggdb -fPIC \
		  $(shell guile-config compile)
LDFLAGS = -fPIC $(shell guile-config link)

H_FILES = $(wildcard *.h)

C_FILES = $(wildcard *.c)

all: parse libguile-calendar.so

O_FILES = $(addprefix obj/,$(C_FILES:.c=.o))

all: parse libguile-calendar.so

parse: $(O_FILES)
	$(CC) -o $@ $^ $(LDFLAGS)

$(O_FILES): | $(OBJDIR)

$(OBJDIR)/%.o : %.c $(H_FILES)
	$(CC) -c -o $@ $< $(CFLAGS)

$(OBJDIR):
	mkdir -p $(OBJDIR)

libguile-calendar.so: $(O_FILES)
	$(CC) -shared -o $@ $^ $(LDFLAGS)

clean:
	-rm parse
	-rm $(OBJDIR)/*.o
	-rmdir $(OBJDIR)
	-rm *.so
