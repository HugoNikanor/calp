.PHONY: all clean

CC  := gcc
LEX := flex

DIRS := obj

CFLAGS  = -std=gnu99 -Wall -Wextra -pedantic \
		  -DSAFE_STR -DSAFE_HASH -ggdb \
		  -fPIC \
		  $(shell guile-config compile)
# LFLAGS  =
LDFLAGS = -fPIC $(shell guile-config link)

C_FILES = $(wildcard *.c)
INC_FILES = $(wildcard *.inc)
O_FILES = $(addprefix obj/,$(C_FILES:.c=.o))
H_FILES = $(wildcard *.h)

$(shell mkdir -p $(DIRS))

all: parse libguile-calendar.so

obj/%.o : %.c $(H_FILES) $(INC_FILES)
	$(CC) -c -o $@ $< ${CFLAGS}

libguile-calendar.so: $(O_FILES)
	$(CC) -shared -o $@ $^ $(LDFLAGS)

parse: $(O_FILES)
	$(CC) -o $@ $^ $(LDFLAGS)

clean:
	-rm parse
	-rm obj/*.o
	-rm *.so
