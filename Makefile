.PHONY: all clean

CC  := gcc
LEX := flex

DIRS := obj

CFLAGS  = -Wall -DSAFE_STR -DSAFE_HASH -ggdb
#LFLAGS  =
#LDFLAGS = 

C_FILES = $(wildcard *.c)
INC_FILES = $(wildcard *.inc)
O_FILES = $(addprefix obj/,$(C_FILES:.c=.o))
H_FILES = $(wildcard *.h)

$(shell mkdir -p $(DIRS))

all: parse

obj/%.o : %.c $(H_FILES) $(INC_FILES)
	$(CC) -c -o $@ $< ${CFLAGS}

parse: $(O_FILES)
	$(CC) -o $@ $^ ${LDFLAGS}

clean:
	-rm parse
	-rm obj/*.o
