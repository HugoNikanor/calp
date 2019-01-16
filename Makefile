.PHONY: all clean

CC  := gcc
LEX := flex

CFLAGS  = -Wall -ggdb
#LFLAGS  =
#LDFLAGS = 

C_FILES = $(wildcard *.c)
O_FILES = $(C_FILES:.c=.o)
H_FILES = $(wildcard *.h)

all: parse

%.o : %.c $(H_FILES)
	$(CC) -c -o $@ $< ${CFLAGS}

parse: $(O_FILES)
	$(CC) -o $@ $^ ${LDFLAGS}

clean:
	-rm parse
	-rm *.o
