CC  := gcc
LEX := flex

CFLAGS  = -Wall -ggdb
#LFLAGS  =
#LDFLAGS = 

#%.yy.c : %.yy
#	$(LEX) -o $@ ${LFLAGS} $<

%.o : %.c
	$(CC) -c -o $@ $^ ${CFLAGS}

parse: parse.o
	$(CC) -o $@ $^ ${LDFLAGS}

test: test.yy.o
	$(CC) -o $@ $^ ${LDFLAGS}
