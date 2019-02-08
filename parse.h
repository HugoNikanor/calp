#ifndef PARSE_H
#define PARSE_H

#include <stdio.h>
#include <stdlib.h>

#include "strbuf.h"
#include "vcal.h"

#define TYPE vcomponent
#include "linked_list.h"
#undef TYPE

/*
 * Max length of a line.
 * TODO update this to allow longer lines, in case someone doesn't
 * follow the standard.
 * (I currently only realloc memmory at end of lines, not when my
 * buffer is full).
 */
#define SEGSIZE 75

#define LINE(nr, key, value) fprintf(stderr, "(%i) %i: [%s] := [%s]\n", __LINE__, nr, key, value);

typedef enum {
	p_key, p_value, p_param_name, p_param_value
} part_context;

typedef enum {
	s_none = 1, s_calendar, s_event,
	s_skip
} scope_context;

typedef struct {
	char* filename;
	LLIST(strbuf) key_stack;
	LLIST(vcomponent) comp_stack;
} parse_ctx;

INIT_F(parse_ctx, char* filename);

int handle_kv(
		content_line*  cline,
		int            line,
		parse_ctx*     ctx
		);

int parse_file(char* filename, FILE* f, vcomponent* cal);

#endif /* PARSE_H */
