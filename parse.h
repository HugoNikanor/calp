#ifndef PARSE_H
#define PARSE_H

#include <stdio.h>
#include <stdlib.h>

#include "strbuf.h"
#include "vcal.h"

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
	scope_context scope;
	strbuf* skip_to;
} parse_ctx;

int handle_kv(
		vcalendar*     cal,
		vevent*        ev,
		content_line*  cline,
		int            line,
		parse_ctx*     ctx
		);

int parse_file(char* fname, FILE* f, vcalendar* cal);

#endif /* PARSE_H */
