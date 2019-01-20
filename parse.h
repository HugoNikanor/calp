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
 */
#define SEGSIZE 75

#define ERR(x, line) fprintf(stderr, "ERR %i: %s (cal %i)\n", __LINE__, (x), (line));

#define LINE(nr, key, value) fprintf(stderr, "(%i) %i: [%s] := [%s]\n", __LINE__, nr, key, value);

typedef enum {
	p_key, p_value, p_param_name, p_param_value
} part_context;

typedef enum {
	s_none, s_calendar, s_event
} scope_context;

int handle_kv(
		vcalendar*     cal,
		vevent*        ev,
		content_line*  cline,
		int            line,
		scope_context* s_ctx
		);

int parse_file(FILE* f, vcalendar* cal);

#endif /* PARSE_H */
