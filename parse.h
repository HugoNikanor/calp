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
 * The standard says that no line should be longer than 75 octets.
 * This sets the default amount of memory to allocate for each string,
 * but strings are reallocated when needed.
 */
#define SEGSIZE 75

typedef enum {
	p_key, p_value, p_param_name, p_param_value, p_escape
} part_context;

/*
 * Struct holding most state information needed while parsing.
 * Kept together for simplicity.
 */
typedef struct {
	char* filename;
	LLIST(strbuf) key_stack;
	LLIST(vcomponent) comp_stack;

	/* Number for unfolded lines */
	int line;
	int column;

	/* Actuall lines and columns from file */
	int pline;
	int pcolumn;

	strbuf str;
} parse_ctx;

INIT_F(parse_ctx, char* filename);
FREE_F(parse_ctx);

int handle_kv(
		content_line*  cline,
		parse_ctx*     ctx
		);

int parse_file(char* filename, FILE* f, vcomponent* cal);

#endif /* PARSE_H */
