#ifndef PARSE_H
#define PARSE_H

#include <stdio.h>
#include <stdlib.h>

#include "strbuf.h"
#include "vcal.h"

// #define TYPE vcomponent
#include "linked_list.h"
// #undef TYPE

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
struct parse_ctx {
	char* filename;
	llist<strbuf> key_stack;
	llist<vcomponent> comp_stack;

	/* Number for unfolded lines */
	int line;
	int column;

	/* Actuall lines and columns from file */
	int pline;
	int pcolumn;

	strbuf str;

	parse_ctx (const char* filename);

	~parse_ctx ();
};

int handle_kv(
		strbuf* key,
		// content_line*  cline,
		parse_ctx*     ctx
		);

int parse_file(char* filename, FILE* f, vcomponent* cal);

/*
 * Input
 *   f: file to get characters from
 *   ctx: current parse context
 *   c: last read character
 * output:
 *   0: line folded
 *   1: line ended
 *
 * A carrige return means that the current line is at an
 * end. The following character should always be \n.
 * However, if the first character on the next line is a
 * whitespace then the two lines should be concatenated.
 *
 * NOTE
 * The above is true according to the standard. But I have
 * found files with only NL. The code below ends line on the
 * first of NL or CR, and then ensures that the program thinks
 * it got the expected CRNL.
 */
int fold(FILE* f, parse_ctx* ctx, char c);

#endif /* PARSE_H */
