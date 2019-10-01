#ifndef PARSE_H
#define PARSE_H

#include <stdio.h>
#include <stdlib.h>

#include "strbuf.h"
#include "vcal.h"

// #define TYPE vcomponent
// #include "linked_list.h"
// #undef TYPE

/*
 * The standard says that no line should be longer than 75 octets.
 * This sets the default amount of memory to allocate for each string,
 * but strings are reallocated when needed.
 */
#define SEGSIZE 75

/*
 * Transfers a strbuf from src to target. 
 * Does this first copying the contents, followed by capping the
 * target and reseting the src.
 */
#define TRANSFER(target, src) do { \
	DEEP_COPY(strbuf)((target), (src)); \
	strbuf_cap(target); \
	strbuf_soft_reset(src); \
} while (0)

/*
 * Current context for the character consumer (parse_file).
 */
typedef enum {
	p_key, p_value, p_param_name, p_param_value, p_escape
} part_context;

/*
 * Struct holding most state information needed while parsing.
 * Kept together for simplicity.
 */
typedef struct {
	/* Which file we are parsing, copied to all components to allow
	 * writebacks later */
	char* filename;

	FILE* f;

	/*
	 * context stacks used since ICS files form a tree. key_stack is
	 * only for sequrity purposes.
	 */
	LLIST(strbuf) key_stack;
	LLIST(vcomponent) comp_stack;

	/* Number for unfolded lines
	 * TODO remove this
	 * */
	int line;
	int column;

	/* Actuall lines and columns from file */
	int pline;
	int pcolumn;

	/*
	 * String which we write everything read into.
	 * Later copied to appropiate places.
	 */
	// strbuf str;
} parse_ctx;

INIT_F(parse_ctx, FILE* f, char* filename);
FREE_F(parse_ctx);


/*
 * Character consumer. Reads characters from stdin until end of file.
 * Whenever it finds a token with a special value (such as ':', ';',
 * ...) it saves it away.
 * Once It has parsed a full line it calls handel_kv. Which build my
 * actuall datastructure.
 */
int parse_file(char* filename, FILE* f, vcomponent* cal);

/*
 * Called whenever parse_file finishes a line. Copies the contents of
 * ctx and the current content_line into the object stack, stored in
 * ctx. 
 */
int handle_kv(
		strbuf* key,
		content_line*  cline,
		parse_ctx*     ctx
		);

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
int fold(parse_ctx* ctx, char c);

char handle_escape (parse_ctx* ctx);

#endif /* PARSE_H */
