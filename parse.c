#include "parse.h"

#include <errno.h>
#include <string.h>
#include <assert.h>

#include "macro.h"
#include "vcal.h"

#include "err.h"

#define TYPE vcomponent
#include "linked_list.inc.h"
#undef TYPE

#define T strbuf
#define V strbuf
#include "pair.h"
#include "pair.inc.h"
#undef T
#undef V

/*
 * name *(";" param) ":" value CRLF
 */
int parse_file(char* filename, FILE* f, vcomponent* root) {
	part_context p_ctx = p_key;

	SNEW(parse_ctx, ctx, f, filename);
	PUSH(LLIST(vcomponent))(&ctx.comp_stack, root);

	/*
	 * Create a content_line which we use as storage while we are
	 * parsing. This object is constantly broken down and rebuilt.
	 */
	SNEW(content_line, cline);
	SNEW(strbuf, cline_key);
	SNEW(strbuf, param_key);

	char c;
	while ( (c = fgetc(f)) != EOF) {

		/* We have a linebreak */
		if (c == '\r' || c == '\n') {

			if (fold(&ctx, c) > 0) {
				/* Actuall end of line, handle value */
				TRANSFER(CLINE_CUR_VAL(&cline), &ctx.str);
				handle_kv(&param_key, &cline, &ctx);
				p_ctx = p_key;
			} /* Else continue on current line */

		/* We have an escaped character */
		} else if (c == '\\') {
			handle_escape (&ctx);

		/* Border between param {key, value} */
		} else if (p_ctx == p_param_name && c == '=') {

			/* Create a new parameter set and push the current string
			 * as its key */
			TRANSFER (&param_key, &ctx.str);
			// PUSH(LLIST(param_set))(CLINE_CUR_PARAMS(&cline), ps);

			p_ctx = p_param_value;

		/*
		 * One of four cases:
		 * 1) end of key  , start of value
		 * 2)   ,,   key  ,    ,,    param
		 * 3)   ,,   param,    ,,    param
		 * 4)   ,,   param,    ,,    value
		 */
		} else if ((p_ctx == p_key || p_ctx == p_param_value) && (c == ':' || c == ';')) {

			/* We got a parameter value, push the current string to
			 * the current parameter set. */
			if (p_ctx == p_param_value) {
				/* push kv pair */

				NEW(strbuf, s);
				TRANSFER(s, &ctx.str);

				NEW(param_set, ps);
				PUSH(LLIST(strbuf))(ps, s);

				PUSH(TRIE(param_set))(CLINE_CUR_PARAMS(&cline), s->mem, ps);
			}

			/*
			 * Top level key.
			 * Copy the key into the current cline, and create a
			 * content_set for the upcomming value and (possible)
			 * parameters.
			 */
			if (p_ctx == p_key) {

				TRANSFER(&cline_key, &ctx.str);

				NEW(content_set, p);
				PUSH(LLIST(content_set))(&cline, p);
			}

			if      (c == ':') p_ctx = p_value;
			else if (c == ';') p_ctx = p_param_name;

		/*
		 * Nothing interesting happened, append the read character to
		 * the current string.
		 */
		} else {
			strbuf_append(&ctx.str, c);

			++ctx.column;
			++ctx.pcolumn;
		}
	}

	if (! feof(f)) {
		ERR("Error parsing");
	}
	/* Check to see if empty line */
	else if (ctx.str.ptr != 0) {
		/*
		 * The standard (3.4, l. 2675) says that each icalobject must
		 * end with CRLF. My files however does not, so we also parse
		 * the end here.
		 */

		TRANSFER(CLINE_CUR_VAL(&cline), &ctx.str);
		handle_kv(&param_key, &cline, &ctx);

	}

	FREE(content_line)(&cline);

	assert(POP(LLIST(vcomponent))(&ctx.comp_stack) == root);
	assert(EMPTY(LLIST(strbuf))(&ctx.key_stack));
	assert(EMPTY(LLIST(vcomponent))(&ctx.comp_stack));

	FREE(parse_ctx)(&ctx);

	return 0;
}

/*
 * We have a complete key value pair.
 */
int handle_kv (
	strbuf* key,
	content_line* cline,
	parse_ctx* ctx
	) {

	/*
	 * The key being BEGIN means that we decend into a new component.
	 */
	if (strbuf_c(key, "BEGIN")) {
		/* key \in { VCALENDAR, VEVENT, VALARM, VTODO, VTIMEZONE, ...  } */

		/*
		 * Take a copy of the name of the entered component, and store
		 * it on the stack of component names.
		 */
		NEW(strbuf, s);
		DEEP_COPY(strbuf)(s, CLINE_CUR_VAL(cline));
		PUSH(LLIST(strbuf))(&ctx->key_stack, s);

		/* Clear the value list in the parse content_line */
		RESET(LLIST(content_set))(cline);

		/*
		 * Create the new curent component, link it with the current
		 * component in a parent/child relationship.
		 * Finally push the new component on to the top of the
		 * component stack.
		 */
		NEW(vcomponent, e,
				s->mem,
				ctx->filename);
		vcomponent* parent = PEEK(LLIST(vcomponent))(&ctx->comp_stack);
		PUSH(vcomponent)(parent, e);

		PUSH(LLIST(vcomponent))(&ctx->comp_stack, e);

	/*
	 * The end of a component, go back along the stack to the previous
	 * component.
	 */
	} else if (strbuf_c(key, "END")) {
		strbuf* expected_key = POP(LLIST(strbuf))(&ctx->key_stack);

		if (strbuf_cmp(expected_key, CLINE_CUR_VAL(cline)) != 0) {

			ERR_P(ctx, "Expected END:%s, got END:%s.\n%s line",
					expected_key->mem,
					CLINE_CUR_VAL(cline)->mem,
					PEEK(LLIST(vcomponent))(&ctx->comp_stack)->filename);
			PUSH(LLIST(strbuf))(&ctx->key_stack, expected_key);

			return -1;

		} else {
			FFREE(strbuf, expected_key);
			POP(LLIST(vcomponent))(&ctx->comp_stack);
		}

	/*
	 * A regular key, value pair. Push it into to the current
	 * component.
	 */
	} else {

		/*
		 * cline is the value store used during parsing, meaning that
		 * its values WILL mutate at a later point. Therefore we take
		 * a copy of it here.
		 */
		NEW(content_line, c);
		DEEP_COPY(content_line)(c, cline);

		/*
		 * The PUSH(TRIE(T)) method handles collisions by calling
		 * RESOLVE(T). content_line resolves by merging the new value
		 * into the old value, and freeing the new value's container.
		 *
		 * This means that |c| declared above might be destroyed
		 * here.
		 */
		PUSH(TRIE(content_line))(
				&PEEK(LLIST(vcomponent))(&ctx->comp_stack)->clines,
				key->mem, c);

		RESET(LLIST(content_set))(cline);
	}

	return 0;
}

int fold(parse_ctx* ctx, char c) {
	int retval;

	char buf[2] = {
		(c == '\n' ? '\n' : (char) fgetc(ctx->f)),
		(char) fgetc(ctx->f)
	};

	ctx->pcolumn = 1;

	if (buf[0] != '\n') {
		ERR_P(ctx, "expected new_line after CR");
		retval = -1;

	} else if (buf[1] == ' ' || buf[1] == '\t') {
		retval = 0;
		ctx->pcolumn++;

	} else if (ungetc(buf[1], ctx->f) != buf[1]) {
		ERR_P(ctx, "Failed to put character back on FILE");
		retval = -2;

	} else {
		retval = 1;
		++ctx->line;
		ctx->column = 0;
	}

	++ctx->pline;

	return retval;
}


INIT_F(parse_ctx, FILE* f, char* filename) {
	INIT(LLIST(strbuf), &self->key_stack);
	INIT(LLIST(vcomponent), &self->comp_stack);
	self->filename = (char*) calloc(sizeof(*filename), strlen(filename) + 1);
	strcpy(self->filename, filename);
	self->f = f;

	self->line    = 0;
	self->column  = 0;

	self->pline   = 1;
	self->pcolumn = 1;

	INIT(strbuf, &self->str);

	return 0;
}

FREE_F(parse_ctx) {

	FREE(LLIST(strbuf))(&self->key_stack);
	FREE(LLIST(vcomponent))(&self->comp_stack);
	free(self->filename);

	self->line = 0;
	self->column = 0;
	FREE(strbuf)(&self->str);

	return 0;
}

int handle_escape (parse_ctx* ctx) {
	char esc = fgetc(ctx->f);
	char target;

	/*
	 * Escape character '\' and escaped token sepparated by a newline
	 * (since the standard for some reason allows that (!!!))
	 * We are at least guaranteed that it's a folded line, so just
	 * unfold it and continue trying to find a token to escape.
	 */
	if (esc == '\r' || esc == '\n') {
		int ret;
		if ( (ret = fold(ctx, esc)) != 0) {
			if (ret == 1) ERR_P(ctx, "ESC before not folded line");
			else          ERR_P(ctx, "other error: val = %i", ret);
			exit (2);
		} else {
			esc = fgetc(ctx->f);
		}
	}

	/* Escaped new_line */
	if (esc == 'n' || esc == 'N') {
		target = '\n';

		/* "Standard" escaped character */
	} else if (esc == ';' || esc == ',' || esc == '\\') {
		target = esc;

		/* Invalid escaped character */
	} else {
		ERR_P(ctx, "Non escapable character '%c' (%i)", esc, esc);
	}

	/* save escapade character as a normal character */
	strbuf_append(&ctx->str, target);

	++ctx->column;
	++ctx->pcolumn;

	return 0;
}
