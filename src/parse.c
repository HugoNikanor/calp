#include "parse.h"

#include <errno.h>
#include <string.h>
#include <assert.h>
#include <libguile.h>
#include "guile_interface.h"

#include "macro.h"
#include "vcal.h"

#include "err.h"

/*
 * name *(";" param) ":" value CRLF
 */
int parse_file(char* filename, FILE* f, vcomponent* root) {
	part_context p_ctx = p_key;

	SNEW(parse_ctx, ctx, f, filename);
	SCM_PUSH_X(ctx.comp_stack, scm_from_vcomponent(root));

	/*
	 * Create a content_line which we use as storage while we are
	 * parsing. This object is constantly broken down and rebuilt.
	 *
	 * {cline,param}_key is also temporary register used during
	 * parsing.
	 */
	// SNEW(content_line, cline);
	SCM content_pair = scm_cons (SCM_BOOL_F, scm_make_hash_table (scm_from_ulong(32)));
	scm_gc_protect_object (content_pair);

	SNEW(strbuf, cline_key);
	SNEW(strbuf, cline_val);

	SNEW(strbuf, param_key);

	char c;
	while ( (c = fgetc(f)) != EOF) {

		/* We have a linebreak */
		if (c == '\r' || c == '\n') {

			if (fold(&ctx, c) > 0) {
				 TRANSFER(&cline_val, &ctx.str);

				/* Actuall end of line, handle value */
				INFO_F("cp: %p", content_pair);
				handle_kv(&cline_key, &cline_val, &content_pair, &ctx);
				INFO_F("cp: %p", content_pair);
				p_ctx = p_key;
			} /* Else continue on current line */

		/* We have an escaped character */
		} else if (c == '\\') {
			handle_escape (&ctx);

		/* Border between param {key, value} */
		} else if (p_ctx == p_param_name && c == '=') {

			/* Save the current parameter key */
			TRANSFER (&param_key, &ctx.str);
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
				/* save current parameter value. */

				// TODO resolve
				scm_hashq_set_x (
						scm_cdr (content_pair),
						scm_string_to_symbol (scm_from_utf8_stringn (param_key.mem, param_key.len)),
						scm_from_utf8_stringn (ctx.str.mem, ctx.str.len));

				strbuf_soft_reset (&ctx.str);
			}

			/*
			 * Top level key.
			 * Copy the key into the current cline, and create a
			 * content_set for the upcomming value and (possible)
			 * parameters.
			 */
			if (p_ctx == p_key) {
				TRANSFER(&cline_key, &ctx.str);
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

		// TODO
		handle_kv(&cline_key, &cline_val, &content_pair, &ctx);

	}

	/*
	FREE(content_line)(&cline);
	FREE(strbuf)(&cline_key);
	FREE(strbuf)(&param_key);

	assert(POP(LLIST(vcomponent))(&ctx.comp_stack) == root);
	assert(EMPTY(LLIST(strbuf))(&ctx.key_stack));
	assert(EMPTY(LLIST(vcomponent))(&ctx.comp_stack));
	*/

	FREE(parse_ctx)(&ctx);

	return 0;
}

/*
 * We have a complete key value pair.
 */
int handle_kv (
	strbuf* key,
	strbuf* val,
	SCM* content_pair,
	parse_ctx* ctx
	) {

	strbuf_cap (key);
	strbuf_cap (val);
	INFO_F("%s: %s", key->mem, val->mem);

	/*
	 * The key being BEGIN means that we decend into a new component.
	 */
	if (strbuf_c(key, "BEGIN")) {
		/* key \in { VCALENDAR, VEVENT, VALARM, VTODO, VTIMEZONE, ...  } */

		NEW(vcomponent, e,
				ctx->str.mem,
				ctx->filename);

		SCM_PUSH_X(ctx->key_stack, scm_string_to_symbol(scm_from_utf8_stringn(ctx->str.mem, ctx->str.len)));

		/* Clear the value list in the parse content_line */
		// RESET(LLIST(content_set))(cline);

		/*
		 * Create the new curent component, link it with the current
		 * component in a parent/child relationship.
		 * Finally push the new component on to the top of the
		 * component stack.
		 */

		// vcomponent* parent = PEEK(LLIST(vcomponent))(&ctx->comp_stack);
		vcomponent* parent = scm_to_vcomponent(scm_car(ctx->comp_stack));
		PUSH(vcomponent)(parent, e);

		// PUSH(LLIST(vcomponent))(&ctx->comp_stack, e);
		SCM_PUSH_X(ctx->comp_stack, scm_from_vcomponent (e));

	/*
	 * The end of a component, go back along the stack to the previous
	 * component.
	 */
	} else if (strbuf_c(key, "END")) {
		// strbuf* expected_key = POP(LLIST(strbuf))(&ctx->key_stack);
		SCM expected_key = scm_car(ctx->key_stack);

		if (! scm_is_eq (expected_key,
					scm_string_to_symbol (scm_from_utf8_stringn (ctx->str.mem, ctx->str.len)))) {

			// ERR_P(ctx, "Expected END:%s, got END:%s.\n%s line",
			// 		expected_key->mem,
			// 		CLINE_CUR_VAL(cline)->mem,
			// 		vcomponent_get_val(
			// 			PEEK(LLIST(vcomponent))(&ctx->comp_stack),
			// 			"X-HNH-FILENAME"));
			// PUSH(LLIST(strbuf))(&ctx->key_stack, expected_key);

			return -1;

		} else {
			// FFREE(strbuf, expected_key);
			// POP(LLIST(vcomponent))(&ctx->comp_stack);
			SCM_POP_X (ctx->key_stack);
			SCM_POP_X (ctx->comp_stack);
		}

	/*
	 * A regular key, value pair. Push it into to the current
	 * component.
	 */
	} else {

		SNEW(strbuf, sbuf);
		TRANSFER (&sbuf, val);

		SCM str = scm_from_utf8_stringn (sbuf.mem, sbuf.len);
		FREE(strbuf) (&sbuf);
		scm_set_car_x (*content_pair, str);

		vcomponent* e = scm_to_vcomponent (scm_car (ctx->comp_stack));
		vcomponent_push_val (e, key, *content_pair);

		*content_pair = scm_cons (SCM_BOOL_F, scm_make_hash_table (scm_from_ulong(32)));
		scm_gc_protect_object (*content_pair);
	}

	strbuf_reset(key);
	strbuf_reset(&ctx->str);

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
	self->key_stack = SCM_EOL;
	self->comp_stack = SCM_EOL;

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

	// FREE(LLIST(strbuf))(&self->key_stack);
	// FREE(LLIST(vcomponent))(&self->comp_stack);
	self->key_stack = SCM_UNDEFINED;
	self->comp_stack = SCM_UNDEFINED;
	free(self->filename);

	self->line = 0;
	self->column = 0;
	FREE(strbuf)(&self->str);

	return 0;
}

int handle_escape (parse_ctx* ctx) {
	char esc = fgetc(ctx->f);

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
		esc = '\n';

	/* "Standard" escaped character */
	} else if (esc == ';' || esc == ',' || esc == '\\') {
		/* esc already contains character, do nothing */

	/* Invalid escaped character */
	} else {
		ERR_P(ctx, "Non escapable character '%c' (%i)", esc, esc);
	}

	/* save escapade character as a normal character */
	strbuf_append(&ctx->str, esc);

	++ctx->column;
	++ctx->pcolumn;

	return 0;
}
