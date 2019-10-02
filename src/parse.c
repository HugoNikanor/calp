#include "parse.h"

#include <errno.h>
#include <string.h>
#include <assert.h>

#include "macro.h"
// #include "vcal.h"

#include "err.h"

#include <libguile.h>
#include "struct.h"
#include "guile_type_helpers.h"

// #define TYPE vcomponent
// #include "linked_list.inc.h"
// #undef TYPE

// #define T strbuf
// #define V strbuf
// #include "pair.h"
// #include "pair.inc.h"
// #undef T
// #undef V

/*
           +-------------------------------------------------------+
           v                                                       |
  BEGIN → key -------------------------------→ ':' → value → CRLF -+-→ EOF
           |                                    ^
           v                                    |
          ';' → param-key → ':' → param-value --+
           ^                                    |
           +------------------------------------+


   vcomponent := map<string, list<line>>
   line := pair<value, attributes>
   attributes := map<string, list<value>>


 */

#define string_eq(a, b) scm_is_true(scm_string_eq(a, b, SCM_UNDEFINED,SCM_UNDEFINED,SCM_UNDEFINED,SCM_UNDEFINED))

/*
 * name *(";" param) ":" value CRLF
 */
int parse_file(char* filename, FILE* f, SCM root) {

	part_context p_ctx = p_key;

	SNEW(parse_ctx, ctx, f, filename);
	// PUSH(LLIST(vcomponent))(&ctx.comp_stack, root);

	/*
	 * Create a content_line which we use as storage while we are
	 * parsing. This object is constantly broken down and rebuilt.
	 *
	 * {cline,param}_key is also temporary register used during
	 * parsing.
	 */
	// SNEW(content_line, cline);
	// SNEW(strbuf, param_key);
	// SNEW(strbuf, param_val);
	// SNEW(strbuf, attr_key);
	// SNEW(strbuf, attr_val);

	SNEW(strbuf, str);
	SCM component = root;
	SCM line = scm_make_vline();
	SCM attr_key;           /* string */
	SCM line_key = scm_from_utf8_string("");           /* string */

	INFO("Starting parsing");
	char c;
	INFO("here");
	while ( (c = fgetc(f)) != EOF) {
		INFO_F("LOOP %c", c);

		/* We have a linebreak */
		if (c == '\r' || c == '\n') {
			INFO("EOL");

			if (fold(&ctx, c) > 0) {
				/* Actuall end of line, handle value */
				// TRANSFER(CLINE_CUR_VAL(&cline), &ctx.str);
				/*
				 * The key being BEGIN means that we decend into a new component.
				 */
				if (string_eq(line_key, scm_from_utf8_string("BEGIN"))) {
					/* key \in { VCALENDAR, VEVENT, VALARM, VTODO, VTIMEZONE, ...  } */
					INFO("Creating child");
					SCM child = scm_make_vcomponent(scm_string_to_symbol(scm_from_strbuf(&str)));
					scm_add_child_x (component, child);
					component = child;

				} else if (string_eq(line_key, scm_from_utf8_string("END"))) {
					// TODO make current component be parent of current component?
					INFO("back to parent");
					component = scm_component_parent(component);

					/*
					 * A regular key, value pair. Push it into to the current
					 * component.
					 */
				} else {
					INFO("Adding attribute");
					scm_struct_set_x(line, vline_value, scm_from_strbuf(&str));
					scm_add_line_x(component, line_key, line);
					line = scm_make_vline();
				}

				strbuf_soft_reset (&str);
				p_ctx = p_key;
			} /* Else continue on current line */

		/* We have an escaped character */
		} else if (c == '\\') {
			char esc = handle_escape (&ctx);
			strbuf_append(&str, esc);

		/* Border between param {key, value} */
		} else if (p_ctx == p_param_name && c == '=') {

			/* Save the current parameter key */
			// TODO
			// TRANSFER (&param_key, &ctx.str);
			INFO("Param key");
			attr_key = scm_from_strbuf(&str);
			p_ctx = p_param_value;
			strbuf_soft_reset (&str);

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
				INFO("param value");
				/* save current parameter value. */
				scm_add_attribute_x(line, attr_key, scm_from_strbuf(&str));
				strbuf_soft_reset (&str);
			}

			/*
			 * Top level key.
			 * Copy the key into the current cline, and create a
			 * content_set for the upcomming value and (possible)
			 * parameters.
			 */
			if (p_ctx == p_key) {

				INFO("key");
				// TRANSFER(&cline_key, &ctx.str);

				// NEW(content_set, p);
				// PUSH(LLIST(content_set))(&cline, p);
				// attr_key 
				line_key = scm_from_strbuf(&str);
				strbuf_soft_reset (&str);
			}

			if      (c == ':') p_ctx = p_value;
			else if (c == ';') p_ctx = p_param_name;

		/*
		 * Nothing interesting happened, append the read character to
		 * the current string.
		 */
		} else {
			strbuf_append(&str, c);

			++ctx.column;
			++ctx.pcolumn;
		}
	}

	if (! feof(f)) {
		ERR_F("Error parsing errno = %i", errno);
	}
	/* Check to see if empty line */
	else if (str.ptr != 0) {
		/*
		 * The standard (3.4, l. 2675) says that each icalobject must
		 * end with CRLF. My files however does not, so we also parse
		 * the end here.
		 */

		// TRANSFER(CLINE_CUR_VAL(&cline), &ctx.str);
		// TODO
		// handle_kv(&cline_key, &cline, &ctx);

	}

	// FREE(content_line)(&cline);
	// FREE(strbuf)(&cline_key);
	// FREE(strbuf)(&param_key);

	FREE(strbuf)(&str);

	// assert(POP(LLIST(vcomponent))(&ctx.comp_stack) == root);
	// assert(EMPTY(LLIST(strbuf))(&ctx.key_stack));
	// assert(EMPTY(LLIST(vcomponent))(&ctx.comp_stack));

	FREE(parse_ctx)(&ctx);

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
	// INIT(LLIST(strbuf), &self->key_stack);
	// INIT(LLIST(vcomponent), &self->comp_stack);
	self->filename = (char*) calloc(sizeof(*filename), strlen(filename) + 1);
	strcpy(self->filename, filename);
	self->f = f;

	self->line    = 0;
	self->column  = 0;

	self->pline   = 1;
	self->pcolumn = 1;

	// INIT(strbuf, &self->str);

	return 0;
}

FREE_F(parse_ctx) {

	// FREE(LLIST(strbuf))(&self->key_stack);
	// FREE(LLIST(vcomponent))(&self->comp_stack);
	free(self->filename);

	self->line = 0;
	self->column = 0;
	// FREE(strbuf)(&self->str);

	return 0;
}

char handle_escape (parse_ctx* ctx) {
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

	++ctx->column;
	++ctx->pcolumn;

	return esc;

	/* save escapade character as a normal character */
	// strbuf_append(&ctx->str, esc);

	// return 0;
}
