#include "parse.h"

#include <iostream>

#include <errno.h>
#include <string.h>
#include <assert.h>

#include "macro.h"
#include "vcal.h"

#include "err.h"
#include "pair.h"

/*
 * name *(";" param) ":" value CRLF
 */
int parse_file(char* filename, FILE* f, vcomponent* root) {
	part_context p_ctx = p_key;

	parse_ctx ctx(filename);
	ctx.comp_stack.push(root);

	std::string key;
	param_set* ps;

	char c;
	while ( (c = fgetc(f)) != EOF) {

		/* We have a linebreak */
		if (c == '\r' || c == '\n') {

			if (fold(f, &ctx, c) > 0) {
				/* Actuall end of line, handle value */

				// strbuf_cap(&ctx.str);
				ctx.str += '\0';
				handle_kv(&key, &ctx);
				// strbuf_soft_reset(&ctx.str);
				ctx.str.clear();

				p_ctx = p_key;
			} /* Else continue on current line */

		/* We have an escaped character */
		} else if (c == '\\') {
			char esc = fgetc(f);
			char target;

			/*
			 * An actuall linebreak after the backslash. Unfold the
			 * line and find the next "real" character, and escape
			 * that.
			 */
			if (esc == '\r' || esc == '\n') {
				int ret;
				if ( (ret = fold(f, &ctx, esc)) != 0) {
					if (ret == 1) ERR_P(&ctx, "ESC before not folded line");
					else          ERR_P(&ctx, "other error: val = %i", ret);
					exit (2);
				} else {
					esc = fgetc(f);
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
				ERR_P(&ctx, "Non escapable character '%c' (%i)", esc, esc);
			}

			/* save escapade character as a normal character */
			// strbuf_append(&ctx.str, target);
			ctx.str += target;

			++ctx.column;
			++ctx.pcolumn;


		/* Border between param {key, value} */
		} else if (p_ctx == p_param_name && c == '=') {
			// strbuf_cap(&ctx.str);
			ctx.str += '\0';

			ps = new param_set;
			*ps->key = ctx.str;
			ctx.str.clear();
			// strbuf_soft_reset(&ctx.str);

			p_ctx = p_param_value;

		/*
		 * One of four cases:
		 * 1) end of key  , start of value
		 * 2)   ,,   key  ,    ,,    param
		 * 3)   ,,   param,    ,,    param
		 * 4)   ,,   param,    ,,    value
		 */
		} else if ((p_ctx == p_key || p_ctx == p_param_value) && (c == ':' || c == ';')) {
			/* We have the end of the initial key, or the end of a
			 * parameter value */ 
			// strbuf_cap(&ctx.str);
			ctx.str += '\0';

			if (p_ctx == p_param_value) {
				/* push kv pair */
				ps->val->push(new std::string(ctx.str));
				ctx.str.clear();
				// strbuf_soft_reset(&ctx.str);

			}

			if (p_ctx == p_key) {
				// strbuf_cap(&ctx.str);
				ctx.str += '\0';
				key = ctx.str;
				// strbuf_soft_reset(&ctx.str);
				ctx.str.clear();
			}

			if      (c == ':') p_ctx = p_value;
			else if (c == ';') p_ctx = p_param_name;

		} else {
			/* Regular character, append it to the current read
			 * buffer */
			// strbuf_append(&ctx.str, c);
			ctx.str += c;

			++ctx.column;
			++ctx.pcolumn;
		}
	}

	if (! feof(f)) {
		ERR("Error parsing");
	}
	/* Check to see if empty line */
	else if (ctx.str.length() != 0) {
		/*
		 * The standard (3.4, l. 2675) says that each icalobject must
		 * end with CRLF. My files however does not, so we also parse
		 * the end here.
		 */

		// strbuf_cap(&ctx.str);
		ctx.str += '\0';
		handle_kv(&key, &ctx);
		ctx.str.clear();
		// strbuf_soft_reset(&ctx.str);

		++ctx.line;
		++ctx.pline;
		ctx.column = 0;


	}

	assert(ctx.comp_stack.pop() == root);
	assert(ctx.key_stack.empty());
	assert(ctx.comp_stack.empty());

	return 0;
}

int handle_kv (
	std::string* key,
	// content_line* cline,
	parse_ctx* ctx
	) {

	std::string* val = &ctx->str;

	// std::cout << *key << ':' << *val << std::endl;
	if (*key == "BEGIN") {
		/* val \in { VCALENDAR, VEVENT, VALARM, VTODO, VTIMEZONE } */

		ctx->key_stack.push(val);

		auto e = new vcomponent(val->c_str(), ctx->filename);
		e->parent = ctx->comp_stack.peek();
		ctx->comp_stack.push(e);

	/* A block is ending */
	} else if (*key == "END") {
		/* Fetch what we are supposed to end */ 

		std::string* s = ctx->key_stack.pop();

		/* Error if we got something else */
		if ( ! (*val == *s) ) {
			ERR_P(ctx, "Expected END:%s, got %s:%s.\n%s",
					s->c_str(),
					key->c_str(),
					val->c_str(),
					ctx->comp_stack.peek()->filename
					);

			/* put value back on stack */
			ctx->key_stack.push(s);
			return -1;

		/* We got a propper end, make the current vcomponent a child
		 * to the vcomponent under it on the stack.*/
		} else {
			/* free the key string */
			// delete s;

			/* Received propper end, push cur into parent */
			vcomponent* cur = ctx->comp_stack.pop();

			// TODO should self instead be done at creation time?
			PUSH(vcomponent)(ctx->comp_stack.peek(), cur);
		}

	/* Neither BEGIN nor END, so a regular KV pair */
	} else {
		vcomponent* cur_comp = ctx->comp_stack.peek();
		cur_comp->push_kv (key, val);
	}

	return 0;
}

int fold(FILE* f, parse_ctx* ctx, char c) {
	int retval;

	char buf[2] = {
		(c == '\n' ? '\n' : (char) fgetc(f)),
		(char) fgetc(f)
	};

	ctx->pcolumn = 1;

	if (buf[0] != '\n') {
		ERR_P(ctx, "expected new_line after CR");
		retval = -1;

	} else if (buf[1] == ' ' || buf[1] == '\t') {
		retval = 0;
		ctx->pcolumn++;

	} else if (ungetc(buf[1], f) != buf[1]) {
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


parse_ctx::parse_ctx (const char* filename) {
	this->filename = (char*) calloc(sizeof(*filename), strlen(filename) + 1);
	strcpy(this->filename, filename);

	this->line    = 0;
	this->column  = 0;

	this->pline   = 1;
	this->pcolumn = 1;
}

parse_ctx::~parse_ctx () {
	free(this->filename);

	this->line = 0;
	this->column = 0;
}
