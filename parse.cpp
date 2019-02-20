#include "parse.h"

#include <cstring>

#include <errno.h>
#include <assert.h>

#include "vcal.h"
#include "strbuf.h"

#include "err.h"

/*
 * name *(";" param) ":" value CRLF
 */
int parse_file(char* filename, FILE* f, vcomponent* root) {
	part_context p_ctx = p_key;

	parse_ctx ctx(filename);
	ctx.comp_stack.push(root);

	content_line cline;

	char c;
	while ( (c = fgetc(f)) != EOF) {

		/* We have a linebreak */
		if (c == '\r' || c == '\n') {

			if (fold(f, &ctx, c) > 0) {
				/* Actuall end of line, handle value */

				cline.push_value(new strbuf(ctx.str));

				std::cout << _RED << ctx.str.len << ' ' << ctx.str << _RESET << std::endl;
				strbuf* n = cline.values.back()->value;
				std::cout << _BLUE << n->len << ' ' << *n << _RESET << std::endl;
				handle_kv(&cline, &ctx);
				ctx.str.soft_reset();

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

			/* Escaped newline */
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
			ctx.str += target;

			++ctx.column;
			++ctx.pcolumn;


		/* Border between param {key, value} */
		} else if (p_ctx == p_param_name && c == '=') {

			// ctx.str holds param_key;
			// cline.values.back()->params.push_back(new __param_set(ctx.str));
			cline.values.back()->push_param_key(new strbuf(ctx.str));

			ctx.str.soft_reset();

			p_ctx = p_param_value;

		/*
		 * One of four cases:
		 * 1) end of key  , start of value
		 * 2)   ,,   key  ,    ,,    param
		 * 3)   ,,   param,    ,,    param
		 * 4)   ,,   param,    ,,    value
		 */
		} else if ((p_ctx == p_key || p_ctx == p_param_value) && (c == ':' || c == ';')) {

			if (p_ctx == p_param_value) {
				/* push kv pair */

				cline.values.back()->push_param_value(new strbuf(ctx.str));
				ctx.str.soft_reset();
			}

			if (p_ctx == p_key) {
				/* set key for content line */
				*cline.key = ctx.str;
				cline.key->cap();
				ctx.str.soft_reset();


			}

			if      (c == ':') p_ctx = p_value;
			else if (c == ';') p_ctx = p_param_name;

		} else {
			ctx.str += c;

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

		++ctx.line;
		ctx.column = 0;

		cline.push_value(new strbuf(ctx.str));

		std::cout << ctx.str;
		handle_kv(&cline, &ctx);
		ctx.str.soft_reset();

	}

	// assert(POP(LLIST(vcomponent))(&ctx.comp_stack) == root);
	// assert(EMPTY(LLIST(strbuf))(&ctx.key_stack));
	// assert(EMPTY(LLIST(vcomponent))(&ctx.comp_stack));

	return 0;
}

int handle_kv (
	content_line* cline,
	parse_ctx* ctx
	) {

	if (*cline->key == "BEGIN") {
		/* should be one of:
		 * VCALENDAR, VEVENT, VALARM, VTODO, VTIMEZONE,
		 * and possibly some others I forget.
		 */

		ctx->key_stack.push(new strbuf(ctx->str));

		// cline->values.reset();
		// TODO ompty cline->second here;
		// RESET(LLIST(content_set))(&cline->val);

		auto e = new vcomponent(ctx->str.to_string(), ctx->filename);
		e->parent = ctx->comp_stack.top();
		ctx->comp_stack.push(e);

	} else if (*cline->key == "END") {
		// strbuf* s = POP(LLIST(strbuf))(&ctx->key_stack);
		strbuf* s = ctx->key_stack.top(); ctx->key_stack.pop();
		if (*s == ctx->str) {
			ERR_P(ctx, "Expected END:%s, got END:%s.\n%s line",
					s->c_str(),
					cline->values.back()->value->c_str(),
					ctx->comp_stack.top()->filename.c_str());

			ctx->key_stack.push(s);
			return -1;

		} else {
			delete s;

			/* Received propper end, push cur into parent */
			vcomponent* cur = ctx->comp_stack.top(); ctx->comp_stack.pop();

			// TODO we never get here
			INFO(here);

			// TODO should self instead be done at creation time?
			ctx->comp_stack.top()->push(*cur);
		}
	} else {
		content_line* c = new content_line;

		// TODO make sure deep-copy
		*c = *cline;

		// TODO
		// RESET(LLIST(content_set))(&cline->val);

		ctx->comp_stack.top()->clines.push(c->key->c_str(), c);
		// PUSH(TRIE(content_line))(
		// 		&PEEK(LLIST(vcomponent))(&ctx->comp_stack)->clines,
		// 		c->key.mem, c);

		// TODO?
		// ctx->comp_stack.top()->clines.push_back(c->first, c);
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
