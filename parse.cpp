#include "parse.h"

#include <errno.h>
#include <string.h>
#include <assert.h>

#include "vcal.h"
#include "strbuf.h"

#include "err.h"

/*
 * name *(";" param) ":" value CRLF
 */
int parse_file(char* filename, FILE* f, vcomponent& root) {
	part_context p_ctx = p_key;

	parse_ctx ctx(filename);
	ctx.comp_stack.push(&root);

	content_line cline;

	char c;
	while ( (c = fgetc(f)) != EOF) {

		/* We have a linebreak */
		if (c == '\r' || c == '\n') {

			if (fold(f, &ctx, c) > 0) {
				/* Actuall end of line, handle value */

				// std::string& target = CLINE_CUR_VAL(&cline);
				// TODO solve current;
				// std::string& target = cline

				strbuf target = ctx.str;
				// DEEP_COPY(strbuf)(target, &ctx.str);
				target.cap();
				// strbuf_cap(target);
				ctx.str.soft_reset();
				// strbuf_soft_reset(&ctx.str);

				handle_kv(&cline, &ctx);

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
			// LLIST(param_set)* params = CLINE_CUR_PARAMS(&cline);
			/*
			std::list<param_set>* params = cline.val.val;

			// NEW(param_set, ps);
			auto ps = new param_set;
			// TODO make sure this is a deep copy
			ps->first = ctx.str;
			ps->first.cap();
			ctx.str.soft_reset();
			params += ps;
			// PUSH(LLIST(param_set))(params, ps);
			*
			*/

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

				auto s = new strbuf; 
				// TODO make sure this is a deep copy
				*s = ctx.str;

				s->cap();
				ctx.str.soft_reset();

				llist<strbuf>* ls = & CLINE_CUR_PARAMS(&cline)->cur->value->second;
				ls->push(s);

			}

			if (p_ctx == p_key) {
				cline.first = ctx.str;
				cline.first.cap();
				ctx.str.soft_reset();

				content_set* p = new content_set;
				cline.second.push(p);
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

		strbuf* target = CLINE_CUR_VAL(&cline);
		*target = ctx.str;
		target->cap();
		ctx.str.soft_reset();

		++ctx.line;
		ctx.column = 0;

		handle_kv(&cline, &ctx);

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

	if (cline->first == "BEGIN") {
		/* should be one of:
		 * VCALENDAR, VEVENT, VALARM, VTODO, VTIMEZONE,
		 * and possibly some others I forget.
		 */

		strbuf* s = new strbuf;
		strbuf* type = CLINE_CUR_VAL(cline);
		*s = *type;
		ctx->key_stack.push(s);

		// TODO ompty cline->second here;
		// RESET(LLIST(content_set))(&cline->val);

		auto e = new vcomponent(s->mem, ctx->filename);
		e->parent = ctx->comp_stack.top();
		ctx->comp_stack.push(e);

	} else if (cline->first == "END") {
		// strbuf* s = POP(LLIST(strbuf))(&ctx->key_stack);
		strbuf* s = ctx->key_stack.top(); ctx->key_stack.pop();
		if (s == CLINE_CUR_VAL(cline)) {
#if 0
			ERR_P(ctx, "Expected END:%s, got END:%s.\n%s line",
					s->mem,
					CLINE_CUR_VAL(cline)->mem,
					PEEK(LLIST(vcomponent))(&ctx->comp_stack)->filename);
#endif
			ctx->key_stack.push(s);
			return -1;

		} else {
			delete s;

			/* Received propper end, push cur into parent */
			vcomponent* cur = ctx->comp_stack.top(); ctx->comp_stack.pop();

			// TODO should self instead be done at creation time?
			ctx->comp_stack.push(cur);
		}
	} else {
		content_line* c = new content_line;
		// TODO make sure deep-copy
		*c = *cline;

		// PUSH(TRIE(content_line))(
		// 		&PEEK(LLIST(vcomponent))(&ctx->comp_stack)->clines,
		// 		c->key.mem, c);

		// TODO?
		// ctx->comp_stack.top()->clines.push_back(c->first, c);
		// TODO
		// RESET(LLIST(content_set))(&cline->val);
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
