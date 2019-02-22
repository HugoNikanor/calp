#include "parse.h"

#include <errno.h>
#include <string.h>
#include <assert.h>

#include "macro.h"
#include "vcal.h"

#include "err.h"

// #define TYPE vcomponent
// #include "linked_list.inc.h"
// #undef TYPE

// #define T strbuf
// #define V strbuf
#include "pair.h"
// #include "pair.inc.h"
// #undef T
// #undef V

/*
 * name *(";" param) ":" value CRLF
 */
int parse_file(char* filename, FILE* f, vcomponent* root) {
	part_context p_ctx = p_key;

	parse_ctx ctx(filename);
	// PUSH(LLIST(vcomponent))(&ctx.comp_stack, root);
	ctx.comp_stack.push(root);

	content_line cline;
	// SNEW(content_line, cline);

	char c;
	while ( (c = fgetc(f)) != EOF) {

		/* We have a linebreak */
		if (c == '\r' || c == '\n') {

			if (fold(f, &ctx, c) > 0) {
				/* Actuall end of line, handle value */

				strbuf* target = CLINE_CUR_VAL(&cline);

				// DEEP_COPY(strbuf)(target, &ctx.str);
				*target = ctx.str;
				strbuf_cap(target);
				strbuf_soft_reset(&ctx.str);

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
			strbuf_append(&ctx.str, target);

			++ctx.column;
			++ctx.pcolumn;


		/* Border between param {key, value} */
		} else if (p_ctx == p_param_name && c == '=') {
			auto params = CLINE_CUR_PARAMS(&cline);

			// NEW(param_set, ps);
			auto ps = new param_set;
			// DEEP_COPY(strbuf)(&ps->key, &ctx.str);
			*ps->key = ctx.str;
			strbuf_cap(ps->key);
			strbuf_soft_reset(&ctx.str);
			// PUSH(LLIST(param_set))(params, ps);
			params->push(ps);

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

				// NEW(strbuf, s);
				auto s = new strbuf;
				*s = ctx.str;

				// DEEP_COPY(strbuf)(s, &ctx.str);
				strbuf_cap(s);
				strbuf_soft_reset(&ctx.str);

				CLINE_CUR_PARAMS(&cline)->cur->value->val->push(s);
				// PUSH(LLIST(strbuf))(ls, s);

			}

			if (p_ctx == p_key) {
				// DEEP_COPY(strbuf)(&cline.key, &ctx.str);
				*cline.key = ctx.str;
				strbuf_cap(cline.key);
				strbuf_soft_reset(&ctx.str);

				// NEW(content_set, p);
				auto p = new content_set;
				// PUSH(LLIST(content_set))(&cline.val, p);
				cline.val->push(p);
			}

			if      (c == ':') p_ctx = p_value;
			else if (c == ';') p_ctx = p_param_name;

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

		strbuf* target = CLINE_CUR_VAL(&cline);
		*target = ctx.str;
		strbuf_cap(target);
		strbuf_soft_reset(&ctx.str);

		++ctx.line;
		ctx.column = 0;

		handle_kv(&cline, &ctx);

	}

	// FREE(content_line)(&cline);

	// assert(POP(LLIST(vcomponent))(&ctx.comp_stack) == root);
	assert(ctx.comp_stack.pop() == root);
	// assert(EMPTY(LLIST(strbuf))(&ctx.key_stack));
	assert(ctx.key_stack.empty());
	// assert(EMPTY(LLIST(vcomponent))(&ctx.comp_stack));
	assert(ctx.comp_stack.empty());

	// FREE(parse_ctx)(&ctx);

	return 0;
}

int handle_kv (
	content_line* cline,
	parse_ctx* ctx
	) {

	if (strbuf_c(cline->key, "BEGIN")) {
		/* should be one of:
		 * VCALENDAR, VEVENT, VALARM, VTODO, VTIMEZONE,
		 * and possibly some others I forget.
		 */

		// NEW(strbuf, s);
		auto s = new strbuf;
		// strbuf* type = CLINE_CUR_VAL(cline);
		// DEEP_COPY(strbuf)(s, type);
		*s = *CLINE_CUR_VAL(cline);
		// PUSH(LLIST(strbuf))(&ctx->key_stack, s);
		ctx->key_stack.push(s);

		// RESET(LLIST(content_set))(&cline->val);
		cline->val->reset();

		auto e = new vcomponent(s->mem, ctx->filename);
		// e->parent = PEEK(LLIST(vcomponent))(&ctx->comp_stack);
		e->parent = ctx->comp_stack.peek();
		//PUSH(LLIST(vcomponent))(&ctx->comp_stack, e);
		ctx->comp_stack.push(e);

	} else if (strbuf_c(cline->key, "END")) {
		//strbuf* s = POP(LLIST(strbuf))(&ctx->key_stack);
		strbuf* s = ctx->key_stack.pop();
		if (strbuf_cmp(s, CLINE_CUR_VAL(cline)) != 0) {
			// ERR_P(ctx, "Expected END:%s, got END:%s.\n%s line",
			// 		s->mem,
			// 		CLINE_CUR_VAL(cline)->mem,
			// 		PEEK(LLIST(vcomponent))(&ctx->comp_stack)->filename);
			// PUSH(LLIST(strbuf))(&ctx->key_stack, s);
			ctx->key_stack.push(s);
			return -1;

		} else {
			delete s;
			// FFREE(strbuf, s);
			/* Received propper end, push cur into parent */
			// vcomponent* cur = POP(LLIST(vcomponent))(&ctx->comp_stack);
			vcomponent* cur = ctx->comp_stack.pop();

			// TODO should self instead be done at creation time?
			PUSH(vcomponent)(ctx->comp_stack.peek(), cur);
		}
	} else {
		//NEW(content_line, c);
		content_line* c = new content_line(*cline);
		// DEEP_COPY(content_line)(c, cline);

		// PUSH(TRIE(content_line))(
		// 		&PEEK(LLIST(vcomponent))(&ctx->comp_stack)->clines,
		// 		c->key.mem, c);
		// (PEEK(LLIST(vcomponent))(&ctx->comp_stack)->clines).push(c->key.mem, c);
		ctx->comp_stack.peek()->clines.push(c->key->mem, c);

		//RESET(LLIST(content_set))(&cline->val);
		cline->val->reset();
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


//INIT_F(parse_ctx, char* filename) {
parse_ctx::parse_ctx (const char* filename) {
	// INIT(LLIST(strbuf), &this->key_stack);
	// INIT(LLIST(vcomponent), &this->comp_stack);
	this->filename = (char*) calloc(sizeof(*filename), strlen(filename) + 1);
	strcpy(this->filename, filename);

	this->line    = 0;
	this->column  = 0;

	this->pline   = 1;
	this->pcolumn = 1;

	// INIT(strbuf, &this->str);
}

// FREE_F(parse_ctx) {
parse_ctx::~parse_ctx () {

	// FREE(LLIST(strbuf))(&this->key_stack);
	// FREE(LLIST(vcomponent))(&this->comp_stack);
	free(this->filename);

	this->line = 0;
	this->column = 0;
	// FREE(strbuf)(&this->str);
}
