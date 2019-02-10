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

/*
 * name *(";" param) ":" value CRLF
 */
int parse_file(char* filename, FILE* f, vcomponent* root) {
	part_context p_ctx = p_key;

	SNEW(parse_ctx, ctx, filename);
	PUSH(LLIST(vcomponent))(&ctx.comp_stack, root);

	//                      vallen -+
	//                 keylen -+    |
	//                         |    |
	SNEW(content_line, cline, 100, 100);
	SNEW(PAIR(strbuf,strbuf), kv);

	char c;
	while ( (c = fgetc(f)) != EOF) {

		/*
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
		if (c == '\r' || c == '\n') {

			char s[2];
			s[0] = (c == '\n' ? '\n' : fgetc(f));
			s[1] = fgetc(f);

			if (s[0] != '\n') { ERR_F("%s, %i", "expected newline after CR", ctx.line); }
			else if (s[1] == ' ' || s[1] == '\t') {
				/* Folded line, don't end line */
			} else {

				/* Actuall end of line, handle values.  */

				/* Push back the last written character to the stream,
				 * since it's part of the next line.
				 */
				if (ungetc(s[1], f) != s[1]) {
					ERR_F("%s, %i", "Failed to put character back on FILE", ctx.line);
					exit (2);
				}

				DEEP_COPY(strbuf)(cline.vals.cur->value, &ctx.str);
				strbuf_cap(cline.vals.cur->value);
				strbuf_soft_reset(&ctx.str);

				++ctx.line;
				ctx.column = 0;

				handle_kv(&cline, &ctx);

				p_ctx = p_key;
			}


		/*
		 * Border between param {key, value}
		 */
		} else if (p_ctx == p_param_name && c == '=') {
			DEEP_COPY(strbuf)(&kv.left, &ctx.str);
			strbuf_cap(&kv.right);
			strbuf_soft_reset(&ctx.str);

			p_ctx = p_param_value;

		/*
		 * One of four cases:
		 * 1) end of key  , start of value
		 * 2)   ,,   key  ,    ,,    param
		 * 3)   ,,   param,    ,,    param
		 * 4)   ,,   param,    ,,    value
		 */
		} else if ((p_ctx == p_key || p_ctx == p_param_value) && (c == ':' || c == ';')) {
			strbuf* dest;
			if      (p_ctx == p_key)         dest = &cline.key;
			else if (p_ctx == p_param_value) dest = &kv.right;

			DEEP_COPY(strbuf)(dest, &ctx.str);
			strbuf_cap(dest);
			strbuf_soft_reset(&ctx.str);

			if (p_ctx == p_param_value) {
				/* push kv pair */
				NEW (PAIR(strbuf,strbuf), _kv);
				DEEP_COPY(PAIR(strbuf,strbuf))(_kv, &kv);
				PUSH(LLIST(PAIR(strbuf,strbuf)))(&cline.params, _kv);
			}

			if      (c == ':') p_ctx = p_value;
			else if (c == ';') p_ctx = p_param_name;

		} else {
			strbuf_append(&ctx.str, c);
			++ctx.column;
		}
	}

	if (! feof(f)) {
		ERR("Error parsing");
	} else if (cline.vals.cur->value->len != 0 && ctx.str.ptr != 0) {
		/*
		 * The standard (3.4, l. 2675) says that each icalobject must
		 * end with CRLF. My files however does not, so we also parse
		 * the end here.
		 */

		DEEP_COPY(strbuf)(cline.vals.cur->value, &ctx.str);
		strbuf_cap(cline.vals.cur->value);
		handle_kv(&cline, &ctx);

	}

	FREE(content_line)(&cline);

	assert(POP(LLIST(vcomponent))(&ctx.comp_stack) == root);
	assert(EMPTY(LLIST(strbuf))(&ctx.key_stack));
	assert(EMPTY(LLIST(vcomponent))(&ctx.comp_stack));

	FREE(parse_ctx)(&ctx);

	FREE(PAIR(strbuf,strbuf))(&kv);

	return 0;
}

int handle_kv (
	content_line* cline,
	parse_ctx* ctx
	) {

	if (strbuf_c(&cline->key, "BEGIN")) {
		/* should be one of:
		 * VCALENDAR, VEVENT, VALARM, VTODO, VTIMEZONE,
		 * and possibly some others I forget.
		 */

		NEW(strbuf, s);
		DEEP_COPY(strbuf)(s, cline->vals.cur->value);
		PUSH(LLIST(strbuf))(&ctx->key_stack, s);

		NEW(vcomponent, e,
				s->mem,
				ctx->filename);
		e->parent = PEEK(LLIST(vcomponent))(&ctx->comp_stack);
		PUSH(LLIST(vcomponent))(&ctx->comp_stack, e);

	} else if (strbuf_c(&cline->key, "END")) {
		strbuf* s = POP(LLIST(strbuf))(&ctx->key_stack);
		if (strbuf_cmp(s, cline->vals.cur->value) != 0) {
			ERR_F("Expected END:%s, got END:%s.\n%s line %i",
					s->mem, cline->vals.cur->value->mem, PEEK(LLIST(vcomponent))(&ctx->comp_stack)->filename, ctx->line);
			PUSH(LLIST(strbuf))(&ctx->key_stack, s);
			return -1;

		} else {
			FFREE(strbuf, s);
			/* Received propper end, push cur into parent */
			vcomponent* cur = POP(LLIST(vcomponent))(&ctx->comp_stack);

			// TODO should this instead be done at creation time?
			PUSH(vcomponent)(PEEK(LLIST(vcomponent))(&ctx->comp_stack), cur);
		}
	} else {
		NEW(content_line, c);
		content_line_copy(c, cline);

		PUSH(TRIE(content_line))(
				&PEEK(LLIST(vcomponent))(&ctx->comp_stack)->clines,
				c->key.mem, c);

		if ( SIZE(LLIST(PAIR(strbuf,strbuf)))(&c->params) != 0 ) {
			RESET(LLIST(PAIR(strbuf,strbuf)))(&cline->params);
		}
	}

	return 0;
}

INIT_F(parse_ctx, char* filename) {
	INIT(LLIST(strbuf), &this->key_stack);
	INIT(LLIST(vcomponent), &this->comp_stack);
	this->filename = calloc(sizeof(*filename), strlen(filename) + 1);
	strcpy(this->filename, filename);

	this->line = 0;
	this->column = 0;
	INIT(strbuf, &this->str);

	return 0;
}

FREE_F(parse_ctx) {

	FREE(LLIST(strbuf))(&this->key_stack);
	FREE(LLIST(vcomponent))(&this->comp_stack);
	free(this->filename);

	this->line = 0;
	this->column = 0;
	FREE(strbuf)(&this->str);

	return 0;
}
