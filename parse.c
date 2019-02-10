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
	int segments = 1;
	SNEW(strbuf, str, segments * SEGSIZE);

	part_context p_ctx = p_key;

	SNEW(parse_ctx, ctx, filename);
	PUSH(LLIST(vcomponent))(&ctx.comp_stack, root);

	int keylen = 100;
	int vallen = 100;

	int line = 0;

	SNEW(content_line, cline, keylen, vallen);

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

			if (s[0] != '\n') { ERR_F("%s, %i", "expected newline after CR", line); }
			else if (s[1] == ' ' || s[1] == '\t') {
				/* Folded line, increase size of key and continue.  */

				/* TODO segments is always incremented here, meaning
				 * that segment grows larger for every multi line
				 * encountered.
				 */
				if (strbuf_realloc(&str, ++segments * SEGSIZE) != 0) {
					ERR_F("%s, %i", "Failed to realloc strbuf", line);
					exit (1);
				}
				continue;
			} else {

				/* Actuall end of line, handle values.  */

				/* Push back the last written character to the stream,
				 * since it's part of the next line.
				 */
				if (ungetc(s[1], f) != s[1]) {
					ERR_F("%s, %i", "Failed to put character back on FILE", line);
					exit (2);
				}

				if (str.ptr + 1 > vallen) {
					vallen = str.ptr + 1;
					strbuf_realloc(cline.vals.cur->value, vallen);
				}

				strbuf_copy(cline.vals.cur->value, &str);

				/* TODO when do I actualy cap? */
				// strbuf_cap(cline.vals.cur->value);

				++line;
				handle_kv(&cline, line, &ctx);
				strbuf_soft_reset(&str);
				p_ctx = p_key;

				continue;
			}

		/*
		 * TODO context for property_{key,val}.
		 */
		} else if (p_ctx == p_key && c == ':') {
			if (str.ptr + 1 > keylen) {
				keylen = str.ptr + 1;
				/*
				 * Allow for key's longer than 100 octets. It
				 * currently is of no use, since key's can't be
				 * folded.
				 * TODO check if everything should be unfolded at 75
				 * octets, or if that is only for values.
				 *
				 * TODO earlier there was a bug here. Test if that is
				 * fixed and reenable this line.
				 */
				// strbuf_realloc(&cline.key, keylen);
			}
			strbuf_copy(&cline.key, &str);
			strbuf_cap(&cline.key);
			strbuf_soft_reset(&str);
			p_ctx = p_value;
			continue;
		}

		strbuf_append(&str, c);
	}

	if (! feof(f)) {
		ERR("Error parsing");
	} else if (cline.vals.cur->value->len != 0 && str.ptr != 0) {
		/*
		 * The standard (3.4, l. 2675) says that each icalobject must
		 * end with CRLF. My files however does not, so we also parse
		 * the end here.
		 */
		if (str.ptr + 1 > vallen) {
			vallen = str.ptr + 1;
			strbuf_realloc(cline.vals.cur->value, vallen);
		}

		strbuf_copy(cline.vals.cur->value, &str);
		strbuf_cap(cline.vals.cur->value);
		handle_kv(&cline, line, &ctx);

	}

	FREE(strbuf)(&str);
	FREE(content_line)(&cline);
	// FREE(strbuf)(ctx.skip_to);

	assert(POP(LLIST(vcomponent))(&ctx.comp_stack) == root);
	assert(EMPTY(LLIST(strbuf))(&ctx.key_stack));
	assert(EMPTY(LLIST(vcomponent))(&ctx.comp_stack));

	FREE(LLIST(strbuf))(&ctx.key_stack);
	FREE(LLIST(vcomponent))(&ctx.comp_stack);
	free(ctx.filename);

	FREE(strbuf)(&kv.key);
	FREE(strbuf)(&kv.val);

	return 0;
}

int handle_kv (
	content_line* cline,
	int	line,
	parse_ctx* ctx
	) {

	if (strbuf_c(&cline->key, "BEGIN")) {
		/* should be one of:
		 * VCALENDAR, VEVENT, VALARM, VTODO, VTIMEZONE,
		 * and possibly some others I forget.
		 */
		NEW(strbuf, s);
		strbuf_init_copy(s, cline->vals.cur->value);
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
					s->mem, cline->vals.cur->value->mem, PEEK(LLIST(vcomponent))(&ctx->comp_stack)->filename, line);
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
	}

	return 0;
}

INIT_F(parse_ctx, char* filename) {
	INIT(LLIST(strbuf), &this->key_stack);
	INIT(LLIST(vcomponent), &this->comp_stack);
	this->filename = calloc(sizeof(*filename), strlen(filename) + 1);
	strcpy(this->filename, filename);
	return 0;
}

int push_strbuf(strbuf* target, strbuf* src) {
#if 0
	if (src->ptr + 1 > keylen) {
		keylen = str->ptr + 1;
		/*
		 * Allow for key's longer than 100 octets. It
		 * currently is of no use, since key's can't be
		 * folded.
		 * TODO check if everything should be unfolded at 75
		 * octets, or if that is only for values.
		 *
		 * TODO earlier there was a bug here. Test if that is
		 * fixed and reenable this line.
		 */
		// strbuf_realloc(&cline.key, keylen);
	}
#endif

	strbuf_copy(target, src);
	strbuf_cap(target);
	strbuf_soft_reset(src);

	// continue;
	return 0;
}
