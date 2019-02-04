#include "parse.h"

#include <errno.h>
#include <string.h>

#include "macro.h"
#include "vcal.h"

int parse_file(char* fname, FILE* f, vcalendar* cal) {
	int segments = 1;
	SNEW(strbuf, str, segments * SEGSIZE);

	part_context p_ctx = p_key;
	SNEW(strbuf, skip, SEGSIZE);
	parse_ctx ctx = {
		.scope = s_none,
		.skip_to = &skip
	};
	// scope_context s_ctx = s_none;

	int keylen = 100;
	int vallen = 100;

	int line = 0;

	// NEW(vevent, ev, fname);
	vevent* ev = NULL;

	SNEW(content_line, cline, keylen, vallen);

	char c;
	while ( (c = fgetc(f)) != EOF) {
		/*
		 * A carrige return means that the current line is at an
		 * end. The following character should always be \n.
		 * However, if the first character on the next line is a
		 * whitespace then the two lines should be concatenated.
		 */
		if (c == '\r') {

			char s[2];
			s[0] = fgetc(f);
			s[1] = fgetc(f);

			if (s[0] != '\n') { ERR("expected newline after CR", line); }
			else if (s[1] == ' ' || s[1] == '\t') {
				/* Folded line, increase size of key and continue.  */

				/* TODO segments is always incremented here, meaning
				 * that segment grows larger for every multi line
				 * encountered.
				 */
				if (strbuf_realloc(&str, ++segments * SEGSIZE) != 0) {
					ERR("Failed to realloc strbuf", line);
					exit (1);
				}
				continue;
			} else {
				/* Actuall end of line, handle values.  */
				if (ungetc(s[1], f) != s[1]) {
					ERR("Failed to put character back on FILE", line); 
					exit (2);
				}

				/* At TRUE end of line */
				if (str.ptr + 1 > vallen) {
					vallen = str.ptr + 1;
					strbuf_realloc(cline.vals.cur->value, vallen);
				}

				strbuf_copy(cline.vals.cur->value, &str);
				strbuf_cap(cline.vals.cur->value);

				++line;

				switch (handle_kv(cal, ev, &cline, line, &ctx)) {
					case s_event:
						RENEW(vevent, ev, fname);
						// ev = malloc(sizeof(*ev));
						// INIT(vevent, ev, fname);
						break;
				}
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
				// TODO this might break everything
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
		ERR("Error parsing", errno);
	} else {
		/*
		 * The standard (3.4, l. 2675) says that each icalobject must
		 * end with CRLF. My files however does not, so we also parse
		 * the end here.
		 * TODO -- this doesn't do anything with its read value
		 * TODO This might crash if we have the CRLF
		 */
		if (str.ptr + 1 > vallen) {
			vallen = str.ptr + 1;
			strbuf_realloc(cline.vals.cur->value, vallen);
		}
		strbuf_copy(cline.vals.cur->value, &str);
		strbuf_cap(cline.vals.cur->value);
		handle_kv(cal, ev, &cline, line, &ctx);
	}
	FREE(strbuf)(&str);
	FREE(content_line)(&cline);
	FREE(strbuf)(ctx.skip_to);

	return 0;
}

/*
 * TODO Extend this to handle properties
 */
int handle_kv(
		vcalendar*     cal,
		vevent*        ev,
		content_line*  cline,
		int            line,
		// scope_context* s_ctx
		parse_ctx* ctx
		) {
	switch (ctx->scope) {

		case s_skip:
			if (strbuf_c(&cline->key, "END") && strbuf_cmp(cline->vals.cur->value, ctx->skip_to)) {
				ctx->scope = s_calendar;
				// FREE(strbuf)(ctx->skip_to);
			}
			break;

		case s_none:
			if (! (strbuf_c(&cline->key, "BEGIN") && strbuf_c(cline->vals.cur->value, "VCALENDAR"))) {
				ERR("Invalid start of calendar", line);
				return -1;
			}
			ctx->scope = s_calendar;
			break;

		case s_calendar:
			if (strbuf_c(&cline->key, "BEGIN")) {
				if (strbuf_c(cline->vals.cur->value, "VEVENT")) {
					ctx->scope = s_event;
					return ctx->scope;
					break;
				} else {
					// ERR("Unsupported start", line);
					ctx->scope = s_skip;
					strbuf_copy(ctx->skip_to, cline->vals.cur->value);
				}
			} else if (strbuf_c(&cline->key, "END")) {
				if (strbuf_c(cline->vals.cur->value, "VCALENDAR")) {
					ctx->scope = s_none;
					break;
				}
			}
			break;

		case s_event:
			if (ev == NULL) {
				ERR("Something has gone terribly wrong", line);
				return -5;
			}
			if (strbuf_c(&cline->key, "END")) {
				if (strbuf_c(cline->vals.cur->value, "VEVENT")) {
					push_event(cal, ev);
					ctx->scope = s_calendar;
					return ctx->scope;
				} else {
					ERR("Trying to end something, expected VEVENT", line);
					return -3;
				}
			} else {
				NEW(content_line, c);
				content_line_copy(c, cline);
				add_content_line (ev, c);
			}
			break;
	}
	return 0;
}
