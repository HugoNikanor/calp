#include "parse.h"

#include <errno.h>
#include <string.h>

#include "macro.h"

int parse_file(FILE* f, vcalendar* cal) {
	int segments = 1;
	SNEW(strbuf, str, segments * SEGSIZE);

	part_context p_ctx = p_key;
	scope_context s_ctx = s_none;

	int keylen = 100;
	int vallen = 100;

	int line = 0;

	NEW(vevent, ev);

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
					strbuf_realloc(&cline.val, vallen);
				}

				strbuf_copy(&cline.val, &str);
				strbuf_cap(&cline.val);

				++line;

				/* We just got a value */
				// LINE(line, key.mem, val.mem);
				/*
				if (strbuf_c(&cline.key, "LOCATION")) {
					if (strbuf_c(&cline.val, "")) return 1;
					LINE(line, cline.key.mem, cline.val.mem);
				}
				*/

				handle_kv(cal, ev, &cline, line, &s_ctx);
				strbuf_soft_reset(&str);
				p_ctx = p_key;

				continue;
			}
		} else if (p_ctx == p_key && c == ':') {
			/*
			   if (str.ptr + 1 > keylen) {
			   keylen = str.ptr + 1;
			// TODO this might break everything
			strbuf_realloc(&key, keylen);
			}
			*/
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
		 * Close last pair if the file is lacking trailing whitespace.
		 * A file with trailing whitespace would however fail.
		 * TODO check the spec and adjust accordingly
		 */
		if (str.ptr + 1 > vallen) {
			vallen = str.ptr + 1;
			strbuf_realloc(&cline.val, vallen);
		}
		strbuf_copy(&cline.val, &str);
		*strbuf_cur(&cline.val) = 0;
	}
	FREE(strbuf)(&str);
	FREE(content_line)(&cline);

	return 0;
}

int handle_kv(
		vcalendar*     cal,
		vevent*        ev,
		content_line*  cline,
		int            line,
		scope_context* s_ctx
		) {
	switch (*s_ctx) {
		case s_none:
			if (! (strbuf_c(&cline->key, "BEGIN") && strbuf_c(&cline->val, "VCALENDAR"))) {
				ERR("Invalid start of calendar", line);
				return 1;
			}
			*s_ctx = s_calendar;
			break;
		case s_calendar:
			if (strbuf_c(&cline->key, "BEGIN")) {
				if (strbuf_c(&cline->val, "VEVENT")) {
					*s_ctx = s_event;
					break;
				} else {
					ERR("Unsupported start", line);
					return 2;
				}
			} else if (strbuf_c(&cline->key, "END")) {
				if (strbuf_c(&cline->val, "VCALENDAR")) {
					*s_ctx = s_none;
					break;
				}
			}
			break;
		case s_event:
			if (strbuf_c(&cline->key, "END")) {
				if (strbuf_c(&cline->val, "VEVENT")) {
					push_event(cal, ev);
					*s_ctx = s_calendar;
				} else {
					ERR("Trying to end something, expected VEVENT", line);
					return 3;
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
