#define SAFE_STR

/*
 * TODO currently not all pointers inside strings are reset correctly,
 * leading to old garbage data being read way to much. 
 *
 * A better ERR macro would solve most problems,
 * along with the introduction of a MSG macro.
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <dirent.h>

/*
 * These three are only for some FD hacks.
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "strbuf.h"
#include "vcal.h"

/*
 * Max length of a line.
 * TODO update this to allow longer lines, in case someone doesn't
 * follow the standard.
 */
#define SEGSIZE 75

#define ERR(x, line) do { \
	fprintf(stderr, "ERR %i: %s (cal %i)\n", __LINE__, (x), (line)); \
} while(0)

#define LINE(nr, key, value) fprintf(stderr, "%i: [%s] := [%s]\n", nr, key, value);

typedef enum {
	p_key, p_value
} part_context;

typedef enum {
	s_none, s_calendar, s_event
} scope_context;

int handle_kv(vcalendar* cal, vevent* ev, string* key, string* val, int line, scope_context* s_ctx);

int parse_file(FILE* f, vcalendar* cal) {
	int segments = 1;
	string str;
	init_string (&str, segments * SEGSIZE);

	part_context p_ctx = p_key;
	scope_context s_ctx = s_none;

	int keylen = 100;
	int vallen = 100;

	string key, val;

	init_string(&key, keylen);
	init_string(&val, vallen);

	int line = 0;

	vevent ev;

	char c;
	// TODO this segfaults...
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
				// TODO check return value
				// TODO segments is always incremented here, meaning
				// that segment grows larger for every multi line
				// encountered.
#if 1
				if (realloc_string(&str, ++segments * SEGSIZE) != 0) { /* TODO signal error */
					ERR("Failed to realloc string", line);
					exit (1);
				}
#endif
				continue;
			} else {
				if (ungetc(s[1], f) != s[1]) { /* TODO signal error */
					exit (2);
				}

				/* At TRUE end of line */
				if (str.ptr + 1 > vallen) {
					vallen = str.ptr + 1;
					realloc_string(&val, vallen);
				}
				copy_strbuf(&val, &str);
				strbuf_cap(&val);

				++line;

				/* We just got a value */
				// LINE(line, key.mem, val.mem);
				handle_kv(cal, &ev, &key, &val, line, &s_ctx);
				strbuf_soft_reset(&str);
				p_ctx = p_key;

				continue;
			}
		} else if (p_ctx == p_key && c == ':') {
			/*
			if (str.ptr + 1 > keylen) {
				keylen = str.ptr + 1;
				// TODO this might break everything
				realloc_string(&key, keylen);
			}
			*/
			copy_strbuf(&key, &str);
			*strbuf_end(&key) = 0;
			strbuf_soft_reset(&str);
			p_ctx = p_value;
			continue;
		}

		strbuf_append(&str, c);
	}
	if (errno != 0) {
		ERR("Error parsing", errno);
	} else {
		/*
		 * Close last pair if the file is lacking trailing whitespace.
		 * A file with trailing whitespace would however fail.
		 * TODO check the spec and adjust accordingly
		 */
		if (str.ptr + 1 > vallen) {
			vallen = str.ptr + 1;
			realloc_string(&val, vallen);
		}
		copy_strbuf(&val, &str);
		*strbuf_cur(&val) = 0;
	}
	free_vevent(&ev);
	free_string(&str);
	free_string(&key);
	free_string(&val);

	return 0;
}

int main (int argc, char* argv[argc]) {
	if (argc < 2) {
		//puts("Please give a ics file as first argument");
		puts("Please give vdir as first argument");
	   exit (1);	
	}
	vcalendar cal;
	init_vcalendar(&cal);

	char* dname = argv[1];
	DIR* dir = opendir(dname);
	struct dirent* d;
	int fcount = 0;
	while ((d = readdir(dir)) != NULL) {


		/* Check that it's a regular file */
		if (d->d_type != DT_REG) continue;

		/* Check that we have an ICS file */
		char *s, *fname;
	   	s = fname = d->d_name;
		while (*(s++) != '.');
		if (strcmp(s, "ics") != 0) continue;

		/* We now assume that it's a good file, and start parsing it */

		int fd = openat(dirfd(dir), fname, O_RDONLY);

		FILE* f = fdopen(fd, "r");
		if (f == NULL) {
			fprintf(stderr, "Error opening file [%s], errno = %i\n",
					fname, errno);
			exit (1);
		}

		printf("%3i | %s\n", fcount++, fname);
		/* TODO currently the hedaers cal is overwritten each
		 * iteration (not really, since I don't save any headers).
		 * Preferably, a special case is made for vdir structures
		 * which can assume that all headers are equal. */
		parse_file(f, &cal);
		fclose(f);

	}

	printf("\nParsed calendar file containing [%lu] events\n",
			cal.n_events);
	for (size_t i = 0; i < cal.n_events; i++) {
		printf("%3lu. %s\n", i + 1, cal.events[i].summary.mem);
	}

	free_vcalendar(&cal);
}

int handle_kv(vcalendar* cal, vevent* ev, string* key, string* val, int line, scope_context* s_ctx) {
	switch (*s_ctx) {
		case s_none:
			/* Both key and val is null here */
			if (! (strbuf_c(key, "BEGIN") && strbuf_c(val, "VCALENDAR"))) {
				ERR("Invalid start of calendar", line);
				return 1;
			}
			*s_ctx = s_calendar;
			break;
		case s_calendar:
			if (strbuf_c(key, "BEGIN")) {
				if (strbuf_c(val, "VEVENT")) {
					*s_ctx = s_event;
					break;
				} else {
					ERR("Unsupported start", line);
					return 2;
				}
			} else if (strbuf_c(key, "END")) {
				if (strbuf_c(val, "VCALENDAR")) {
					*s_ctx = s_none;
					/* GOTO cleanup */
					break;
				}
			}
			break;
		case s_event:
			/*  */ if (strbuf_c(key, "DTSTART")) {
				strbuf_init_copy(&ev->dtstart, val);
			} else if (strbuf_c(key, "DTEND")) {
				strbuf_init_copy(&ev->dtend, val);
			} else if (strbuf_c(key, "SUMMARY")) {
				strbuf_init_copy(&ev->summary, val);
			} else if (strbuf_c(key, "DESCRIPTION")) {
				strbuf_init_copy(&ev->description, val);
			} else if (strbuf_c(key, "END")) {
				if (strbuf_c(val, "VEVENT")) {
					push_event(cal, ev);
					*s_ctx = s_calendar;
				} else {
					ERR("Trying to end something, expected VEVENT", line);
					return 3;
				}
			}

			break;
	}

	return 0;
}
