#ifndef VCAL_H
#define VCAL_H

#include <stdlib.h>

#include "strbuf.h"

#define TYPE strbuf
#include "linked_list.h"
#undef TYPE

typedef struct {
	strbuf key;
	strbuf value;
	strbuf* vals;
	int val_count;
} parameter;

typedef struct {
	strbuf key;

	LLIST(strbuf) vals;

	parameter* params;
	int param_count;
} content_line;

INIT_F(content_line);
INIT_F(content_line, int keylen, int vallen);

/*
 * This takes two content lines, and return a content line that is the
 * merge of the two.
 * The function also cleans up the unused memmory between the two.
 */
content_line* RESOLVE(content_line)
	(content_line* dest, content_line* new);

#define TYPE content_line
// #include "hash.h"
#include "trie.h"
#undef TYPE

typedef struct s_vevent {
	char* filename;
	struct s_vcalendar* calendar;
	TRIE(content_line) clines;
} vevent;

INIT_F(vevent, char* filename);

FREE_F(content_line);
int content_line_copy (content_line* dest, content_line* src);

content_line* get_property (vevent* ev, char* key);

int add_content_line (vevent* ev, content_line* c);

int free_vevent(vevent* ev);

typedef struct s_vcalendar {
	size_t n_events;
	size_t alloc;
	vevent** events;
} vcalendar;

INIT_F(vcalendar);
int free_vcalendar (vcalendar* cal);

/*
 * Appends ev to cal. Doesn't copy ev. So make sure that it wont go
 * out of scope.
 */
int push_event(vcalendar* cal, vevent* ev);

/*
 * NOTE
 * these create a very basic garbage collector. It shouldn't be used,
 * but is useful for finding lost memmory during debugging.
 * TODO
 * remove it.
 */
extern content_line** clines;
extern int cline_ptr;

#endif /* VCAL_H */
