#ifndef VCAL_H
#define VCAL_H

#include <stdlib.h>

#include "strbuf.h"

typedef struct {
	strbuf key;
	strbuf value;
	strbuf* vals;
	int val_count;
} parameter;

typedef struct {
	strbuf key;
	strbuf val;

	strbuf* aux_values;
	int value_count;

	parameter* params;
	int param_count;
} content_line;

int CONSTRUCTOR_DECL(content_line);
int CONSTRUCTOR_DECL(content_line, int keylen, int vallen);

#define TYPE content_line
// #include "hash.h"
#include "trie.h"
#undef TYPE

typedef struct s_vevent {
	TRIE(content_line) clines;
} vevent;

int CONSTRUCTOR_DECL(vevent);

int FREE_DECL(content_line);
int content_line_copy (content_line* dest, content_line* src);

content_line* get_property (vevent* ev, char* key);

int add_content_line (vevent* ev, content_line* c);

int free_vevent(vevent* ev);

typedef struct {
	size_t n_events;
	size_t alloc;
	vevent** events;
} vcalendar;

int CONSTRUCTOR_DECL(vcalendar);
int free_vcalendar (vcalendar* cal);

/*
 * Appends ev to cal. Doesn't copy ev. So make sure that it wont go
 * out of scope.
 */
int push_event(vcalendar* cal, vevent* ev);

#endif /* VCAL_H */
