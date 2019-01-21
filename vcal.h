#ifndef VCAL_H
#define VCAL_H

#include <stdlib.h>

#include "strbuf.h"

typedef struct {
	string key;
	string value;
	string* vals;
	int val_count;
} parameter;

typedef struct {
	string key;
	string val;

	string* aux_values;
	int value_count;

	parameter* params;
	int param_count;
} content_line;

#define TYPE content_line
#include "hash.h"
#undef TYPE

struct s_vevent {
	/*
	string dtstart;
	string dtend;
	string summary;
	string description;
	*/
	TABLE(content_line) clines;
};

struct s_vevent;
typedef struct s_vevent vevent;

int CONSTRUCTOR_DECL(vevent, int init_size);

int CONSTRUCTOR_DECL(content_line);
int CONSTRUCTOR_DECL(content_line, int keylen, int vallen);

int content_line_free (content_line* c);
int content_line_copy (content_line* dest, content_line* src);

content_line* get_property (vevent* ev, char* key);

int add_content_line (vevent* ev, content_line* c);

/*
 * Deep copy from src -> dest
 * Requires dest to be initialized beforehand
 * TODO possibly remove this.
 */
int copy_vevent(vevent* dest, vevent* src);

/*
 * Copies src -> dest, initializing all the strings along the way.
 * Requires dest to be initialized.
 */
int vevent_init_copy(vevent* dest, vevent* src);
int free_vevent(vevent* ev);

typedef struct {
	size_t n_events;
	size_t alloc;
	vevent* events;
} vcalendar;

int CONSTRUCTOR_DECL(vcalendar);
int free_vcalendar (vcalendar* cal);

int push_event(vcalendar* cal, vevent* ev);

#endif /* VCAL_H */
