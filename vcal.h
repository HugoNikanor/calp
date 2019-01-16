#ifndef VCAL_H
#define VCAL_H

#include <stdlib.h>

#include "strbuf.h"

typedef struct {
	string dtstart;
	string dtend;
	string summary;
	string description;
} vevent;

int copy_vevent(vevent* dest, vevent* src);
int free_vevent(vevent* ev);

typedef struct {
	size_t n_events;
	size_t alloc;
	vevent* events;
} vcalendar;

int init_vcalendar(vcalendar* cal);
int free_vcalendar(vcalendar* cal);

int push_event(vcalendar* cal, vevent* ev);

#endif /* VCAL_H */
