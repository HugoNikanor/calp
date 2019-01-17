#ifndef VCAL_H
#define VCAL_H

#include <stdlib.h>

#include "strbuf.h"


/*
 * It's intentionall that there is no vevent_init. That since
 * the length of the strings isn't known.
 */
typedef struct {
	string dtstart;
	string dtend;
	string summary;
	string description;
} vevent;

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

int init_vcalendar(vcalendar* cal);
int free_vcalendar(vcalendar* cal);

int push_event(vcalendar* cal, vevent* ev);

#endif /* VCAL_H */
