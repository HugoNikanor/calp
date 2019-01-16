#include "vcal.h"

#include <string.h>

int copy_vevent(vevent* dest, vevent* src) {
	copy_strbuf(&dest->dtstart     , &src->dtstart);
	copy_strbuf(&dest->dtend       , &src->dtend);
	copy_strbuf(&dest->summary     , &src->summary);
	copy_strbuf(&dest->description , &src->description);
	return 0;
}

int free_vevent (vevent* ev) {
	free_string(&ev->dtstart);
	free_string(&ev->dtend);
	free_string(&ev->summary);
	free_string(&ev->description);
	return 0;
}

int push_event(vcalendar* cal, vevent* ev) {
	if (cal->n_events + 1> cal->alloc) {
		cal->alloc <<= 1;
	}
	cal->events = realloc(cal->events, cal->alloc);
	copy_vevent(&cal->events[cal->n_events], ev);
	cal->n_events++;
	return 0;
}

int init_vcalendar(vcalendar* cal) {
	cal->events = malloc(sizeof(*cal->events));
	cal->alloc = 1;
	cal->n_events = 0;
	return 0;
}

int free_vcalendar (vcalendar* cal) {
	for (size_t i = 0; i < cal->n_events; i++) {
		vevent* v = & cal->events[i];
		free_vevent(v);
		free(v);
	}
	return 0;
}
