#include "vcal.h"

#include <string.h>

int copy_vevent(vevent* dest, vevent* src) {
	copy_strbuf(&dest->dtstart     , &src->dtstart);
	copy_strbuf(&dest->dtend       , &src->dtend);
	copy_strbuf(&dest->summary     , &src->summary);
	copy_strbuf(&dest->description , &src->description);
	return 0;
}

int vevent_init_copy(vevent* dest, vevent* src) {
	strbuf_init_copy(&dest->dtstart     , &src->dtstart);
	strbuf_init_copy(&dest->dtend       , &src->dtend);
	strbuf_init_copy(&dest->summary     , &src->summary);
	strbuf_init_copy(&dest->description , &src->description);
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

	/* Make sure that cal->events is large enough */
	if (cal->n_events + 1 > cal->alloc) {
		cal->alloc <<= 1;
		cal->events = realloc(cal->events, sizeof(*cal->events) * cal->alloc);
	}

	vevent_init_copy(&cal->events[cal->n_events], ev);

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
		free_vevent(& cal->events[i]);
	}
	free (cal->events);
	return 0;
}
