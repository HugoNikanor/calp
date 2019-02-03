#include "scheme.h"

#include "macro.h"
#include "calendar.h"
#include "strbuf.h"

static SCM calendar_type;

void init_calendar_type (void) {
	SCM name = scm_from_utf8_symbol("calendar");
	SCM slots = scm_list_1(scm_from_utf8_symbol("data"));

	calendar_type = scm_make_foreign_object_type(name, slots, NULL);
}

SCM_DEFINE (make_calendar, "make-calendar", 1, 0, 0,
		(SCM path),
		"Loads a vdir iCalendar from the given path.")
{
	vcalendar* cal =
		(vcalendar*) scm_gc_malloc (
				sizeof(*cal), "calendar");
	CONSTRUCT(vcalendar, cal);

	char* p = scm_to_utf8_stringn(path, NULL);
	read_vcalendar(cal, p);
	free(p);

	return scm_make_foreign_object_1
		(calendar_type, cal);

}

static SCM scm_from_strbuf(strbuf* s) {
	return scm_from_utf8_stringn (s->mem, s->len - 1);
}

SCM_DEFINE (calendar_get_attr, "calendar-get-attr", 3, 0, 0,
		(SCM calendar, SCM id, SCM attr),
		"Retuns the given attribute from the vevent object at index in calendar.")
{
	scm_assert_foreign_object_type (calendar_type, calendar);
	vcalendar* cal = scm_foreign_object_ref (calendar, 0);

	vevent* v = cal->events[scm_to_int(id)];
	char* key = scm_to_utf8_stringn(scm_string_upcase(attr), NULL);
	content_line* c = get_property (v, key);
	free(key);

	if (c == NULL) return SCM_BOOL_F;

	return scm_from_strbuf(&c->val);
}

SCM_DEFINE (calendar_size, "calendar-size", 1, 0, 0,
		(SCM calendar),
		"Returns number of events in a vcalendar.")
{
	scm_assert_foreign_object_type (calendar_type, calendar);
	vcalendar* cal = scm_foreign_object_ref (calendar, 0);
	return scm_from_size_t (cal->n_events);
}

void init_calendar () {
	init_calendar_type();

#ifndef SCM_MAGIC_SNARFER
#include "scheme.x"
#endif

}