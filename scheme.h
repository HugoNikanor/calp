#ifndef SCHEME_H
#define SCHEME_H

#include <libguile.h>

SCM make_calendar(SCM path);

SCM calendar_get_attr(SCM calendar, SCM id, SCM attr);

SCM number_events(SCM calendar);

void init_calendar ();

#endif /* SCHEME_H */
