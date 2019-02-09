#ifndef SCHEME_H
#define SCHEME_H

#include <libguile.h>
#include "calendar.h"
#include "strbuf.h"

SCM make_calendar(SCM path);

SCM calendar_get_attr(SCM calendar, SCM id, SCM attr);

SCM number_events(SCM calendar);

void init_calendar ();

SCM scm_from_strbuf(strbuf* s);
SCM scm_from_llist(LLIST(strbuf)* lst);
SCM scm_from_trie_node(TRIE_NODE(content_line)* node);
SCM scm_from_trie(TRIE(content_line)* trie);
SCM scm_from_vector(VECT(vcomponent)* vect);
SCM calendar_size (SCM);
SCM calendar_components (SCM);

#endif /* SCHEME_H */
