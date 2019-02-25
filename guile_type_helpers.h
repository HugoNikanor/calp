#ifndef GUILE_TYPE_HELPERS_H
#define GUILE_TYPE_HELPERS_H

#include <libguile.h>

#include "calendar.h"
#include "strbuf.h"

#define SCM_IS_LIST(x) scm_is_true(scm_list_p(x))

SCM scm_from_strbuf(strbuf* s);

SCM scm_from_vector(VECT(vcomponent)* vect, SCM element_type);

#endif /* GUILE_TYPE_HELPERS_H */
