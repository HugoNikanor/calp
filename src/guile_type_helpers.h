#ifndef GUILE_TYPE_HELPERS_H
#define GUILE_TYPE_HELPERS_H

#include <libguile.h>

#include "strbuf.h"

#define SCM_IS_LIST(x) scm_is_true(scm_list_p(x))
#define string_eq(a, b) \
	scm_is_true(scm_string_eq(a, b, \
				SCM_UNDEFINED,SCM_UNDEFINED,SCM_UNDEFINED,SCM_UNDEFINED))

SCM scm_from_strbuf(strbuf* s);
SCM scm_from_strbuf_symbol(strbuf* s);

#endif /* GUILE_TYPE_HELPERS_H */
