#ifndef GUILE_TYPE_HELPERS_H
#define GUILE_TYPE_HELPERS_H

#include <libguile.h>

#include "calendar.h"
#include "strbuf.h"

#define SCM_IS_LIST(x) scm_is_true(scm_list_p(x))
#define SCM_MAKE_HASH_TABLE() scm_c_eval_string("(make-hash-table)")

SCM scm_from_strbuf(strbuf* s);
SCM scm_from_strbuf_symbol(strbuf* s);

#endif /* GUILE_TYPE_HELPERS_H */
