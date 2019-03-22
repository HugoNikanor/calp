#include "guile_type_helpers.h"
#include "guile_interface.h"

#include "macro.h"

SCM scm_from_strbuf(strbuf* s) {
	if (s->scm == NULL) {
		s->scm = scm_from_utf8_stringn (s->mem, s->len);
		scm_gc_protect_object(s->scm);
	}

	return s->scm;
}
