#include "guile_type_helpers.h"

#include "macro.h"

SCM scm_from_strbuf(strbuf* s)
	{ return scm_from_utf8_stringn (s->mem, s->len - 1); }

SCM scm_from_vector(VECT(vcomponent)* vect, SCM element_type) {
	SCM l = SCM_EOL;
	for (size_t i = 0; i < vect->length; i++) {
		l = scm_cons(
				scm_make_foreign_object_1 (element_type, GET(VECT(vcomponent))(vect, i)),
				l);
	}
	return scm_reverse(l);
}


