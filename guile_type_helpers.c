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

SCM scm_from_vector(VECT(vcomponent)* vect) {
	SCM l = SCM_EOL;
	for (size_t i = 0; i < vect->length; i++) {
		vcomponent* v = GET(VECT(vcomponent))(vect, i);
		l = scm_cons(scm_from_vcomponent(v), l);
	}
	return scm_reverse(l);
}

