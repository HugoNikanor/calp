#include "guile_type_helpers.h"

#include "macro.h"

SCM scm_from_strbuf(strbuf* s) {
	if (s->scm == NULL) {
		s->scm = scm_from_utf8_stringn (s->mem, s->len - 1);
		scm_gc_protect_object(s->scm);
	}

	return s->scm;
}

SCM scm_from_vector(VECT(vcomponent)* vect, SCM element_type) {
	SCM l = SCM_EOL;
	for (size_t i = 0; i < vect->length; i++) {
		vcomponent* v = GET(VECT(vcomponent))(vect, i);
		if (v->scm == NULL) {
			v->scm = scm_make_foreign_object_1 (element_type, v);
			scm_gc_protect_object(v->scm);
		}
		l = scm_cons(v->scm, l);
	}
	return scm_reverse(l);
}

