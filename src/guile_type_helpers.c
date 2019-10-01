#include "guile_type_helpers.h"
#include "guile_interface.h"

#include "macro.h"

SCM scm_from_strbuf(strbuf* s) {
	// if (s->scm == NULL) {
        SCM ret = scm_from_utf8_stringn (s->mem, s->len);
        scm_gc_protect_object(ret);
        // }

        // return s->scm;
        return ret;
}

SCM scm_from_strbuf_symbol(strbuf* s) {
	return scm_string_to_symbol(scm_from_strbuf(s));
}
