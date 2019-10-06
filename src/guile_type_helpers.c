#include "guile_type_helpers.h"

#include "macro.h"

SCM scm_from_strbuf(strbuf* s) {
	SCM ret = scm_from_utf8_stringn (s->mem, s->len);
	scm_gc_protect_object(ret);
	return ret;
}

SCM scm_from_strbuf_symbol(strbuf* s) {
	return scm_string_to_symbol(scm_from_strbuf(s));
}
