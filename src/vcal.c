#include "vcal.h"

#include <string.h>
#include <libguile.h>

#include "guile_interface.h"
#include "err.h"

// #define TYPE strbuf
// #include "linked_list.inc.h"
// #undef TYPE

#if 0
#define TYPE param_set
#include "trie.inc.h"
#undef TYPE

#define TYPE content_set
#include "linked_list.inc.h"
#undef TYPE

#define T strbuf
	#define V TRIE(param_set)
		#include "pair.inc.h"
	#undef V
#undef T

#define TYPE content_line
// #include "hash.inc"
#include "trie.inc.h"
#undef TYPE

#define TYPE vcomponent
// #include "vector.inc.h"
#include "linked_list.inc.h"
#undef TYPE
#endif

INIT_F(vcomponent) {
	// INIT(TRIE(content_line), &self->clines);
	// INIT(LLIST(vcomponent), &self->components);

	self->clines = scm_make_hash_table(scm_from_ulong(32));
	scm_gc_protect_object (self->clines);
	self->components = SCM_EOL;

	// vcomponent_push_val (self, "X-HNH-FILENAME", "VIRTUAL");
	SNEW(strbuf, s);
	strbuf_load(&s, "X-HNH-SOURCETYPE");
	vcomponent_push_val (self, &s, scm_cons(scm_from_utf8_symbol("virtual"), SCM_BOOL_F));
	FREE(strbuf)(&s);
	char* type = "VIRTUAL";
	self->type = (char*) calloc(sizeof(*type), strlen(type) + 1);
	strcpy(self->type, type);

	self->parent = NULL;
	self->scm = NULL;
	self->scmtype = NULL;

	return 0;

}

INIT_F(vcomponent, const char* type) {
	return INIT(vcomponent, self, type, NULL);
}

INIT_F(vcomponent, const char* type, const char* filename) {

	// INIT(TRIE(content_line), &self->clines);
	// INIT(LLIST(vcomponent), &self->components);
	self->clines = scm_make_hash_table(scm_from_ulong(32));
	scm_gc_protect_object (self->clines);
	self->components = SCM_EOL;

	if (filename != NULL) {
		/*
		 * NOTE
		 * RFC-7986 adds additional parameters linked to this one.
		 * - `SOURCE' :: where a (possibly) updated version of the data can be
		 *   found 
		 * - `URL' :: Where the same data can be fonud, but
		 *   differently (but not where the original data can be fonud
		 *   agani).
		 */
		SNEW(strbuf, fname);
		strbuf_load (&fname,  "X-HNH-FILENAME");
		vcomponent_push_val (self, &fname, scm_cons(scm_from_utf8_stringn(filename, strlen(filename)), SCM_BOOL_F));
	}

	self->type = (char*) calloc(sizeof(*type), strlen(type) + 1);
	strcpy(self->type, type);

	self->parent = NULL;
	self->scm = NULL;
	self->scmtype = NULL;

	return 0;
}

#if 0
content_line* get_attributes (vcomponent* ev, const char* key) {
	size_t len = strlen(key) + 1;
	char* cpy = (char*) (calloc(sizeof(*cpy), len));
	strncpy (cpy, key, len);

	content_line* ret = GET(TRIE(content_line))(&ev->clines, cpy);

	free (cpy);
	return ret;
}
#endif

FREE_F(vcomponent) {
	free(self->type);

	/*
	if (FREE(TRIE(content_line))(&self->clines) != 0) {
		ERR("Error freeing vcomponent");
	}
	*/

	// FREE(LLIST(vcomponent))(&self->components);

	return 0;
}

int PUSH(vcomponent)(vcomponent* parent, vcomponent* child) {
	child->parent = parent;
	SCM_PUSH_X (parent->components, scm_from_vcomponent(child));
	scm_gc_protect_object (parent->components);
	return 0;
	// return PUSH(LLIST(vcomponent))(&parent->components, child);
}

int DEEP_COPY(vcomponent)(vcomponent* a, vcomponent* b) {
	(void) a;
	(void) b;
	ERR("Deep copy not implemented for vcomponent");
	return -1;
}

// TODO
int vcomponent_copy(vcomponent* dest, vcomponent* src) {
	ERR("Deep copy not implemented for vcomponent");
	(void) dest;
	(void) src;

#if 0
	DEEP_COPY(TRIE(content_line))(&dest->clines, &src->clines);

	/* Children are the same objects */
	FOR(LLIST, vcomponent, c, &src->components) {
		PUSH(LLIST(vcomponent))(&dest->components, c);
	}

	PUSH(vcomponent)(src->parent, dest);
#endif

	return 0;
}

FMT_F(vcomponent) {
	int seek = 0;
#if 0

	for (int i = 0; i < 40; i++) fmtf("_");

	seek += sprintf(buf + seek, _YELLOW);
	seek += sprintf(buf + seek, "\nVComponet (Type := %s)\n", self->type);
	seek += sprintf(buf + seek, _RESET);
	seek += FMT(TRIE(content_line))(&self->clines, buf + seek);
	seek += sprintf(buf + seek, "\nComponents:\n");
	FOR(LLIST, vcomponent, comp, &self->components) {
		seek += FMT(vcomponent)(comp, buf + seek);
	}
#endif
	seek += sprintf(buf + seek, "#<vcomponent %p>", self);

	return seek;
}

int vcomponent_push_val (vcomponent* comp, strbuf* key, SCM val) {
	/*
	NEW(content_line, cl);
	NEW(content_set, cs);
	strbuf_load(&cs->key, val);
	PUSH(content_line)(cl, cs);

	char* key_cpy = calloc(sizeof(*key_cpy), strlen(key) + 1);
	strcpy (key_cpy, key);
	PUSH(TRIE(content_line))(&comp->clines, key_cpy, cl);
	free (key_cpy);
	*/

	SCM k = scm_string_to_symbol (scm_from_utf8_stringn (key->mem, key->len));
	// TODO this should cons
	scm_hashq_set_x (comp->clines, k, scm_list_1 (val));

	return 0;
}

#if 0
char* vcomponent_get_val (vcomponent* comp, const char* key) {
	/*
	char* key_cpy = calloc(sizeof(*key_cpy), strlen(key) + 1);
	strcpy (key_cpy, key);
	content_line* cl = GET(TRIE(content_line))(&comp->clines, key_cpy);
	free (key_cpy);

	if (cl != NULL && cl->cval != NULL) {
		return cl->cval->key.mem;
	}
	*/

	return scm_i_string_chars (scm_hashq_ref (comp->clines, scm_from_utf8_symbol (key), NULL));

	// return NULL;
}
#endif
