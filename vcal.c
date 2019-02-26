#include "vcal.h"

#include <string.h>

#define TYPE strbuf
#include "linked_list.inc.h"
#undef TYPE

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
#include "vector.inc.h"
#undef TYPE

INIT_F(vcomponent) {
	(void) self;
	ERR("Do not use");
	return 0;
}

INIT_F(vcomponent, const char* type) {
	return INIT(vcomponent, self, type, NULL);
}

INIT_F(vcomponent, const char* type, const char* filename) {

	INIT(TRIE(content_line), &self->clines);
	INIT(VECT(vcomponent), &self->components);

	if (filename != NULL) {
		vcomponent_push_val (self, "X-HH-FILENAME", filename);
	}

	self->type = (char*) calloc(sizeof(*type), strlen(type) + 1);
	strcpy(self->type, type);

	self->parent = NULL;
	self->scm = NULL;

	return 0;
}

content_line* get_property (vcomponent* ev, const char* key) {
	size_t len = strlen(key) + 1;
	char* cpy = (char*) (calloc(sizeof(*cpy), len));
	strncpy (cpy, key, len);

	content_line* ret = GET(TRIE(content_line))(&ev->clines, cpy);

	free (cpy);
	return ret;
}

FREE_F(vcomponent) {
	free(self->type);

	if (FREE(TRIE(content_line))(&self->clines) != 0) {
		ERR("Error freeing vcomponent");
	}

	FREE(VECT(vcomponent))(&self->components);

	return 0;
}

int PUSH(vcomponent)(vcomponent* parent, vcomponent* child) {
	child->parent = parent;
	return PUSH(VECT(vcomponent))(&parent->components, child);
}

int DEEP_COPY(vcomponent)(vcomponent* a, vcomponent* b) {
	(void) a;
	(void) b;
	ERR("Deep copy not implemented for vcomponent");
	return -1;
}

FMT_F(vcomponent) {
	int seek = 0;

	for (int i = 0; i < 40; i++) fmtf("_");

	seek += sprintf(buf + seek, _YELLOW);
	seek += sprintf(buf + seek, "\nVComponet (Type := %s)\n", self->type); 
	seek += sprintf(buf + seek, _RESET);
	seek += FMT(TRIE(content_line))(&self->clines, buf + seek);
	seek += sprintf(buf + seek, "\nComponents:\n");
	FOR(VECT, vcomponent, comp, &self->components) {
		seek += FMT(vcomponent)(comp, buf + seek);
	}

	return seek;
}

int vcomponent_push_val (vcomponent* comp, const char* key, const char* val) {
	NEW(content_line, cl);
	NEW(content_set, cs);
	strbuf_load(&cs->key, val);
	PUSH(content_line)(cl, cs);

	char* key_cpy = calloc(sizeof(*key_cpy), strlen(key) + 1);
	strcpy (key_cpy, key);
	PUSH(TRIE(content_line))(&comp->clines, key_cpy, cl);
	free (key_cpy);

	return 0;
}

char* vcomponent_get_val (vcomponent* comp, const char* key) {
	char* key_cpy = calloc(sizeof(*key_cpy), strlen(key) + 1);
	strcpy (key_cpy, key);
	content_line* cl = GET(TRIE(content_line))(&comp->clines, key_cpy);
	free (key_cpy);

	if (cl != NULL && cl->cur->value != NULL) {
		return cl->cur->value->key.mem;
	}

	return NULL;
}
