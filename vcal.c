#include "vcal.h"

#include <string.h>

#define TYPE strbuf
#include "linked_list.inc.h"
#undef TYPE

#define TYPE param_set
#include "linked_list.inc.h"
#undef TYPE

#define TYPE content_set
#include "linked_list.inc.h"
#undef TYPE

#define T strbuf
	#define V LLIST(strbuf)
		#include "pair.inc.h"
	#undef V
	#define V LLIST(param_set)
		#include "pair.inc.h"
	#undef V
	#define V LLIST(content_set)
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

	self->filename = NULL;
	if (filename != NULL) {
		self->filename = (char*) calloc(sizeof(*filename), strlen(filename) + 1);
		strcpy(self->filename, filename);
	}

	self->type = (char*) calloc(sizeof(*type), strlen(type) + 1);
	strcpy(self->type, type);

	self->parent = NULL;

	return 0;
}

content_line* RESOLVE(content_line)
	(content_line* dest, content_line* new_)
{
	if (dest == NULL) return new_;

	if (strbuf_cmp(&dest->key, &new_->key) != 0) {
		ERR("Can't resolve between these two types");
		return NULL;
	}

	/* This destroys new_->val. */
	APPEND(LLIST(content_set)) (&dest->val, &new_->val);

	FREE(strbuf)(&new_->key);
	free(new_);

	return dest;
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
	if (self->filename != NULL) free(self->filename);
	free(self->type);

	if (FREE(TRIE(content_line))(&self->clines) != 0) {
		fprintf(stderr, "Error freeing vcomponent belonging to file \n %s \n",
				self->filename);
	}

	FREE(VECT(vcomponent))(&self->components);

	return 0;
}

int PUSH(vcomponent)(vcomponent* parent, vcomponent* child) {
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
