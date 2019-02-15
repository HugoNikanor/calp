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
	(void) this;
	ERR("Do not use");
	return 0;
}

INIT_F(vcomponent, char* type) {
	return INIT(vcomponent, this, type, NULL);
}

INIT_F(vcomponent, char* type, char* filename) {

	INIT(TRIE(content_line), &this->clines);
	INIT(VECT(vcomponent), &this->components);

	this->filename = NULL;
	if (filename != NULL) {
		this->filename = calloc(sizeof(*filename), strlen(filename) + 1);
		strcpy(this->filename, filename);
	}

	this->type = calloc(sizeof(*type), strlen(type) + 1);
	strcpy(this->type, type);

	this->parent = NULL;

	return 0;
}

content_line* RESOLVE(content_line)
	(content_line* dest, content_line* new)
{
	if (dest == NULL) return new;

	if (strbuf_cmp(&dest->key, &new->key) != 0) {
		ERR("Can't resolve between these two types");
		return NULL;
	}

	/* This destroys new->val. */
	APPEND(LLIST(content_set)) (&dest->val, &new->val);

	FREE(strbuf)(&new->key);
	free(new);

	return dest;
}

content_line* get_property (vcomponent* ev, char* key) {
	return GET(TRIE(content_line))(&ev->clines, key);
}

FREE_F(vcomponent) {
	if (this->filename != NULL) free(this->filename);
	free(this->type);

	if (FREE(TRIE(content_line))(&this->clines) != 0) {
		fprintf(stderr, "Error freeing vcomponent belonging to file \n %s \n",
				this->filename);
	}

	FREE(VECT(vcomponent))(&this->components);

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
	seek += sprintf(buf + seek, "\nVComponet (Type := %s)\n", this->type); 
	seek += sprintf(buf + seek, _RESET);
	seek += FMT(TRIE(content_line))(&this->clines, buf + seek);
	seek += sprintf(buf + seek, "\nComponents:\n");
	FOR(VECT(vcomponent), vcomponent, comp, &this->components) {
		seek += FMT(vcomponent)(comp, buf + seek);
	}

	return seek;
}
