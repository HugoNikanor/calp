#include "vcal.h"

#include <string.h>

#define TYPE content_line
// #include "hash.inc"
#include "trie.inc.h"
#undef TYPE

#define TYPE strbuf
#include "linked_list.inc.h"
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
	(content_line* dest, content_line* new) {

	if (dest == NULL) return new;

	if (strbuf_cmp(&dest->key, &new->key) != 0) {
		ERR("Can't resolve between these two types");
		return NULL;
	}

	/*
	 * This destroys new.
	 */
	APPEND(LLIST(strbuf)) (&dest->vals, &new->vals);

	INFO_F("param length = %i", SIZE(LLIST(strbuf))(&new->params));
	FOR(LLIST(strbuf), link, &new->params) {
		INFO_F("| %s", link->value->mem);
	}

	APPEND(LLIST(strbuf)) (&dest->params, &new->params);

	FREE(strbuf)(&new->key);
	free(new);

	return dest;
}

content_line* get_property (vcomponent* ev, char* key) {
	return GET(TRIE(content_line))(&ev->clines, key);
}

INIT_F(content_line) {
	INIT(strbuf, &this->key);
	INIT( LLIST(strbuf), &this->vals );

	INIT( LLIST(strbuf), &this->params );

	return 0;
}

INIT_F(content_line, int keylen, int vallen) {
	INIT(strbuf, &this->key, keylen);
	INIT( LLIST(strbuf), &this->vals );
	NEW(strbuf, s, vallen);
	PUSH(LLIST(strbuf))(&this->vals, s);

	INIT( LLIST(strbuf), &this->params );

	return 0;
}

FREE_F(content_line) {
	FREE(strbuf)(&this->key);
	FREE(LLIST(strbuf))(&this->vals);

	FREE(LLIST(strbuf))(&this->params);

	return 0;
}

int content_line_copy (content_line* dest, content_line* src) {
	if (! EMPTY(LLIST(strbuf))(&src->params)) {
		INFO_F("[%s] : %s", src->key.mem, FIRST_V(&src->params)->mem);
	}

	DEEP_COPY(strbuf)(&dest->key, &src->key);
	DEEP_COPY(LLIST(strbuf))(&dest->vals, &src->vals);
	DEEP_COPY(LLIST(strbuf))(&dest->params, &src->params);

	return 0;
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
