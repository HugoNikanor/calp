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
	return INIT(vcomponent, this, NULL);
}

INIT_F(vcomponent, char* filename) {

	INIT(TRIE(content_line), &this->clines);
	INIT(VECT(vcomponent), &this->components);

	if (filename != NULL) {
		this->filename = calloc(sizeof(*filename), strlen(filename) + 1);
		strcpy(this->filename, filename);
	}

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

	APPEND(LLIST(strbuf)) (&dest->vals, &new->vals);

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

	// TODO remaining fields

	return 0;
}

INIT_F(content_line, int keylen, int vallen) {
	INIT(strbuf, &this->key, keylen);
	INIT( LLIST(strbuf), &this->vals );
	NEW(strbuf, s, vallen);
	PUSH(LLIST(strbuf))(&this->vals, s);

	// TODO remaining fields

	return 0;
}

FREE_F(content_line) {
	FREE(strbuf)(&this->key);
	FREE(LLIST(strbuf))(&this->vals);

	// TODO remaining fields

	return 0;
}

int content_line_copy (content_line* dest, content_line* src) {
	DEEP_COPY(strbuf)(&dest->key, &src->key);
	DEEP_COPY(LLIST(strbuf))(&dest->vals, &src->vals);

	// TODO remaining fields

	return 0;
}

FREE_F(vcomponent) {
	if (this->filename != NULL) free(this->filename);

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


