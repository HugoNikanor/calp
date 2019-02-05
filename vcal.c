#include "vcal.h"

#include <string.h>

#define TYPE content_line
// #include "hash.inc"
#include "trie.inc.h"
#undef TYPE

#define TYPE strbuf
#include "linked_list.inc.h"
#undef TYPE

INIT_F(vevent, char* filename) {

	INIT(TRIE(content_line), &this->clines);

	this->filename = calloc(sizeof(*filename), strlen(filename) + 1);
	strcpy(this->filename, filename);

	this->calendar = NULL;

	return 0;
}

/*
 * Resolves a collision in some form of structure (probably a hash-map
 * or a trie). If dest is NULL just return new. Otherwise mutates dest
 * to have the correct form, and returns it. Destroying new in the
 * process.
 */
content_line* RESOLVE(content_line)
	(content_line* dest, content_line* new) {

	if (dest == NULL) return new;

	if (strbuf_cmp(&dest->key, &new->key) != 0) {
		ERR("Can't resolve between these two types");
		return NULL;
	}

	APPEND(LLIST(strbuf)) (&dest->vals, &new->vals);

	FREE(strbuf)(&new->key);
	gc_free(new);
	free(new);

	return dest;
}

content_line* get_property (vevent* ev, char* key) {
	return GET(TRIE(content_line))(&ev->clines, key);
}

INIT_F(content_line) {
	gc_register(this);

	INIT(strbuf, &this->key);
	// INIT(strbuf, &this->val);
	INIT( LLIST(strbuf), &this->vals );
	// TODO remaining fields
	return 0;
}

INIT_F(content_line, int keylen, int vallen) {
	gc_register(this);
	INIT(strbuf, &this->key, keylen);
	// INIT(strbuf, &this->val, vallen);
	INIT( LLIST(strbuf), &this->vals );
	NEW(strbuf, s, vallen);
	PUSH(LLIST(strbuf))(&this->vals, s);
	// TODO remaining fields
	return 0;
}


int content_line_copy (content_line* dest, content_line* src) {
	//strbuf_init_copy(&dest->key, &src->key);
	DEEP_COPY(strbuf)(&dest->key, &src->key);
	// strbuf_init_copy(&dest->val, &src->val);
	DEEP_COPY(LLIST(strbuf))(&dest->vals, &src->vals);
	// TODO remaining fields
	return 0;
}

FREE_F(content_line) {
	FREE(strbuf)(&this->key);
	// FREE(strbuf)(&this->val);
	// LLIST_FREE(strbuf)(&this->vals);
	FREE(LLIST(strbuf))(&this->vals);

	gc_free(this);

	// TODO remaining fields
	return 0;
}

FREE_F(vevent) {
	if (this->filename != NULL) free(this->filename);
	if (FREE(TRIE(content_line))(&this->clines) != 0) {
		fprintf(stderr, "Error freeing vevent belonging to file \n %s \n",
				this->filename);
	}

	return 0;
}

/*
 * TODO change this into PUSH(VCALENDAR) (vevent*) ?
 */
int push_event(vcalendar* cal, vevent* ev) {

	ev->calendar = cal;

	/* Make sure that cal->eents is large enough */
	if (cal->n_events + 1 > cal->alloc) {
		cal->alloc <<= 1;
		cal->events = realloc(cal->events, sizeof(*cal->events) * cal->alloc);
	}

	// vevent_init_copy(&cal->events[cal->n_events], ev);
	cal->events[cal->n_events] = ev;

	cal->n_events++;
	return 0;
}

INIT_F(vcalendar) {
	// TODO remove
	gc_vect = calloc(sizeof(*gc_vect), 10000);
	gc_ptr = 0;

	this->alloc = 1;
	this->events = calloc(sizeof(*this->events), this->alloc);
	this->n_events = 0;
	return 0;
}

int free_vcalendar (vcalendar* cal) {
	for (size_t i = 0; i < cal->n_events; i++) {
		FFREE(vevent, cal->events[i]);
	}
	free (cal->events);
	return 0;
}

void** gc_vect;
int gc_ptr;

int gc_register(void* ptr) {
	gc_vect[gc_ptr++] = ptr;
	return 0;
}

int gc_free(void* ptr) {
	for (int i = 0; i < gc_ptr; i++) {
		if (gc_vect[i] == ptr) {
			gc_vect[i] = NULL;
		}
	}

	return 0;
}
