#include "vcal.h"

#include <string.h>

#define TYPE content_line
// #include "hash.inc"
#include "trie.inc.h"
#undef TYPE

#define TYPE strbuf
#include "linked_list.inc.h"
#undef TYPE

content_line** clines;
int cline_ptr;

int CONSTRUCTOR_DECL(vevent, char* filename) {

	CONSTRUCT(TRIE(content_line), &this->clines);

	this->filename = calloc(sizeof(*filename), strlen(filename) + 1);
	strcpy(this->filename, filename);

	this->calendar = NULL;

	return 0;
}

int RESOLVE(content_line)
	(content_line** dest, content_line* new) {
	if (*dest == NULL) *dest = new;

	if (strbuf_cmp(&(*dest)->key, &new->key) != 0) {
		ERR("Can't resolve between these two types");
		return 1;
	}

	// printf("len = %i\n", dest->vals.length);
	APPEND(LLIST(strbuf))(&(*dest)->vals, &new->vals);
	return 0;
}

content_line* get_property (vevent* ev, char* key) {
	return TRIE_GET(content_line)(&ev->clines, key);
}

int add_content_line (vevent* ev, content_line* c) {
	// TODO memmory safety on strbuf?
	return TRIE_PUT(content_line)(&ev->clines, c->key.mem, c);
}

int CONSTRUCTOR_DECL(content_line) {
	clines[cline_ptr++] = this;
	CONSTRUCT(strbuf, &this->key);
	// CONSTRUCT(strbuf, &this->val);
	CONSTRUCT( LLIST(strbuf), &this->vals );
	// TODO remaining fields
	return 0;
}

int CONSTRUCTOR_DECL(content_line, int keylen, int vallen) {
	clines[cline_ptr++] = this;
	CONSTRUCT(strbuf, &this->key, keylen);
	// CONSTRUCT(strbuf, &this->val, vallen);
	CONSTRUCT( LLIST(strbuf), &this->vals );
	NEW(strbuf, s, vallen);
	LLIST_CONS(strbuf)(&this->vals, s);
	// TODO remaining fields
	return 0;
}


int content_line_copy (content_line* dest, content_line* src) {
	strbuf_init_copy(&dest->key, &src->key);
	// strbuf_init_copy(&dest->val, &src->val);
	DEEP_COPY(LLIST(strbuf))(&dest->vals, &src->vals);
	// TODO remaining fields
	return 0;
}

int FREE_DECL(content_line) {
	FREE(strbuf)(&this->key);
	// FREE(strbuf)(&this->val);
	// LLIST_FREE(strbuf)(&this->vals);
	FREE(LLIST(strbuf))(&this->vals);
	for (int i = 0; i < cline_ptr; i++) {
		if (clines[i] == this) {
			clines[i] = NULL;
		}
	}
	// TODO remaining fields
	return 0;
}

int FREE_DECL(vevent) {
	if (this->filename != NULL) free(this->filename);
	if (TRIE_FREE(content_line)(&this->clines) != 0) {
		fprintf(stderr, "Error freeing vevent belonging to file \n %s \n",
				this->filename);
	}

	return 0;
}

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

int CONSTRUCTOR_DECL(vcalendar) {
	clines = calloc(sizeof(*clines), 10000);
	cline_ptr = 0;
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
