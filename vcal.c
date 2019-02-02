#include "vcal.h"

#include <string.h>

#define TYPE content_line
// #include "hash.inc"
#include "trie.inc.h"
#undef TYPE

int CONSTRUCTOR_DECL(vevent) {
	CONSTRUCT(TRIE(content_line), &this->clines);
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
	CONSTRUCT(strbuf, &this->key);
	CONSTRUCT(strbuf, &this->val);
	// TODO remaining fields
	return 0;
}

int CONSTRUCTOR_DECL(content_line, int keylen, int vallen) {
	CONSTRUCT(strbuf, &this->key, keylen);
	CONSTRUCT(strbuf, &this->val, vallen);
	// TODO remaining fields
	return 0;
}


int content_line_copy (content_line* dest, content_line* src) {
	strbuf_init_copy(&dest->key, &src->key);
	strbuf_init_copy(&dest->val, &src->val);
	// TODO remaining fields
	return 0;
}

int FREE_DECL(content_line) {
	FREE(strbuf)(&this->key);
	FREE(strbuf)(&this->val);
	// TODO remaining fields
	return 0;
}

int FREE_DECL(vevent) {
	TRIE_FREE(content_line)(&this->clines);
	return 0;
}

int push_event(vcalendar* cal, vevent* ev) {

	/* Make sure that cal->events is large enough */
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
