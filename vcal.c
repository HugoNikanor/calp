#include "vcal.h"

#include <string.h>

#define TYPE content_line
// #include "hash_help.inc"
#include "trie.inc"
#undef TYPE

int CONSTRUCTOR_DECL(vevent, int init_size) {
	// HASH_INIT(content_line)(&this->clines, init_size);
	CONSTRUCT(TRIE(content_line), &this->clines);
	return 0;
}

content_line* get_property (vevent* ev, char* key) {
	// return HASH_GET(content_line)(&ev->clines, key);
	return TRIE_GET(content_line)(&ev->clines, key);
}

int add_content_line (vevent* ev, content_line* c) {
	// return HASH_PUT(content_line)(&ev->clines, c);
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

/* TODO reimplement this */
int copy_vevent(vevent* dest, vevent* src) {
	// strbuf_copy(&dest->dtstart     , &src->dtstart);
	// strbuf_copy(&dest->dtend       , &src->dtend);
	// strbuf_copy(&dest->summary     , &src->summary);
	// strbuf_copy(&dest->description , &src->description);
	return 0;
}

/* TODO reimplement this */
int vevent_init_copy(vevent* dest, vevent* src) {
	// strbuf_init_copy(&dest->dtstart     , &src->dtstart);
	// strbuf_init_copy(&dest->dtend       , &src->dtend);
	// strbuf_init_copy(&dest->summary     , &src->summary);
	// strbuf_init_copy(&dest->description , &src->description);
	return 0;
}

int free_vevent (vevent* ev) {
	// strbuf_free(&ev->dtstart);
	// strbuf_free(&ev->dtend);
	// strbuf_free(&ev->summary);
	// strbuf_free(&ev->description);
	// HASH_FREE(content_line)(&ev->clines);
	TRIE_FREE(content_line)(&ev->clines);
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
		free_vevent(cal->events[i]);
	}
	free (cal->events);
	return 0;
}
