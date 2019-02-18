#include "strbuf.h"

#include <string.h>
#include <stdio.h>

#include "err.h"

INIT_F(strbuf) {
	INIT(strbuf, self, 1);
	return 0;
}

/*
 * Giving len < 1 is an error.
 */
INIT_F(strbuf, size_t len) {
	self->mem   = (char*) calloc(sizeof(*self->mem), len);
	self->alloc = len;
	self->ptr   = 0;
	self->len   = 0;
	return 0;
}

int strbuf_realloc(strbuf* str, size_t len) {
	str->mem = (char*) realloc(str->mem, len);
	str->alloc = len;
	return 0;
}

FREE_F(strbuf) {
	/* has already been freed */
	if (self->mem == NULL) return 1;

	free (self->mem);
	self->mem = NULL;
	self->alloc = 0;
	self->len = 0;
	return 0;
}

/*
 * Reallocates memmory for you. Returns 1 if memory was reallocated.
 */
int strbuf_append(strbuf* s, char c) {
	int retval = 0;

	if (s->len + 1 > s->alloc) {
		s->alloc <<= 1;
		s->mem = (char*) realloc(s->mem, s->alloc);
		retval = 1;
	}

	s->mem[s->len] = c;
	s->ptr = ++s->len;
	return retval;
}

int strbuf_cap(strbuf* s) {
	/*
	 * TODO check memmory usage
	 */
	return strbuf_append(s, 0);
}

int DEEP_COPY(strbuf)(strbuf* dest, strbuf* src) {
	int retval = 0;

	if (dest->alloc < src->len) {
		/* +1 in length is to have room for  '\0'. */
		strbuf_realloc(dest, src->len + 1);
		retval = 1;
	}

	dest->len = src->len;
	memcpy(dest->mem, src->mem, src->len);
	return retval;
}

int strbuf_cmp(strbuf* a, strbuf* b) {
	if (a == NULL || a->alloc == 0 ||
	    b == NULL || b->alloc == 0)
	{
		ERR("a or b not alloced");
		// return -1;
	}

	return strncmp(a->mem, b->mem, a->len);
}

int strbuf_c(strbuf* a, char* b) {
	if (a == NULL || a->alloc == 0) {
		ERR("a not allocated");
		return -1;
	}

	return strcmp(a->mem, b) == 0;
}

char* charat(strbuf* s, unsigned int idx) {
	if (idx > s->len) {
		ERR("Index out of bounds");
		return (char*) -1;
	}

	return &s->mem[idx];
}

char* strbuf_cur(strbuf* s) {
	return &s->mem[s->ptr];
}

char* strbuf_end(strbuf* s) {
	return &s->mem[s->len];
}

int strbuf_reset(strbuf* s) {
	s->ptr = 0;
	return 0;
}


int strbuf_soft_reset(strbuf* s) {
	s->ptr = s->len = 0;
	return 0;
}

strbuf* RESOLVE(strbuf)(strbuf* dest, strbuf* new_) {
	if (dest == NULL) return new_;
	else return dest;
}

FMT_F(strbuf) {
	return sprintf(buf, "%s", self->mem);
}

int SIZE(strbuf)(strbuf* self) {
	return self->len;
}
