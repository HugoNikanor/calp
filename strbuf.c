#include "strbuf.h"

#include <string.h>

#ifdef SAFE_STR
#include "err.h"
#endif

INIT_F(strbuf) {
	this->mem   = NULL;
	this->alloc = 0;
	this->len   = 0;
	this->ptr   = 0;
	return 0;
}

INIT_F(strbuf, size_t len) {
	this->mem   = calloc(sizeof(*this->mem), len);
	this->alloc = len;
	this->ptr   = 0;
	this->len   = 0;
	return 0;
}

int strbuf_realloc(strbuf* str, size_t len) {
#ifdef SAFE_STR
	if (str->mem == NULL) {
		/* NOTE
		 * this isn't an error, since
		 * realloc(NULL, 10) ≡ malloc(10)
		 */
		ERR("String memory not initialized");
	}
#endif
	str->mem = realloc(str->mem, len);
	str->alloc = len;
	return 0;
}

FREE_F(strbuf) {
#ifdef SAFE_STR
	if (this->mem == NULL) return 1;
#endif
	free (this->mem);
	this->mem = NULL;
	this->alloc = 0;
	this->len = 0;
	return 0;
}

int strbuf_append(strbuf* s, char c) {
#ifdef SAFE_STR
	if (s->len > s->alloc) {
		// printf("s->len = %i, s->alloc = %i\n", s->len, s->alloc);
		// ERR("Not enough memmory allocated");
		return 1;
	}
#endif
	s->mem[s->len] = c;
	s->ptr = ++s->len;
	return 0;
}

int strbuf_cap(strbuf* s) {
	return strbuf_append(s, 0);
}


int strbuf_copy(strbuf* dest, strbuf* src) {
#ifdef SAFE_STR
	if (dest->alloc + 1 < src->len) {
		ERR("Not enough memmory allocated");
		return 1;
	}
#endif
	dest->len = src->len;
	memcpy(dest->mem, src->mem, src->len);

	// TODO should this be here?
	strbuf_cap(dest);
	return 0;
}

int strbuf_cmp(strbuf* a, strbuf* b) {
#ifdef SAFE_STR
	if (a->alloc == 0 || b->alloc == 0) {
		ERR("a or b not alloced");
		return -1;
	}
#endif
	return strcmp(a->mem, b->mem);
}

int strbuf_c(strbuf* a, char* b) {
	return strcmp(a->mem, b) == 0;
}

char* charat(strbuf* s, unsigned int idx) {
#ifdef SAFE_STR
	if (idx > s->len) {
		ERR("Index out of bounds");
		return (char*) -1;
	}
#endif
	return &s->mem[idx];
}

char* strbuf_cur(strbuf* s) {
	return &s->mem[s->ptr];
}

int strbuf_reset(strbuf* s)
{
	s->ptr = 0;
	return 0;
}

int strbuf_realloc_copy(strbuf* dest, strbuf* src) {
	strbuf_realloc(dest, src->len);
	strbuf_copy(dest, src);
	return 0;
}

int strbuf_init_copy(strbuf* dest, strbuf* src) {
#ifdef SAFE_STR
	if (dest->alloc != 0) {
		printf("%u  ", dest->alloc);
		ERR("Dest already allocated");
		return 1;
	}

	if (src == NULL) {
		ERR("Attempting to copy NULL strbuf");
		return 2;
	}
#endif

	INIT(strbuf, dest, src->len + 1);
	strbuf_copy(dest, src);

	return 0;
}

int DEEP_COPY(strbuf)(strbuf* dest, strbuf* src) {
	return strbuf_init_copy(dest, src);
}

char* strbuf_end(strbuf* s) {
	return &s->mem[s->len];
}

int strbuf_soft_reset(strbuf* s) {
	s->ptr = s->len = 0;
	return 0;
}
