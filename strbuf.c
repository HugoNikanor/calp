#include "strbuf.h"

#include <string.h>

#ifdef SAFE_STR
#include <stdio.h>
#define ERR(s) fprintf(stderr, "\x1B[0;31mERR\x1b[m (strbuf %3i): %s\n", __LINE__, s)
#endif

int CONSTRUCTOR_DECL(strbuf) {
	this->mem   = NULL;
	this->alloc = 0;
	this->len   = 0;
	this->ptr   = 0;
	return 0;
}

int CONSTRUCTOR_DECL(strbuf, size_t len) {
	this->mem   = calloc(sizeof(*this->mem), len);
	this->alloc = len;
	this->ptr   = 0;
	this->len   = 0;
	return 0;
}

int strbuf_realloc(strbuf* str, size_t len) {
#ifdef SAFE_STR
	if (str->mem == NULL || str->alloc == 0) {
		ERR("String memory not initialized");
		return 1;
	}
#endif
	str->mem = realloc(str->mem, len);
	str->alloc = len;
	return 0;
}

// int strbuf_free(strbuf* str) {
int FREE_DECL(strbuf) {
#ifdef SAFE_STR
	if (this->alloc == 0 || this->mem == NULL) {
		ERR("String not allocated");
		return 1;
	}
#endif
	// fprintf(stderr, "Memmory = %p | %4lu | %s\n", this->mem, this->alloc, this->mem);
	free (this->mem);
	this->alloc = 0;
	return 0;
}

/*
 * TODO this should do bounds check
 */
int strbuf_append(strbuf* s, char c) {
	s->mem[s->len] = c;
	s->ptr = ++s->len;
	return 0;
}

int strbuf_cap(strbuf* s) {
	return strbuf_append(s, 0);
}


int strbuf_copy(strbuf* dest, strbuf* src) {
#ifdef SAFE_STR
	if (dest->alloc < src->len) {
		ERR("Not enough memmory allocated");
		return 1;
	}
#endif
	dest->len = src->len;
	memcpy(dest->mem, src->mem, src->len);
	return 0;
}

int strbuf_cmp(strbuf* a, strbuf* b) {
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

int strbuf_init_copy(strbuf* dest, strbuf* src) {
#ifdef SAFE_STR
	if (dest->alloc != 0) {
		printf("%u  ", dest->alloc);
		ERR("Dest already allocated");
		return 1;
	}
#endif

	CONSTRUCT(strbuf, dest, src->len + 1);
	strbuf_copy(dest, src);

	return 0;
}

char* strbuf_end(strbuf* s) {
	return &s->mem[s->len];
}

int strbuf_soft_reset(strbuf* s) {
	s->ptr = s->len = 0;
	return 0;
}
