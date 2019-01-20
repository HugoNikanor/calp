#include "strbuf.h"

#include <string.h>

#include <stdio.h>
#define ERR(s) fprintf(stderr, "\x1B[0;31mERR\x1b[m (strbuf %3i): %s\n", __LINE__, s)

int init_string(string* str, size_t len) {
	str->mem = malloc(len);
	str->alloc = len;
	str->ptr = 0;
	str->len = 0;
	return 0;
}

int realloc_string(string* str, size_t len) {
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

int free_string(string* str) {
#ifdef SAFE_STR
	if (str->alloc == 0 || str->mem == NULL) {
		ERR("String not allocated");
		return 1;
	}
#endif
	free (str->mem);
	str->alloc = 0;
	return 0;
}

int strbuf_append(string* s, char c) {
	s->mem[s->len] = c;
	s->ptr = ++s->len;
	return 0;
}

int strbuf_cap(string* s) {
	return strbuf_append(s, 0);
}


int copy_strbuf(string* dest, string* src) {
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

int strbuf_cmp(string* a, string* b) {
	return strcmp(a->mem, b->mem);
}

int strbuf_c(string* a, char* b) {
	return strcmp(a->mem, b) == 0;
}

char* charat(string* s, int idx) {
#ifdef SAFE_STR
	if (idx > s->len) {
		ERR("Index out of bounds");
		return (char*) -1;
	}
#endif
	return &s->mem[idx];
}

char* strbuf_cur(string* s) {
	return &s->mem[s->ptr];
}

int strbuf_reset(string* s)
{
	s->ptr = 0;
	return 0;
}

int strbuf_init_copy(string* dest, string* src) {
#ifdef SAFE_STR
	if (dest->alloc != 0) {
		ERR("Dest already allocated");
		return 1;
	}
#endif

	init_string(dest, src->len + 1);
	copy_strbuf(dest, src);

	return 0;
}

char* strbuf_end(string* s) {
	return &s->mem[s->len];
}

int strbuf_soft_reset(string* s) {
	s->ptr = s->len = 0;
	return 0;
}
