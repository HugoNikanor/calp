#ifndef STRBUF_H
#define STRBUF_H

#include <stdlib.h>
#include "macro.h"

typedef struct {
	char* mem;
	/* TODO add support for negative ptr */
	int ptr;
	unsigned int alloc;
	unsigned int len;
} strbuf;

/*
 * TODO Check memmory allocation for last +1 byte for null.
 */

/*
 * Init strbuf to size of 0
 * Doesnt't call malloc.
 */
int CONSTRUCTOR_DECL(strbuf);

/*
 * Constructor
 */
int CONSTRUCTOR_DECL(strbuf, size_t len);

/*
 * Like realloc, but for strbuf
 */
int strbuf_realloc(strbuf* str, size_t len);

/*
 * Free's contents of str, but keeps str.
 */
// int strbuf_free(strbuf* str);
int FREE_DECL(strbuf);

/*
 * Copy contents from src to dest.
 * Assumes that dest is already initialized.
 *
 * also see: strbuf_alloc_copy
 */
int strbuf_copy(strbuf* dest, strbuf* src);
int strbuf_cmp(strbuf* a, strbuf* b);
int strbuf_c(strbuf* a, char* b);

/*
 * Append char to end of strbuf, determined by s->len.
 */
int strbuf_append(strbuf* s, char c);

/*
 * Calls strbuf_append with NULL.
 */
int strbuf_cap(strbuf* s);
int strbuf_reset(strbuf* s);
char* charat(strbuf* s, unsigned int idx);
char* strbuf_cur(strbuf* s);

/*
 * Sets the length and seek ptr to 0, but doesn't touch the memmory. 
 */
int strbuf_soft_reset(strbuf* s);

/*
 * Returns the character after the last, so where null hopefully is.
 */
char* strbuf_end(strbuf* s);

/*
 * Copies contents from src to dest, also allocating dest in the
 * process. dest should not be initialized before this call. 
 */
int strbuf_init_copy(strbuf* dest, strbuf* src);

#endif /* STRBUF_H */
