#ifndef STRBUF_H
#define STRBUF_H

#include <stdlib.h>
#include "macro.h"

/*
 * A high level string type which holds it's own length, how much
 * memmory it has allocated for itself, and a seek pointer into the
 * string.
 *
 * Also comes with a number of functions which allow for safe(er)
 * access to the memmory.
 */
typedef struct {
	char* mem;
	/* TODO add support for negative ptr */
	int ptr;
	unsigned int alloc;
	unsigned int len;
} strbuf;

/*
 * Init strbuf to size of 0
 * Doesnt't call malloc.
 */
INIT_F(strbuf);

/* Constructor */
INIT_F(strbuf, size_t len);

/*
 * Like realloc, but for strbuf
 */
int strbuf_realloc(strbuf* str, size_t len);

/*
 * Free's contents of str, but keeps str.
 */
FREE_F(strbuf);

int strbuf_cmp(strbuf* a, strbuf* b);
int strbuf_c(strbuf* a, char* b);

/*
 * Copy contents from src to dest.
 * Assumes that dest is already initialized.
 */
int DEEP_COPY(strbuf)(strbuf*, strbuf*);

/*
 * Append char to end of strbuf, determined by s->len.
 */
int strbuf_append(strbuf* s, char c);

/*
 * Calls strbuf_append with NULL.
 */
int strbuf_cap(strbuf* s);

/*
 * Returns a pointer to character at index. Allows mutation of the
 * value pointed to by the return address.
 */
char* charat(strbuf* s, unsigned int idx);

/*
 * Same as `charat`, But returns the current character.
 */
char* strbuf_cur(strbuf* s);

/*
 * Resets the seek for strbuf to 0.
 */
int strbuf_reset(strbuf* s);

/*
 * Sets the length and seek ptr to 0, but doesn't touch the memmory.
 */
int strbuf_soft_reset(strbuf* s);

/*
 * Returns the character after the last, so where null hopefully is.
 */
char* strbuf_end(strbuf* s);

/*
 * Reallocs dest to be the same size as src, and copies the contents
 * of src into dest.
 */
int strbuf_realloc_copy(strbuf* dest, strbuf* src);

/*
 * Copies contents from src to dest, also allocating dest in the
 * process. dest should not be initialized before this call.
 */
int strbuf_init_copy(strbuf* dest, strbuf* src);

strbuf* RESOLVE(strbuf)(strbuf*, strbuf*);

FMT_F(strbuf);

int SIZE(strbuf)(strbuf*);

#endif /* STRBUF_H */
