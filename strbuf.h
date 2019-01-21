#ifndef STRBUF_H
#define STRBUF_H

#include <stdlib.h>

typedef struct {
	char* mem;
	size_t ptr;
	size_t alloc;
	size_t len;
} string;

/*
 * TODO rename everything to be on the form
 *	strbuf_.*
 *
 * TODO Check memmory allocation for last +1 byte for null.
 */

/*
 * Init string to size of 0
 * Doesnt't call malloc.
 */
int strbuf_init_0(string* str);

/*
 * Constructor
 */
int strbuf_init_1(string* str, size_t len);

/*
 * Like realloc, but for strbuf
 */
int realloc_string(string* str, size_t len);

/*
 * Free's contents of str, but keeps str.
 */
int free_string(string* str);

/*
 * Copy contents from src to dest.
 * Assumes that dest is already initialized.
 *
 * also see: strbuf_alloc_copy
 */
int copy_strbuf(string* dest, string* src);
int strbuf_cmp(string* a, string* b);
int strbuf_c(string* a, char* b);

/*
 * Append char to end of string, determined by s->len.
 */
int strbuf_append(string* s, char c);

/*
 * Calls strbuf_append with NULL.
 */
int strbuf_cap(string* s);
int strbuf_reset(string* s);
char* charat(string* s, int idx);
char* strbuf_cur(string* s);

/*
 * Sets the length and seek ptr to 0, but doesn't touch the memmory. 
 */
int strbuf_soft_reset(string* s);

/*
 * Returns the character after the last, so where null hopefully is.
 */
char* strbuf_end(string* s);

/*
 * Copies contents from src to dest, also allocating dest in the
 * process. dest should not be initialized before this call. 
 */
int strbuf_init_copy(string* dest, string* src);

#endif /* STRBUF_H */
