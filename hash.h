#ifndef HASH_H
#define HASH_H

#include <string.h>

#include "macro.h"

unsigned long hash(char*);

#define HASHT(T)     TEMPL(hash_t, T)

#endif /* HASH_H */
#ifdef TYPE

typedef struct {
	int size;
	int item_count;
	/* NOTE
	 * Hash maps are always assumed to hold pointers to objects
	 * Double pointer means a list of pointers.
	 */
	TYPE** values;
} TABLE(TYPE);

int PUSH(HASHT(TYPE)) ( HASHT(TYPE)* table, TYPE* value );

INIT_F(HASHT(TYPE), int init_size );

TYPE* GET(HASHT(TYPE)) ( HASHT(TYPE)* table, char* key );

/*
 * Free's all item's stored in table.
 * And finally frees table.
 */
FREE_F(HASHT(TYPE));

#endif /* HASH_H */
