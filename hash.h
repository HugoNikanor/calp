#ifndef HASH_H
#define HASH_H

#include <string.h>

#include "macro.h"

unsigned long hash(char*);

#define TABLE(T)     TP(table__,    T)
#define HASH_PUT(T)  TP(hash_put_,  T)
#define HASH_GET(T)  TP(hash_get_,  T)

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

int HASH_PUT(TYPE) ( TABLE(TYPE)* table, TYPE* value );

INIT_F(HASH(TYPE), int init_size );

TYPE* HASH_GET(TYPE) ( TABLE(TYPE)* table, char* key );

/*
 * Free's all item's stored in table.
 * And finally frees table.
 */
FREE_F(HASH(TYPE));

#endif /* HASH_H */
