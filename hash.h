#ifndef HASH_H
#define HASH_H

#include <string.h>

#include "macro.h"

unsigned long hash(char*);

#define TABLE(T)     TP(table_,     T)
#define HASH_PUT(T)  TP(hash_put_,  T)
#define HASH_GET(T)  TP(hash_get_,  T)
#define HASH_INIT(T) TP(hash_init_, T)

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

int HASH_INIT(TYPE) ( TABLE(TYPE)* table, int init_size );

TYPE* HASH_GET(TYPE) ( TABLE(TYPE)* table, char* key );

#endif /* HASH_H */
