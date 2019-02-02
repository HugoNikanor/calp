#ifndef TYPE
#error "Set TYPE to something before including this header"
#else

#include "err.h"

int HASH_PUT(TYPE) ( TABLE(TYPE)* table, TYPE* value) {
	// TODO genicify the hash function
	unsigned long h = hash(value->key.mem) % table->size;
	TYPE* mem = table->values[h];

	/* TODO conflict resolution */
	if (mem != NULL) ERR("Hash collision");
   	mem = value;

	++table->item_count;
	return 0;
}

int HASH_INIT(TYPE) ( TABLE(TYPE)* table, int init_size ) {
	/*
	 * TODO parts of table might not get properly initialized to 0
	 */
	table->values     = calloc(sizeof(table->values), init_size);
	table->size       = init_size;
	table->item_count = 0;
	return 0;
}

TYPE* HASH_GET(TYPE) ( TABLE(TYPE)* table, char* key ) {
	unsigned long h = hash(key) % table->size;
	TYPE* mem = table->values[h];
	if (mem == NULL) {
		fprintf(stderr, "Trying to access %s\n", key);
		ERR("Nothing in field");
		return 0;
	} else if (strcmp(mem->key.mem, key) == 0) {
		return mem;
	} else {
		/* TODO fix retrival on invalid key */
		ERR("Other error");
		return 0;
	}
}

int HASH_FREE(TYPE) ( TABLE(TYPE)* table ) {
	/*
	 * TODO an early return is possible by checking if all items have
	 * been found. table->item_count
	 */
	for (int i = 0; i < table->size; i++) {
		TYPE* mem = table->values[i];
		if (mem == NULL) continue;

		free(mem);
	}
	free(table->values);
	return 0;
}

#endif /* TYPE */
