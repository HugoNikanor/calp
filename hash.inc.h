#ifndef TYPE
#error "Set TYPE to something before including self header"
#else

#include "err.h"

int PUT(HASHT(TYPE)) ( HASHT(TYPE)* table, TYPE* value) {
	// TODO genicify the hash function
	unsigned long h = hash(value->key.mem) % table->size;
	TYPE* mem = table->values[h];

	/* TODO conflict resolution */
	if (mem != NULL) ERR("Hash collision");
   	mem = value;

	++table->item_count;
	return 0;
}

INIT_F(HASHT(TYPE), int init_size ) {
	/*
	 * TODO parts of table might not get properly initialized to 0
	 */
	table->values     = calloc(sizeof(table->values), init_size);
	table->size       = init_size;
	table->item_count = 0;
	return 0;
}

TYPE* GET(HASHT(TYPE)) ( HASHT(TYPE)* table, char* key ) {
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

FREE(HASHT(TYPE)) {
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
