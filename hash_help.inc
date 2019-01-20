#ifndef TYPE
#error "Set TYPE to something before including this header"
#else

int HASH_PUT(TYPE) ( TABLE(TYPE)* table, TYPE* value) {
	// TODO genicify the hash function
	unsigned long h = hash(value->key.mem) % table->size;
	table->values[h] = value;

	/* TODO conflict resolution */

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
	// TYPE* mem = (table->values + h);
	TYPE* mem = table->values[h];
	if (mem == NULL) {
		return 0;
	} else if (strcmp(mem->key.mem, key) == 0) {
		return mem;
	} else {
		/* TODO fix retrival on invalid key */
	}
	return 0;
}

#endif /* TYPE */

// vim: ft=c
