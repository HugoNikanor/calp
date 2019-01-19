#include "hash.h" 

/*
 * http://www.cse.yorku.ca/~oz/hash.html
 * djb2 from above url.
 */
unsigned long hash(char* str) {
	unsigned long hash = 5381;
	int c;

	while ( (c = *str++) )
		hash = ((hash << 5) + hash) + c;

	return hash;
}
