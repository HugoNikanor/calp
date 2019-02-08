#ifndef TYPE
#error "Set TYPE before including this file"
#else

#include "macro.h"
#include "err.h"

INIT_F(VECT(TYPE)) {
	this->length = 0;
	this->alloc = 0x10;
	this->items = calloc(sizeof(*this->items), this->alloc);
	return 0;
}

FREE_F(VECT(TYPE)) {
	for (unsigned int i = 0; i < this->length; i++) {
		FFREE(TYPE, this->items[i]);
	}
	free(this->items);
	return 0;
}

int PUSH(VECT(TYPE))(VECT(TYPE)* this, TYPE* t) {
	if (this->length + 1 > this->alloc) {
		this->alloc <<= 1;
		this->items = realloc(this->items, sizeof(*this->items) * this->alloc);
	}

	this->items[this->length] = t;
	++this->length;
	return 0;
}

TYPE* GET(VECT(TYPE))(VECT(TYPE)* this, unsigned int idx) {
	if (idx >= this->length) {
		ERR("Index out of range"); 
		return NULL;
	}

	return this->items[idx];
}

int EMPTY(VECT(TYPE))(VECT(TYPE)* this) {
	return this->length == 0;
}

unsigned int SIZE(VECT(TYPE))(VECT(TYPE)* this) {
	return this->length;
}

#endif /* TYPE */
