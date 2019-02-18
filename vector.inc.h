#ifndef TYPE
#error "Set TYPE before including self file"
#else

#include "macro.h"
#include "err.h"

INIT_F(VECT(TYPE)) {
	self->length = 0;
	self->alloc = 1;
	self->items = (TYPE**) calloc(sizeof(*self->items), self->alloc);
	return 0;
}

FREE_F(VECT(TYPE)) {
	for (unsigned int i = 0; i < self->length; i++) {
		FFREE(TYPE, self->items[i]);
	}
	free(self->items);
	return 0;
}

int PUSH(VECT(TYPE))(VECT(TYPE)* self, TYPE* t) {
	if (self->length + 1 > self->alloc) {
		self->alloc <<= 1;
		self->items = (TYPE**) realloc(self->items, sizeof(*self->items) * self->alloc);
	}

	self->items[self->length] = t;
	++self->length;
	return 0;
}

TYPE* GET(VECT(TYPE))(VECT(TYPE)* self, unsigned int idx) {
	if (idx >= self->length) {
		ERR("Index out of range"); 
		return NULL;
	}

	return self->items[idx];
}

int EMPTY(VECT(TYPE))(VECT(TYPE)* self) {
	return self->length == 0;
}

unsigned int SIZE(VECT(TYPE))(VECT(TYPE)* self) {
	return self->length;
}

#endif /* TYPE */
