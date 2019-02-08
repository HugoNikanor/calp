#ifndef VECTOR_H
#define VECTOR_H

#include <stdlib.h>
#include "macro.h"

#define VECT(T) TEMPL(vect, T)

#endif /* VECTOR_H */

#ifdef TYPE

typedef struct {
	unsigned int length;
	unsigned int alloc;
	TYPE** items;
} VECT(TYPE);

INIT_F(VECT(TYPE));
FREE_F(VECT(TYPE));

int PUSH(VECT(TYPE))(VECT(TYPE)*, TYPE*);
TYPE* GET(VECT(TYPE))(VECT(TYPE)*, unsigned int idx);
int EMPTY(VECT(TYPE))(VECT(TYPE)*);
unsigned int SIZE(VECT(TYPE))(VECT(TYPE)*);

#endif /* TYPE */
