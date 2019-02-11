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

#define __BEG_VECT(i, set) unsigned int i = 0
#define BEG_VECT(T) __BEG_VECT

#define __END_VECT(i, set) (set) > i
#define END_VECT(T) SIZE(VECT(T)) __END_VECT

#define __NXT_VECT(l, set) i++
#define NXT_VECT(T) __NXT_VECT

#endif /* TYPE */
