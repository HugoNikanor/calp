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

#define __PRE_VECT(T, i, set) \
	unsigned int __INTER(i) = 0; T* i;
#define PRE_FOR_VECT(T) __PRE_VECT

#define __BEG_VECT(T, i, set) i = GET(VECT(T))(set, __INTER(i))
#define BEG_VECT(T) __BEG_VECT

#define __END_VECT(T, i, set) __INTER(i) < SIZE(VECT(T))(set)
#define END_VECT(T) __END_VECT

#define __NXT_VECT(T, i, set) i = GET(VECT(T))(set, ++__INTER(i))
#define NXT_VECT(T) __NXT_VECT

#endif /* TYPE */
