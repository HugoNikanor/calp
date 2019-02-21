#ifndef PAIR_H
#define PAIR_H

// #define PAIR(T, V) TEMPL2(pair, T, V)

// #endif /* PAIR_H */
// #if defined(T) && defined(V)

template<class T, class V> struct pair {
	T* key;
	V* val;

	pair () { }
	pair (pair<T,V>& other);
};

#if 0
INIT_F(PAIR(T, V));
FREE_F(PAIR(T, V));
FMT_F(PAIR(T, V));
#endif
// int DEEP_COPY(PAIR(T, V)) (PAIR(T, V)* dest, PAIR(T, V)* src);

#include "pair.inc.h"

#endif
