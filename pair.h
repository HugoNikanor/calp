#ifndef PAIR_H
#define PAIR_H

#define PAIR(T, V) TEMPL2(pair, T, V)

#endif /* PAIR_H */
#if defined(T) && defined(V)

typedef struct {
	T left;
	V right;
} PAIR(T, V);

INIT_F(PAIR(T, V));
FREE_F(PAIR(T, V));
FMT_F(PAIR(T, V));
int DEEP_COPY(PAIR(T, V)) (PAIR(T, V)* dest, PAIR(T, V)* src);

#endif
