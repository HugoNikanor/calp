#ifndef MACRO_H
#define MACRO_H

#define TP(a, b) a ## b

#define NEW(T, N, ...) \
	T* N = malloc(sizeof(*N)); \
	TP(T, _init) (N, __VA_ARGS__);

#endif /* MACRO_H */
