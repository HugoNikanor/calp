#ifndef MACRO_H
#define MACRO_H

/*
 * NOTE This file uses __VA_OPT__. This is not standard compliant.
 */

/*
 * Token paste
 */
#define TP(a, b) a ## b
#define TP3(a, b, c) a ## b ## c
#define TP4(a, b, c, d) a ## b ## c ## d

/*
 * Get length of __VA_ARGS__
 * Borrowed fram:
 * https://stackoverflow.com/a/35986932
 */
#define VA_ARGS_NUM_PRIV(P1, P2, P3, P4, P5, P6, Pn, ...) Pn
#define VA_ARGS_NUM(...) VA_ARGS_NUM_PRIV(-1, ## __VA_ARGS__, 5, 4, 3, 2, 1, 0)

#define NEW_HELPER(T, ARG_COUNT) \
	TP3(T, _init_, ARG_COUNT)

/*
 * Constructor type name
 */
#define CONSTRUCTOR_T(T, C) TP3(T, _init_, C)

#define CONSTRUCTOR_GEN(parent, child, C) \
	CONSTRUCTOR_T(parent ## _ ## child, C)

/*
 * Returns full type of constructor
 */
#define CONSTRUCTOR_DECL(T, ...) \
	CONSTRUCTOR_T(T, VA_ARGS_NUM(__VA_ARGS__)) (T* this __VA_OPT__(,) __VA_ARGS__)

/*
 * Call the constructor of an object
 */
#define CONSTRUCT(T, N, ...) \
	 CONSTRUCTOR_T(T, VA_ARGS_NUM(__VA_ARGS__)) (N __VA_OPT__(,) __VA_ARGS__)

/*
 * Allocate a new object on the HEAP
 */
#define NEW(T, N, ...) \
	T* N = malloc(sizeof(*N)); \
	CONSTRUCT(T, N, __VA_ARGS__);

/*
 * Allocate a new object on the STACK
 */
#define SNEW(T, N, ...) \
	T N; \
	CONSTRUCT(T, & N, __VA_ARGS__);

#endif /* MACRO_H */
