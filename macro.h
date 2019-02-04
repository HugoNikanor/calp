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
#define TP5(a, b, c, d, e) a ## b ## c ## d ## e

/*
 * Get length of __VA_ARGS__
 * Borrowed fram:
 * https://stackoverflow.com/a/35986932
 */
#define VA_ARGS_NUM_PRIV(P1, P2, P3, P4, P5, P6, Pn, ...) Pn
#define VA_ARGS_NUM(...) VA_ARGS_NUM_PRIV(-1, ## __VA_ARGS__, 5, 4, 3, 2, 1, 0)

/* Constructor type name */
#define __INIT_T(T, C) TP3(T, __init__, C)

/* Returns full type of constructor */
#define INIT_F(T, ...) \
	int __INIT_T(T, VA_ARGS_NUM(__VA_ARGS__)) (T* this __VA_OPT__(,) __VA_ARGS__)

/* Call the constructor of an object */
#define INIT(T, N, ...) \
	 __INIT_T(T, VA_ARGS_NUM(__VA_ARGS__)) (N __VA_OPT__(,) __VA_ARGS__)

/* Allocate a new object on the HEAP */
#define NEW(T, N, ...) \
	T* N = malloc(sizeof(*N)); \
	INIT(T, N, __VA_ARGS__);

#define RENEW(T, N, ...) do { \
	N = malloc(sizeof(*N)); \
	INIT(T, N, __VA_ARGS__); \
} while (0)


/* Allocate a new object on the STACK */
#define SNEW(T, N, ...) \
	T N; \
	INIT(T, & N, __VA_ARGS__);

/* Destructor for type */
#define FREE(T) TP(T, __free)

/*  Call destructor for type, and free object */
#define FFREE(T, N) do { FREE(T)(N); free(N); } while (0)

/* Declare destructor */
#define FREE_F(T) int TP(T, __free) (T* this)

#define DEEP_COPY(T) TP(deep_copy__, T)
#define RESOLVE(T) TP(resolve__, T)
#define APPEND(T) TP(append__, T)
#define SIZE(T) TP(size__, T)
#define EMPTY(T) TP(empty__, T)

#endif /* MACRO_H */
