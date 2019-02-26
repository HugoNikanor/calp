#ifndef MACRO_H
#define MACRO_H

/*
 * Token paste
 */
#define TP(a, b) a ## b
#define TP3(a, b, c) a ## b ## c
#define TP4(a, b, c, d) a ## b ## c ## d
#define TP5(a, b, c, d, e) a ## b ## c ## d ## e
#define TP6(a, b, c, d, e, f) a ## b ## c ## d ## e ## f

/*
 * Get length of __VA_ARGS__
 * Borrowed fram:
 * https://stackoverflow.com/a/35986932
 */
#define VA_ARGS_NUM_PRIV(P1, P2, P3, P4, P5, P6, Pn, ...) Pn
#define VA_ARGS_NUM(...) VA_ARGS_NUM_PRIV(-1, ## __VA_ARGS__, 5, 4, 3, 2, 1, 0)

/*
 * Templatization macros. Forms symbols on the from name<T>, which
 * looks really good in debuggers and the like. Unicode characters
 * written in \U notation since C apparently doesn't support unicode
 * literals.
 *
 * Can be nested (useful for container types).
 *
 * Doesn't use ASCII <>, but rather some other ᐸᐳ, meaning that it's
 * not a reserved character.
 *
 * nameᐸTᐳ
 */
#define TEMPL(name, T)     TP4(name, \U00001438 , T, \U00001433 )
#define TEMPL2(name, T, V) TP6(name, \U00001438\U00001438 , T , \U00001433_\U00001438 , V, \U00001433\U00001433)
#define TEMPL_N(name, T, argcount) TP6(name, \U00001438 , T, _, argcount, \U00001433 )

/* Constructor type name */
#define __INIT_T(T, C) TEMPL_N(init, T, C)

/* Returns full type of constructor */
#define INIT_F(T, ...) \
	int __INIT_T(T, VA_ARGS_NUM(__VA_ARGS__)) (T* self, ## __VA_ARGS__)

/*
 * Call the constructor of an object
 * `int` part of the macro, to ensure that any attempt to call self
 * function results in an error.
 */
#define INIT(T, N, ...) \
	 __INIT_T(T, VA_ARGS_NUM(__VA_ARGS__)) (N, ## __VA_ARGS__)

/* Allocate a new_ object on the HEAP */
#define NEW(T, N, ...) \
	T* N = (T*) malloc(sizeof(*N)); \
	INIT(T, N, ## __VA_ARGS__);

/*
 * Reconstructs a object. Use with caution.
 */
#define RENEW(T, N, ...) do { \
	N = (T*) malloc(sizeof(*N)); \
	INIT(T, N, ## __VA_ARGS__); \
} while (0)


/* Allocate a new_ object on the STACK */
#define SNEW(T, N, ...) \
	T N; \
	INIT(T, & N, ## __VA_ARGS__);

/* Destructor for type */
#define FREE(T) TEMPL(free, T)

/*  Call destructor for type, and free object */
#define FFREE(T, N) do { FREE(T)(N); free(N); } while (0)

/* Declare destructor */
#define FREE_F(T) int FREE(T) (T* self)

/* generate reusable internal symbol */
#define __INTER(s) TP3(__, s, __internal)
#define __INTER2(s) __INTER(__INTER(s))
#define __INTER3(s) __INTER(__INTER(__INTER(s)))

/* Iterator macros.  */
#define FOR(CONT_T, T, var, set) \
	PRE_FOR_  ## CONT_T (T) (T, var, set); \
	for( BEG_ ## CONT_T (T) (T, var, set); \
		 END_ ## CONT_T (T) (T, var, set); \
		 NXT_ ## CONT_T (T) (T, var, set))

/* Example int implementation
 * FOR(int, i, 10) { } */

#define PRE_FOR_int(i, set)
#define BEG_int(i, set) int i = 0
#define NXT_int(i, set) i++
#define END_int(i, set) i < set

/*
 * General functions that different container types may implement.
 * Actuall implementation and type signature is mostly left to
 * individual implementations.
 */
#define DEEP_COPY(T) TEMPL(copy      , T)
#define RESOLVE(T)   TEMPL(resolve   , T)
#define APPEND(T)    TEMPL(append    , T)
#define SIZE(T)      TEMPL(size      , T)
#define EMPTY(T)     TEMPL(empty     , T)
#define PUSH(T)      TEMPL(push      , T)
#define PEEK(T)      TEMPL(peek      , T)
#define POP(T)       TEMPL(pop       , T)
#define GET(T)       TEMPL(get       , T)
#define RESET(T)     TEMPL(reset     , T)
#define KEYS(T)      TEMPL(keys      , T)

/*
 * Formatting macros.
 * Transform objects into string representation of themselves. 
 * buf should be a suffisiently large memmory location, if it's to
 * small then bad stuff might happen.
 *
 * Should return the number of bytes written (like sprintf).
 */

#define FMT_T(T)     TEMPL(format    , T)
#define FMT_F(T) int FMT_T(T)(T* self, char* buf, ...)
// TODO change order of buf and item
#define __FMT_HELP(item, buf, ...) ((item), (buf), VA_ARGS_NUM(__VA_ARGS__), ## __VA_ARGS__)
#define FMT(T) FMT_T(T) __FMT_HELP
#define fmtf(...) seek += sprintf(buf + seek, __VA_ARGS__)

#endif /* MACRO_H */
