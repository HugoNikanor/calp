#ifndef LINKED_LIST_H
#define LINKED_LIST_H

#include "macro.h"

#define LLIST(T) TEMPL(llist, T)
#define LINK(T)  TEMPL(llist_link, T)

#define UNLINK(T) TEMPL(unlink, T)

#endif /* LINKED_LIST_H */
#ifdef TYPE

typedef struct LINK(TYPE) {
	struct LINK(TYPE)* before;
	struct LINK(TYPE)* after;
	TYPE* value;
} LINK(TYPE);

typedef struct {
	LINK(TYPE)* head;
	LINK(TYPE)* tail;
	LINK(TYPE)* cur;
	int length;
} LLIST(TYPE);

#define FIRST(lst) (lst)->head->after
#define FIRST_V(lst) (lst)->head->after->value
#define LAST(lst)  (lst)->tail->before
#define LAST_V(lst)  (lst)->tail->before->value

INIT_F ( LLIST(TYPE) );

/*
 * NOTE freeing a linked list alsa FFREE's all its contents.
 * TODO some form of shared pointer to ensure nothing is free'd twice
 * would be a good idea.
 */
FREE_F ( LLIST(TYPE) );

INIT_F ( LINK(TYPE) );
INIT_F ( LINK(TYPE), TYPE* val );
FREE_F ( LINK(TYPE) );

int UNLINK(LINK(TYPE)) ( LINK(TYPE)* );

int PUSH(LLIST(TYPE)) ( LLIST(TYPE)*, TYPE* );
TYPE* PEEK(LLIST(TYPE)) ( LLIST(TYPE)* );
TYPE* POP(LLIST(TYPE)) ( LLIST(TYPE)* );

int DEEP_COPY(LLIST(TYPE)) ( LLIST(TYPE)* dest, LLIST(TYPE)* src );

int APPEND(LLIST(TYPE)) ( LLIST(TYPE)* dest, LLIST(TYPE)* new );

int SIZE(LLIST(TYPE)) ( LLIST(TYPE)* llist );
int EMPTY(LLIST(TYPE)) ( LLIST(TYPE)* llist );

int RESET(LLIST(TYPE)) ( LLIST(TYPE)* llist );

LLIST(TYPE)* RESOLVE(LLIST(TYPE)) (LLIST(TYPE)* dest, LLIST(TYPE)* new);

/* Iterator */

// #define __BEG_LLIST(v, set) v = (set)->head
#define __BEG_LLIST(l, set) l = FIRST(set)
#define BEG_LLIST(T) LINK(T)* __BEG_LLIST

#define __END_LLIST(l, set) l != (set)->tail
#define END_LLIST(T) __END_LLIST

#define __NXT_LLIST(l, set) l = l->after
#define NXT_LLIST(T) __NXT_LLIST

#endif /* TYPE */
