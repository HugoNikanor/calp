#ifndef LINKED_LIST_H
#define LINKED_LIST_H

#include "macro.h"

#define LLIST(T) TP(llist__, T)
#define LINK(T)  TP(llist_link__, T)

#define LLIST_CONS(T) TP(llist_cons__, T)

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

INIT_F ( LLIST(TYPE) );
FREE_F ( LLIST(TYPE) );

INIT_F ( LINK(TYPE) );
INIT_F ( LINK(TYPE), TYPE* val );
FREE_F ( LINK(TYPE) );

int LLIST_CONS(TYPE) ( LLIST(TYPE)* lst, TYPE* val);

int DEEP_COPY(LLIST(TYPE)) ( LLIST(TYPE)* dest, LLIST(TYPE)* src );

int APPEND(LLIST(TYPE)) ( LLIST(TYPE)* dest, LLIST(TYPE)* new );

int SIZE(LLIST(TYPE)) ( LLIST(TYPE)* llist );
int EMPTY(LLIST(TYPE)) ( LLIST(TYPE)* llist );

#endif /* TYPE */
