#ifndef LINKED_LIST_H
#define LINKED_LIST_H

#include "macro.h"

// #define LLIST(T) TEMPL(llist, T)
// #define LINK(T)  TEMPL(llist_link, T)
// 
// #define UNLINK(T) TEMPL(unlink, T)

// #endif /* LINKED_LIST_H */
// #ifdef TYPE

#define FIRST(lst) (lst)->head->after
#define FIRST_V(lst) (lst)->head->after->value
#define LAST(lst)  (lst)->tail->before
#define LAST_V(lst)  (lst)->tail->before->value

template <class T> struct llink {
	struct llink<T>* before;
	struct llink<T>* after;
	T* value;

	llink (T* val);
	llink () : llink (NULL) { };
	~llink ();

	int unlink ();
};

#define L(link) (link)->value

template <class T> struct llist {
	llink<T>* head;
	llink<T>* tail;
	llink<T>* cur;
	int length;

	llist ();
	llist (llist<T>* other);
	~llist ();

	int push (T*);
	T* peek ();
	T* pop ();

	int size () { return this->length; }
	int empty () { return FIRST(this) == this->tail; }
	int append (llist<T>* other);

	/*
	 * Resets a linked list by removing all it's objects.
	 * FREE's all elements stored in the list.
	 */
	int reset ();

};


// INIT_F ( LLIST(TYPE) );

/*
 * NOTE freeing a linked list alsa FFREE's all its contents.
 * TODO some form of shared pointer to ensure nothing is free'd twice
 * would be a good idea.
 */
// FREE_F ( LLIST(TYPE) );
// 
// INIT_F ( LINK(TYPE) );
// INIT_F ( LINK(TYPE), TYPE* val );
// FREE_F ( LINK(TYPE) );

// int UNLINK(LINK(TYPE)) ( LINK(TYPE)* );

// int PUSH(LLIST(TYPE)) ( LLIST(TYPE)*, TYPE* );
// TYPE* PEEK(LLIST(TYPE)) ( LLIST(TYPE)* );
// TYPE* POP(LLIST(TYPE)) ( LLIST(TYPE)* );

// // int DEEP_COPY(LLIST(TYPE)) ( LLIST(TYPE)* dest, LLIST(TYPE)* src );


// LLIST(TYPE)* RESOLVE(LLIST(TYPE)) (LLIST(TYPE)* dest, LLIST(TYPE)* new_);
template <class T>
llist<T>* resolve (llist<T>* dest, llist<T>* new__);

// FMT_F(LLIST(TYPE));

/* Iterator */

#define __PRE_LLIST(T, l, set) \
	T* l; LINK(T)* __INTER(l);

#define PRE_FOR_LLIST(T) __PRE_LLIST

// #define __BEG_LLIST(v, set) v = (set)->head
#define __BEG_LLIST(T, l, set) __INTER(l) = FIRST(set), l = L(__INTER(l))
#define BEG_LLIST(T) __BEG_LLIST

#define __END_LLIST(T, l, set) __INTER(l) != (set)->tail
#define END_LLIST(T) __END_LLIST

#define __NXT_LLIST(T, l, set) __INTER(l) = __INTER(l)->after, l = L(__INTER(l))
// #define __NXT_LLIST(T, l, set) l = L(__INTER(l) = __INTER(l)->after)
#define NXT_LLIST(T) __NXT_LLIST

#include "linked_list.inc.h"

#endif /* TYPE */
