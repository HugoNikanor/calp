#ifndef LINKED_LIST_H
#define LINKED_LIST_H


template <typename T>
struct link {
	link<T>* before = nullptr;
	link<T>* after  = nullptr;
	T* value;

	link ();
	link (T* val) : value(val) { }
	~link ();

	void unlink ();
};

// #define L(link) (link)->value

template <typename T>
struct llist {
	link<T>* head;
	link<T>* tail;
	link<T>* cur;
	int length;

	llist ();

	void push ( T& );
	T& peek ();
	T& pop ();

	llist& operator += (llist& other);
};

#define FIRST(lst) (lst)->head->after
#define FIRST_V(lst) (lst)->head->after->value
#define LAST(lst)  (lst)->tail->before
#define LAST_V(lst)  (lst)->tail->before->value

/*
 * NOTE freeing a linked list alsa FFREE's all its contents.
 * TODO some form of shared pointer to ensure nothing is free'd twice
 * would be a good idea.
 */

int DEEP_COPY(LLIST(TYPE)) ( LLIST(TYPE)* dest, LLIST(TYPE)* src );

int APPEND(LLIST(TYPE)) ( LLIST(TYPE)* dest, LLIST(TYPE)* new_ );

int SIZE(LLIST(TYPE)) ( LLIST(TYPE)* llist );
int EMPTY(LLIST(TYPE)) ( LLIST(TYPE)* llist );

/*
 * Resets a linked list by removing all it's objects.
 * FREE's all elements stored in the list.
 */
int RESET(LLIST(TYPE)) ( LLIST(TYPE)* llist );

LLIST(TYPE)* RESOLVE(LLIST(TYPE)) (LLIST(TYPE)* dest, LLIST(TYPE)* new_);

FMT_F(LLIST(TYPE));

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

#endif /* LINKED_LIST_H */
