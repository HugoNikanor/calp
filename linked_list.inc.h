// #ifndef TYPE
// #error "Set TYPE before including this file"
// #else

#include <stdio.h>
#include "err.h"

// INIT_F ( LLIST(TYPE) ) {
template <class T>
llist<T>::llist () {
	this->length = 0;
	head = new llink<T>;
	tail = new llink<T>;
	this->head   = head;
	this->tail   = tail;
	head->after  = tail;
	tail->before = head;
	this->cur    = head;
	// return 0;
}

// FREE_F (LINK(TYPE)) {
template <class T>
llink<T>::~llink () {
	this->unlink();

	// if (this->value != NULL) delete this->value;
}

// FREE_F( LLIST(TYPE) ) {
template <class T>
llist<T>::~llist () {
	llink<T> *n, *next;
	n = this->head;
	while ( n != NULL ) {
		next = n->after;
		delete n;
		// FFREE(LINK(TYPE), n);
		n = next;
	}

	this->length = -1;
}

/*
INIT_F ( LINK(TYPE) ) {
	this->before = NULL;
	this->after  = NULL;
	this->value  = NULL;
	return 0;
}
*/

// INIT_F ( LINK(TYPE), TYPE* val ) {
template <class T>
llink<T>::llink (T* val) {
	this->before = NULL;
	this->after  = NULL;
	this->value  = val;
}

// int UNLINK(LINK(TYPE)) ( LINK(TYPE)* this ) {
template <class T>
int llink<T>::unlink () {
	if (this->before != NULL) this->before->after = this->after;
	if (this->after  != NULL) this->after->before = this->before;
	return 0;
}
	 

// int PUSH(LLIST(TYPE)) ( LLIST(TYPE)* lst, TYPE* val) {
template <class T>
int llist<T>::push (T* val) {
	// NEW(LINK(TYPE), link, val);
	auto link = new llink<T>(val);

	link->after         = FIRST(this);
	FIRST(this)          = link;

	link->after->before = link;
	link->before        = this->head;

	++this->length;

	// TODO do I want to change that?
	this->cur = link;

	return 0;
}

// TYPE* PEEK(LLIST(TYPE)) ( LLIST(TYPE)* lst ) {
template <class T>
T* llist<T>::peek () {
	if (this->empty()) return NULL;
	// if (EMPTY(LLIST(TYPE))(lst)) return NULL;

	return FIRST(this)->value;
}

// TYPE* POP(LLIST(TYPE)) ( LLIST(TYPE)* lst) {
template <class T>
T* llist<T>::pop () {
	// if (EMPTY(LLIST(TYPE))(lst)) return NULL;
	if (this->empty()) return NULL;

	// LINK(TYPE)* frst = FIRST(lst);
	//UNLINK(LINK(TYPE))(frst);
	auto frst = FIRST(this);
	frst->unlink();

	T* retval = frst->value;
	--this->length;
	delete frst;
	return retval;
}

// int DEEP_COPY(LLIST(TYPE)) ( LLIST(TYPE)* dest, LLIST(TYPE)* src ) {
template <class T>
llist<T>::llist (llist<T>* other) {
	llink<T>* n = FIRST(other);

	while (n->after != NULL) {
		// NEW(TYPE, cpy);
		T* cpy = new T(n->value);
		// DEEP_COPY(TYPE)(cpy, n->value);
		//PUSH(LLIST(TYPE)) ( dest, cpy );
		this->push (cpy);
		n = n->after;
	}
}

/*
 * Adds two linked lists together.
 * O(1) time.
 * destroys new__ in the process, but keeps the elements.
 * make sure to free(new__) after.
 */
template <class T>
int llist<T>::append ( llist<T>* other ) {

	/* Link end of dest onto start of new__. */
	LAST(this)->after  = FIRST(other);
	FIRST(other)->before = LAST(this);

	/* Free the two now not needed end links.  */
	// free(new__->head);
	// free(dest->tail);
	delete this->tail;
	delete other->head;

	/* Update dest with new__ tail ptr. */
	this->tail = other->tail;

	this->length += other->length;

	return 0;
}

template <class T>
llist<T>* resolve (llist<T>* dest, llist<T>* new__) {
	if (dest == NULL) return new__;
	dest.append(new__);
	return dest;
}

// int RESET(LLIST(TYPE)) ( LLIST(TYPE)* llist ) {
template <class T>
int llist<T>::reset () {

	llink<T> *link = FIRST(this), *next;
	/*
	 * Manual looping rather than itterators since we destroyed the
	 * loop variable.
	 */
	fprintf(stderr, _RED "head = %p, tail = %p\n" _RESET, this->head, this->tail);
	while (link != this->tail) {
		next = link->after;
		fprintf(stderr, _RED "link = %p\n" _RESET, link);
		// FFREE(LINK(TYPE), link);
		delete link;
		link = next;
	}

	this->cur = this->head;

	return 0;
}

#if 0
FMT_F(LLIST(TYPE)) {
	int seek = 0;
	fmtf("(");
	FOR(LLIST, TYPE, v, this) {
		seek += FMT(TYPE)(v, buf + seek);
		fmtf(" ");
	}
	fmtf(")");

	return seek;
}
#endif

// #endif /* TYPE */
