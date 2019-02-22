#ifndef HEADER_ONLY
#error "Only include this file from the appropriate header."
#endif
// #ifndef TYPE
// #error "Set TYPE before including this file"
// #else

#include "macro.h"
#include "err.h"

// INIT_F(VECT(TYPE)) {
template <class T>
vect<T>::vect () {
	this->length = 0;
	this->alloc = 1;
	this->items = (T**) calloc(sizeof(*this->items), this->alloc);
}

template <class T>
vect<T>::~vect() {
// FREE_F(VECT(TYPE)) {
	for (unsigned int i = 0; i < this->length; i++) {
		delete this->items[i];
		// FFREE(TYPE, this->items[i]);
	}
	free(this->items);
}

template <class T>
int vect<T>::push (T* t) {
// int PUSH(VECT(TYPE))(VECT(TYPE)* this, TYPE* t) {
	if (this->length + 1 > this->alloc) {
		this->alloc <<= 1;
		this->items = (T**) realloc(this->items, sizeof(*this->items) * this->alloc);
	}

	this->items[this->length] = t;
	++this->length;
	return 0;
}

template <class T>
T* vect<T>::operator[] (unsigned int idx) {
// TYPE* GET(VECT(TYPE))(VECT(TYPE)* this, unsigned int idx) {
	if (idx >= this->length) {
		ERR("Index out of range"); 
		return NULL;
	}

	return this->items[idx];
}

template <class T>
int vect<T>::empty () {
//int EMPTY(VECT(TYPE))(VECT(TYPE)* this) {
	return this->length == 0;
}

template <class T>
unsigned int vect<T>::size () {
//unsigned int SIZE(VECT(TYPE))(VECT(TYPE)* this) {
	return this->length;
}

// #endif /* TYPE */
