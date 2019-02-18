#include "linked_list.h"

template <typename T>
llist<T>::llist () {
	this->length = 0;
	this->cur = this->head = new link<T>;
	this->tail = new link<T>;

	head->after  = tail;
	tail->before = head;
}

template <typename T>
link<T>::~link () {
	this.unlink();
}

template <typename T>
void link<T>::unlink () {
	if (this->before != nullptr) this->before->after = this->after;
	if (this->after  != nullptr) this->after->before = this->before;
}

template <typename T>
void llist<T>::push(T& val) {
	auto l = new link<T>(val);

	l->after    = FIRST(this);
	FIRST(this) = l;

	l->after->before = l;
	l->before        = this->head;

	++this->length;

	// TODO do I want to change that?
	this->cur = l;
}
