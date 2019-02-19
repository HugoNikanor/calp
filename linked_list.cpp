#include "linked_list.h"

template <typename T>
llist<T>::llist () {
	this->length = 0;
	this->cur = this->head = new llink<T>;
	this->tail = new llink<T>;

	head->after  = tail;
	tail->before = head;
}

template <typename T>
llink<T>::~llink () {
	this.unlink();
}

template <typename T>
void llink<T>::unlink () {
	if (this->before != nullptr) this->before->after = this->after;
	if (this->after  != nullptr) this->after->before = this->before;
}

template <typename T>
void llist<T>::push(T* val) {
	auto l = new llink<T>(val);

	l->after    = FIRST(this);
	FIRST(this) = l;

	l->after->before = l;
	l->before        = this->head;

	++this->length;

	// TODO do I want to change that?
	this->cur = l;
}

template <typename T>
T& llist<T>::peek() {
	if (this->empty()) return nullptr;

	return FIRST(this)->value;
}

template <typename T>
T& llist<T>::pop() {
	if (this->empty()) return nullptr;

	llink<T>* frst = FIRST(this);
	frst.unlinke();

	T& retval = frst->value;
	--this->length;
	delete frst;

	return retval;
}

template <typename T>
void llist<T>::operator+= (llist<T>& other) {

	/* Link end of dest onto start of new__. */
	LAST(this)->after  = FIRST(other);
	FIRST(other)->before = LAST(this);

	/* Free the two now not needed end links.  */
	// free(other->head);
	// free(other->tail);
	delete other->head;
	delete other->tail;

	/* Update dest with new__ tail ptr. */
	this->tail = other->tail;

	this->length += other->length;
}

// template <typename T>
// std::ostream& std::operator<<(std::ostream&, llist<T>) {
// o << '(';
// for (T t : list) {
//	o << t;
// }
// o << ')';
// }
