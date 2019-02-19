#ifndef LINKED_LIST_H
#define LINKED_LIST_H


template <typename T>
struct llink {
	llink<T>* before = nullptr;
	llink<T>* after  = nullptr;
	T* value;

	llink () { };
	llink (T* val) : value(val) { }
	~llink () { this->unlink(); }

	void unlink ();
};

template <typename T>
struct llist {
	llink<T>* head;
	llink<T>* tail;
	llink<T>* __cur;
	T* cur() { return __cur->value; }
	int length = 0;

	llist ();

	void push ( T* );
	T& peek ();
	T& pop ();

	/*
	 * Adds two linked lists together.
	 * O(1) time.
	 * destroys new__ in the process, but keeps the elements.
	 * make sure to free(new__) after.
	 */
	void operator += (llist<T>& other);

	int size () { return length; }
	bool empty () { return length == 0; }

	void reset ();
};

template <typename T>
llist<T>::llist () {
	this->__cur = this->head = new llink<T>;
	this->tail = new llink<T>;

	head->after  = tail;
	tail->before = head;
}


#define FIRST(lst) (lst)->head->after
#define FIRST_V(lst) (lst)->head->after->value
#define LAST(lst)  (lst)->tail->before
#define LAST_V(lst)  (lst)->tail->before->value

template <typename T>
void llist<T>::push(T* val) {
	auto l = new llink<T>(val);

	l->after    = FIRST(this);
	FIRST(this) = l;

	l->after->before = l;
	l->before        = this->head;

	++this->length;

	// TODO do I want to change that?
	this->__cur = l;
}

template <typename T>
void llist<T>::reset () {
	llink<T> *link = this->head, *next;

	while (link != this->tail) {
		next = link->after;
		delete link;
		link = next;
	}

	this->__cur = this->head;
}
template <typename T>
void llink<T>::unlink () {
	if (this->before != nullptr) this->before->after = this->after;
	if (this->after  != nullptr) this->after->before = this->before;
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
	delete other->head;
	delete other->tail;

	/* Update dest with new__ tail ptr. */
	this->tail = other->tail;

	this->length += other->length;
}

// template <typename T>
// std::ostream& std::operator<<(std::ostream&, llist<T>);

#if 0
DEEP_COPY
	LINK(TYPE)* n = FIRST(src);

	while (n->after != NULL) {
		NEW(TYPE, cpy);
		DEEP_COPY(TYPE)(cpy, n->value);
		PUSH(LLIST(TYPE)) ( dest, cpy );
		n = n->after;
	}

	return 0;
#endif

/*
 * Resets a linked list by removing all it's objects.
 * FREE's all elements stored in the list.
 */
// int RESET(LLIST(TYPE)) ( LLIST(TYPE)* llist );

#if 0
LLIST(TYPE)* RESOLVE(LLIST(TYPE)) (LLIST(TYPE)* dest, LLIST(TYPE)* new_);
	if (dest == NULL) return new__;
	APPEND(LLIST(TYPE))(dest, new__);
	return dest;
#endif

#endif /* LINKED_LIST_H */
