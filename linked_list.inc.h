#ifndef TYPE
#error "Set TYPE before including this file"
#else

INIT_F ( LLIST(TYPE) ) {
	this->length = 0;
	NEW(LINK(TYPE), head);
	NEW(LINK(TYPE), tail);
	this->head   = head;
	this->tail   = tail;
	head->after  = tail;
	tail->before = head;
	this->cur    = head;
	return 0;
}

FREE_F (LINK(TYPE)) {
	if (this->before != NULL) this->before->after = NULL;
	if (this->after  != NULL) this->after->before = NULL;
	// TODO how much of value do I really wanna free?
	// Should I implement some form of shared pointer?
	if (this->value  != NULL) FFREE(TYPE, this->value);
	return 0;
}

FREE_F( LLIST(TYPE) ) {

	LINK(TYPE) *n, *next;
	n = this->head;
	while ( n != NULL ) {
		next = n->after;
		FFREE(LINK(TYPE), n);
		n = next;
	}

	this->length = -1;

	return 0;
}

INIT_F ( LINK(TYPE) ) {
	this->before = NULL;
	this->after  = NULL;
	this->value  = NULL;
	return 0;
}

INIT_F ( LINK(TYPE), TYPE* val ) {
	this->before = NULL;
	this->after  = NULL;
	this->value  = val;
	return 0;
}

int LLIST_CONS(TYPE) ( LLIST(TYPE)* lst, TYPE* val) {
	NEW(LINK(TYPE), link, val);

	link->after = lst->head->after;
	lst->head->after = link;
	link->after->before = link;
	link->before = lst->head;
	++lst->length;

	// TODO do I want to change that?
	lst->cur = link;

	return 0;
}

int DEEP_COPY(LLIST(TYPE)) ( LLIST(TYPE)* dest, LLIST(TYPE)* src ) {
	LINK(TYPE)* n = src->head->after;

	while (n->after != NULL) {
		NEW(TYPE, cpy);
		DEEP_COPY(TYPE)(cpy, n->value);
		LLIST_CONS(TYPE) ( dest, cpy );
		n = n->after;
	}

	return 0;
}

/*
 * TODO
 * this or cons links wrong, creating infinite loops.
 * TODO
 * or maybe not
 */
int APPEND(LLIST(TYPE)) ( LLIST(TYPE)* dest, LLIST(TYPE)* new ) {

	dest->tail->before->after = new->head->after;
	new->head->after->before = dest->tail->before;
	LINK(TYPE)* old_tail = dest->tail;
	dest->tail = new->tail;
	dest->length += new->length;

	free(old_tail);
	// free(new->head);

	/*
	 * TODO `new` can now be handled as an empty husk.
	 * Somehow it needs to be collected
	 */

	return 0;
}

int SIZE(LLIST(TYPE)) ( LLIST(TYPE)* llist ) {
	int size = 0;
	LINK(TYPE)* l = llist->head->after;
	while (l->after != NULL) {
		++size;
	}
	return size;
}

int EMPTY(LLIST(TYPE)) ( LLIST(TYPE)* llist ) {
	return llist->head->after == llist->tail;
}

#endif /* TYPE */
