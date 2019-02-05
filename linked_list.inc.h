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

int PUSH(LLIST(TYPE)) ( LLIST(TYPE)* lst, TYPE* val) {
	NEW(LINK(TYPE), link, val);

	link->after         = FIRST(lst);
	FIRST(lst)          = link;

	link->after->before = link;
	link->before        = lst->head;

	++lst->length;

	// TODO do I want to change that?
	lst->cur = link;

	return 0;
}

int DEEP_COPY(LLIST(TYPE)) ( LLIST(TYPE)* dest, LLIST(TYPE)* src ) {
	LINK(TYPE)* n = FIRST(src);

	while (n->after != NULL) {
		NEW(TYPE, cpy);
		DEEP_COPY(TYPE)(cpy, n->value);
		PUSH(LLIST(TYPE)) ( dest, cpy );
		n = n->after;
	}

	return 0;
}

/*
 * Adds two linked lists together.
 * O(1) time.
 * destroys new in the process, but keeps the elements.
 * make sure to free(new) after.
 */
int APPEND(LLIST(TYPE)) ( LLIST(TYPE)* dest, LLIST(TYPE)* new ) {

	/* Link end of dest onto start of new. */
	LAST(dest)->after  = FIRST(new);
	FIRST(new)->before = LAST(dest);

	/* Free the two now not needed end links.  */
	free(new->head);
	free(dest->tail);

	/* Update dest with new tail ptr. */
	dest->tail = new->tail;

	dest->length += new->length;

	return 0;
}

int SIZE(LLIST(TYPE)) ( LLIST(TYPE)* llist ) {
	int size = 0;
	LINK(TYPE)* l = FIRST(llist);
	while (l->after != NULL) {
		++size;
	}
	return size;
}

int EMPTY(LLIST(TYPE)) ( LLIST(TYPE)* llist ) {
	return FIRST(llist) == llist->tail;
}

#endif /* TYPE */
