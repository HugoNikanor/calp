#ifndef TYPE
#error "Set TYPE before including this file"
#else

int CONSTRUCTOR_DECL ( LLIST(TYPE) ) {
	this->length = 0;
	NEW(LINK(TYPE), head);
	NEW(LINK(TYPE), tail);
	this->head = head;
	this->tail = tail;
	head->after = tail;
	tail->before = head;
	this->cur  = head;
	return 0;
}

int FREE_DECL( LLIST(TYPE) ) {
	LINK(TYPE) *node, *next;
	node = this->head->after;
	while (node->after != NULL) {
		next = node->after;
		if (node->value != NULL) {
			FFREE(TYPE, node->value);
		}
		// free(node);
		node = next;
	}
	free (this->head);
	free (this->tail);
	this->length = 0;
	return 0;
}

int CONSTRUCTOR_DECL ( LINK(TYPE) ) {
	this->before = NULL;
	this->after  = NULL;
	this->value  = NULL;
	return 0;
}

int CONSTRUCTOR_DECL ( LINK(TYPE), TYPE* val ) {
	this->before = NULL;
	this->after  = NULL;
	this->value  = val;
	return 0;
}

int LLIST_CONS(TYPE) ( LLIST(TYPE)* lst, TYPE* val) {
	NEW(LINK(TYPE), l, val);

	l->after = lst->head->after;
	lst->head->after = l;
	l->after->before = l;
	l->before = lst->head;
	++lst->length;

	// TODO do I want to change that?
	lst->cur = l;

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
 */
int APPEND(LLIST(TYPE)) ( LLIST(TYPE)* dest, LLIST(TYPE)* new ) {

	dest->tail->before->after = new->head->after;
	new->head->after->before = dest->tail->before;
	LINK(TYPE)* old_tail = dest->tail;
	dest->tail = new->tail;
	dest->length += new->length;

	// free(old_tail);
	// free(new->head);

	/*
	 * TODO `new` can now be handled as an empty husk.
	 * Somehow it needs to be collected
	 */

	return 0;
}

#endif /* TYPE */
