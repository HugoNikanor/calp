#ifndef TYPE
#error "Set TYPE before including this file"
#else

int CONSTRUCTOR_DECL ( LLIST(TYPE) ) {
	this->length = 0;
	this->head = NULL;
	this->tail = NULL;
	this->cur = NULL;
	return 0;
}

int LLIST_FREE(TYPE) (LLIST(TYPE)* this ) {
	LINK(TYPE) *node, *next;
	node = this->head;
	while (node != NULL) {
		next = node->after;
		FFREE(TYPE, node->value);
		free(node);
		node = next;
	}
	this->length = 0;
	return 0;
}

int CONSTRUCTOR_DECL ( LINK(TYPE) ) {
	this->before = NULL;
	this->after  = NULL;
	this->value  = NULL;
	return 0;
}

int LLIST_CONS(TYPE) ( LLIST(TYPE)* lst, TYPE* val) {
	NEW(LINK(TYPE), l);
	l->after = lst->head;
	if (l->after != NULL) {
		l->after->before = l;
	}
	lst->head = l;
	l->value = val;
	++lst->length;

	lst->cur = l;

	return 0;
}

int DEEP_COPY(LLIST(TYPE)) ( LLIST(TYPE)* dest, LLIST(TYPE)* src ) {
	LINK(TYPE)* n = src->head;

	while (n != NULL) {
		NEW(TYPE, cpy);
		DEEP_COPY(TYPE)(cpy, n->value);
		LLIST_CONS(TYPE) ( dest, cpy );
		n = n->after;
	}

	return 0;
}

#endif /* TYPE */
