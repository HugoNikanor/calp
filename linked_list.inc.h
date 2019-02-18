FREE_F( LLIST(TYPE) ) {
	LINK(TYPE) *n, *next;
	n = self->head;
	while ( n != NULL ) {
		next = n->after;
		FFREE(LINK(TYPE), n);
		n = next;
	}

	self->length = -1;

	return 0;
}

INIT_F ( LINK(TYPE) ) {
	self->before = NULL;
	self->after  = NULL;
	self->value  = NULL;
	return 0;
}

INIT_F ( LINK(TYPE), TYPE* val ) {
	self->before = NULL;
	self->after  = NULL;
	self->value  = val;
	return 0;
}

int UNLINK(LINK(TYPE)) ( LINK(TYPE)* self ) {
	if (self->before != NULL) self->before->after = self->after;
	if (self->after  != NULL) self->after->before = self->before;
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

TYPE* PEEK(LLIST(TYPE)) ( LLIST(TYPE)* lst ) {
	if (EMPTY(LLIST(TYPE))(lst)) return NULL;

	return FIRST(lst)->value;
}

TYPE* POP(LLIST(TYPE)) ( LLIST(TYPE)* lst) {
	if (EMPTY(LLIST(TYPE))(lst)) return NULL;

	LINK(TYPE)* frst = FIRST(lst);
	UNLINK(LINK(TYPE))(frst);

	TYPE* retval = frst->value;
	--lst->length;
	free (frst);
	return retval;
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
 * destroys new__ in the process, but keeps the elements.
 * make sure to free(new__) after.
 */
int APPEND(LLIST(TYPE)) ( LLIST(TYPE)* dest, LLIST(TYPE)* new__ ) {

	/* Link end of dest onto start of new__. */
	LAST(dest)->after  = FIRST(new__);
	FIRST(new__)->before = LAST(dest);

	/* Free the two now not needed end links.  */
	free(new__->head);
	free(dest->tail);

	/* Update dest with new__ tail ptr. */
	dest->tail = new__->tail;

	dest->length += new__->length;

	return 0;
}

int SIZE(LLIST(TYPE)) ( LLIST(TYPE)* llist ) {
	return llist->length;
}

int EMPTY(LLIST(TYPE)) ( LLIST(TYPE)* llist ) {
	return FIRST(llist) == llist->tail;
}

LLIST(TYPE)* RESOLVE(LLIST(TYPE)) (LLIST(TYPE)* dest, LLIST(TYPE)* new__) {
	if (dest == NULL) return new__;
	APPEND(LLIST(TYPE))(dest, new__);
	return dest;
}

int RESET(LLIST(TYPE)) ( LLIST(TYPE)* llist ) {

	LINK(TYPE) *link = FIRST(llist), *next;
	/*
	 * Manual looping rather than itterators since we destroyed the
	 * loop variable.
	 */
	while (link != llist->tail) {
		next = link->after;
		FFREE(LINK(TYPE), link);
		link = next;
	}

	llist->cur = llist->head;

	return 0;
}

FMT_F(LLIST(TYPE)) {
	int seek = 0;
	fmtf("(");
	FOR(LLIST, TYPE, v, self) {
		seek += FMT(TYPE)(v, buf + seek);
		fmtf(" ");
	}
	fmtf(")");

	return seek;
}

#endif /* TYPE */
