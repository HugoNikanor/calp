#ifndef TYPE
#error "Set TYPE before including self file"
#else

#include <stdarg.h>

#include "err.h"
#include "macro.h"
#include "linked_list.inc.h"
#include "strbuf.h"

INIT_F ( TRIE(TYPE) ) {
	NEW(TRIE_NODE(TYPE), t, '\0');
	self->root = t;
	return 0;
}

INIT_F (TRIE_NODE(TYPE), char c) {
	self->c = c;
	self->value = NULL;
	self->next  = NULL;
	self->child = NULL;
	return 0;
}

INIT_F (TRIE_NODE(TYPE),
	char c,
	TRIE_NODE(TYPE)* next,
	TRIE_NODE(TYPE)* child )
{
	self->c = c;
	self->next = next;
	self->child = child;
	return 0;
}

int PUSH(TRIE(TYPE)) ( TRIE(TYPE)* trie, char* key, TYPE* val ) {
	TRIE_NODE(TYPE) *cur, *last;

	last = trie->root;
	cur = last->child;

	char* subkey = key;

	while (1) {
		if (cur == NULL) {
			/* Build direct LL for remaining subkey */
			for (char* c = subkey; c[0] != '\0'; c++) {
				NEW(TRIE_NODE(TYPE), t, *c);
				last->child = t;
				last = t;
			}
			last->value = RESOLVE(TYPE)(last->value, val);
			return 0;
		} else if (cur->c == subkey[0]) {
			/* This node belongs to the key,
			 * Decend further */
			last = cur;
			cur = cur->child;
			subkey++;
		} else if (subkey[0] == '\0') {
			/* Key finished */
			last->value = RESOLVE(TYPE)(last->value, val);
			return 0;
		} else if (cur->next != NULL) {
			/* This node was not part of the set, but it's sibling might */
			cur = cur->next;
			/* `last` not set since we aren't moving down */ 
		} else {
			/* No node on self level was part of the set, create a new__
			 * sibling and follow down that parse */
			NEW(TRIE_NODE(TYPE), t, *subkey);
			cur->next = t;
			last = cur;
			cur = t;
		}
	}

	return 0;
}

/*
 * TODO what happens when I give an invalid key?
 */
TYPE* GET(TRIE(TYPE)) ( TRIE(TYPE)* trie, char* key ) {
	TRIE_NODE(TYPE)* n = trie->root->child;
	char* subkey = key;

	while (n != NULL) {
		if (subkey[1] == '\0') {
			return n->value;
		} else if (subkey[0] == n->c) {
			n = n->child;
			subkey++;
		} else {
			n = n->next;
		}
	}

	ERR("Position not found");
	return 0;
}

FREE_F(TRIE_NODE(TYPE)) {
	if (self == NULL) return 0;
	if (self->value != NULL) FFREE(TYPE, self->value);
	if (self->next  != NULL) FREE(TRIE_NODE(TYPE))(self->next);
	if (self->child != NULL) FREE(TRIE_NODE(TYPE))(self->child);
	free (self);
	return 0;
}

FREE_F(TRIE(TYPE)) {
	if (self->root->c != '\0') {
		// ERR("Invalid trie");
		return 1;
	}
	return FREE(TRIE_NODE(TYPE))(self->root);
}

int EMPTY(TRIE(TYPE))(TRIE(TYPE)* self) {
	return self->root->child == NULL;
}

FMT_F(TRIE_NODE(TYPE)) {

	va_list ap;
	va_start(ap, buf);
	int argc = va_arg(ap, int);
	int depth = argc >= 1
		? va_arg(ap, int)
		: 0;
	va_end(ap);

	int seek = 0;

	TRIE_NODE(TYPE)* n = self;

	if (n == NULL) { fmtf("\n"); }
	while (n != NULL) {
		fmtf("|");
		// FOR(int, i, depth) fmtf(" ");
		for (int i = 0; i < depth; i++) fmtf(" ");
		fmtf("%c ", n->c == '\0' ? '0' : n->c);
		if (n->value != NULL) {
			seek += FMT(TYPE)(n->value, buf + seek);
		   	fmtf("\n");
		}

		if (n->child != NULL) {
			fmtf("\n");
			seek += FMT(TRIE_NODE(TYPE))(n->child, buf + seek, depth + 1);
		}
		n = n->next;
	}
	return seek;

}

FMT_F(TRIE(TYPE)) {
	int seek = 0;
	fmtf("Trie: %p: {", self);
	if (EMPTY(TRIE(TYPE))(self)) {
		fmtf(" [EMPTY] ");
	} else {
		fmtf("\n");
		seek += FMT(TRIE_NODE(TYPE))(self->root->child, buf + seek);
	}
	fmtf("}");
	return seek;
}

int DEEP_COPY(TRIE_NODE(TYPE)) (TRIE_NODE(TYPE)* dest, TRIE_NODE(TYPE)* src) {
	dest->c = src->c;

	if (src->value != NULL) {
		RENEW(TYPE, dest->value);
		DEEP_COPY(TYPE)(dest->value, src->value);
	}

	if (src->next != NULL) {
		RENEW(TRIE_NODE(TYPE), dest->next, '\0');
		DEEP_COPY(TRIE_NODE(TYPE))(dest->next, src->next);
	}

	if (src->child != NULL) {
		RENEW(TRIE_NODE(TYPE), dest->child, '\0');
		DEEP_COPY(TRIE_NODE(TYPE))(dest->child, src->child);
	}

	return 0;
}

int DEEP_COPY(TRIE(TYPE)) (TRIE(TYPE)* dest, TRIE(TYPE)* src) {
	return DEEP_COPY(TRIE_NODE(TYPE))(dest->root, src->root);
}

void KEYS(TRIE_NODE(TYPE)) (TRIE_NODE(TYPE)* node, LLIST(strbuf)* list, strbuf* path) {
	if (node == NULL) return;


	if (node->value != NULL) {
		strbuf_append(path, node->c);
		NEW(strbuf, c);
		DEEP_COPY(strbuf)(c, path);
		PUSH(LLIST(strbuf))(list, c);
		strbuf_pop(path);
	}
	if (node->next != NULL) {
		KEYS(TRIE_NODE(TYPE)) (node->next, list, path);
	}
	if (node->child != NULL) {
		if (node->c != '\0') strbuf_append(path, node->c);
		KEYS(TRIE_NODE(TYPE)) (node->child, list, path);
		if (node->c != '\0') strbuf_pop(path);
	}
}

LLIST(strbuf)* KEYS(TRIE(TYPE)) (TRIE(TYPE)* trie) {
	NEW(LLIST(strbuf), retlist);
	SNEW(strbuf, key);
	KEYS(TRIE_NODE(TYPE)) (trie->root, retlist, &key);
	return retlist;
}

#endif /* TYPE */
