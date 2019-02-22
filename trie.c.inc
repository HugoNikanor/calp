// #include "trie.h"
// #ifndef TYPE
// #error "Set TYPE before including this file"
// #else

#include <stdarg.h>

#include "err.h"
#include "macro.h"

// INIT_F ( TRIE(TYPE) ) {
template <class T>
trie<T>::trie () {
	// NEW(TRIE_NODE(TYPE), t, '\0');
	auto t = new trie_node<T>('\0');
	this->root = t;
}

// INIT_F (TRIE_NODE(TYPE), char c) {
template <class T>
trie_node<T>::trie_node (char c) {
	this->c = c;
	this->value = NULL;
	this->next  = NULL;
	this->child = NULL;
}

// INIT_F (TRIE_NODE(TYPE),
// 	char c,
// 	TRIE_NODE(TYPE)* next,
// 	TRIE_NODE(TYPE)* child )
template <class T>
trie_node<T>::trie_node (
		char c,
		trie_node<T>* next,
		trie_node<T>* child )
{
	this->c = c;
	this->next = next;
	this->child = child;
}

// int PUSH(TRIE(TYPE)) ( TRIE(TYPE)* trie, char* key, TYPE* val ) {
template <class T>
int trie<T>::push (char* key, T* val) {
	trie_node<T> *cur, *last;

	last = this->root;
	cur = last->child;

	char* subkey = key;

	while (1) {
		if (cur == NULL) {
			/* Build direct LL for remaining subkey */
			for (char* c = subkey; c[0] != '\0'; c++) {
				// NEW(TRIE_NODE(TYPE), t, *c);
				auto t = new trie_node<T>(*c);
				last->child = t;
				last = t;
			}
			// last->value = RESOLVE(TYPE)(last->value, val);
			last->value = last->value->resolve(val);
			return 0;
		} else if (cur->c == subkey[0]) {
			/* This node belongs to the key,
			 * Decend further */
			last = cur;
			cur = cur->child;
			subkey++;
		} else if (subkey[0] == '\0') {
			/* Key finished */
			// last->value = RESOLVE(TYPE)(last->value, val);
			last->value->resolve(val);
			return 0;
		} else if (cur->next != NULL) {
			/* This node was not part of the set, but it's sibling might */
			cur = cur->next;
			/* `last` not set since we aren't moving down */ 
		} else {
			/* No node on this level was part of the set, create a new__
			 * sibling and follow down that parse */
			// NEW(TRIE_NODE(TYPE), t, *subkey);
			auto t = new trie_node<T>(*subkey);
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
template <class T>
T* trie<T>::get (char* key) {
// TYPE* GET(TRIE(TYPE)) ( TRIE(TYPE)* trie, char* key ) {
	// TRIE_NODE(TYPE)* n = trie->root->child;
	trie_node<T>* n = this->root->child;
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

// FREE_F(TRIE_NODE(TYPE)) {
template <class T>
trie_node<T>::~trie_node () {
	// if (this == NULL) return 0;
	if (this->value != NULL) free (this->value); // FFREE(TYPE, this->value);
	if (this->next  != NULL) free (this->next);
	if (this->child != NULL) free (this->child);
	// free (this);
}

// FREE_F(TRIE(TYPE)) {
template <class T>
trie<T>::~trie () {
	if (this->root->c != '\0') {
		// ERR("Invalid trie");
		// return 1;
		return; // error
	}
	// free (this->root);
	delete this->root;
}

template <class T>
int trie<T>::empty () {
//int EMPTY(TRIE(TYPE))(TRIE(TYPE)* this) {
	return this->root->child == NULL;
}

#if 0
FMT_F(TRIE_NODE(TYPE)) {

	va_list ap;
	va_start(ap, buf);
	int argc = va_arg(ap, int);
	int depth = argc >= 1
		? va_arg(ap, int)
		: 0;
	va_end(ap);

	int seek = 0;

	TRIE_NODE(TYPE)* n = this;

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
	fmtf("Trie: %p: {\n", this);
	seek += FMT(TRIE_NODE(TYPE))(self->root->child, buf + seek);
	fmtf("}");
	return seek;
}

#endif /* TYPE */