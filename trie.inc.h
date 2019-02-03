#ifndef TYPE
#error "Set TYPE before including this file"
#else

#include "err.h"

int CONSTRUCTOR_DECL ( TRIE(TYPE) ) {
	NEW(TRIE_NODE(TYPE), t, '\0');
	this->root = t;
	return 0;
}

int CONSTRUCTOR_DECL (TRIE_NODE(TYPE), char c) {
	this->c = c;
	this->value = NULL;
	this->next  = NULL;
	this->child = NULL;
	return 0;
}

int CONSTRUCTOR_DECL (TRIE_NODE(TYPE),
	char c,
	TRIE_NODE(TYPE)* next,
	TRIE_NODE(TYPE)* child )
{
	this->c = c;
	this->next = next;
	this->child = child;
	return 0;
}

int TRIE_PUT(TYPE) ( TRIE(TYPE)* trie, char* key, TYPE* val ) {
	TRIE_NODE(TYPE) *cur, *last;

	last = trie->root;
	cur = last->child;

	char* subkey = key;
	// --------------------------------------------------

	while (1) {
		if (cur == NULL) {
			/* Build direct LL for remaining subkey */
			for (char* c = subkey; c[0] != '\0'; c++) {
				NEW(TRIE_NODE(TYPE), t, *c);
				last->child = t;
				last = t;
			}
			last->value = val;
			return 0;
		} else if (cur->c == subkey[0]) {
			/* This node belongs to the key,
			 * Decend further */
			last = cur;
			cur = cur->child;
			subkey++;
		} else if (subkey[0] == '\0') {
			/* Key finished */
			last->value = val;
			return 0;
		} else if (cur->next != NULL) {
			/* This node was not part of the set, but it's sibling might */
			cur = cur->next;
			/* `last` not set since we aren't moving down */ 
		} else {
			/* No node on this level was part of the set, create a new
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
TYPE* TRIE_GET(TYPE) ( TRIE(TYPE)* trie, char* key ) {
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

int TRIE_NODE_FREE(TYPE) ( TRIE_NODE(TYPE)* node ) {
	if (node == NULL) return 0;
	if (node->value != NULL) FFREE(TYPE, node->value);
	if (node->next  != NULL) TRIE_NODE_FREE(TYPE)(node->next);
	if (node->child != NULL) TRIE_NODE_FREE(TYPE)(node->child);
	free (node);
	return 0;
}

int TRIE_FREE(TYPE) ( TRIE(TYPE)* trie ) {
	if (trie->root->c != '\0') {
		// ERR("Invalid trie");
		return 1;
	}
	return TRIE_NODE_FREE(TYPE)(trie->root);
}

int TRIE_DOT(TYPE) ( TRIE(TYPE)* trie, FILE* f ) {
	TRIE_NODE(TYPE)* n = trie->root->child;
	fputs("root;", f);
	while (n != NULL) {
		fprintf(f, "root -> \"%p\"\n",
				(void*) n);
		fprintf(f, "subgraph \"cluster_%c\" {\n",
				n->c);
		TRIE_DOT_HELP(TYPE) ( n, f );
		fputs("}", f);
		n = n->next;
	}
	return 0;
}

int TRIE_DOT_HELP(TYPE) ( TRIE_NODE(TYPE)* root, FILE* f  ) {
	fprintf(f, "\"%p\"[label = \"%c\" style=filled fillcolor=\"%s\"];\n",
			(void*) root, root->c,
			root->value == NULL ? "white" : "green");
	TRIE_NODE(TYPE)* child = root->child;

	while (child != NULL) {
		fprintf(f, "\"%p\" -> \"%p\";\n",
				(void*) root, (void*) child);
		TRIE_DOT_HELP(TYPE)(child, f);
		child = child->next;
	}
	return 0;
}

#endif /* TYPE */
