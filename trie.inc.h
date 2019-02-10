#ifndef TYPE
#error "Set TYPE before including this file"
#else

#include <stdarg.h>

#include "err.h"

INIT_F ( TRIE(TYPE) ) {
	NEW(TRIE_NODE(TYPE), t, '\0');
	this->root = t;
	return 0;
}

INIT_F (TRIE_NODE(TYPE), char c) {
	this->c = c;
	this->value = NULL;
	this->next  = NULL;
	this->child = NULL;
	return 0;
}

INIT_F (TRIE_NODE(TYPE),
	char c,
	TRIE_NODE(TYPE)* next,
	TRIE_NODE(TYPE)* child )
{
	this->c = c;
	this->next = next;
	this->child = child;
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
	if (this == NULL) return 0;
	if (this->value != NULL) FFREE(TYPE, this->value);
	if (this->next  != NULL) FREE(TRIE_NODE(TYPE))(this->next);
	if (this->child != NULL) FREE(TRIE_NODE(TYPE))(this->child);
	free (this);
	return 0;
}

FREE_F(TRIE(TYPE)) {
	if (this->root->c != '\0') {
		// ERR("Invalid trie");
		return 1;
	}
	return FREE(TRIE_NODE(TYPE))(this->root);
}

int EMPTY(TRIE(TYPE))(TRIE(TYPE)* this) {
	return this->root->child == NULL;
}

int TRIE_DOT(TYPE) ( TRIE(TYPE)* trie, FILE* f ) {
	TRIE_NODE(TYPE)* n = trie->root->child;
	fprintf(f, "\"%p\" [label=root fontcolor=gray, color=gray];", trie);
	while (n != NULL) {
		fprintf(f, "\"%p\" -> \"%p\" [color=gray]\n",
				(void*) trie,
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

	// ----------------------------------------
	if (root->value != NULL) {
		if (! EMPTY(LLIST(key_val))(&root->value->params)) {
			fprintf(f, "subgraph \"cluster_param_%p\"{\n	color=blue;\n", root);
			FOR(LLIST(key_val), link, &root->value->params) {
				fprintf(f, "\"%p\"  [label=\"%s := %s\"];", link,
						link->value->key.mem,
						link->value->val.mem);
			}
			fputs("}", f);
			FOR(LLIST(key_val), link, &root->value->params) {
				fprintf(f, "\"%p\" -> \"%p\";\n", root, link);
			}
		}
	}
	// ----------------------------------------

	while (child != NULL) {
		fprintf(f, "\"%p\" -> \"%p\";\n",
				(void*) root, (void*) child);
		TRIE_DOT_HELP(TYPE)(child, f);
		child = child->next;
	}
	return 0;
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

	TRIE_NODE(TYPE)* n = this;

	if (n == NULL) { fmtf("\n"); }
	while (n != NULL) {
		fmtf("|");
		FOR(int, i, depth) fmtf(" ");
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
	seek += FMT(TRIE_NODE(TYPE))(this->root->child, buf + seek);
	fmtf("}");
	return seek;
}

#endif /* TYPE */
