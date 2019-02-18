#include "trie.h"

template <class T>
trie<T>::trie () {
	this->root = new trie_node<T> ('\0');
}

template <class T>
trie_node<T>::trie_node (char c) {
	this->c = c;
}

template <class T>
trie_node<T>::trie_node (char c, trie_node<T>* next, trie_node<T>* child) {
	this->c = c;
	this->next = next;
	this->child = child;
}

template <class T>
int trie<T>::push_back (const char* key, const T& item) {
	trie_node<T> *cur, *last;

	last = this->root;
	cur = last->child;

	char* subkey = key;

	while (true) {
		if (cur == NULL) {
			/* Build direct LL for remaining subkey */
			for (char* c = subkey; c[0] != '\0'; c++) {
				// NEW(TRIE_NODE(TYPE), t, *c);
				auto t = new trie_node<T>(*c);
				last->child = t;
				last = t;
			}
			// TODO fix resolve
			// last->value = RESOLVE(TYPE)(last->value, val);
			last->value = item;
			return 0;
		} else if (cur->c == subkey[0]) {
			/* This node belongs to the key,
			 * Decend further */
			last = cur;
			cur = cur->child;
			subkey++;
		} else if (subkey[0] == '\0') {
			/* Key finished */
			// TODO fix resolve
			// last->value = RESOLVE(TYPE)(last->value, val);
			last->value = item;
			return 0;
		} else if (cur->next != NULL) {
			/* This node was not part of the set, but it's sibling might */
			cur = cur->next;
			/* `last` not set since we aren't moving down */ 
		} else {
			/* No node on self level was part of the set, create a new__
			 * sibling and follow down that parse */
			// NEW(TRIE_NODE(TYPE), t, *subkey);
			auto t = new trie_node<T>(*subkey);
			cur->next = t;
			last = cur;
			cur = t;
		}
	}
}

template <class T>
T& trie<T>::operator[] (char* key) {
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

	// ERR("Position not found");
	// return 0;
	return nullptr;
}

template <class T>
std::ostream& operator<<(std::ostream& o, trie_node<T>* node) {
#if 0
	va_list ap;
	va_start(ap, buf);
	int argc = va_arg(ap, int);
	int depth = argc >= 1
		? va_arg(ap, int)
		: 0;
	va_end(ap);

	int seek = 0;
#endif
	// TODO figure out depth
	int depth = 1;

	trie_node<T>* n = node;


	if (n == NULL) { o << std::endl; }
	while (n != NULL) {
		o << '|';
		for (int i = 0; i < depth; i++) o << ' ';
		o << (n->c == '\0' ? '0' : n->c);
		if (n->value != NULL) {
			o << n->value << std::endl;
		}

		if (n->child != NULL) {
			o << std::endl;
			o << n->child; // depth + 1
		}
		n = n->next;
	}
	return o;
}

template <class T>
std::ostream& operator<<(std::ostream& o, trie<T>* trie) {
	o << "Trie: " << trie << " {\n";
	o << trie->root->child;
	o << "}";
	return o;
}
