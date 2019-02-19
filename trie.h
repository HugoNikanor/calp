#ifndef TRIE_H
#define TRIE_H

#include <iostream>

template <typename T>
struct trie_node {
	char c;
	T* value = NULL;
	trie_node<T>* next  = nullptr;
	trie_node<T>* child = nullptr;

	trie_node (char c);
	trie_node (char c, trie_node<T>* next, trie_node<T>* child);
};

template <class T>
std::ostream& operator<<(std::ostream&, trie_node<T>* node);

template <typename T>
struct trie {
	trie_node<T>* root;

	trie ();

	int push_back (const char* key, const T&);

	T& operator[] ( const char* key );

	bool empty () { return this->root->child == NULL; }
};

template <class T>
std::ostream& operator<<(std::ostream&, trie<T>* trie);

#endif /* TRIE_H */
