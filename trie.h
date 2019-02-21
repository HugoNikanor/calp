#ifndef TRIE_H
#define TRIE_H

#include <stdio.h>

#include "macro.h"

// #define TRIE(T)      TEMPL(trie, T)
// #define TRIE_NODE(T) TEMPL(trie_node, T)

// #endif /* TRIE_H */
// #ifdef TYPE

template<class T> struct trie_node {
	char c;
	T* value;
	trie_node* next;
	trie_node* child;

	trie_node (char c);
	trie_node (char c, trie_node* next, trie_node* child);

	~trie_node ();
};

template<class T> struct trie {
	trie_node<T>* root;

	trie ();
	~trie();

	int push (char* key, T* val);
	T* get (char* key);

	int empty ();
};

// FMT_F(TRIE_NODE(TYPE));
// FMT_F(TRIE(TYPE));

extern template struct trie<content_line>;
extern template struct trie_node<content_line>;

#include "trie.c.inc"

#endif /* TYPE */
