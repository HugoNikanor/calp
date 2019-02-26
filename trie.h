#ifndef TRIE_H
#define TRIE_H

#include <stdio.h>

#include "macro.h"

#define TRIE(T)      TEMPL(trie, T)
#define TRIE_NODE(T) TEMPL(trie_node, T)

#endif /* TRIE_H */
#ifdef TYPE

typedef struct TRIE_NODE(TYPE) {
	char c;
	TYPE* value;
	struct TRIE_NODE(TYPE)* next;
	struct TRIE_NODE(TYPE)* child;
} TRIE_NODE(TYPE);

typedef struct {
	TRIE_NODE(TYPE)* root;
} TRIE(TYPE);


INIT_F ( TRIE(TYPE) );

INIT_F (TRIE_NODE(TYPE), char c);

INIT_F (TRIE_NODE(TYPE), 
	  char c, TRIE_NODE(TYPE)* next, TRIE_NODE(TYPE)* child );

int PUSH(TRIE(TYPE)) ( TRIE(TYPE)* trie, char* key, TYPE* val );

TYPE* GET(TRIE(TYPE)) ( TRIE(TYPE)* trie, char* key );

FREE_F(TRIE_NODE(TYPE));

FREE_F(TRIE(TYPE));

int EMPTY(TRIE(TYPE))(TRIE(TYPE)*);

FMT_F(TRIE_NODE(TYPE));
FMT_F(TRIE(TYPE));

int DEEP_COPY(TRIE_NODE(TYPE)) (TRIE_NODE(TYPE)* dest, TRIE_NODE(TYPE)* src);
int DEEP_COPY(TRIE(TYPE)) (TRIE(TYPE)* dest, TRIE(TYPE)* src);

#endif /* TYPE */
