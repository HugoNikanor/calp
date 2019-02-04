#ifndef TRIE_H
#define TRIE_H

#include <stdio.h>

#include "macro.h"

#define TRIE(T) TP(trie__, T)
#define TRIE_NODE(T) TP(trie_node__, T)

#define TRIE_PUT(T) TP(trie_put__, T)
#define TRIE_GET(T) TP(trie_get__, T)

#define TRIE_DOT(T) TP(trie_to_dot__, T)
#define TRIE_DOT_HELP(T) TP(trie_to_dot_helper__, T)

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

int TRIE_PUT(TYPE) ( TRIE(TYPE)* trie, char* key, TYPE* val );

TYPE* TRIE_GET(TYPE) ( TRIE(TYPE)* trie, char* key );

// int TRIE_NODE_FREE(TYPE) ( TRIE_NODE(TYPE)* node );
FREE_F(TRIE_NODE(TYPE));

// int TRIE_FREE(TYPE) ( TRIE(TYPE)* trie );
FREE_F(TRIE(TYPE));

int TRIE_DOT(TYPE) (  TRIE(TYPE)*, FILE* );
int TRIE_DOT_HELP(TYPE) ( TRIE_NODE(TYPE)*, FILE* );

#endif /* TYPE */
