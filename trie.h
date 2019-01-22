#ifndef TRIE_H
#define TRIE_H

#include "macro.h"

#define TRIE(T) TP(trie__, T)
#define TRIE_NODE(T) TP(trie_node__, T)

#define TRIE_INIT(T)      CONSTRUCTOR_GEN(trie, T, 0)
#define TRIE_NODE_INIT(T) CONSTRUCTOR_GEN(trie_node, T, 0)

#define TRIE_FREE(T) TP(trie_free__, T)
#define TRIE_NODE_FREE(T) TP(trie_node_free__, T)

#define TRIE_PUT(T) TP(trie_put__, T)
#define TRIE_GET(T) TP(trie_get__, T)

#endif /* TRIE_H */
#ifdef TYPE

typedef struct TRIE_NODE(TYPE) {
	char c;
	TYPE* value;
	struct TRIE_NODE(TYPE)* next;
	/* child == NULL means leaf? */
	struct TRIE_NODE(TYPE)* child;
} TRIE_NODE(TYPE);

typedef struct {
	TRIE_NODE(TYPE)* root;
} TRIE(TYPE);


int CONSTRUCTOR_DECL ( TRIE(TYPE) );

int CONSTRUCTOR_DECL (TRIE_NODE(TYPE), char c);

int CONSTRUCTOR_DECL (TRIE_NODE(TYPE), 
	  char c, TRIE_NODE(TYPE)* next, TRIE_NODE(TYPE)* child );

int TRIE_PUT(TYPE) ( TRIE(TYPE)* trie, char* key, TYPE* val );

TYPE* TRIE_GET(TYPE) ( TRIE(TYPE)* trie, char* key );

int TRIE_NODE_FREE(TYPE) ( TRIE_NODE(TYPE)* node );

int TRIE_FREE(TYPE) ( TRIE(TYPE)* trie );

#endif /* TYPE */
