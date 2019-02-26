#ifndef VCAL_H
#define VCAL_H

#include <stdlib.h>

#include <libguile.h>

#include "strbuf.h"

#define TYPE strbuf
#include "linked_list.h"
// #include "trie.h"
#undef TYPE

/*
 * content_line:
 *     a mapping between a top level key, and everything it contains.
 * content_set:
 *     A top level value, along with a list of kv pairs for all its
 *     possible parameters.
 * param_set:
 *     A parameter key, along with a list of all its values. 
 */

#define param_set LLIST(strbuf)

#define TYPE param_set
#include "trie.h"
#undef TYPE

#define T strbuf
	#define V TRIE(param_set)
		#include "pair.h"
		/* left := content | right := params */
		#define content_set PAIR(strbuf, TRIE(param_set))
	#undef V
#undef T

#define TYPE content_set
#include "linked_list.h"
#undef TYPE

#define content_line LLIST(content_set)

/*
 * Helper macros for accessing fields in
 * content_line, content_set, and param_set
 */

/* content_set */
#define CLINE_CUR(c)        ((c)->cur->value)

/* strbuf */
#define CLINE_CUR_VAL(c)    (& CLINE_CUR(c)->key)

/* TRIE(param_set) */
#define CLINE_CUR_PARAMS(c) (& CLINE_CUR(c)->val)

#define TYPE content_line
#include "trie.h"
#undef TYPE

typedef struct s_vcomponent vcomponent;

#define TYPE vcomponent
#include "vector.h"
#undef TYPE

struct s_vcomponent {
	char* filename;
	char* type;
	vcomponent* parent;
	TRIE(content_line) clines;
	VECT(vcomponent) components;

	SCM scm;
};

#define FCHILD(v) GET(VECT(vcomponent))(&(v)->components, 0)

INIT_F(vcomponent);
INIT_F(vcomponent, const char* type);
INIT_F(vcomponent, const char* type, const char* filename);
FREE_F(vcomponent);

content_line* get_property (vcomponent* ev, const char* key);

int add_content_line (vcomponent* ev, content_line* c);

/*
 * Appends ev to cal. Doesn't copy ev. So make sure that it wont go
 * out of scope.
 */
int PUSH(vcomponent)(vcomponent*, vcomponent*);

int DEEP_COPY(vcomponent)(vcomponent*, vcomponent*);

FMT_F(vcomponent);

#endif /* VCAL_H */
