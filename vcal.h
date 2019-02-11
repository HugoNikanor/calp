#ifndef VCAL_H
#define VCAL_H

#include <stdlib.h>

#include "strbuf.h"

#define TYPE strbuf
#include "linked_list.h"
// #include "trie.h"
#undef TYPE

#define T strbuf
	#define V LLIST(strbuf)
		#include "pair.h"
		/* left := param_name | right := param_values */
		#define param_set PAIR(strbuf, LLIST(strbuf))
	#undef V
#undef T

#define TYPE param_set
#include "linked_list.h"
#undef TYPE

#define T strbuf
	#define V LLIST(param_set)
		#include "pair.h"
		/* left := content | right := params */
		#define content_set PAIR(strbuf, LLIST(param_set))
	#undef V
#undef T

#define TYPE content_set
#include "linked_list.h"
#undef TYPE

#define T strbuf
	#define V LLIST(content_set)
		#include "pair.h"
		/* left := content | right := params */
		#define content_line PAIR(strbuf, LLIST(content_set))
	#undef V
#undef T

/*
 * Helper macros for accessing fields in
 * content_line, content_set, and param_set
 *
 * TODO find a better way to do this.
 */

/* ptr -> ptr */
#define CLINE_KEY(c) (&(c)->key)
#define CLINE_CUR_CSET(c) (&((c)->val.cur->value))

/* content_set */
#define CLINE_CUR(c)        ((c)->val.cur->value)
/* strbuf */
#define CLINE_CUR_VAL(c)    (& CLINE_CUR(c)->key)

/* LLIST(param_set) */
#define CLINE_CUR_PARAMS(c) (& CLINE_CUR(c)->val)

/* strbuf */
#define CLINE_CUR_PARAM_KEY(c) (CLINE_CUR_PARAMS(c)->cur->value->key)
/* strbuf */
#define CLINE_CUR_PARAM_VAL(c) (CLINE_CUR_PARAMS(c)->cur->value->val.cur->value)

/*
 * Resolves a collision in some form of structure (probably a hash-map
 * or a trie). If dest is NULL just return new. Otherwise mutates dest
 * to have the correct form, and returns it. Destroying new in the
 * process.
 */
content_line* RESOLVE(content_line)
	(content_line* dest, content_line* new);

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
};

#define FCHILD(v) GET(VECT(vcomponent))(&(v)->components, 0)

INIT_F(vcomponent);
INIT_F(vcomponent, char* type);
INIT_F(vcomponent, char* type, char* filename);
FREE_F(vcomponent);

content_line* get_property (vcomponent* ev, char* key);

int add_content_line (vcomponent* ev, content_line* c);

/*
 * Appends ev to cal. Doesn't copy ev. So make sure that it wont go
 * out of scope.
 */
int PUSH(vcomponent)(vcomponent*, vcomponent*);

int DEEP_COPY(vcomponent)(vcomponent*, vcomponent*);

FMT_F(vcomponent);

#endif /* VCAL_H */
