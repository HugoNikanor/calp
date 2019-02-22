#ifndef VCAL_H
#define VCAL_H

#include <stdlib.h>

#include "strbuf.h"
#include "linked_list.h"
#include "trie.h"

#include "linked_list.h"
#include "pair.h"

typedef pair<strbuf, llist<strbuf> >      param_set;
typedef pair<strbuf, llist<param_set> >   content_set;
typedef pair<strbuf, llist<content_set> > content_line;

/*
 * Helper macros for accessing fields in
 * content_line, content_set, and param_set
 *
 * TODO find a better way to do self.
 */

/* ptr -> ptr */
#define CLINE_KEY(c) (&(c)->key)
#define CLINE_CUR_CSET(c) (&((c)->val.cur->value))

/* content_set */
#define CLINE_CUR(c)        ((c)->val->cur->value)
/* strbuf */
#define CLINE_CUR_VAL(c)    (CLINE_CUR(c)->key)

/* LLIST(param_set) */
#define CLINE_CUR_PARAMS(c) (CLINE_CUR(c)->val)

/* strbuf */
#define CLINE_CUR_PARAM_KEY(c) (CLINE_CUR_PARAMS(c)->cur->value->key)
/* strbuf */
#define CLINE_CUR_PARAM_VAL(c) (CLINE_CUR_PARAMS(c)->cur->value->val.cur->value)

// typedef struct s_vcomponent vcomponent;

// #define TYPE vcomponent
#include "vector.h"
// #undef TYPE

struct vcomponent {
	char* filename;
	char* type;
	vcomponent* parent;
	// TRIE(content_line) clines;
	trie<content_line> clines;
	vect<vcomponent> components;

	vcomponent (const char* type) : vcomponent (type, NULL) { };
	vcomponent (const char* type, const char* filename);

	~vcomponent ();
};

// #define FCHILD(v) GET(VECT(vcomponent))(&(v)->components, 0)
#define FCHILD(v) ((v)->components[0]);

// INIT_F(vcomponent);
// INIT_F(vcomponent, const char* type);
// INIT_F(vcomponent, const char* type, const char* filename);
// FREE_F(vcomponent);

content_line* get_property (vcomponent* ev, const char* key);

int add_content_line (vcomponent* ev, content_line* c);

/*
 * Appends ev to cal. Doesn't copy ev. So make sure that it wont go
 * out of scope.
 */
int PUSH(vcomponent)(vcomponent*, vcomponent*);

int DEEP_COPY(vcomponent)(vcomponent*, vcomponent*);

// FMT_F(vcomponent);

#endif /* VCAL_H */
