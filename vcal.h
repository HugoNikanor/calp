#ifndef VCAL_H
#define VCAL_H

#include <stdlib.h>

#include "strbuf.h"

#define TYPE strbuf
#include "linked_list.h"
// #include "trie.h"
#undef TYPE

typedef struct {
	strbuf key;
	strbuf val;
} key_val;

typedef struct {
	strbuf key;

	LLIST(strbuf) vals;

	/* parcams NULL by default, since most content lines doesn't have
	 * any properties */
	LLIST(strbuf) params;
} content_line;

INIT_F(content_line);
INIT_F(content_line, int keylen, int vallen);
FREE_F(content_line);

int content_line_copy (content_line* dest, content_line* src);

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

FMT_F(content_line);
FMT_F(vcomponent);

#endif /* VCAL_H */
