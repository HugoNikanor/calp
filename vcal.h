#ifndef VCAL_H
#define VCAL_H

#include <string>
#include <utility>
#include <vector>
#include <list>
#include <iostream>

#include "trie.h"
#include "strbuf.h"
#include "linked_list.h"

// typedef std::pair<strbuf, llist<strbuf>      > param_set;
// typedef std::pair<strbuf, llist<param_set>   > content_set;
// typedef std::pair<strbuf, llist<strbuf> > content_line;
typedef llist<strbuf> content_line;

struct vcomponent {
	std::string filename;
	std::string type;
	vcomponent* parent = nullptr;
	trie<content_line> clines;
	std::vector<vcomponent> components;

	vcomponent(const std::string& type) : vcomponent(type, nullptr) { };

	vcomponent(const std::string& type, const std::string& filename);


	/*
	 * Resolves a collision in some form of structure (probably a hash-map
	 * or a trie). If dest is NULL just return new_. Otherwise mutates dest
	 * to have the correct form, and returns it. Destroying new_ in the
	 * process.
	 */
	vcomponent* operator= (vcomponent* other);

	content_line& operator[] (const char* key) {
		return this->clines[key];
	}

	void push_back(const vcomponent& child)
	{ this->components.push_back(child); }
};

std::ostream& operator<<(std::ostream&, vcomponent*);


#if 1
/*
 * Helper macros for accessing fields in
 * content_line, content_set, and param_set
 *
 * TODO find a better way to do self.
 */

/* ptr -> ptr */
#define CLINE_KEY(c) (&(c)->first)
#define CLINE_CUR_CSET(c) (&((c)->second.cur->value))

/* content_set */
#define CLINE_CUR(c)        ((c)->second.cur->value)
/* strbuf */
#define CLINE_CUR_VAL(c)    (& CLINE_CUR(c)->first)

	/* LLIST(param_set) */
#define CLINE_CUR_PARAMS(c) (& CLINE_CUR(c)->second)

	/* strbuf */
#define CLINE_CUR_PARAM_KEY(c) (CLINE_CUR_PARAMS(c)->cur->value->first)
	/* strbuf */
#define CLINE_CUR_PARAM_VAL(c) (CLINE_CUR_PARAMS(c)->cur->value->second.cur->value)
#endif

#endif /* VCAL_H */
