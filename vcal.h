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

struct __param_set {
	strbuf key;
	llist<strbuf> values;

	__param_set (strbuf key) : key (key) { }
};

/*
 * A content set is a single instance of a content line, having a
 * specific value and it's own (possible) set of parameters. 
 */
struct __content_set {
	strbuf value;
	llist<__param_set> params;
};

/*
 * A content line is the collection of all lines which share the same
 * key.
 */
struct content_line {
	strbuf key;
	llist<__content_set> values;

	inline void push_param_key (strbuf key) {
		auto p = new __param_set(key);
		this->values.cur()->params.push(p);
   	}

	inline void push_param_value (strbuf* value)
		{ this->values.cur()->params.cur()->values.push(value); }

	inline strbuf* value() { return &this->values.cur()->value; }

};

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
#define CLINE_CUR(c)        ((c)->second.cur())
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
