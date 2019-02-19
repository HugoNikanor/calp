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
	std::list<strbuf> values;

	__param_set (strbuf key) : key (key) { this->key.cap(); }

	void push_value (strbuf val) {
		this->values.push_back(val);
		this->values.back().cap();
	}
};

/*
 * A content set is a single instance of a content line, having a
 * specific value and it's own (possible) set of parameters. 
 */
struct __content_set {
	strbuf value;
	std::list<__param_set*> params;

	__content_set (strbuf value) : value(value) { this->value.cap(); }

	void push_param_key (strbuf key) {
		this->params.push_back (new __param_set(key));
	}

	void push_param_value (strbuf val) {
		this->params.back()->push_value(val);
	}
};

/*
 * A content line is the collection of all lines which share the same
 * key.
 */
struct content_line {
	strbuf key;
	// llist<__content_set> values;
	std::list<__content_set*> values;

	void push_value (strbuf str) {
		this->values.push_back(new __content_set(str));
	}
};

struct vcomponent {
	std::string filename;
	std::string type;
	vcomponent* parent = nullptr;
	trie<content_line> clines;
	std::vector<vcomponent> components;

	vcomponent(const std::string& type) : vcomponent(type, nullptr) { };

	vcomponent(const std::string& type, const std::string& filename);

	void add_content_line (content_line* c) {
		clines.push(c->key.c_str(), c);
	}

	/*
	 * Resolves a collision in some form of structure (probably a hash-map
	 * or a trie). If dest is NULL just return new_. Otherwise mutates dest
	 * to have the correct form, and returns it. Destroying new_ in the
	 * process.
	 */
	vcomponent* operator= (vcomponent* other);

	content_line* operator[] (const char* key)
		{ return this->clines[key]; }

	void push_back(const vcomponent& child)
		{ this->components.push_back(child); }
};

std::ostream& operator<<(std::ostream&, vcomponent*);


#if 0
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
