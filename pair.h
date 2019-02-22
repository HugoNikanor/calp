#ifndef PAIR_H
#define PAIR_H

// #define PAIR(T, V) TEMPL2(pair, T, V)

// #endif /* PAIR_H */
// #if defined(T) && defined(V)

template<class T, class V> struct pair {
	T* key;
	V* val;

	pair () { 
		key = new T;
		val = new V;
	}
	pair (pair<T,V>& other);

	T& operator= (const T& other) {
		delete this->key;
		delete this->val;

		*this->key = *other.key;
		*this->val = *other.val;

		return this;
	}

	pair* resolve (pair* other) {
		if (this == NULL) return other; 
		return this;
#if 0
/*
 * Resolves a collision in some form of structure (probably a hash-map
 * or a trie). If dest is NULL just return new_. Otherwise mutates dest
 * to have the correct form, and returns it. Destroying new_ in the
 * process.
 */
	if (dest == NULL) return new_;

	if (strbuf_cmp(dest->key, new_->key) != 0) {
		ERR("Can't resolve between these two types");
		return NULL;
	}

	/* This destroys new_->val. */
	// APPEND(LLIST(content_set)) (&dest->val, &new_->val);
	dest->val->append(new_->val);

	// FREE(strbuf)(&new_->key);
	// delete new_->key;
	delete new_;
	// free(new_);

	return dest;
#endif
	}

};

#if 0
INIT_F(PAIR(T, V));
FREE_F(PAIR(T, V));
FMT_F(PAIR(T, V));
#endif
// int DEEP_COPY(PAIR(T, V)) (PAIR(T, V)* dest, PAIR(T, V)* src);

#include "pair.inc.h"

#endif
