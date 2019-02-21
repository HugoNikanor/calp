// #if ! (defined(T) && defined(V))
// #error "Both T and V must be defiend here"
// #else

#if 0
INIT_F(PAIR(T, V)) {
	INIT(T, &self->key);
	INIT(V, &self->val);

	return 0;
}

FREE_F(PAIR(T, V)) {
	FREE(T)(&self->key);
	FREE(V)(&self->val);

	return 0;
}

FMT_F(PAIR(T, V)) {
	char lbuf[0x100];
	char rbuf[0x1000];
	FMT(T)(&self->key, lbuf);
	FMT(V)(&self->val, rbuf);

	return sprintf(buf, "<%s, %s>", lbuf, rbuf);
}
#endif

// int DEEP_COPY(PAIR(T, V)) (PAIR(T, V)* dest, PAIR(T, V)* src) {
template <class T, class V>
pair<T, V>::pair (pair<T,V>& other) {
	this->key = new T(*other.key);
	this->val = new V(*other.val);
	// DEEP_COPY(T)(&dest->key, &src->key);
	// DEEP_COPY(V)(&dest->val, &src->val);
	// return 0;
}

// #endif /* T & V */
