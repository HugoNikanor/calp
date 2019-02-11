#if ! (defined(T) && defined(V))
#error "Both T and V must be defiend here"
#else

INIT_F(PAIR(T, V)) {
	INIT(T, &this->key);
	INIT(V, &this->val);

	return 0;
}

FREE_F(PAIR(T, V)) {
	FREE(T)(&this->key);
	FREE(V)(&this->val);

	return 0;
}

FMT_F(PAIR(T, V)) {
	char lbuf[0x100];
	char rbuf[0x1000];
	FMT(T)(&this->key, lbuf);
	FMT(V)(&this->val, rbuf);

	return sprintf(buf, "<%s, %s>", lbuf, rbuf);
}

int DEEP_COPY(PAIR(T, V)) (PAIR(T, V)* dest, PAIR(T, V)* src) {
	DEEP_COPY(T)(&dest->key,  &src->key);
	DEEP_COPY(V)(&dest->val, &src->val);
	return 0;
}

#endif /* T & V */
