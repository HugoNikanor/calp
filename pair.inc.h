#if ! (defined(T) && defined(V))
#error "Both T and V must be defiend here"
#else

INIT_F(PAIR(T, V)) {
	INIT(T, &this->left);
	INIT(V, &this->right);

	return 0;
}

FREE_F(PAIR(T, V)) {
	FREE(T)(&this->left);
	FREE(V)(&this->right);
	return 0;
}

FMT_F(PAIR(T, V)) {
	char lbuf[100];
	char rbuf[100];
	FMT(T)(&this->left,  lbuf);
	FMT(V)(&this->right, rbuf);

	return sprintf(buf, "<%s, %s>", lbuf, rbuf);
}

int DEEP_COPY(PAIR(T, V)) (PAIR(T, V)* dest, PAIR(T, V)* src) {
	DEEP_COPY(T)(&dest->left,  &src->left);
	DEEP_COPY(V)(&dest->right, &src->right);
	return 0;
}

#endif /* T & V */
