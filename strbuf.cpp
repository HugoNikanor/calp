#include "strbuf.h"

#include <cstdlib>

strbuf::strbuf (const strbuf& other) {
	this->alloc = other.len + 1;
	this->mem = static_cast<char*>(malloc(this->alloc));
	strncpy(this->mem, other.mem, other.len);
}

void strbuf::realloc (size_t len) {
	this->mem = static_cast<char*>(std::realloc(this->mem, len));
	this->alloc = len;
}

strbuf::~strbuf() {
	free (this->mem);
	this->mem = NULL;
	this->alloc = 0;
	this->len = 0;
}

strbuf& strbuf::operator+=(char c) {
	if (this->len + 1 > this->alloc) {
		this->alloc <<= 1;
		this->mem = static_cast<char*> (std::realloc(this->mem, this->alloc));
	}

	this->mem[this->len] = c;
	this->ptr = ++this->len;
	return *this;
}
