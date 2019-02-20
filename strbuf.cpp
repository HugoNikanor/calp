#include "strbuf.h"

#include <cstdlib>
#include <cstring>

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

bool strbuf::operator==(strbuf& other) {
	return strncmp(this->mem, other.mem, this->len) == 0;
}

bool strbuf::operator==(const char* other) {
	std::cerr << __FILE__ << ':' << __LINE__ << ' ' << this->c_str() << "==" << other << std::endl;
	return strncmp(this->mem, other, this->len) == 0 ;
}

std::ostream& operator << (std::ostream& out, strbuf& str) {
	out << (str.to_string());
	return out;
}

// TODO this leaks memmory
char* strbuf::c_str() {
	char* buf = static_cast<char*>(malloc(this->len + 1));
	memcpy(buf, this->mem, this->len);
	buf[this->len] = '\0';
	return buf;
}
