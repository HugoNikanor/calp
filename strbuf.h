#ifndef STRBUF_H
#define STRBUF_H

#include <unistd.h>
#include <cstdlib>
#include <cstring>
#include <string>

/*
 * A high level string type which holds it's own length, how much
 * memmory it has allocated for itself, and a seek pointer into the
 * string.
 *
 * Also comes with a number of functions which allow for safe(er)
 * access to the memmory.
 */
struct strbuf {
	char* mem;
	/* TODO add support for negative ptr */
	int ptr = 0;
	size_t alloc;
	size_t len   = 0;

	strbuf () : strbuf (1) { };
	strbuf (size_t alloc)
		: alloc(alloc)
		, mem(static_cast<char*>(malloc(alloc))) { };

	~strbuf();

	/*
	 * Like realloc, but for strbuf
	 */
	void realloc (size_t len);

	bool operator==(strbuf& other)
		{ return strncmp(this->mem, other.mem, this->len) == 0; }

	bool operator==(const char* other)
		{ return strncmp(this->mem, other, this->len) == 0 ; }

	strbuf& operator=(strbuf* other);

	strbuf& operator+=(char c);

	void cap() { this->mem += '\0'; }

	/*
	 * Returns a pointer to character at index. Allows mutation of the
	 * value pointed to by the return address.
	 */
	char& operator[](int idx) 
		{ return this->mem[idx]; }

	/* Same as `charat`, But returns the current character.  */
	char& strbuf_cur()
		{ return this->mem[this->ptr]; }

	/* Returns the character after the last, so where null hopefully is.  */
	char& back()
		{ return this->mem[this->len]; }

	/* Resets the seek for strbuf to 0.  */
	void reset();

	/* Sets the length and seek ptr to 0, but doesn't touch the memmory.  */
	void soft_reset();

	std::string to_string() {
		return std::string (this->mem);
	}
};

/*
 * Reallocs dest to be the same size as src, and copies the contents
 * of src into dest.
 */
// int strbuf_realloc_copy(strbuf* dest, strbuf* src);

/*
 * Copies contents from src to dest, also allocating dest in the
 * process. dest should not be initialized before self call.
 */
#if 0
int strbuf_init_copy(strbuf* dest, strbuf* src);

strbuf* RESOLVE(strbuf)(strbuf*, strbuf*);

FMT_F(strbuf);

int SIZE(strbuf)(strbuf*);
#endif

#endif /* STRBUF_H */
