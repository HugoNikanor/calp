#include "vcal.h"
#include "err.h"

#include <iostream>

__content_set::__content_set (strbuf* value) : value(value) {
	std::cout << _GREEN << value->len << ' ' << value << _RESET << std::endl;
	this->value->cap();
}

vcomponent::vcomponent(
		const std::string& type,
		const std::string& filename)
	: type(type)
	, filename(filename)
{ }

std::ostream& operator<<(std::ostream& o, vcomponent* self) {
	for (int i = 0; i < 40; i++) o << '_';

	o << _YELLOW << std::endl
	  << "VComponet (Type := " << self->type << _RESET
	  // << self->clines
	  << std::endl << "Components:" << std::endl;
	for (auto v : self->components) {
		o << &v;
	}
	  //<< self->components;

	return o;
}
