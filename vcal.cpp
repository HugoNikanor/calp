#include "vcal.h"
#include "err.h"

#include <iostream>

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
