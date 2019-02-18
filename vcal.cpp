#include "vcal.h"
#include "err.h"

std::ostream& operator<<(std::ostream& o, vcomponent* self) {
	for (int i = 0; i < 40; i++) o << '_';

	o << _YELLOW << std::endl
	  << "VComponet (Type := " << self->type << _RESET
	  << self->clines
	  << std::endl << "Components:" << std::endl
	  << self->components;

	return o;
}
