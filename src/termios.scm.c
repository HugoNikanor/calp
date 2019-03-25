#include <libguile.h>
#include <unistd.h>
#include <termios.h>
#include <stdio.h>

#include "err.h"

static struct termios *oldt, *newt;

SCM_DEFINE(termios_lflags_and, "c-lflags-disable!", 2, 0, 0,
           (SCM _fd, SCM _bits),
           "")
{

	int fd   = scm_to_int (_fd);
	int bits = scm_to_int (_bits);

	INFO_F("Setting bits [%x]", bits);

	tcgetattr(fd, oldt);
	*newt = *oldt;

	// Make the terminal not echo back,
	// along with enabling cononical mode
	newt->c_lflag &= ~ bits;
	tcsetattr(fd, TCSANOW, newt);
	return SCM_UNSPECIFIED;
}

SCM_DEFINE(termios_restore, "c-lflag-restore!", 1, 0, 0,
           (SCM _fd),
           "")
{
	int fd   = scm_to_int (_fd);
	tcsetattr(fd, TCSANOW, oldt);
	return SCM_UNSPECIFIED;
}

void init_termios (void) {
	oldt = scm_gc_malloc(sizeof(*oldt), "Termios");
	newt = scm_gc_malloc(sizeof(*newt), "Termios");

#ifndef SCM_MAGIC_SNARFER
#include "termios.x"
#endif
}
