#include "calendar.h"

/*
 * These three are only for some FD hacks.
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

#include "macro.h"
#include "parse.h"

int read_vcalendar(vcalendar* cal, char* path) {

	DIR* dir = opendir(path);
	struct dirent* d;
	while ((d = readdir(dir)) != NULL) {

		/* Check that it's a regular file */
		if (d->d_type != DT_REG) continue;

		/* Check that we have an ICS file */
		char *s, *fname;
		s = fname = d->d_name;
		while (*(s++) != '.');

		if (strcmp(s, "ics") != 0) continue;

		/* We now assume that it's a good file, and start parsing it */

		int fd = openat(dirfd(dir), fname, O_RDONLY);

		FILE* f = fdopen(fd, "r");
		if (f == NULL) {
			fprintf(stderr, "Error opening file [%s], errno = %i\n",
					fname, errno);
			exit (1);
		}

		/* TODO currently the hedaers cal is overwritten each
		 * iteration (not really, since I don't save any headers).
		 * Preferably, a special case is made for vdir structures
		 * which can assume that all headers are equal. */
		parse_file(f, cal);
		cal->events[cal->n_events - 1]->filename = fname;
		fclose(f);

	}

	closedir(dir);

	return 0;
}
