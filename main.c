#include <dirent.h>
#include <errno.h>

/*
 * These three are only for some FD hacks.
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "parse.h"
#include "macro.h"

int main (int argc, char* argv[argc]) {
	if (argc < 2) {
		//puts("Please give a ics file as first argument");
		puts("Please give vdir as first argument");
	   exit (1);	
	}
	// vcalendar cal;
	// init_vcalendar(&cal);
	// CONSTRUCT(vcalendar, &cal);
	SNEW(vcalendar, cal);

	char* dname = argv[1];
	DIR* dir = opendir(dname);
	struct dirent* d;
	int fcount = 0;
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

		printf("%3i | %s\n", fcount++, fname);
		/* TODO currently the hedaers cal is overwritten each
		 * iteration (not really, since I don't save any headers).
		 * Preferably, a special case is made for vdir structures
		 * which can assume that all headers are equal. */
		parse_file(f, &cal);
		fclose(f);

	}

	printf("\nParsed calendar file containing [%lu] events\n",
			cal.n_events);
	for (size_t i = 0; i < cal.n_events; i++) {
		// printf("%3lu. %s\n", i + 1, cal.events[i].summary.mem);
		printf("%3lu. %s\n", i + 1, get_property(&cal.events[i], "SUMMARY")->val.mem);
	}

	closedir(dir);
	free_vcalendar(&cal);
}
