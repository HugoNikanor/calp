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
#include <unistd.h>

#include "macro.h"
#include "parse.h"

/*
 * Returns 0 if file has no extersion
 */
int get_extension(const char* filename, char* ext, ssize_t max_len) {
	int ext_idx = -1;
	ext[0] = '\0';
	for (char* c = (char*) filename; *c != '\0'; c++) {
		if (*c == '.') {
			ext_idx = 0;
			continue;
		}
		if (ext_idx >= 0) {
			ext[ext_idx++] = *c;
			if (ext_idx == max_len) break;
		}
	}
	ext[ext_idx] = '\0';
	return (ext_idx == -1)
		? 0
		: ext_idx;
}

/*
 * TODO merge the code for files and dirs.
 */

int parse_dir(vcalendar* cal, char* path) {
	DIR* dir = opendir(path);
	struct dirent* d;
	while ((d = readdir(dir)) != NULL) {

		/* Check that it's a regular file */
		if (d->d_type != DT_REG) continue;

		/* Check that we have an ICS file */
		char* fname = d->d_name;

		char ext[10];
		int has_ext = get_extension(fname, ext, 9);
		if (! has_ext || strcmp(ext, "ics") != 0) continue;

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
		parse_file(fname, f, cal);

		fclose(f);
	}

	closedir(dir);

	return 0;
}

int read_vcalendar(vcalendar* cal, char* path) {

	struct stat statbuf;
	if (stat (path, &statbuf) != 0) {
		fprintf(stderr,
				"Error opening file or directory, errno = %i\npath = [%s]\n",
				errno, path);
	}

	int type  = statbuf.st_mode & 0777000;
	int chmod = statbuf.st_mode & 0777;
	printf("file has mode 0%o, with chmod = 0%o\n", type, chmod);

	switch (type) {
		case S_IFREG:
			puts("Parsing a single file");

			char ext[10];
			int has_ext = get_extension(path, ext, 9);
			if (! has_ext || strcmp(ext, "ics") != 0) {
				fprintf(stderr, "File doesn't have .ics extension.\n");
				exit(1);
			}

			FILE* f = fopen(path, "r");
			if (f == NULL) {
				fprintf(stderr, "Error opening file [%s], errno = %i\n",
						path, errno);
				exit (1);
			}

			parse_file(path, f, cal);
			fclose(f);
			break;

		case S_IFDIR:
			puts("Parsing a directory");
			parse_dir (cal, path);
			break;

		case S_IFLNK:
			fputs("Found symlink, can't be bothered to check it further.", stderr);
			break;

		default:
			;
	}


	return 0;
}
