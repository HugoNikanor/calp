#include "calendar.h"

#include <sys/stat.h>
#include <dirent.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

#include "parse.h"
#include "err.h"

int read_vcalendar(vcomponent* cal, char* path) {

	struct stat statbuf;
	if (stat (path, &statbuf) != 0) {
		fprintf(stderr,
				"Error stating file or directory, errno = %i\npath = [%s]\n",
				errno, path);
	}

	int type  = statbuf.st_mode & 0777000;
	int chmod = statbuf.st_mode & 0777;
	INFO_F("file has mode 0%o, with chmod = 0%o", type, chmod);

	switch (type) {
		case S_IFREG: handle_file(cal, path); break;
		case S_IFDIR: handle_dir (cal, path); break;
		case S_IFLNK:
			ERR("Found symlink, can't be bothered to check it further.");
			break;

		default: ;
	}

	return 0;
}

int handle_file(vcomponent* cal, char* path) {
	INFO("Parsing a single file");

	char* resolved_path = realpath(path, NULL);
	open_ics (resolved_path, cal);
	free (resolved_path);

	return 0;
}


int handle_dir(vcomponent* cal, char* path) {
	INFO("Parsing a directory");
	DIR* dir = opendir(path);

	/* Buffer for holding search path and filename */
	char buf[PATH_MAX];
	strcpy(buf, path);
	int path_len = strlen(path) + 1;

	/* Slash to guarantee we have at least one */
	buf[path_len - 1] = '/';

	struct dirent* d;
	while ((d = readdir(dir)) != NULL) {
		/* Check that it's a regular file */
		if (d->d_type != DT_REG) continue;

		/* Append filename with currentt searchpath */
		strcat(buf, d->d_name);
		char* resolved_path = realpath(buf, NULL);
		/* Remove file part from combined path */
		buf[path_len] = '\0';

		open_ics (resolved_path, cal);

		free (resolved_path);
	}

	closedir(dir);
	return 0;
}

int get_extension(const char* filename, char* ext, ssize_t max_len) {
	int ext_idx = -1;
	ext[0] = '\0';
	for (int i = 0; filename[i] != '\0'; i++) {
		if (filename[i] == '.') ext_idx = i + 1;
		if (filename[i] == '/') ext_idx = -1;
	}

	if (ext_idx == -1) return 0;

	int ext_len = 0;
	for (int i = 0; i < max_len; i++, ext_len++) {
		char c = filename[i + ext_idx];
		if (c == '\0') break;
		ext[i] = c;
	}
	ext[ext_len] = '\0';
	return ext_len;
}

int check_ext (const char* path, const char* ext) {
	char buf[10];
	int has_ext = get_extension(path, buf, 9);

	return has_ext && strcmp(buf, ext) == 0;
}

int open_ics (char* resolved_path, vcomponent* cal) {
	puts(resolved_path);
	if (! check_ext(resolved_path, "ics") ) return 2;

	FILE* f = fopen(resolved_path, "r");

	if (f == NULL) return 1;

	parse_file(resolved_path, f, cal);
	fclose(f);

	return 0;
}

