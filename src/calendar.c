#include "calendar.h"

#include <sys/stat.h>
#include <dirent.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

/* basename */
#include <libgen.h>
#include <libguile.h>

#include "struct.h"

#include "parse.h"
#include "err.h"

int read_vcalendar(SCM cal, char* path) {

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

int handle_file(SCM cal, char* path) {
	INFO("Parsing a single file");

	/* NAME is the `fancy' name of the calendar. */
	// vcomponent_push_val(cal, "NAME", basename(path));
	// vcomponent_push_val(cal, "X-HNH-SOURCETYPE", "file");
	SCM line = scm_make_vline(scm_from_utf8_string("file"));
	scm_add_line_x(cal, scm_from_utf8_string("X-HNH-SOURCETYPE"), line);
	char* resolved_path = realpath(path, NULL);
	open_ics (resolved_path, cal);
	free (resolved_path);

	return 0;
}


int handle_dir(SCM cal, char* path) {
	INFO("Parsing a directory");
	DIR* dir = opendir(path);

	/* Buffer for holding search path and filename */
	char buf[PATH_MAX] = { [0 ... PATH_MAX - 1] = '\0' };
	strcpy(buf, path);
	int path_len = strlen(path) + 1;

	/* Slash to guarantee we have at least one */
	buf[path_len - 1] = '/';


	/* NAME is the `fancy' name of the calendar. */
	// vcomponent_push_val(cal, "NAME", basename(path));
	SCM line = scm_make_vline(scm_from_utf8_string("vdir"));
	scm_add_line_x(cal, scm_from_utf8_string("X-HNH-SOURCETYPE"), line);

	struct dirent* d;
	while ((d = readdir(dir)) != NULL) {
		/* Check that it's a regular file */
		if (d->d_type != DT_REG) continue;

		/* Append filename with currentt searchpath */
		strcat(buf, d->d_name);
		char* resolved_path = realpath(buf, NULL);
		/* Remove file part from combined path */
		buf[path_len] = '\0';

		FILE* f;
		size_t read, size = 0x100;
		char* info_buf = malloc(size);
		if (strcmp (d->d_name, "color") == 0) {
			f = fopen(resolved_path, "r");
			read = getline(&info_buf, &size, f);
			if (info_buf[read - 1] == '\n')
				info_buf[read - 1] = '\0';

			fclose(f);
			// TODO 
			// vcomponent_push_val(cal, "COLOR", info_buf);
		} else if (strcmp (d->d_name, "displayname") == 0) {
			f = fopen(resolved_path, "r");
			read = getline(&info_buf, &size, f);
			if (info_buf[read - 1] == '\n')
				info_buf[read - 1] = '\0';

			fclose(f);

			/* This adds the new list to the set of names, keeping the
			 * filename name.
			 * This works since *currently* values are returned in
			 * reverse order
			 */
			// TODO
			// vcomponent_push_val(cal, "NAME", info_buf);
		} else {
			open_ics (resolved_path, cal);
		}

		free (resolved_path);
	}

	closedir(dir);
	return 0;
}

int get_extension(const char* filename, char* ext, ssize_t max_len) {

	if (filename == NULL) {
		ext[0] = '\0';
		return 0;
	}

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

int open_ics (char* resolved_path, SCM cal) {
	if (! check_ext(resolved_path, "ics") ) return 2;

	FILE* f = fopen(resolved_path, "r");

	if (f == NULL) return 1;

	parse_file(resolved_path, f, cal);
	fclose(f);

	return 0;
}
