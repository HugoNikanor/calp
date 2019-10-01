#ifndef CALENDAR_H
#define CALENDAR_H

#include <libguile.h>

// #include "vcal.h"

/*
 * Reads all ics flies in path into the given vcomponent. The
 * component is assumed to be a abstract ROOT element, whose first
 * component will most likely become a VCALENDAR.
 *
 * path should either be a single .ics file (vcalendar), or a
 * directory directly containing .ics files (vdir).
 */
int read_vcalendar(SCM cal, char* path);

/*
 * Gets extension from filename. Writes output to ext.
 * Assumes that the extension is the text between the last dot and
 * the end of the string, and that no slashes can occur between the
 * dot and the end.
 *
 * Returns the length of the extension, 0 if no extension.
 */
int get_extension(const char* filename, char* ext, ssize_t max_len);

/* Returns 1 if path has extension ext, 0 otherwise */
int check_ext (const char* path, const char* ext);

/* Handle a lone ics file */
int handle_file(SCM cal, char* path);

/* Handle a directory of ics files */
int handle_dir(SCM cal, char* path);

/*
 * Helper for opening a single ICS file. Handles file internally, and
 * writes output to cal.
 */
int open_ics (char* resolved_path, SCM cal);

#endif /* CALENDAR_H */
