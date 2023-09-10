#define _XOPEN_SOURCE 500
#define _POSIX_C_SOURCE 200112L

/**
 * Primary entry point for calp.
 *
 * This is a C file, rather than a shellscript for 2 reasons:
 * 1. It makes the binary show up with the proper name in process
 *    listings
 * 2. Guile's default command line handling leaves a lot to be
 *    desired.
 *
 * The following pre-processor variables are checked during compilation:
 *
 * BUILD_ENV
 *	   If defined then the environment is fetched from where the file
 *	   resides. This is for development when all dependencies are in the
 *	   repo alongside this file.
 * MAIN_MODULE
 *     Guile module containing the programs entry point. Note that it
 *     should be given without parenthesis.
 *
 *     Defaults to "calp main"
 * MAIN_PROC
 *     Procedure within the MAIN_MODULE which is the programs entry
 *     point.
 *
 *     Default to "main"
 *
 * During runtime, the environment variable "__PRINT_ENVIRONMENT" is
 * checked, and if its set then a environment variables suitable for
 * guile is printed instead of running anything. This is mainly used
 * by the test runners.
 *
 * Parts of this file is noted as being "borrowed from Guile". Guile
 * is covered under LGPL3, this file is covered under AGPL, which
 * means it is ok.
 */

#ifndef MAIN_MODULE
#define MAIN_MODULE "calp main"
#endif

#ifndef MAIN_PROC
#define MAIN_PROC "main"
#endif

#include <libgen.h>

#include <libguile.h>
#include <string.h>
#include <stdio.h>
#include <locale.h>
#include <errno.h>
#include <limits.h>

/** Definitions to ensure each instance is correctly spelled below */
#define GUILE_LOAD_PATH "GUILE_LOAD_PATH"
#define GUILE_LOAD_COMPILED_PATH "GUILE_LOAD_COMPILED_PATH"
#define GUILE_AUTO_COMPILE "GUILE_AUTO_COMPILE"

#define PRINT_ENVIRONMENT "__PRINT_ENVIRONMENT"

/** Borrowed from Guile 3.0.9 libguile/guile.c */
static int
get_integer_from_environment (const char *var, int def)
{
	char *end = 0;
	char *val = getenv (var);
	long res = def;
	if (!val)
		return def;
	res = strtol (val, &end, 10);
	if (end == val)
	{
		fprintf (stderr, "guile: warning: invalid %s: %s\n", var, val);
		return def;
	}
	return res;
}

static void inner_main (void *closure, int argc, char **argv) {
	SCM main = scm_c_public_ref(MAIN_MODULE, MAIN_PROC);
	SCM scm_args = scm_c_make_vector (argc, SCM_UNDEFINED);
	for (size_t i = 0; i < (size_t) argc; i++) {
		scm_c_vector_set_x (scm_args, i, scm_from_locale_string(argv[i]));
	}
	scm_call_1(main, scm_vector_to_list(scm_args));
}

/** Procedure marked unused since GCC dosen't understand function
 * pointers when checking usage */
__attribute__((__unused__))
static void *get_guile_version (void *data) {
	(void) data;
	return scm_to_locale_string(scm_version());
}


static void export (const char *key, const char *val) {
	printf("export %s=%s;\n", key, val);
}


int main(int argc, char *argv[]) {

	/* Locale initialization code borrowed from Guile 3.0.9 libguile/guile.c */
	if (get_integer_from_environment("GUILE_INSTALL_LOCALE", 1) && setlocale(LC_ALL, "") == NULL) {
		fprintf(stderr, "calp: warning: failed to install locale\n");
	}

#ifdef BUILD_ENV

	char *bin_path = realpath(argv[0], NULL);
	if (bin_path == NULL) {
		fprintf(stderr, "%s\n", strerror(errno));
		return 1;
	}
	char *here = dirname(bin_path);

	{
		const char *load_path = getenv(GUILE_LOAD_PATH);
		size_t len = PATH_MAX + strlen("module") + 1;
		if (load_path) len += strlen(load_path);

		char *buf = malloc(len);

		if (load_path) {
			sprintf(buf, "%s:%s/module", load_path, here);
		} else {
			sprintf(buf, "%s/module", here);
		}

		setenv(GUILE_LOAD_PATH, buf, 1);

		free(buf);
	}

	{
		char *version = scm_with_guile(&get_guile_version, NULL);

		const char *load_path = getenv(GUILE_LOAD_COMPILED_PATH);
		size_t len = PATH_MAX + strlen("module") + strlen(version) + 1;
		if (load_path) len += strlen(load_path);

		char *buf = malloc(len);

		if (load_path) {
			sprintf(buf, "%s:%s/obj-%s", load_path, here, version);
		} else {
			sprintf(buf, "%s/obj-%s", here, version);
		}

		setenv(GUILE_LOAD_COMPILED_PATH, buf, 1);

		free(buf);
		free(version);
	}

	setenv(GUILE_AUTO_COMPILE, "0", 1);

	free(bin_path);
#endif

	if (getenv(PRINT_ENVIRONMENT)) {
		export(GUILE_LOAD_PATH, getenv(GUILE_LOAD_PATH));
		export(GUILE_LOAD_COMPILED_PATH, getenv(GUILE_LOAD_COMPILED_PATH));
		export(GUILE_AUTO_COMPILE, "0");
		export("GUILE", "${GUILE:-guile}");
		export("CALP_TEST_ENVIRONMENT", "1");
		return 0;
	}

	scm_boot_guile (argc, argv, inner_main, 0);
	return 0; /* never reacher */
}
