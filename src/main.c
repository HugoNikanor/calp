#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "calendar.h"
#include "macro.h"
#include "vcal.h"
#include "graphs.h"
#include "err.h"

typedef struct {
	int argc;
	char** argv;
} arg;

int arg_shift (arg* a) {
	if (a->argc == 0) return 0;

	++a->argv;
	return --a->argc;

}

#define GETSET(C, KEY) \
	vcomponent_push_val((C), (KEY), "DUMMY VALUE"); \
	INFO_F("cline = %p", get_property((C), (KEY)));

/*
 * Tests defined here instead of in own header to ensure that all the
 * correct modules are loaded.
 */
int run_tests() {
	NEW(vcomponent, c);
	INFO(All the following should print a valid pointer ≠ 0x0);
	GETSET(c, "FILENAME");
	GETSET(c, "X-HNH-FILENAME");
	GETSET(c, "DATA");
	GETSET(c, "DAT");
	GETSET(c, "DA");
	GETSET(c, "D");
	GETSET(c, "A");
	GETSET(c, "F");
	FFREE(vcomponent, c);
	return 0;
}

int main (int argc, char** argv) {
	arg args = { .argc = argc, .argv = argv };


	if (arg_shift(&args) == 0) {
		ERR("Please give something to parse, or some other flags");
		exit (1);
	}

	if (strcmp(args.argv[0], "--run-tests") == 0) {
		run_tests();
		return 0;
	}

	char* rootpath = args.argv[0];
	SNEW(vcomponent, root, "ROOT", rootpath);
	read_vcalendar(&root, rootpath);

	arg_shift(&args);

	if (args.argc == 0 || strcmp(args.argv[0], "-p") == 0) {
		INFO_F("Parsed calendar file containing [%u] events",
				root.components.length);

		puts("CAL : OBJ | Filename | Description");
		puts("----------+----------+------------");

		/* This loops over all VCALENDAR's in root */
		FOR (LLIST, vcomponent, cal, &root.components) {
			assert(strcmp(cal->type, "VCALENDAR") == 0);

			char* filename = vcomponent_get_val(cal, "X-HNH-FILENAME");

			/* This loop over all VEVENT's in the current VCALENDAR */
			FOR (LLIST, vcomponent, ev, &cal->components) {
				if (strcmp(ev->type, "VEVENT") != 0) continue;

				printf("%s | %s\n",
						filename,
						get_property(ev, "SUMMARY")->cval->key.mem);
			}
		}
	} else if (strcmp(args.argv[0], "-g") == 0) {
		/* TODO self might be broken */
		if (arg_shift(&args) == 0) {
			FOR (LLIST, vcomponent, cal, &root.components) {
				assert(strcmp(cal->type, "VCALENDAR") == 0);

				vcomponent* ev = FCHILD(cal);

				char target[0xFF];
				target[0] = '\0';
				strcat(target, "/tmp/dot/");
				strcat(target, vcomponent_get_val(ev, "X-HNH-FILENAME"));
				strcat(target, ".dot");
				// create_graph(ev, target);
			}
		} else {
			// create_graph(FCHILD(FCHILD(&root)), args.argv[0]);
			INFO("Creating graph for single file");
			INFO_F("output = %s\n", args.argv[0]);
			create_graph_vcomponent(&root, args.argv[0]);
		}
	}

	/*
	char buf[0x20000];
	FMT(vcomponent)(&root, buf);
	puts(buf);
	*/

	FREE(vcomponent)(&root);
}
