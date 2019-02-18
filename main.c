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

int main (int argc, char** argv) {
	arg args = { .argc = argc, .argv = argv };

	if (arg_shift(&args) == 0) {
		ERR("Please give vdir or a vcalendar file as first argument");
		exit (1);
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
		for (size_t i = 0; i < root.components.length; i++) {
			vcomponent* cal = GET(VECT(vcomponent))(&root.components, i);
			assert(strcmp(cal->type, "VCALENDAR") == 0);

			char* filename = cal->filename;
			/* This loop over all VEVENT's in the current VCALENDAR */
			for (size_t j = 0; j < cal->components.length; j++) {
				vcomponent* ev = GET(VECT(vcomponent))(&cal->components, j);

				if (strcmp(ev->type, "VEVENT") != 0) continue;

				printf("%3lu : %3lu | %s | %s\n",
						i + 1, j + 1,
						filename,
						get_property(ev, "SUMMARY")->val.cur->value->key.mem);
			}
		}
	} else if (strcmp(args.argv[0], "-g") == 0) {
		/* TODO self might be broken */
		if (arg_shift(&args) == 0) {
			for (size_t i = 0; i < root.components.length; i++) {
				vcomponent* cal = GET(VECT(vcomponent))(&root.components, i);
				assert(strcmp(cal->type, "VCALENDAR") == 0);

				vcomponent* ev = FCHILD(cal);

				char target[0xFF];
				target[0] = '\0';
				strcat(target, "/tmp/dot/");
				strcat(target, ev->filename);
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
