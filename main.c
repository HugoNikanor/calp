#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "calendar.h"
#include "macro.h"
#include "vcal.h"
#include "graphs.h"

typedef struct {
	int argc;
	char** argv;
} arg;

int arg_shift (arg* a) {
	if (a->argc == 0) return 0;

	++a->argv;
	return --a->argc;

}

int main (int argc, char* argv[argc]) {
	arg args = { .argc = argc, .argv = argv };

	if (arg_shift(&args) == 0) {
		puts("Please give vdir or a vcalendar file as first argument");
		exit (1);
	}

	char* rootpath = args.argv[0];
	SNEW(vcomponent, root, "ROOT", rootpath);
	read_vcalendar(&root, rootpath);

	arg_shift(&args);

	vcomponent* cal = FCHILD(&root);

	assert(strcmp(root.type, "ROOT") == 0);
	assert(strcmp(cal->type, "VCALENDAR") == 0);

	if (args.argc == 0 || strcmp(args.argv[0], "-p") == 0) {
		printf("\nParsed calendar file containing [%u] events\n",
				root.components.length
				);
		for (size_t i = 0; i < cal->components.length; i++) {
			char* filename = cal->components.items[i]->filename;
			vcomponent* ev = GET(VECT(vcomponent))(&cal->components, i);

			printf("TYPE = %s\n", ev->type);
			if (strcmp(ev->type, "VEVENT") != 0) continue;

			printf("%3lu | %s | %s\n",
					i + 1,
					filename,
					get_property(ev, "SUMMARY")->vals.cur->value->mem);
		}
	} else if (strcmp(args.argv[0], "-g") == 0) {
		if (arg_shift(&args) == 0) {
			for (size_t i = 0; i < cal->components.length; i++) {
				char target[0xFF];
				target[0] = '\0';
				strcat(target, "/tmp/dot/");
				vcomponent* ev = GET(VECT(vcomponent))(&cal->components, i);
				strcat(target, ev->filename);
				strcat(target, ".dot");
				create_graph(ev, target);
			}
		} else {
			create_graph(GET(VECT(vcomponent))(&cal->components, 0), args.argv[0]);
		}
	}

	FREE(vcomponent)(&root);
}
