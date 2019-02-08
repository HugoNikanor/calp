#include <errno.h>
#include <stdio.h>
#include <string.h>

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

	SNEW(vcomponent, cal);
	read_vcalendar(&cal, args.argv[0]);

	arg_shift(&args);

	if (args.argc == 0 || strcmp(args.argv[0], "-p") == 0) {
		printf("\nParsed calendar file containing [%u] events\n",
				cal.components.length
				);
		for (size_t i = 0; i < cal.components.length; i++) {
			char* filename = cal.components.items[i]->filename;

			printf("%3lu | %s | %s\n",
					i + 1,
					filename,
					get_property(cal.components.items[i], "SUMMARY")->vals.cur->value->mem);
		}
	} else if (strcmp(args.argv[0], "-g") == 0) {
		if (arg_shift(&args) == 0) {
			for (size_t i = 0; i < cal.components.length; i++) {
				char target[0xFF];
				target[0] = '\0';
				strcat(target, "/tmp/dot/");
				strcat(target, cal.components.items[i]->filename);
				strcat(target, ".dot");
				create_graph(cal.components.items[i], target);
			}
		} else {
			create_graph(cal.components.items[0], args.argv[0]);
		}
	}
	FREE(vcomponent)(&cal);
}
