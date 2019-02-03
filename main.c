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
		puts("Please give vdir as first argument");
		exit (1);
	}

	SNEW(vcalendar, cal);
	read_vcalendar(&cal, args.argv[0]);

	arg_shift(&args);

	if (args.argc == 0 || strcmp(args.argv[0], "-p") == 0) {
		printf("\nParsed calendar file containing [%lu] events\n",
				cal.n_events);
		for (size_t i = 0; i < cal.n_events; i++) {
			char* filename = cal.events[i]->filename;

			printf("%3lu | %s | %s\n",
					i + 1,
					filename,
					// get_property(cal.events[i], "SUMMARY")->val.mem);
					get_property(cal.events[i], "SUMMARY")->vals.head->value->mem);
		}
	} else if (strcmp(args.argv[0], "-g") == 0) {
		if (arg_shift(&args) == 0) {
			for (size_t i = 0; i < cal.n_events; i++) {
				char target[0xFF];
				target[0] = '\0';
				strcat(target, "/tmp/dot/");
				strcat(target, cal.events[i]->filename);
				strcat(target, ".dot");
				create_graph(cal.events[i], target);
			}
			// create_graph(cal.events[0], "graph.dot");
		} else {
			create_graph(cal.events[0], args.argv[0]);
		}
	}
	free_vcalendar(&cal);

	for (int i = 0; i < cline_ptr; i++) {
		if (clines[i] != NULL) {
			// printf("clines[%i] : [%s] := [%s]\n", i, clines[i]->key.mem, clines[i]->val.mem);
			printf("clines[%i] : [%s] := [%s]\n", i, clines[i]->key.mem, clines[i]->vals.head->value->mem);
		}
	}

	free(clines);
}
