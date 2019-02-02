#include <errno.h>

#include "calendar.h"
#include "macro.h"
#include "vcal.h"
#include "stdio.h"

int main (int argc, char* argv[argc]) {
	if (argc < 2) {
		puts("Please give vdir as first argument");
		exit (1);
	}

	SNEW(vcalendar, cal);

	read_vcalendar(&cal, argv[1]);

	printf("\nParsed calendar file containing [%lu] events\n",
			cal.n_events);
	for (size_t i = 0; i < cal.n_events; i++) {
		printf("%3lu. %s\n", i + 1, get_property(cal.events[i], "SUMMARY")->val.mem);
	}

	free_vcalendar(&cal);
}
