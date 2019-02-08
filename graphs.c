#include "graphs.h"

#include <stdio.h>

int create_graph (vcomponent* ev, char* filename) {
	FILE* f = fopen(filename, "w");

	fputs("digraph {\n	rankdir=LR;", f);
	TRIE_DOT(content_line)(&ev->clines, f);
	fputs("}", f);

	fclose(f);

	printf("Wrote '%s' to '%s'\n", ev->filename, filename);

	return 0;
}
