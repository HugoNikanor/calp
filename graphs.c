#include "graphs.h"

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "err.h"

int create_graph_trie (vcomponent* ev, char* filename) {
	FILE* f = fopen(filename, "w");

	fputs("digraph {\n	rankdir=LR;", f);
	TRIE_DOT(content_line)(&ev->clines, f);
	fputs("}", f);

	fclose(f);


	return 0;
}

int attr_helper(TRIE_NODE(content_line)* root, FILE* f) {

	if (root->value != NULL) {
		if (! EMPTY(LLIST(strbuf))(&root->value->params)) {
			printf("%s\n", root->value->key.mem);
			fprintf(f, "subgraph \"cluster_param_%p\"{\n	color=blue;\n", root);
			FOR(LLIST(strbuf), link, &root->value->params) {
				fprintf(f, "\"%p\"  [label=\"%s\"];", link, link->value->mem);
				fprintf(f, "\"%p\" -> \"%p\";\n", root, link);
			}
			fputs("}", f);
		}
	}

	TRIE_NODE(content_line)* child = root->child;
	while (child != NULL) {
		attr_helper(root->child, f);
		child = child->next;
	}
	INFO_F("Wrote '%s' to '%s'", ev->filename, filename);

	return 0;
}

int helper_vcomponent (vcomponent* root, FILE* f) {
	fprintf(f, "subgraph \"cluster_root\" { label=File; \"%p\" [label=%s] }\n", root, root->type);

	TRIE(content_line)* trie = &root->clines;
	TRIE_NODE(content_line)* n = trie->root->child;

	if (! EMPTY(TRIE(content_line))(trie)) {
		fprintf(f, "subgraph \"cluster_%p\" {\n", root);
		fprintf(f, "\"%p\" [label=trie fontcolor=gray, color=gray];", trie);
		fprintf(f, "\"%p\" -> \"%p\" [color=red]\n", root, trie);
		while (n != NULL) {
			fprintf(f, "\"%p\" -> \"%p\" [color=gray]\n",
					(void*) trie,
					(void*) n);
			fprintf(f, "subgraph \"cluster_%c_%p\" {\ncolor=red; \n",
					n->c, root);
			TRIE_DOT_HELP(content_line) ( n, f );


			fputs("}", f);
			n = n->next;
		}
		fputs("}", f);

		attr_helper(trie->root, f);
	}

	vcomponent* child;
	for (size_t i = 0; i < root->components.length; i++) {
		child = GET(VECT(vcomponent))(&root->components, i);
		if (child == NULL) continue;
		fprintf(f, "\"%p\" -> \"%p\"\n", root, child);
		helper_vcomponent(child, f);
	}
	return 0;
}

int create_graph_vcomponent (vcomponent* root, char* outfile) {
	FILE* f = fopen(outfile, "w");
	if (f == NULL) {
		ERR_F("Error opening file %s, errno = %i", outfile, errno);
		return 1;
	}
	vcomponent* c = root;
	fputs("digraph {", f);
	helper_vcomponent(c, f);
	fputs("}", f);
	fclose(f);
	return 0;
}
