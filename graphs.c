#include "graphs.h"

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "err.h"

// #define TYPE strbuf
// #include "linked_list.h"
// #include "linked_list.inc.h"
// #undef TYPE

int create_graph_trie (vcomponent* ev, char* filename) {
	FILE* f = fopen(filename, "w");

	fputs("digraph {\n	rankdir=LR;", f);
	trie_to_dot(&ev->clines, f);
	fputs("}", f);

	fclose(f);

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
			trie_to_dot_helper( n, f );


			fputs("}", f);
			n = n->next;
		}
		fputs("}", f);
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

#define T content_line

int trie_to_dot ( TRIE(T)* trie, FILE* f ) {
	TRIE_NODE(T)* n = trie->root->child;
	fprintf(f, "\"%p\" [label=root fontcolor=gray, color=gray];", trie);
	while (n != NULL) {
		fprintf(f, "\"%p\" -> \"%p\" [color=gray]\n",
				(void*) trie,
				(void*) n);
		fprintf(f, "subgraph \"cluster_%c\" {\n",
				n->c);
		trie_to_dot_helper( n, f );
		fputs("}", f);
		n = n->next;
	}
	return 0;
}

int trie_to_dot_helper ( TRIE_NODE(T)* root, FILE* f  ) {
	if (L(root) == NULL) {
		fprintf(f, "\"%p\"[label = \"%c\" style=filled fillcolor=white];\n",
				(void*) root, root->c);
	} else {
		fprintf(f, "\"%p\"[label = \"%c [%i]\" style=filled fillcolor=green];\n",
				(void*) root, root->c,
				SIZE(LLIST(content_set))(&L(root)->val)
				);
	}
	TRIE_NODE(T)* child = root->child;

	// ----------------------------------------
#if 0 /* Toggle values */
	if (root->value != NULL) {

		FOR(LLIST(content_set), link, &L(root)->val) {
			char buf[0x100];
			FMT(strbuf)(&L(link)->key, buf);
			fprintf(f, "\"%p\" [label=\"%s\" shape=rectangle color=darkgreen];\n",
					link->value, buf);
			fprintf(f, "\"%p\" -> \"%p\";\n", root, L(link));

			/* Parameters */
			FOR(LLIST(param_set), param_link, &L(link)->val) {
				strbuf* param_key = &L(param_link)->key;

				fprintf(f, "\"%p\" [label=\"%s\" color=blue];\n",
						param_key, param_key->mem);
				fprintf(f, "\"%p\" -> \"%p\";", L(link), param_key);

				FOR(LLIST(strbuf), str, &L(param_link)->val) {
					fprintf(f, "\"%p\" [label=\"%s\" color=orange];",
							str, L(str)->mem);
					fprintf(f, "\"%p\" -> \"%p\";", param_key, str);
				}
			}
		}
	}
#endif

	// ----------------------------------------

	while (child != NULL) {
		fprintf(f, "\"%p\" -> \"%p\";\n",
				(void*) root, (void*) child);
		trie_to_dot_helper(child, f);
		child = child->next;
	}
	return 0;
}
