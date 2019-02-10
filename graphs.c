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
	if (root->value == NULL) {
		fprintf(f, "\"%p\"[label = \"%c\" style=filled fillcolor=white];\n",
				(void*) root, root->c);
	} else {
		fprintf(f, "\"%p\"[label = \"%c [%i]\" style=filled fillcolor=green];\n",
				(void*) root, root->c,
				SIZE(LLIST(strbuf))(&root->value->vals));
	}
	TRIE_NODE(T)* child = root->child;

	// ----------------------------------------
	if (root->value != NULL) {

		FOR(LLIST(strbuf), link, &root->value->vals) {
			char buf[0x100];
			FMT(strbuf)(link->value, buf);
			fprintf(f, "\"%p\" [label=\"%s\" shape=rectangle color=darkgreen];\n",
					link->value, buf);
			fprintf(f, "\"%p\" -> \"%p\";\n", root, link->value);
		}


		/* Parameters */
		if (! EMPTY(LLIST(key_val))(&root->value->params)) {
			fprintf(f, "subgraph \"cluster_param_%p\"{\n	color=blue;\n", root);
			FOR(LLIST(key_val), link, &root->value->params) {
				fprintf(f, "\"%p\"  [shape=rectangle, label=\"%s := %s\"];", link,
						link->value->key.mem,
						link->value->val.mem);
			}
			fputs("}", f);

			/* 
			 * TODO
			 * Params should actually belong to each value. Not to
			 * each content_line (since a content line is a collection
			 * of lines in the original file). This is a temporary
			 * hack.
			 */
			fprintf(f, "\"link_%p\" [label=params style=filled fillcolor=lightblue];\n", root);
			fprintf(f, "\"%p\" -> \"link_%p\";\n", root, root);
			FOR(LLIST(key_val), link, &root->value->params) {
				fprintf(f, "\"link_%p\" -> \"%p\";\n", root, link);
			}
		}
	}
	// ----------------------------------------

	while (child != NULL) {
		fprintf(f, "\"%p\" -> \"%p\";\n",
				(void*) root, (void*) child);
		trie_to_dot_helper(child, f);
		child = child->next;
	}
	return 0;
}
