#ifndef GRAPHS_H
#define GRAPHS_H

#include "vcal.h"

int create_graph_trie (vcomponent* ev, char* filename);

int create_graph_vcomponent (vcomponent* root, char* outfile);

int helper_vcomponent (vcomponent* root, FILE* f);

#endif /* GRAPHS_H */
