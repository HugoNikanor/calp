#include "vcal.h"

#include <string.h>

// INIT_F(vcomponent, const char* type, const char* filename) {
vcomponent::vcomponent (const char* type, const char* filename) {

	// INIT(TRIE(content_line), &this->clines);
	// INIT(VECT(vcomponent), &this->components);

	this->filename = NULL;
	if (filename != NULL) {
		this->filename = (char*) calloc(sizeof(*filename), strlen(filename) + 1);
		strcpy(this->filename, filename);
	}

	this->type = (char*) calloc(sizeof(*type), strlen(type) + 1);
	strcpy(this->type, type);

	this->parent = NULL;
}

content_line* get_property (vcomponent* ev, const char* key) {
	size_t len = strlen(key) + 1;
	char* cpy = (char*) (calloc(sizeof(*cpy), len));
	strncpy (cpy, key, len);

	// content_line* ret = GET(TRIE(content_line))(&ev->clines, cpy);
	content_line* ret = ev->clines.get(cpy);

	free (cpy);
	return ret;
}

// FREE_F(vcomponent) {
vcomponent::~vcomponent () {
	if (this->filename != NULL) free(this->filename);
	free(this->type);

	// if (FREE(TRIE(content_line))(&self->clines) != 0) {
	// 	fprintf(stderr, "Error freeing vcomponent belonging to file \n %s \n",
	// 			self->filename);
	// }

	// FREE(VECT(vcomponent))(&self->components);
}

int PUSH(vcomponent)(vcomponent* parent, vcomponent* child) {
	// return PUSH(VECT(vcomponent))(&parent->components, child);
	return parent->components.push(child);
}

int DEEP_COPY(vcomponent)(vcomponent* a, vcomponent* b) {
	(void) a;
	(void) b;
	ERR("Deep copy not implemented for vcomponent");
	return -1;
}

#if 0
FMT_F(vcomponent) {
	int seek = 0;

	for (int i = 0; i < 40; i++) fmtf("_");

	seek += sprintf(buf + seek, _YELLOW);
	seek += sprintf(buf + seek, "\nVComponet (Type := %s)\n", self->type); 
	seek += sprintf(buf + seek, _RESET);
	// seek += FMT(TRIE(content_line))(&self->clines, buf + seek);
	seek += sprintf(buf + seek, "\nComponents:\n");
	FOR(VECT, vcomponent, comp, &self->components) {
		seek += FMT(vcomponent)(comp, buf + seek);
	}

	return seek;
}
#endif
