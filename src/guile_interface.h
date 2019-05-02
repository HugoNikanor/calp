#ifndef GUILE_INTERFACE_H
#define GUILE_INTERFACE_H

#include <libguile.h>
#include "vcal.h"

/*
 * At a number of places scm_gc_{un,}protect_object is called.
 * This is needed since most of my structures are allocated with the
 * regular malloc, instead of the scm_gc_malloc variants.
 * This leads to the garbage collector not realizing that I still have
 * the components, and deletes them.
 *
 * The protection markers stop the GC from doing its thing.
 */

void init_lib (void);
void init_vcomponent_type (void);

SCM make_vcomponent (SCM);
SCM vcomponent_get_attribute (SCM, SCM);
SCM vcomponent_child_count (SCM);
SCM vcomponent_children (SCM);
SCM vcomponent_typeof (SCM);

SCM scm_from_vcomponent (vcomponent*);
vcomponent* scm_to_vcomponent (SCM);

#endif /* GUILE_INTERFACE_H */
