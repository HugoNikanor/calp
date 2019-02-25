#ifndef GUILE_INTERFACE_H
#define GUILE_INTERFACE_H

#include <libguile.h>

#define SCM_IS_LIST(x) scm_is_true(scm_list_p(x))

void init_vcomponent ();
void init_vcomponent_type (void);

SCM make_vcomponent (SCM);
SCM vcomponent_get_attribute (SCM, SCM);
SCM vcomponent_child_count (SCM);
SCM vcomponent_children (SCM);
SCM vcomponent_typeof (SCM);

#endif /* GUILE_INTERFACE_H */
