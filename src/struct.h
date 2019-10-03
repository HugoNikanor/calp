#ifndef STRUCT_H
#define STRUCT_H

#include <libguile.h>

#define vcomponent_type     scm_from_uint8(0)
#define vcomponent_children scm_from_uint8(1)
#define vcomponent_parent   scm_from_uint8(2)
#define vcomponent_lines    scm_from_uint8(3)

#define scm_component_parent(component) \
	scm_struct_ref (component, vcomponent_parent)

#define vline_value      scm_from_uint8(0)
#define vline_attributes scm_from_uint8(1)

SCM scm_make_vcomponent(SCM);
SCM scm_add_line_x (SCM, SCM, SCM);
SCM scm_add_child_x (SCM, SCM);
SCM scm_make_vline (SCM);
SCM scm_add_attribute_x (SCM, SCM, SCM);

#endif /* STRUCT_H */
