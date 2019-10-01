#include "struct.h"

#include <libguile.h>

SCM_DEFINE(scm_make_vcomponent, "make-vcomponent", 1, 0, 0,
           (SCM type),
           "")
{
	SCM str = scm_from_utf8_string("pr" "pw" "pw" "pr");
	SCM vcomponent_vtable = scm_make_vtable(str, SCM_BOOL_F);
	return scm_c_make_struct (vcomponent_vtable, scm_from_int(0),
	                          type, SCM_EOL, SCM_BOOL_F,
	                          scm_make_hash_table(SCM_BOOL_F),
	                          SCM_UNDEFINED);
}


SCM_DEFINE(scm_add_line_x, "add-line!", 3, 0, 0,
           (SCM vcomponent, SCM key, SCM line),
           "")
{
	scm_hash_set_x (scm_struct_ref(vcomponent, vcomponent_lines), key, line);
	return SCM_UNSPECIFIED;
}


SCM_DEFINE(scm_add_child_x, "add-child!", 2, 0, 0,
           (SCM vcomponent, SCM child),
           "")
{
	scm_struct_set_x (child, vcomponent_parent, vcomponent);
	scm_struct_set_x (vcomponent, vcomponent_children,
	                  scm_cons (child, scm_struct_ref (vcomponent, vcomponent_children)));

	return SCM_UNSPECIFIED;
}


SCM_DEFINE(scm_make_vline, "make-vline", 0, 0, 0,
           (), "")
{
	SCM vline_vtable =
		scm_make_vtable(scm_from_utf8_string("pw" "pw"),
		                SCM_BOOL_F);
	return scm_c_make_struct (vline_vtable, scm_from_int(0),
	                          SCM_BOOL_F, scm_make_hash_table(SCM_BOOL_F),
	                          SCM_UNDEFINED);
}


SCM_DEFINE(scm_add_attribute_x, "add-attribute!", 3, 0, 0,
           (SCM vline, SCM key, SCM value),
           "")
{
	SCM table = scm_struct_ref (vline, vline_attributes);
	scm_hash_set_x (table, key,
	                scm_cons(value, scm_hash_ref(table, key, SCM_EOL)));
	return SCM_UNSPECIFIED;
}
