#include "struct.h"

#include <libguile.h>

#include "parse.h"
#include "calendar.h"

SCM vcomponent_vtable;
SCM vline_vtable;

SCM_DEFINE(scm_make_vcomponent, "make-vcomponent", 0, 1, 0,
           (SCM type),
           "")
{

	if (SCM_UNBNDP (type) || scm_is_false (type))
		type = scm_from_utf8_symbol("VIRTUAL");

	return scm_make_struct_no_tail
		(vcomponent_vtable,
		 scm_list_4(type, SCM_EOL, SCM_BOOL_F,
		            scm_make_hash_table(scm_from_int(0x10))));
}



SCM_DEFINE(scm_parse_cal_path, "parse-cal-path", 1, 0, 0,
           (SCM path),
           "")
{
	SCM root = scm_make_vcomponent(SCM_UNDEFINED);

        char* p = scm_to_utf8_stringn(path, NULL);
        read_vcalendar(root, p);
        free(p);

        return root;
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


SCM_DEFINE(scm_make_vline, "make-vline", 0, 1, 0,
           (SCM value), "")
{

	if (SCM_UNBNDP (value)) value = SCM_BOOL_F;

	return scm_make_struct_no_tail
		(vline_vtable,
		 scm_list_2(value, scm_make_hash_table(scm_from_int(0x10))));
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

void init_lib (void) {
	SCM str = scm_from_utf8_string("pr" "pw" "pw" "pr");
	vcomponent_vtable = scm_make_vtable(str, SCM_BOOL_F);
	scm_set_struct_vtable_name_x (vcomponent_vtable, scm_from_utf8_symbol("vcomponent"));

	vline_vtable =
		scm_make_vtable(scm_from_utf8_string("pw" "pw"),
		                SCM_BOOL_F);
	scm_set_struct_vtable_name_x (vline_vtable, scm_from_utf8_symbol("vline"));

#ifndef SCM_MAGIC_SNARFER
#include "struct.x"
#endif
}
