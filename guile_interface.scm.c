#include "guile_interface.h"

#include "vcal.h"
#include "calendar.h"
#include "guile_type_helpers.h"

static SCM vcomponent_type;

void init_vcomponent_type (void) {
	SCM name = scm_from_utf8_symbol("vcomponent");
	SCM slots = scm_list_1(scm_from_utf8_symbol("data"));

	vcomponent_type = scm_make_foreign_object_type(name, slots, NULL);
}

SCM_DEFINE (make_vcomponent, "%vcomponent-make", 1, 0, 0,
		(SCM path),
		"Loads a vdir iCalendar from the given path.")
{
	vcomponent* cal =
		(vcomponent*) scm_gc_malloc (
				sizeof(*cal), "vcomponent");
	INIT(vcomponent, cal, "ROOT");

	char* p = scm_to_utf8_stringn(path, NULL);
	read_vcalendar(cal, p);
	free(p);

	return scm_make_foreign_object_1
		(vcomponent_type, cal);

}

/*
 * Returns a line from a component.
 */
SCM_DEFINE (vcomponent_get_attribute, "%vcomponent-get-attribute", 2, 0, 0,
		(SCM calendar, SCM attr),
		"Retuns the given attribute from the vevent object at index in calendar.")
{
	scm_assert_foreign_object_type (vcomponent_type, calendar);
	vcomponent* cal = scm_foreign_object_ref (calendar, 0);

	char* key = scm_to_utf8_stringn(scm_string_upcase(attr), NULL);
	content_line* c = get_property (cal, key);
	free(key);

	if (c == NULL) return SCM_BOOL_F;

	SCM llist = SCM_EOL;
	FOR (LLIST, content_set, v, &c->val) {
		llist = scm_cons(scm_from_strbuf(&v->key), llist);
	}

	/* returns the car of list if list is one long. */
	if (scm_to_int(scm_length(llist)) == 1) {
		return SCM_CAR(llist);
	} else {
		return llist;
	}
}

SCM_DEFINE (vcomponent_set_attr_x, "%vcomponent-set-attribute!", 3, 0, 0,
		(SCM component, SCM attr, SCM new_value),
		"")
{
	scm_assert_foreign_object_type (vcomponent_type, component);
	vcomponent* com = scm_foreign_object_ref (component, 0);

	char* key = scm_to_utf8_stringn(scm_string_upcase(attr), NULL);
	content_line* c = get_property (com, key);
	free(key);

	scm_gc_unprotect_object(c->val.cur->value->key.scm);
	c->val.cur->value->key.scm = new_value;
	scm_gc_protect_object(c->val.cur->value->key.scm);

	return SCM_UNSPECIFIED;
}

SCM_DEFINE (vcomponent_child_count, "%vcomponent-child-count", 1, 0, 0,
		(SCM component),
		"Returns number of child components.")
{
	scm_assert_foreign_object_type (vcomponent_type, component);
	vcomponent* c = scm_foreign_object_ref (component, 0);
	return scm_from_size_t (SIZE(VECT(vcomponent))(&c->components));
}

SCM_DEFINE(vcomponent_children, "%vcomponent-children", 1, 0, 0,
		(SCM component),
		"")
{
	scm_assert_foreign_object_type (vcomponent_type, component);
	vcomponent* cal = scm_foreign_object_ref (component, 0);
	return scm_from_vector(&cal->components, vcomponent_type);
}

SCM_DEFINE(vcomponent_push_child_x, "%vcomponent-push-child!", 2, 0, 0,
               (SCM component, SCM child),
               "")
{
	scm_assert_foreign_object_type (vcomponent_type, component);
	scm_assert_foreign_object_type (vcomponent_type, child);
	vcomponent* comp = scm_foreign_object_ref (component, 0);
	vcomponent* chil = scm_foreign_object_ref (child, 0);

	PUSH(vcomponent)(comp, chil);

	return SCM_UNSPECIFIED;
}


SCM_DEFINE(vcomponent_typeof, "%vcomponent-type", 1, 0, 0,
		(SCM component),
		"Returns type of vcomponent")
{
	scm_assert_foreign_object_type (vcomponent_type, component);
	vcomponent* comp = scm_foreign_object_ref (component, 0);
	return scm_from_utf8_symbol(comp->type);
}

void init_lib (void) {
	init_vcomponent_type();

#ifndef SCM_MAGIC_SNARFER
#include "guile_interface.x"
#endif
}
