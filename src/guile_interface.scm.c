#include "guile_interface.h"

#include "calendar.h"
#include "guile_type_helpers.h"

static SCM vcomponent_type;

void init_vcomponent_type (void) {
	SCM name = scm_from_utf8_symbol("vcomponent");
	SCM slots = scm_list_1(scm_from_utf8_symbol("data"));

	vcomponent_type = scm_make_foreign_object_type(name, slots, NULL);
}

SCM_DEFINE (make_vcomponent, "%vcomponent-make", 0, 1, 0,
		(SCM path),
		"Loads a vdir iCalendar from the given path.")
{
	vcomponent* cal =
		(vcomponent*) scm_gc_malloc (
				sizeof(*cal), "vcomponent");

	if (SCM_UNBNDP(path)) {
		INIT(vcomponent, cal);
	} else {
		INIT(vcomponent, cal, "ROOT");

		char* p = scm_to_utf8_stringn(path, NULL);
		read_vcalendar(cal, p);
		free(p);
	}

	return scm_from_vcomponent (cal);
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
	content_line* c = get_attributes (cal, key);

	if (c == NULL) {
		vcomponent_push_val(cal, key, "");
		c = get_attributes (cal, key);
		c->cval->key.scm = SCM_BOOL_F;
	}

	free(key);

	SCM val, proplist, attrlist = SCM_EOL;
	LLIST(strbuf) *triekeys, *trievals;
	/* For every instance of a line */
	FOR (LLIST, content_set, v, c) {
		val = scm_from_strbuf(&v->key);
		if (! scm_is_pair(val)) {
			// TODO look into using a weak hash table instead

			// TODO why is it an error to unprotect the object here?
			// scm_from_strbuf should already have protected it...
			// scm_gc_unprotect_object(v->key.scm);
			val = scm_cons(val, SCM_MAKE_HASH_TABLE());
			v->key.scm = val;
			scm_gc_protect_object(v->key.scm);

			triekeys = KEYS(TRIE(param_set))(&v->val);
			/* For every property key bound to the current attribute */
			FOR (LLIST, strbuf, k, triekeys) {
				proplist = SCM_EOL;

				trievals = GET(TRIE(param_set))(&v->val, k->mem);
				/* For every value bound to the current property */
				FOR (LLIST, strbuf, s, trievals) {
					proplist = scm_cons(scm_from_strbuf(s), proplist);
				}

				scm_hashq_set_x(scm_cdr(val), scm_from_strbuf_symbol(k),
						scm_reverse(proplist));
			}
		}

		attrlist = scm_cons(val, attrlist);
	}

	/* returns the car of list if list is one long. */
	if (scm_to_int(scm_length(attrlist)) == 1) {
		return SCM_CAR(attrlist);
	} else {
		return attrlist;
	}
}

SCM_DEFINE (vcomponent_child_count, "%vcomponent-child-count", 1, 0, 0,
		(SCM component),
		"Returns number of child components.")
{
	scm_assert_foreign_object_type (vcomponent_type, component);
	vcomponent* c = scm_foreign_object_ref (component, 0);
	return scm_from_size_t (SIZE(LLIST(vcomponent))(&c->components));
}

SCM_DEFINE(vcomponent_children, "%vcomponent-children", 1, 0, 0,
		(SCM component),
		"")
{
	scm_assert_foreign_object_type (vcomponent_type, component);
	vcomponent* cal = scm_foreign_object_ref (component, 0);

	SCM llist = SCM_EOL;
	FOR (LLIST, vcomponent, v, &cal->components) {
		llist = scm_cons(scm_from_vcomponent(v), llist);
	}
	return llist;
}

SCM_DEFINE(vcomponent_filter_children_x, "%vcomponent-filter-children!",
		2, 0, 0,
		(SCM pred, SCM component),
	   "Remove all children from component who DOESN'T satisfy `pred`")
{
	scm_assert_foreign_object_type (vcomponent_type, component);
	vcomponent* cal = scm_foreign_object_ref (component, 0);

	for (LINK(vcomponent)* l = FIRST(&cal->components);
			l->after != NULL;
			l = l->after)
	{
		if (scm_is_false(scm_call_1 (pred, scm_from_vcomponent(l->value)))) {
			FFREE(vcomponent, l->value);
			UNLINK(LINK(vcomponent))(l);
		}
	}

	return SCM_UNSPECIFIED;
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

SCM_DEFINE (vcomponent_parent, "%vcomponent-parent", 1, 0, 0,
		(SCM component),
		"")
{
	scm_assert_foreign_object_type (vcomponent_type, component);
	vcomponent* comp = scm_foreign_object_ref (component, 0);

	vcomponent* parent = comp->parent;
	if (strcmp(parent->type, "ROOT") == 0) {
		return SCM_BOOL_F;
	} else {
		return scm_from_vcomponent(parent);
	}
}

SCM_DEFINE(vcomponent_typeof, "%vcomponent-get-type", 1, 0, 0,
		(SCM component),
		"Returns type of vcomponent")
{
	scm_assert_foreign_object_type (vcomponent_type, component);
	vcomponent* comp = scm_foreign_object_ref (component, 0);
	return scm_from_utf8_symbol(comp->type);
}

SCM_DEFINE(vcomponent_set_type_x, "%vcomponent-set-type!", 2, 0, 0,
		(SCM component, SCM type),
		"Replace current type of vcomponent")
{
	scm_assert_foreign_object_type (vcomponent_type, component);
	vcomponent* comp = scm_foreign_object_ref (component, 0);

	if (comp->type) free (comp->type);

	char* ntype = scm_to_utf8_stringn (type, NULL);
	comp->type = calloc(sizeof(*ntype), strlen(ntype) + 1);
	strcpy(comp->type, ntype);

	return SCM_UNSPECIFIED;
}

SCM scm_from_vcomponent(vcomponent* v) {
	if (v->scm == NULL) {
		v->scm = scm_make_foreign_object_1 (vcomponent_type, v);
		scm_gc_protect_object(v->scm);
	}
	return v->scm;
}

SCM_DEFINE(vcomponent_attr_list, "%vcomponent-attribute-list", 1, 0, 0,
		(SCM component),
		"Returns list of all keys in component.")
{
	scm_assert_foreign_object_type (vcomponent_type, component);
	vcomponent* comp = scm_foreign_object_ref (component, 0);
	LLIST(strbuf)* keys = KEYS(TRIE(content_line))(&comp->clines);

	SCM llist = SCM_EOL;
	FOR (LLIST, strbuf, s, keys) {
		llist = scm_cons(scm_from_strbuf(s), llist);
	}

	FFREE(LLIST(strbuf), keys);

	return llist;
}

SCM_DEFINE(vcomponent_shallow_copy, "%vcomponent-shallow-copy", 1, 0, 0,
           (SCM component),
           "Creates a shallow copy of the given component.")
{
	scm_assert_foreign_object_type (vcomponent_type, component);
	vcomponent* src = scm_foreign_object_ref (component, 0);

	vcomponent* dest =
		(vcomponent*) scm_gc_malloc (
				sizeof(*dest), "vcomponent");
	INIT(vcomponent, dest, src->type, NULL);
	vcomponent_copy (dest, src);
	return scm_from_vcomponent (dest);
}

void init_lib (void) {
	init_vcomponent_type();

#ifndef SCM_MAGIC_SNARFER
#include "guile_interface.x"
#endif
}
