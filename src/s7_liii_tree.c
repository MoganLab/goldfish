/* s7_liii_tree.c - tree utility implementations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#include "s7_liii_tree.h"
#include "s7_internal_helpers.h"

s7_pointer g_tree_is_cyclic(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, s7i_tree_is_cyclic(sc, s7_car(args))));
}

s7_pointer g_tree_leaves(s7_scheme *sc, s7_pointer args)
{
  return(s7i_tree_leaves_p_p(sc, s7_car(args)));
}

s7_pointer g_tree_memq(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, s7i_tree_memq_b_7pp(sc, s7_car(args), s7_cadr(args))));
}

s7_pointer g_tree_set_memq(s7_scheme *sc, s7_pointer args)
{
  return(s7i_tree_set_memq_p_pp(sc, s7_car(args), s7_cadr(args)));
}

s7_pointer g_tree_set_memq_syms(s7_scheme *sc, s7_pointer args)
{
  return(s7i_tree_set_memq_syms_direct(sc, s7_car(args), s7_cadr(args)));
}

s7_pointer g_tree_count(s7_scheme *sc, s7_pointer args)
{
  s7_pointer obj = s7_car(args);
  s7_pointer tree = s7_cadr(args);
  s7_pointer count;

  if (!s7_is_pair(tree))
    {
      if ((s7_is_pair(s7_cddr(args))) &&
          (!s7_is_integer(s7_caddr(args))))
        return(s7_wrong_type_arg_error(sc, "tree-count", 3, s7_caddr(args), "an integer"));
      if (s7_is_null(sc, tree)) return(s7_make_integer(sc, 0));
      if (!s7i_has_active_methods(sc, tree))
        return(s7_wrong_type_arg_error(sc, "tree-count", 2, tree, "a list"));
      return(s7i_method_or_bust_pp(sc, tree, "tree-count", obj, tree, "a list", 2));
    }
  if (s7i_tree_is_cyclic(sc, tree))
    return(s7_error(sc, s7_make_symbol(sc, "wrong-type-arg"),
                    s7_list(sc, 2, s7_make_string(sc, "tree-count: tree is cyclic: ~S"), tree)));
  if (s7_is_null(sc, s7_cddr(args)))
    return(s7_make_integer(sc, s7i_tree_count(sc, obj, tree, 0)));
  count = s7_caddr(args);
  if (!s7_is_integer(count))
    return(s7_wrong_type_arg_error(sc, "tree-count", 3, count, "an integer"));
  return(s7_make_integer(sc, s7i_tree_count_at_least(sc, obj, tree, 0, s7_integer(count))));
}
