/* s7_liii_vector.c - vector utility implementations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#include "s7_liii_vector.h"

s7_pointer g_is_vector(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_vector(p)) return(s7_t(sc));
  {
    s7_pointer func = s7_method(sc, p, s7_make_symbol(sc, "vector?"));
    if (func == s7_undefined(sc)) return(s7_f(sc));
    return(s7_apply_function(sc, func, s7_cons(sc, p, s7_nil(sc))));
  }
}

s7_pointer g_vector_rank(s7_scheme *sc, s7_pointer args)
{
  s7_pointer vec = s7_car(args);
  if (!s7_is_vector(vec))
    return(s7i_sole_arg_method_or_bust(sc, vec, "vector-rank", args, "a vector"));
  return(s7_make_integer(sc, s7_vector_rank(vec)));
}
