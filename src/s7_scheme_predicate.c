/* s7_scheme_predicate.c - predicate implementations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#include "s7_scheme_predicate.h"
#include "s7_internal_helpers.h"

s7_pointer g_not(s7_scheme *sc, s7_pointer args)
{
  return((s7_car(args) == s7_f(sc)) ? s7_t(sc) : s7_f(sc));
}
