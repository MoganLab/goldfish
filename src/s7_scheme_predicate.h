/* s7_scheme_predicate.h - predicate function declarations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#ifndef S7_SCHEME_PREDICATE_H
#define S7_SCHEME_PREDICATE_H

#include "s7.h"

#ifdef __cplusplus
extern "C" {
#endif

s7_pointer g_not(s7_scheme *sc, s7_pointer args);
s7_pointer g_is_boolean(s7_scheme *sc, s7_pointer args);
s7_pointer g_is_unspecified(s7_scheme *sc, s7_pointer args);
s7_pointer g_is_number(s7_scheme *sc, s7_pointer args);
s7_pointer g_is_integer(s7_scheme *sc, s7_pointer args);
s7_pointer g_is_real(s7_scheme *sc, s7_pointer args);
s7_pointer g_is_complex(s7_scheme *sc, s7_pointer args);
s7_pointer g_is_rational(s7_scheme *sc, s7_pointer args);
s7_pointer g_is_keyword(s7_scheme *sc, s7_pointer args);
s7_pointer g_is_procedure(s7_scheme *sc, s7_pointer args);
s7_pointer g_is_dilambda(s7_scheme *sc, s7_pointer args);
s7_pointer g_is_sequence(s7_scheme *sc, s7_pointer args);

#ifdef __cplusplus
}
#endif

#endif /* S7_SCHEME_PREDICATE_H */
