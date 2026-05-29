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

#ifdef __cplusplus
}
#endif

#endif /* S7_SCHEME_PREDICATE_H */
