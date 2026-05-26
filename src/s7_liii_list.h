/* s7_liii_list.h - list utility declarations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 */

#ifndef S7_LIII_LIST_H
#define S7_LIII_LIST_H

#include "s7.h"

#ifdef __cplusplus
extern "C" {
#endif

s7_pointer g_is_null(s7_scheme *sc, s7_pointer args);
s7_pointer g_car(s7_scheme *sc, s7_pointer args);
s7_pointer g_cdr(s7_scheme *sc, s7_pointer args);
s7_pointer g_caar(s7_scheme *sc, s7_pointer args);
s7_pointer g_cadr(s7_scheme *sc, s7_pointer args);
s7_pointer g_cdar(s7_scheme *sc, s7_pointer args);
s7_pointer g_cddr(s7_scheme *sc, s7_pointer args);
s7_pointer g_set_car(s7_scheme *sc, s7_pointer args);
s7_pointer g_set_cdr(s7_scheme *sc, s7_pointer args);

s7_pointer g_list_ref(s7_scheme *sc, s7_pointer args);
s7_pointer g_list_tail(s7_scheme *sc, s7_pointer args);

#ifdef __cplusplus
}
#endif

#endif /* S7_LIII_LIST_H */
