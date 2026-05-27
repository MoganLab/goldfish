/* s7_liii_string.h - string utility declarations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#ifndef S7_LIII_STRING_H
#define S7_LIII_STRING_H

#include "s7.h"

#ifdef __cplusplus
extern "C" {
#endif

s7_pointer g_string_ref(s7_scheme *sc, s7_pointer args);
s7_pointer string_ref_1(s7_scheme *sc, s7_pointer strng, s7_pointer index);
s7_pointer g_string_set(s7_scheme *sc, s7_pointer args);
s7_pointer g_string_length(s7_scheme *sc, s7_pointer args);

s7_pointer g_make_string(s7_scheme *sc, s7_pointer args);
s7_pointer g_string_to_number(s7_scheme *sc, s7_pointer args);
s7_pointer g_substring(s7_scheme *sc, s7_pointer args);
s7_pointer g_string_copy(s7_scheme *sc, s7_pointer args);
s7_pointer g_string_fill(s7_scheme *sc, s7_pointer args);
s7_pointer g_string_to_list(s7_scheme *sc, s7_pointer args);
s7_pointer g_string_append(s7_scheme *sc, s7_pointer args);
s7_pointer g_string(s7_scheme *sc, s7_pointer args);
s7_pointer g_substring_uncopied(s7_scheme *sc, s7_pointer args);

s7_pointer g_is_string(s7_scheme *sc, s7_pointer args);

s7_pointer g_char_position(s7_scheme *sc, s7_pointer args);
s7_pointer char_position_p_ppi(s7_scheme *sc, s7_pointer chr, s7_pointer str, s7_int start);
s7_pointer g_char_position_csi(s7_scheme *sc, s7_pointer args);

s7_pointer g_string_position(s7_scheme *sc, s7_pointer args);

#if !WITH_PURE_S7
s7_pointer g_list_to_string(s7_scheme *sc, s7_pointer args);
#endif

#ifdef __cplusplus
}
#endif

#endif /* S7_LIII_STRING_H */
