/* s7_internal_helpers.h - internal helper bridge declarations
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 */

#ifndef S7_INTERNAL_HELPERS_H
#define S7_INTERNAL_HELPERS_H

#include "s7.h"

#ifdef __cplusplus
extern "C" {
#endif

s7_pointer s7i_method_or_bust(s7_scheme *sc, s7_pointer obj, const char *method_name,
                              s7_pointer args, const char *type_name, s7_int arg_pos);

bool s7i_method_or_bust_bool(s7_scheme *sc, s7_pointer obj, const char *method_name,
                             s7_pointer args, const char *type_name, s7_int arg_pos);

#ifdef __cplusplus
}
#endif

#endif /* S7_INTERNAL_HELPERS_H */
