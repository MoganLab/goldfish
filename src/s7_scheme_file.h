/* s7_scheme_file.h - file library declarations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#ifndef S7_SCHEME_FILE_H
#define S7_SCHEME_FILE_H

#include "s7.h"

#ifdef __cplusplus
extern "C" {
#endif

s7_pointer g_open_input_file(s7_scheme *sc, s7_pointer args);
s7_pointer g_open_output_file(s7_scheme *sc, s7_pointer args);
s7_pointer g_call_with_input_file(s7_scheme *sc, s7_pointer args);
s7_pointer g_with_input_from_file(s7_scheme *sc, s7_pointer args);
s7_pointer g_call_with_output_file(s7_scheme *sc, s7_pointer args);
s7_pointer g_with_output_to_file(s7_scheme *sc, s7_pointer args);

#ifdef __cplusplus
}
#endif

#endif /* S7_SCHEME_FILE_H */
