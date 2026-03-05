/* s7_scheme_file.h - file utility declarations for s7 Scheme interpreter
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

s7_pointer g_delete_file(s7_scheme *sc, s7_pointer args);
s7_pointer g_access(s7_scheme *sc, s7_pointer args);
s7_pointer g_file_mtime(s7_scheme *sc, s7_pointer args);
s7_pointer g_unlink(s7_scheme *sc, s7_pointer args);

#ifdef __cplusplus
}
#endif

#endif /* S7_SCHEME_FILE_H */
