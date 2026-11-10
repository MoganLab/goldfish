/* s7_scheme_file.h - file port declarations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#ifndef S7_SCHEME_FILE_H
#define S7_SCHEME_FILE_H

#include "s7.h"

#if !MS_WINDOWS
  #include <sys/stat.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define H_open_input_file "(open-input-file filename (mode \"r\")) opens filename for reading"
#define Q_open_input_file s7_make_signature(sc, 3, sc->is_input_port_symbol, sc->is_string_symbol, sc->is_string_symbol)

#define H_open_output_file "(open-output-file filename (mode \"w\")) opens filename for writing"
#define Q_open_output_file s7_make_signature(sc, 3, sc->is_output_port_symbol, sc->is_string_symbol, sc->is_string_symbol)

bool is_directory(const char *filename);
block_t *expand_filename(s7_scheme *sc, const char *name);

s7_pointer open_input_file_1(s7_scheme *sc, const char *name, const char *mode, const char *caller);
s7_pointer s7_open_input_file(s7_scheme *sc, const char *name, const char *mode);
s7_pointer g_open_input_file(s7_scheme *sc, s7_pointer args);

s7_pointer s7_open_output_file(s7_scheme *sc, const char *name, const char *mode);
s7_pointer g_open_output_file(s7_scheme *sc, s7_pointer args);

#ifdef S7_INTERNAL_H
const port_functions_t *s7i_output_file_functions(void);
#endif

#ifdef __cplusplus
}
#endif

#endif /* S7_SCHEME_FILE_H */
