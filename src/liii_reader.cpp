//
// Copyright (C) 2026 The Goldfish Scheme Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations
// under the License.
//

#include "s7.h"
#include <cstring>
#include <cstdlib>

namespace goldfish {

static bool
is_delim (uint8_t c) {
  // same set as delimiter? in reader.scm
  return (c == 0x09 || c == 0x0a || c == 0x0c || c == 0x0d || c == 0x20 ||
          c == 0x22 || c == 0x28 || c == 0x29 || c == 0x3b ||
          c == 0x5b || c == 0x5d || c == 0x7c);
}

/* g-scan-token str start first => token-string
 *
 * The input is a slurped string (S7 strings hold raw bytes). The token starts
 * with the already-consumed char `first`, followed by str[start .. i) where i
 * is the first delimiter position (or the string end). The delimiter is not
 * consumed; the caller derives the new position as
 * start + (string-length token) - 1.
 */
static s7_pointer
f_scan_token (s7_scheme* sc, s7_pointer args) {
  s7_pointer str = s7_car (args);
  s7_int     start = s7_integer (s7_cadr (args));
  s7_int     first = s7_character (s7_caddr (args));
  s7_int     len = s7_string_length (str);
  const uint8_t* s = (const uint8_t*) s7_string (str);

  s7_int i = start;
  while (i < len && !is_delim (s[i]))
    i++;

  s7_int tok_len = i - start + 1;
  if (tok_len <= 64) {
    char buf[64];
    buf[0] = (char) first;
    if (i > start)
      memcpy (buf + 1, s + start, (size_t) (i - start));
    return s7_make_string_with_length (sc, buf, tok_len);
  }
  char* buf = (char*) malloc ((size_t) tok_len);
  buf[0] = (char) first;
  if (i > start)
    memcpy (buf + 1, s + start, (size_t) (i - start));
  s7_pointer tok = s7_make_string_with_length (sc, buf, tok_len);
  free (buf);
  return tok;
}

/* g-skip-whitespace str pos => new-pos
 *
 * Advances pos past whitespace (space, tab, newline, return, form feed).
 */
static s7_pointer
f_skip_whitespace (s7_scheme* sc, s7_pointer args) {
  s7_pointer str = s7_car (args);
  s7_int     pos = s7_integer (s7_cadr (args));
  s7_int     len = s7_string_length (str);
  const uint8_t* s = (const uint8_t*) s7_string (str);
  while (pos < len &&
         (s[pos] == 0x09 || s[pos] == 0x0a || s[pos] == 0x0c ||
          s[pos] == 0x0d || s[pos] == 0x20))
    pos++;
  return s7_make_integer (sc, pos);
}

void
glue_liii_reader (s7_scheme* sc) {
  const char* name = "g-scan-token";
  const char* desc = "(g-scan-token str start first) => token-string";
  s7_define_function (sc, name, f_scan_token, 3, 0, false, desc);
  s7_define_function (sc, "g-skip-whitespace", f_skip_whitespace, 2, 0, false,
                      "(g-skip-whitespace str pos) => new-pos");
}

} // namespace goldfish
