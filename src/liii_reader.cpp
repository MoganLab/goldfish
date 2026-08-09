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
#include <cerrno>
#include <climits>

namespace goldfish {

static bool
is_delim (uint8_t c) {
  // same set as delimiter? in reader.scm
  return (c == 0x09 || c == 0x0a || c == 0x0c || c == 0x0d || c == 0x20 ||
          c == 0x22 || c == 0x28 || c == 0x29 || c == 0x3b ||
          c == 0x5b || c == 0x5d || c == 0x7c);
}

/* plain_decimal: [sign] digits [. digits] [e[sign]digits] variants */
static bool
plain_decimal (const uint8_t* b, s7_int len) {
  s7_int i = 0;
  if (i < len && (b[i] == '+' || b[i] == '-'))
    i++;
  bool saw_dot = false, saw_exp = false, saw_digit = false;
  for (; i < len; i++) {
    uint8_t c = b[i];
    if (c >= '0' && c <= '9') {
      saw_digit = true;
    } else if (c == '.' && !saw_dot && !saw_exp) {
      saw_dot = true;
    } else if ((c == 'e' || c == 'E') && !saw_exp && saw_digit) {
      saw_exp = true;
      if (i + 1 < len && (b[i + 1] == '+' || b[i + 1] == '-'))
        i++;
    } else {
      return false;
    }
  }
  return saw_digit;
}

static bool
has_dot_or_exp (const uint8_t* b, s7_int len) {
  for (s7_int i = 0; i < len; i++)
    if (b[i] == '.' || b[i] == 'e' || b[i] == 'E')
      return true;
  return false;
}

/* g-scan-token str start first end-box as-number? => value
 *
 * The input is a slurped string (S7 strings hold raw bytes). The token starts
 * with the already-consumed char `first`, followed by str[start .. i) where i
 * is the first delimiter position (or the string end). The delimiter is not
 * consumed; end-box is set to i. When as-number? is true, plain decimal
 * tokens are returned as numbers (no string allocation); otherwise the token
 * is always returned as a string.
 */
static s7_pointer
f_scan_token (s7_scheme* sc, s7_pointer args) {
  s7_pointer str = s7_car (args);
  s7_int     start = s7_integer (s7_cadr (args));
  s7_int     first = s7_character (s7_caddr (args));
  s7_pointer box = s7_cadddr (args);
  bool       as_number = s7_boolean (sc, s7_car (s7_cddddr (args)));
  s7_int     len = s7_string_length (str);
  const uint8_t* s = (const uint8_t*) s7_string (str);

  s7_int i = start;
  while (i < len && !is_delim (s[i]))
    i++;
  s7_vector_set (sc, box, 0, s7_make_integer (sc, i));

  s7_int tok_len = i - start + 1;
  const uint8_t* buf;
  uint8_t stack_buf[64];
  uint8_t* heap_buf = nullptr;
  if (tok_len <= 64) {
    stack_buf[0] = (uint8_t) first;
    if (i > start)
      memcpy (stack_buf + 1, s + start, (size_t) (i - start));
    buf = stack_buf;
  } else {
    heap_buf = (uint8_t*) malloc ((size_t) tok_len);
    heap_buf[0] = (uint8_t) first;
    if (i > start)
      memcpy (heap_buf + 1, s + start, (size_t) (i - start));
    buf = heap_buf;
  }

  s7_pointer result;
  if (as_number && plain_decimal (buf, tok_len)) {
    if (!has_dot_or_exp (buf, tok_len)) {
      errno = 0;
      char* end = nullptr;
      long long v = strtoll ((const char*) buf, &end, 10);
      if (end == (const char*) buf + tok_len && errno != ERANGE &&
          v >= (long long) INT32_MIN && v <= (long long) INT32_MAX) {
        if (heap_buf) free (heap_buf);
        return s7_make_integer (sc, (s7_int) v);
      }
    }
    char* end = nullptr;
    double d = strtod ((const char*) buf, &end);
    if (end == (const char*) buf + tok_len) {
      if (heap_buf) free (heap_buf);
      return s7_make_real (sc, d);
    }
  }
  result = s7_make_string_with_length (sc, (const char*) buf, tok_len);
  if (heap_buf) free (heap_buf);
  return result;
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
  const char* desc = "(g-scan-token str start first end-box as-number?) => value";
  s7_define_function (sc, name, f_scan_token, 5, 0, false, desc);
  s7_define_function (sc, "g-skip-whitespace", f_skip_whitespace, 2, 0, false,
                      "(g-skip-whitespace str pos) => new-pos");
}

} // namespace goldfish
