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
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include "gf.h"
#include "gf_glue.hpp"

#include <cstdint>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

namespace goldfish {

// ---------------------------------------------------------------------------
// Plain C++ string helpers (no s7 dependency).
// ---------------------------------------------------------------------------

// UTF-8 sequence length from leading byte; 0 for invalid leading bytes.
static inline int
utf8_seq_len (uint8_t b) {
  if (b < 0x80) return 1;
  if ((b & 0xE0) == 0xC0) return 2;
  if ((b & 0xF0) == 0xE0) return 3;
  if ((b & 0xF8) == 0xF0) return 4;
  return 0;
}

// Encode a codepoint to UTF-8 in out, returning byte count (1..4).
static int
utf8_encode (uint32_t cp, char* out) {
  if (cp < 0x80) {
    out[0]= (char) cp;
    return 1;
  }
  if (cp < 0x800) {
    out[0]= (char) (0xC0 | (cp >> 6));
    out[1]= (char) (0x80 | (cp & 0x3F));
    return 2;
  }
  if (cp < 0x10000) {
    out[0]= (char) (0xE0 | (cp >> 12));
    out[1]= (char) (0x80 | ((cp >> 6) & 0x3F));
    out[2]= (char) (0x80 | (cp & 0x3F));
    return 3;
  }
  out[0]= (char) (0xF0 | (cp >> 18));
  out[1]= (char) (0x80 | ((cp >> 12) & 0x3F));
  out[2]= (char) (0x80 | ((cp >> 6) & 0x3F));
  out[3]= (char) (0x80 | (cp & 0x3F));
  return 4;
}

// Separator is either a string or a character.
using split_sep= std::variant<std::string, char32_t>;

// string-split returns an s7 list of strings, so it yields a gf_strlist.
gf_strlist
string_split (const std::string& str, const split_sep& sep_arg) {
  gf_strlist result;

  std::string sep;
  if (auto* s= std::get_if<std::string> (&sep_arg)) {
    sep= *s;
  }
  else {
    char buf[4];
    int  n= utf8_encode ((uint32_t) std::get<char32_t> (sep_arg), buf);
    sep.assign (buf, (size_t) n);
  }

  std::string_view sv (str);
  if (sep.empty ()) {
    // Empty separator: split on UTF-8 characters (invalid bytes as singles).
    size_t i= 0;
    while (i < sv.size ()) {
      int n= utf8_seq_len ((uint8_t) sv[i]);
      if (n == 0 || i + (size_t) n > sv.size ()) n= 1;
      result.items.emplace_back (sv.substr (i, (size_t) n));
      i+= (size_t) n;
    }
  }
  else {
    size_t start= 0;
    while (true) {
      size_t pos= sv.find (sep, start);
      if (pos == std::string_view::npos) {
        result.items.emplace_back (sv.substr (start));
        break;
      }
      result.items.emplace_back (sv.substr (start, pos - start));
      start= pos + sep.size ();
    }
  }
  return result;
}

// ---------------------------------------------------------------------------
// Declarative glue.
// ---------------------------------------------------------------------------

GF_GLUE ("g_string-split", "(g_string-split str sep) => list of strings", string_split);

void
glue_liii_string (gf::scheme* sc) {
  glue_string_split (sc);
}

} // namespace goldfish