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

#include "s7.h"
#include <cstring>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace goldfish {

static s7_pointer
liii_string_type_error (s7_scheme* sc, const char* msg, s7_pointer arg) {
  return s7_error (sc, s7_make_symbol (sc, "type-error"), s7_list (sc, 2, s7_make_string (sc, msg), arg));
}

// 返回 UTF-8 首字节 b 对应的码点字节宽度（1~4）；非法首字节返回 0
static inline int
utf8_seq_len (uint8_t b) {
  if (b < 0x80) return 1;
  if ((b & 0xE0) == 0xC0) return 2;
  if ((b & 0xF0) == 0xE0) return 3;
  if ((b & 0xF8) == 0xF0) return 4;
  return 0;
}

// 将码点编码为 UTF-8 写入 out，返回字节数（1~4）
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

static s7_pointer
f_string_split (s7_scheme* sc, s7_pointer args) {
  s7_pointer str_arg= s7_car (args);
  s7_pointer sep_arg= s7_cadr (args);

  if (!s7_is_string (str_arg)) {
    return liii_string_type_error (sc, "string-split: first parameter must be string", str_arg);
  }

  std::string sep;
  if (s7_is_string (sep_arg)) {
    sep.assign (s7_string (sep_arg), (size_t) s7_string_length (sep_arg));
  }
  else if (s7_is_character (sep_arg)) {
    char buf[4];
    int  n= utf8_encode (s7_character (sep_arg), buf);
    sep.assign (buf, (size_t) n);
  }
  else {
    return liii_string_type_error (sc, "string-split: second parameter must be string or char", sep_arg);
  }

  const char* s  = s7_string (str_arg);
  size_t      len= (size_t) s7_string_length (str_arg);

  std::vector<std::pair<size_t, size_t>> parts;
  if (sep.empty ()) {
    // 空分隔符：按 UTF-8 字符拆分；非法字节按单字节处理
    size_t i= 0;
    while (i < len) {
      int n= utf8_seq_len ((uint8_t) s[i]);
      if (n == 0 || i + (size_t) n > len) n= 1;
      parts.emplace_back (i, (size_t) n);
      i+= (size_t) n;
    }
  }
  else {
    std::string_view sv (s, len);
    size_t           start= 0;
    while (true) {
      size_t pos= sv.find (sep, start);
      if (pos == std::string_view::npos) {
        parts.emplace_back (start, len - start);
        break;
      }
      parts.emplace_back (start, pos - start);
      start= pos + sep.size ();
    }
  }

  /* no Scheme callbacks here, so args stay put; only the result being built
   * needs a GC anchor, with each new pair linked in right after s7_cons */
  s7_pointer head= s7_cons (sc, s7_nil (sc), s7_nil (sc));
  s7_gc_protect_via_stack (sc, head);
  s7_pointer tail= head;
  for (const auto& part : parts) {
    s7_pointer str= s7_make_string_with_length (sc, s + part.first, (s7_int) part.second);
    s7_set_cdr (tail, s7_cons (sc, str, s7_nil (sc)));
    tail= s7_cdr (tail);
  }
  s7_gc_unprotect_via_stack (sc, head);
  return s7_cdr (head);
}

static s7_pointer
f_string_starts_p (s7_scheme* sc, s7_pointer args) {
  s7_pointer str_arg= s7_car (args);
  s7_pointer prefix_arg= s7_cadr (args);

  if (!s7_is_string (str_arg) || !s7_is_string (prefix_arg)) {
    return s7_error (sc, s7_make_symbol (sc, "type-error"),
                     s7_list (sc, 1, s7_make_string (sc, "string-starts? parameter is not a string")));
  }

  // UTF-8 字节级前缀比较是精确的：前缀字节序列必然落在码点边界上
  const size_t str_len   = (size_t) s7_string_length (str_arg);
  const size_t prefix_len= (size_t) s7_string_length (prefix_arg);
  if (prefix_len > str_len) return s7_f (sc);
  return s7_make_boolean (sc, std::memcmp (s7_string (str_arg), s7_string (prefix_arg), prefix_len) == 0);
}

static s7_pointer
f_string_ends_p (s7_scheme* sc, s7_pointer args) {
  s7_pointer str_arg= s7_car (args);
  s7_pointer suffix_arg= s7_cadr (args);

  if (!s7_is_string (str_arg) || !s7_is_string (suffix_arg)) {
    return s7_error (sc, s7_make_symbol (sc, "type-error"),
                     s7_list (sc, 1, s7_make_string (sc, "string-ends? parameter is not a string")));
  }

  const size_t str_len   = (size_t) s7_string_length (str_arg);
  const size_t suffix_len= (size_t) s7_string_length (suffix_arg);
  if (suffix_len > str_len) return s7_f (sc);
  const char* tail= s7_string (str_arg) + (str_len - suffix_len);
  return s7_make_boolean (sc, std::memcmp (tail, s7_string (suffix_arg), suffix_len) == 0);
}

static void
glue_string_starts_p (s7_scheme* sc) {
  const char* name= "g_string-starts?";
  const char* desc= "(g_string-starts? str prefix) => boolean";
  s7_define_function (sc, name, f_string_starts_p, 2, 0, false, desc);
}

static void
glue_string_ends_p (s7_scheme* sc) {
  const char* name= "g_string-ends?";
  const char* desc= "(g_string-ends? str suffix) => boolean";
  s7_define_function (sc, name, f_string_ends_p, 2, 0, false, desc);
}

static void
glue_string_split (s7_scheme* sc) {
  const char* name= "g_string-split";
  const char* desc= "(g_string-split str sep) => list of strings";
  s7_define_function (sc, name, f_string_split, 2, 0, false, desc);
}

void
glue_liii_string (s7_scheme* sc) {
  glue_string_starts_p (sc);
  glue_string_ends_p (sc);
  glue_string_split (sc);
}

} // namespace goldfish
