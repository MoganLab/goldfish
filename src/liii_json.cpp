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
#include <cstdlib>
#include <string>

namespace goldfish {

// json->string 的 C++ 实现，语义与历史上 (guenchi json) 中的 Scheme 实现完全一致：
//   - vector   => JSON 数组
//   - 序对列表  => JSON 对象（键为符号时输出宽松格式，即不带引号）
//   - '()      => {}
//   - 字符串    => 转义后带引号输出（\" \\ \/ \b \f \n \r \t，多字节 UTF-8 原样输出）
//   - symbol   => 原样输出（如 true false null）
//   - number   => number->string
//   - boolean  => true / false

static s7_pointer
json_type_error (s7_scheme* sc, const char* msg, s7_pointer arg) {
  return s7_error (sc, s7_make_symbol (sc, "type-error"), s7_list (sc, 2, s7_make_string (sc, msg), arg));
}

static void
json_write_escaped_string (std::string& out, const char* s, s7_int len) {
  out.push_back ('"');
  for (s7_int i= 0; i < len; i++) {
    unsigned char c= (unsigned char) s[i];
    if (c < 0x80) {
      switch (c) {
        case '"': out+= "\\\""; break;
        case '\\': out+= "\\\\"; break;
        case '/': out+= "\\/"; break;
        case '\b': out+= "\\b"; break;
        case '\f': out+= "\\f"; break;
        case '\n': out+= "\\n"; break;
        case '\r': out+= "\\r"; break;
        case '\t': out+= "\\t"; break;
        default: out.push_back ((char) c); break;
      }
    }
    else {
      // 多字节 UTF-8 字符，直接输出原始字节
      out.push_back ((char) c);
    }
  }
  out.push_back ('"');
}

static void json_write_value (s7_scheme* sc, s7_pointer x, std::string& out);

static void
json_write_scalar (s7_scheme* sc, s7_pointer x, std::string& out) {
  if (s7_is_string (x)) {
    json_write_escaped_string (out, s7_string (x), s7_string_length (x));
  }
  else if (s7_is_number (x)) {
    char* s= s7_number_to_string (sc, x, 10);
    out+= s;
    free (s);
  }
  else if (s7_is_boolean (x)) {
    out+= (s7_boolean (sc, x) ? "true" : "false");
  }
  else if (s7_is_symbol (x)) {
    out+= s7_symbol_name (x);
  }
  else if (s7_is_null (sc, x)) {
    out+= "{}";
  }
  else {
    json_type_error (sc, "Unexpected x: ", x);
  }
}

// 统计序对链的长度；链以非空原子结尾（非真列表）时返回 -1
static s7_int
json_pair_chain_length (s7_scheme* sc, s7_pointer x) {
  s7_int     len= 0;
  s7_pointer p  = x;
  while (s7_is_pair (p)) {
    len++;
    p= s7_cdr (p);
  }
  if (!s7_is_null (sc, p)) return -1;
  return len;
}

static void
json_write_object_entry (s7_scheme* sc, s7_pointer d, std::string& out) {
  if (s7_is_null (sc, d)) {
    out+= "{}";
    return;
  }
  if (!s7_is_pair (d)) {
    s7_error (sc, s7_make_symbol (sc, "value-error"),
              s7_list (sc, 2, d, s7_make_string (sc, " must be null, pair, or list with at least 2 elements")));
    return;
  }
  s7_int len= json_pair_chain_length (sc, d);
  if (!(len == -1 || len >= 2)) {
    s7_error (sc, s7_make_symbol (sc, "value-error"),
              s7_list (sc, 2, d, s7_make_string (sc, " must be null, pair, or list with at least 2 elements")));
    return;
  }
  s7_pointer k= s7_car (d);
  s7_pointer v= s7_cdr (d);
  json_write_scalar (sc, k, out);
  out.push_back (':');
  if (s7_is_null (sc, v)) {
    out+= "{}";
  }
  else if ((s7_is_pair (v) && json_pair_chain_length (sc, v) != -1) || s7_is_vector (v)) {
    json_write_value (sc, v, out);
  }
  else {
    json_write_scalar (sc, v, out);
  }
}

static void
json_write_value (s7_scheme* sc, s7_pointer x, std::string& out) {
  if (s7_is_vector (x)) {
    out.push_back ('[');
    s7_int      len  = s7_vector_length (x);
    s7_pointer* elems= s7_vector_elements (x);
    for (s7_int i= 0; i < len; i++) {
      if (i > 0) out.push_back (',');
      s7_pointer k= elems[i];
      if (s7_is_vector (k) || s7_is_pair (k)) {
        json_write_value (sc, k, out);
      }
      else {
        json_write_scalar (sc, k, out);
      }
    }
    out.push_back (']');
  }
  else if (s7_is_pair (x)) {
    out.push_back ('{');
    s7_pointer lst= x;
    s7_int     i  = 0;
    while (s7_is_pair (lst)) {
      if (i > 0) out.push_back (',');
      json_write_object_entry (sc, s7_car (lst), out);
      lst= s7_cdr (lst);
      i++;
    }
    if (!s7_is_null (sc, lst)) {
      s7_error (sc, s7_make_symbol (sc, "value-error"),
                s7_list (sc, 2, lst, s7_make_string (sc, " must be null, pair, or list with at least 2 elements")));
      return;
    }
    out.push_back ('}');
  }
  else {
    json_write_scalar (sc, x, out);
  }
}

static s7_pointer
f_json_to_string (s7_scheme* sc, s7_pointer args) {
  s7_pointer x= s7_car (args);
  if (s7_is_procedure (x)) {
    return s7_error (sc, s7_make_symbol (sc, "type-error"),
                     s7_list (sc, 1, s7_make_string (sc, "json->string: input must not be a procedure")));
  }
  std::string out;
  json_write_value (sc, x, out);
  return s7_make_string_with_length (sc, out.data (), (s7_int) out.size ());
}

static void
glue_json_to_string (s7_scheme* sc) {
  const char* name= "g_json->string";
  const char* desc= "(g_json->string data) => string, encode Scheme-form JSON data to a JSON string";
  s7_define_function (sc, name, f_json_to_string, 1, 0, false, desc);
}

void
glue_liii_json (s7_scheme* sc) {
  glue_json_to_string (sc);
}

} // namespace goldfish
