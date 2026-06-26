//
// Copyright (C) 2024-2026 The Goldfish Scheme Authors
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
#include <tbox/tbox.h>
#include <cstring>

namespace goldfish {

inline void
glue_define (s7_scheme* sc, const char* name, const char* desc, s7_function f, s7_int required, s7_int optional) {
  s7_pointer cur_env= s7_curlet (sc);
  s7_pointer func   = s7_make_typed_function (sc, name, f, required, optional, false, desc, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, name), func);
}

static s7_pointer
g_base64_encode_bv (s7_scheme* sc, s7_pointer args) {
  s7_pointer bv= s7_car (args);
  if (!s7_is_byte_vector (bv)) {
    return s7_wrong_type_arg_error (sc, "g_bytevector-base64-encode", 1, bv, "bytevector");
  }
  tb_byte_t* data     = s7_byte_vector_elements (bv);
  s7_int     len      = s7_vector_length (bv);
  tb_size_t  out_len  = ((len + 2) / 3) * 4;
  tb_char_t* out      = new tb_char_t[out_len + 1];
  tb_size_t  real_len = tb_base64_encode (data, len, out, out_len);
  out[real_len]       = '\0';
  s7_pointer result   = s7_make_string_with_length (sc, out, real_len);
  delete[] out;
  return result;
}

static s7_pointer
g_base64_decode_bv (s7_scheme* sc, s7_pointer args) {
  s7_pointer str= s7_car (args);
  if (!s7_is_string (str)) {
    return s7_wrong_type_arg_error (sc, "g_bytevector-base64-decode", 1, str, "string");
  }
  const char* data    = s7_string (str);
  s7_int      len     = s7_string_length (str);
  tb_size_t   out_len = (len / 4) * 3;
  tb_byte_t*  out     = new tb_byte_t[out_len];
  tb_size_t   real_len= tb_base64_decode (data, len, out, out_len);
  if (real_len > out_len) {
    delete[] out;
    return s7_error (sc, s7_make_symbol (sc, "value-error"),
      s7_cons (sc, s7_make_string (sc, "Invalid base64 input"), s7_nil (sc)));
  }
  s7_pointer result= s7_make_byte_vector (sc, real_len, 1, NULL);
  memcpy (s7_byte_vector_elements (result), out, real_len);
  delete[] out;
  return result;
}

static s7_pointer
f_string_base64_encode (s7_scheme* sc, s7_pointer args) {
  s7_pointer str= s7_car (args);
  if (!s7_is_string (str)) {
    return s7_wrong_type_arg_error (sc, "g_string-base64-encode", 1, str, "string");
  }
  s7_pointer bv= s7_apply_function (sc, s7_name_to_value (sc, "string->utf8"), s7_cons (sc, str, s7_nil (sc)));
  return g_base64_encode_bv (sc, s7_cons (sc, bv, s7_nil (sc)));
}

static s7_pointer
f_string_base64_decode (s7_scheme* sc, s7_pointer args) {
  s7_pointer str= s7_car (args);
  if (!s7_is_string (str)) {
    return s7_wrong_type_arg_error (sc, "g_string-base64-decode", 1, str, "string");
  }
  s7_pointer bv= g_base64_decode_bv (sc, s7_cons (sc, str, s7_nil (sc)));
  return s7_apply_function (sc, s7_name_to_value (sc, "utf8->string"), s7_cons (sc, bv, s7_nil (sc)));
}

static s7_pointer
f_bytevector_base64_encode (s7_scheme* sc, s7_pointer args) {
  return g_base64_encode_bv (sc, args);
}

static s7_pointer
f_bytevector_base64_decode (s7_scheme* sc, s7_pointer args) {
  s7_pointer str= s7_car (args);
  if (s7_is_byte_vector (str)) {
    str= s7_apply_function (sc, s7_name_to_value (sc, "utf8->string"), s7_cons (sc, str, s7_nil (sc)));
  }
  else if (!s7_is_string (str)) {
    return s7_wrong_type_arg_error (sc, "g_bytevector-base64-decode", 1, str, "string or bytevector");
  }
  return g_base64_decode_bv (sc, s7_cons (sc, str, s7_nil (sc)));
}

static s7_pointer
f_base64_encode (s7_scheme* sc, s7_pointer args) {
  s7_pointer x= s7_car (args);
  if (s7_is_string (x)) {
    return f_string_base64_encode (sc, args);
  }
  else if (s7_is_byte_vector (x)) {
    return f_bytevector_base64_encode (sc, args);
  }
  else {
    return s7_wrong_type_arg_error (sc, "g_base64-encode", 1, x, "string or bytevector");
  }
}

static s7_pointer
f_base64_decode (s7_scheme* sc, s7_pointer args) {
  s7_pointer x= s7_car (args);
  if (s7_is_string (x)) {
    return f_string_base64_decode (sc, args);
  }
  else if (s7_is_byte_vector (x)) {
    return f_bytevector_base64_decode (sc, args);
  }
  else {
    return s7_wrong_type_arg_error (sc, "g_base64-decode", 1, x, "string or bytevector");
  }
}

static void
glue_string_base64_encode (s7_scheme* sc) {
  glue_define (sc, "g_string-base64-encode", "(g_string-base64-encode str) => string", f_string_base64_encode, 1, 0);
}

static void
glue_string_base64_decode (s7_scheme* sc) {
  glue_define (sc, "g_string-base64-decode", "(g_string-base64-decode str) => string", f_string_base64_decode, 1, 0);
}

static void
glue_bytevector_base64_encode (s7_scheme* sc) {
  glue_define (sc, "g_bytevector-base64-encode", "(g_bytevector-base64-encode bv) => string",
    f_bytevector_base64_encode, 1, 0);
}

static void
glue_bytevector_base64_decode (s7_scheme* sc) {
  glue_define (sc, "g_bytevector-base64-decode", "(g_bytevector-base64-decode str) => bytevector",
    f_bytevector_base64_decode, 1, 0);
}

static void
glue_base64_encode (s7_scheme* sc) {
  glue_define (sc, "g_base64-encode", "(g_base64-encode x) => string", f_base64_encode, 1, 0);
}

static void
glue_base64_decode (s7_scheme* sc) {
  glue_define (sc, "g_base64-decode", "(g_base64-decode x) => string|bytevector", f_base64_decode, 1, 0);
}

void
glue_liii_base64 (s7_scheme* sc) {
  glue_string_base64_encode (sc);
  glue_string_base64_decode (sc);
  glue_bytevector_base64_encode (sc);
  glue_bytevector_base64_decode (sc);
  glue_base64_encode (sc);
  glue_base64_decode (sc);
}

} // namespace goldfish
