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
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied See the
// License for the specific language governing permissions and limitations
// under the License.
//

#include "gf.h"
#include "gf_glue.hpp"

#include <array>
#include <cstdint>
#include <stdexcept>
#include <string_view>
#include <vector>

namespace goldfish {

using std::vector;

// ---------------------------------------------------------------------------
// Plain C++ base64 (no s7 dependency).  C++17: constexpr tables, std::array,
// std::string_view, range-based loops.
// ---------------------------------------------------------------------------

// decode_table[c] = decoded value, or 0xFF for invalid base64 characters.
constexpr std::array<uint8_t, 256>
make_decode_table () {
  std::array<uint8_t, 256> t{};
  for (size_t i= 0; i < 256; i++) t[i]= 0xFF;
  for (int c= 'A'; c <= 'Z'; c++) t[c]= (uint8_t) (c - 'A');
  for (int c= 'a'; c <= 'z'; c++) t[c]= (uint8_t) (c - 'a' + 26);
  for (int c= '0'; c <= '9'; c++) t[c]= (uint8_t) (c - '0' + 52);
  t['+']= 62;
  t['/']= 63;
  return t;
}

constexpr std::array<uint8_t, 64>
make_encode_table () {
  std::array<uint8_t, 64> t{};
  int i= 0;
  for (int c= 'A'; c <= 'Z'; c++) t[i++]= (uint8_t) c;
  for (int c= 'a'; c <= 'z'; c++) t[i++]= (uint8_t) c;
  for (int c= '0'; c <= '9'; c++) t[i++]= (uint8_t) c;
  t[i++]= '+';
  t[i++]= '/';
  return t;
}

constexpr auto BASE64_DECODE_TABLE= make_decode_table ();
constexpr auto BASE64_ENCODE_TABLE= make_encode_table ();
constexpr uint8_t PAD= (uint8_t) '=';

vector<uint8_t>
bytevector_base64_decode (const vector<uint8_t>& in) {
  const size_t in_len= in.size ();

  if (in_len % 4 != 0) {
    throw std::invalid_argument ("bytevector-base64-decode: length of the input bytevector must be 4X");
  }

  vector<uint8_t> out;
  out.reserve ((in_len / 4) * 3);

  for (size_t i= 0; i < in_len; i+= 4) {
    const uint8_t c1= in[i];
    const uint8_t c2= in[i + 1];
    const uint8_t c3= in[i + 2];
    const uint8_t c4= in[i + 3];

    const bool c3_pad= (c3 == PAD);
    const bool c4_pad= (c4 == PAD);

    if (c1 == PAD || c2 == PAD) {
      throw std::invalid_argument ("bytevector-base64-decode: Invalid base64 input");
    }

    const uint8_t v1= BASE64_DECODE_TABLE[c1];
    const uint8_t v2= BASE64_DECODE_TABLE[c2];
    const uint8_t v3= c3_pad ? 0 : BASE64_DECODE_TABLE[c3];
    const uint8_t v4= c4_pad ? 0 : BASE64_DECODE_TABLE[c4];

    if (v1 == 0xFF || v2 == 0xFF || (!c3_pad && v3 == 0xFF) || (!c4_pad && v4 == 0xFF) || (c3_pad && !c4_pad)) {
      throw std::invalid_argument ("bytevector-base64-decode: Invalid base64 input");
    }

    out.push_back ((uint8_t) ((v1 << 2) | (v2 >> 4)));
    if (!c3_pad) {
      out.push_back ((uint8_t) ((v2 << 4) | (v3 >> 2)));
      if (!c4_pad) {
        out.push_back ((uint8_t) ((v3 << 6) | v4));
      }
    }
  }

  return out;
}

vector<uint8_t>
bytevector_base64_encode (const vector<uint8_t>& in) {
  const size_t in_len= in.size ();
  const size_t out_len= (in_len == 0) ? 0 : 4 * ((in_len + 2) / 3);
  vector<uint8_t> out (out_len);

  size_t i= 0;
  size_t j= 0;

  while (i + 2 < in_len) {
    const uint32_t triple= ((uint32_t) in[i] << 16) | ((uint32_t) in[i + 1] << 8) | in[i + 2];
    out[j]     = BASE64_ENCODE_TABLE[(triple >> 18) & 0x3F];
    out[j + 1] = BASE64_ENCODE_TABLE[(triple >> 12) & 0x3F];
    out[j + 2] = BASE64_ENCODE_TABLE[(triple >> 6) & 0x3F];
    out[j + 3] = BASE64_ENCODE_TABLE[triple & 0x3F];
    i+= 3;
    j+= 4;
  }

  const size_t rem= in_len - i;
  if (rem == 1) {
    const uint32_t triple= (uint32_t) in[i] << 16;
    out[j]     = BASE64_ENCODE_TABLE[(triple >> 18) & 0x3F];
    out[j + 1] = BASE64_ENCODE_TABLE[(triple >> 12) & 0x3F];
    out[j + 2] = PAD;
    out[j + 3] = PAD;
  }
  else if (rem == 2) {
    const uint32_t triple= ((uint32_t) in[i] << 16) | ((uint32_t) in[i + 1] << 8);
    out[j]     = BASE64_ENCODE_TABLE[(triple >> 18) & 0x3F];
    out[j + 1] = BASE64_ENCODE_TABLE[(triple >> 12) & 0x3F];
    out[j + 2] = BASE64_ENCODE_TABLE[(triple >> 6) & 0x3F];
    out[j + 3] = PAD;
  }

  return out;
}

// ---------------------------------------------------------------------------
// Declarative glue.
// ---------------------------------------------------------------------------

GF_GLUE ("g_bytevector-base64-decode", "(g_bytevector-base64-decode bv) => bytevector", bytevector_base64_decode);
GF_GLUE ("g_bytevector-base64-encode", "(g_bytevector-base64-encode bv) => bytevector", bytevector_base64_encode);

void
glue_liii_base64 (s7_scheme* sc) {
  glue_bytevector_base64_decode (sc);
  glue_bytevector_base64_encode (sc);
}

} // namespace goldfish
