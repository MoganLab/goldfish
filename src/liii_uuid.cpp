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
#include <chrono>
#include <cstdint>
#include <cstdio>
#include <random>

#include <tbox/tbox.h>

namespace goldfish {

static void
glue_define (s7_scheme* sc, const char* name, const char* desc, s7_function f, s7_int required, s7_int optional) {
  s7_pointer cur_env= s7_curlet (sc);
  s7_pointer func   = s7_make_typed_function (sc, name, f, required, optional, false, desc, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, name), func);
}

static s7_pointer
f_uuid4 (s7_scheme* sc, s7_pointer args) {
  tb_char_t        uuid[37];
  const tb_char_t* ret= tb_uuid4_make_cstr (uuid, tb_null);
  return s7_make_string (sc, ret);
}

static s7_pointer
f_uuid7 (s7_scheme* sc, s7_pointer args) {
  (void) args;
  thread_local std::mt19937_64 rng= [] {
    uint64_t seed= 0;
    try {
      std::random_device rd;
      seed= ((uint64_t) rd () << 32) | rd ();
    } catch (...) {
      seed= (uint64_t) std::chrono::high_resolution_clock::now ().time_since_epoch ().count ();
    }
    if (seed == 0) {
      seed= (uint64_t) std::chrono::high_resolution_clock::now ().time_since_epoch ().count ();
    }
    return std::mt19937_64 (seed);
  }();

  thread_local uint64_t last_ms    = 0;
  thread_local uint16_t last_rand_a= 0;

  uint64_t now_ms=
      std::chrono::duration_cast<std::chrono::milliseconds> (std::chrono::system_clock::now ().time_since_epoch ())
          .count ();

  if (now_ms > last_ms) {
    last_ms    = now_ms;
    last_rand_a= (uint16_t) (rng () & 0x0FFF);
  }
  else {
    now_ms     = last_ms;
    last_rand_a= (last_rand_a + 1) & 0x0FFF;
    if (last_rand_a == 0) {
      now_ms= ++last_ms;
    }
  }

  uint8_t bytes[16];
  // 48-bit timestamp (Big-Endian)
  bytes[0]= (uint8_t) ((now_ms >> 40) & 0xFF);
  bytes[1]= (uint8_t) ((now_ms >> 32) & 0xFF);
  bytes[2]= (uint8_t) ((now_ms >> 24) & 0xFF);
  bytes[3]= (uint8_t) ((now_ms >> 16) & 0xFF);
  bytes[4]= (uint8_t) ((now_ms >> 8) & 0xFF);
  bytes[5]= (uint8_t) (now_ms & 0xFF);

  // Version 7: 4 bits ver (0x7), 12 bits rand_a
  bytes[6]= (uint8_t) (0x70 | ((last_rand_a >> 8) & 0x0F));
  bytes[7]= (uint8_t) (last_rand_a & 0xFF);

  // Variant: 2 bits (10xx), 62 bits rand_b
  uint64_t rand_b= rng ();
  bytes[8]       = (uint8_t) (0x80 | ((rand_b >> 56) & 0x3F));
  bytes[9]       = (uint8_t) ((rand_b >> 48) & 0xFF);
  bytes[10]      = (uint8_t) ((rand_b >> 40) & 0xFF);
  bytes[11]      = (uint8_t) ((rand_b >> 32) & 0xFF);
  bytes[12]      = (uint8_t) ((rand_b >> 24) & 0xFF);
  bytes[13]      = (uint8_t) ((rand_b >> 16) & 0xFF);
  bytes[14]      = (uint8_t) ((rand_b >> 8) & 0xFF);
  bytes[15]      = (uint8_t) (rand_b & 0xFF);

  char buf[37];
  snprintf (buf, sizeof (buf), "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x", bytes[0],
            bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7], bytes[8], bytes[9], bytes[10],
            bytes[11], bytes[12], bytes[13], bytes[14], bytes[15]);

  return s7_make_string (sc, buf);
}

static void
glue_uuid4 (s7_scheme* sc) {
  const char* name= "g_uuid4";
  const char* desc= "(g_uuid4) => string";
  glue_define (sc, name, desc, f_uuid4, 0, 0);
}

static void
glue_uuid7 (s7_scheme* sc) {
  const char* name= "g_uuid7";
  const char* desc= "(g_uuid7) => string";
  glue_define (sc, name, desc, f_uuid7, 0, 0);
}

void
glue_liii_uuid (s7_scheme* sc) {
  glue_uuid4 (sc);
  glue_uuid7 (sc);
}

} // namespace goldfish
