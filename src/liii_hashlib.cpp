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

#include "s7.h"
#include "gf_glue.hpp"

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include <tbox/hash/md5.h>
#include <tbox/hash/sha.h>
#include <tbox/platform/file.h>
#include <tbox/tbox.h>

namespace goldfish {

using std::optional;
using std::string;
using std::vector;

// ---------------------------------------------------------------------------
// Plain C++ hashing helpers (no s7 dependency).
// ---------------------------------------------------------------------------

static string
hash_bytes_to_hex (const vector<uint8_t>& bytes) {
  static const char hex_digits[]= "0123456789abcdef";
  string out (bytes.size () * 2, '0');
  for (size_t i= 0; i < bytes.size (); ++i) {
    out[i * 2]    = hex_digits[bytes[i] >> 4];
    out[i * 2 + 1]= hex_digits[bytes[i] & 0x0f];
  }
  return out;
}

static string
md5_hex (const uint8_t* data, size_t len) {
  tb_byte_t digest[16];
  tb_md5_t  md5;
  tb_md5_init (&md5, 0);
  if (len > 0) tb_md5_spak (&md5, data, (tb_size_t) len);
  tb_md5_exit (&md5, digest, sizeof (digest));
  return hash_bytes_to_hex (vector<uint8_t> (digest, digest + sizeof (digest)));
}

static string
sha_hex (int bits, const uint8_t* data, size_t len) {
  size_t digest_size= (bits == 160) ? 20 : 32;
  tb_byte_t digest[32];
  tb_sha_t  sha;
  tb_sha_init (&sha, (tb_size_t) bits);
  if (len > 0) tb_sha_spak (&sha, data, (tb_size_t) len);
  tb_sha_exit (&sha, digest, digest_size);
  return hash_bytes_to_hex (vector<uint8_t> (digest, digest + digest_size));
}

string
md5 (const string& str) {
  return md5_hex ((const uint8_t*) str.data (), str.size ());
}

string
sha1 (const string& str) {
  return sha_hex (160, (const uint8_t*) str.data (), str.size ());
}

string
sha256 (const string& str) {
  return sha_hex (256, (const uint8_t*) str.data (), str.size ());
}

// Stream a file through the given digest callback (data, len).
template <typename Fn>
static bool
hash_file (const string& path, Fn&& spak) {
  tb_file_ref_t file= tb_file_init (path.c_str (), TB_FILE_MODE_RO);
  if (file == tb_null) return false;

  tb_size_t size  = tb_file_size (file);
  tb_size_t offset= 0;
  uint8_t   buffer[4096];
  while (offset < size) {
    tb_size_t want     = ((size - offset) > sizeof (buffer)) ? (tb_size_t) sizeof (buffer) : (size - offset);
    tb_size_t real_size= tb_file_read (file, buffer, want);
    if (real_size == 0) {
      tb_file_exit (file);
      return false;
    }
    spak (buffer, real_size);
    offset+= real_size;
  }
  tb_file_exit (file);
  return true;
}

optional<string>
md5_by_file (const string& path) {
  tb_md5_t md5;
  tb_md5_init (&md5, 0);
  bool ok= hash_file (path, [&] (const uint8_t* data, tb_size_t len) { tb_md5_spak (&md5, data, len); });
  if (!ok) return std::nullopt;
  tb_byte_t digest[16];
  tb_md5_exit (&md5, digest, sizeof (digest));
  return hash_bytes_to_hex (vector<uint8_t> (digest, digest + sizeof (digest)));
}

optional<string>
sha1_by_file (const string& path) {
  tb_sha_t sha;
  tb_sha_init (&sha, 160);
  bool ok= hash_file (path, [&] (const uint8_t* data, tb_size_t len) { tb_sha_spak (&sha, data, len); });
  if (!ok) return std::nullopt;
  tb_byte_t digest[20];
  tb_sha_exit (&sha, digest, sizeof (digest));
  return hash_bytes_to_hex (vector<uint8_t> (digest, digest + sizeof (digest)));
}

optional<string>
sha256_by_file (const string& path) {
  tb_sha_t sha;
  tb_sha_init (&sha, 256);
  bool ok= hash_file (path, [&] (const uint8_t* data, tb_size_t len) { tb_sha_spak (&sha, data, len); });
  if (!ok) return std::nullopt;
  tb_byte_t digest[32];
  tb_sha_exit (&sha, digest, sizeof (digest));
  return hash_bytes_to_hex (vector<uint8_t> (digest, digest + sizeof (digest)));
}

// ---------------------------------------------------------------------------
// Declarative glue.
// ---------------------------------------------------------------------------

GF_GLUE ("g_md5",          "(g_md5 str) => string",            md5);
GF_GLUE ("g_md5-by-file",  "(g_md5-by-file path) => string|#f", md5_by_file);
GF_GLUE ("g_sha1",         "(g_sha1 str) => string",           sha1);
GF_GLUE ("g_sha1-by-file", "(g_sha1-by-file path) => string|#f", sha1_by_file);
GF_GLUE ("g_sha256",       "(g_sha256 str) => string",         sha256);
GF_GLUE ("g_sha256-by-file", "(g_sha256-by-file path) => string|#f", sha256_by_file);

void
glue_liii_hashlib (s7_scheme* sc) {
  glue_md5 (sc);
  glue_md5_by_file (sc);
  glue_sha1 (sc);
  glue_sha1_by_file (sc);
  glue_sha256 (sc);
  glue_sha256_by_file (sc);
}

} // namespace goldfish