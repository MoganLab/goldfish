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

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include <tbox/platform/file.h>
#include <tbox/tbox.h>

namespace goldfish {

using std::optional;
using std::string;
using std::vector;

// ---------------------------------------------------------------------------
// Plain C++ file helpers (no s7 dependency).
// ---------------------------------------------------------------------------

static bool
file_is_type (const string& path, int type) {
  tb_file_info_t info;
  if (!tb_file_info (path.c_str (), &info)) return false;
  switch (type) {
    case 0: return info.type == TB_FILE_TYPE_DIRECTORY || info.type == TB_FILE_TYPE_DOT || info.type == TB_FILE_TYPE_DOT2;
    case 1: return info.type == TB_FILE_TYPE_FILE;
    default: return false;
  }
}

bool
isdir (const string& path) {
  return file_is_type (path, 0);
}

bool
isfile (const string& path) {
  return file_is_type (path, 1);
}

int64_t
path_getsize (const string& path) {
  tb_file_info_t info;
  if (tb_file_info (path.c_str (), &info)) return (int64_t) info.size;
  return -1;
}

int64_t
path_getmtime (const string& path) {
  tb_file_info_t info;
  if (tb_file_info (path.c_str (), &info)) return (int64_t) info.mtime;
  return -1;
}

optional<string>
path_read_text (const string& path) {
  tb_file_ref_t file= tb_file_init (path.c_str (), TB_FILE_MODE_RO);
  if (file == tb_null) return std::nullopt;

  tb_file_sync (file);
  tb_size_t size= tb_file_size (file);

  if (size == 0) {
    tb_file_exit (file);
    return string ();
  }

  vector<uint8_t> buffer (size);
  tb_size_t real_size= tb_file_read (file, buffer.data (), size);
  tb_file_exit (file);
  if (real_size != size) return std::nullopt;

  string content (reinterpret_cast<char*> (buffer.data ()), real_size);

  // Normalize line endings: convert \r\n to \n (also handles standalone \r).
  string normalized;
  normalized.reserve (content.size ());
  for (size_t i= 0; i < content.size (); ++i) {
    if (content[i] == '\r') {
      if (i + 1 < content.size () && content[i + 1] == '\n') continue;
      normalized.push_back ('\n');
    }
    else {
      normalized.push_back (content[i]);
    }
  }
  return normalized;
}

optional<vector<uint8_t>>
path_read_bytes (const string& path) {
  tb_file_ref_t file= tb_file_init (path.c_str (), TB_FILE_MODE_RO);
  if (file == tb_null) return std::nullopt;

  tb_file_sync (file);
  tb_size_t size= tb_file_size (file);

  if (size == 0) {
    tb_file_exit (file);
    return vector<uint8_t> ();
  }

  vector<uint8_t> buffer (size);
  tb_size_t real_size= tb_file_read (file, buffer.data (), size);
  tb_file_exit (file);
  if (real_size != size) return std::nullopt;
  return buffer;
}

// Write content, return bytes written or -1 on failure.  Binary flag selects
// TB_FILE_MODE semantics (identical here; kept for clarity).
static int64_t
write_file (const string& path, const uint8_t* data, size_t data_size, bool append) {
  tb_size_t flags= TB_FILE_MODE_WO | TB_FILE_MODE_CREAT;
  flags          = append ? (flags | TB_FILE_MODE_APPEND) : (flags | TB_FILE_MODE_TRUNC);

  tb_file_ref_t file= tb_file_init (path.c_str (), flags);
  if (file == tb_null) return -1;

  tb_filelock_ref_t lock= tb_filelock_init (file);
  if (tb_filelock_enter (lock, TB_FILELOCK_MODE_EX) == tb_false) {
    tb_filelock_exit (lock);
    tb_file_exit (file);
    return -1;
  }

  tb_size_t written_size= tb_file_writ (file, reinterpret_cast<const tb_byte_t*> (data), (tb_size_t) data_size);

  bool release_success= tb_filelock_leave (lock);
  tb_filelock_exit (lock);
  bool exit_success= tb_file_exit (file);

  if (written_size == data_size && release_success && exit_success) return (int64_t) written_size;
  return -1;
}

int64_t
path_write_text (const string& path, const string& content) {
  return write_file (path, reinterpret_cast<const uint8_t*> (content.data ()), content.size (), false);
}

int64_t
path_write_bytes (const string& path, const vector<uint8_t>& data) {
  return write_file (path, data.data (), data.size (), false);
}

int64_t
path_append_text (const string& path, const string& content) {
  return write_file (path, reinterpret_cast<const uint8_t*> (content.data ()), content.size (), true);
}

bool
path_touch (const string& path) {
  return tb_file_touch (path.c_str (), 0, 0) == tb_true;
}

bool
path_copy (const string& source, const string& target) {
  tb_file_ref_t src_file= tb_file_init (source.c_str (), TB_FILE_MODE_RO);
  if (src_file == tb_null) return false;

  tb_file_sync (src_file);
  tb_size_t size= tb_file_size (src_file);

  tb_file_ref_t dst_file= tb_file_init (target.c_str (), TB_FILE_MODE_WO | TB_FILE_MODE_CREAT | TB_FILE_MODE_TRUNC);
  if (dst_file == tb_null) {
    tb_file_exit (src_file);
    return false;
  }

  bool success= true;
  if (size > 0) {
    vector<uint8_t> buffer (size);
    tb_size_t read_size= tb_file_read (src_file, buffer.data (), size);
    if (read_size != size) {
      success= false;
    }
    else {
      tb_size_t written_size= tb_file_writ (dst_file, buffer.data (), read_size);
      if (written_size != read_size) success= false;
    }
  }

  tb_file_exit (src_file);
  tb_file_exit (dst_file);
  return success;
}

// ---------------------------------------------------------------------------
// Declarative glue.
// ---------------------------------------------------------------------------

GF_GLUE ("g_isdir",          "(g_isdir string) => boolean",            isdir);
GF_GLUE ("g_isfile",         "(g_isfile string) => boolean",           isfile);
GF_GLUE ("g_path-getsize",   "(g_path-getsize string) => integer",     path_getsize);
GF_GLUE ("g_path-getmtime",  "(g_path-getmtime string) => integer",    path_getmtime);
GF_GLUE ("g_path-read-text", "(g_path-read-text path) => string",      path_read_text);
GF_GLUE ("g_path-read-bytes", "(g_path-read-bytes path) => bytevector", path_read_bytes);
GF_GLUE ("g_path-write-text", "(g_path-write-text path content) => integer", path_write_text);
GF_GLUE ("g_path-write-bytes", "(g_path-write-bytes path bytevector) => integer", path_write_bytes);
GF_GLUE ("g_path-append-text", "(g_path-append-text path content) => integer", path_append_text);
GF_GLUE ("g_path-touch",     "(g_path-touch path) => boolean",         path_touch);
GF_GLUE ("g_path-copy",      "(g_path-copy source target) => boolean", path_copy);

void
glue_liii_path (gf::scheme* sc) {
  glue_isfile (sc);
  glue_isdir (sc);
  glue_path_getsize (sc);
  glue_path_getmtime (sc);
  glue_path_read_text (sc);
  glue_path_read_bytes (sc);
  glue_path_write_text (sc);
  glue_path_write_bytes (sc);
  glue_path_append_text (sc);
  glue_path_touch (sc);
  glue_path_copy (sc);
}

} // namespace goldfish