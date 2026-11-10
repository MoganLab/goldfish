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
// WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations
// under the License.
//

#include "s7.h"
#include <algorithm>
#include <cctype>
#include <cstring>
#include <string>
#include <vector>

#include <limits.h>
#include <stdlib.h>

#include <tbox/platform/file.h>
#include <tbox/tbox.h>

#if !defined(TB_CONFIG_OS_WINDOWS)
#include <sys/stat.h>
#include <unistd.h>
#endif

namespace goldfish {

using std::string;

// 抛出 (liii error) 约定的 type-error，irritant 为出错的参数
inline s7_pointer
string_type_error (s7_scheme* sc, const char* msg, s7_pointer arg) {
  return s7_error (sc, s7_make_symbol (sc, "type-error"), s7_list (sc, 2, s7_make_string (sc, msg), arg));
}

inline void
glue_define (s7_scheme* sc, const char* name, const char* desc, s7_function f, s7_int required, s7_int optional) {
  s7_pointer cur_env= s7_curlet (sc);
  s7_pointer func   = s7_make_typed_function (sc, name, f, required, optional, false, desc, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, name), func);
}

static s7_pointer
f_isdir (s7_scheme* sc, s7_pointer args) {
  s7_pointer dir_arg= s7_car (args);
  if (!s7_is_string (dir_arg)) {
    return string_type_error (sc, "isdir: path must be a string", dir_arg);
  }
  const char*    dir_c= s7_string (dir_arg);
  tb_file_info_t info;
  bool           ret= false;
  if (tb_file_info (dir_c, &info)) {
    switch (info.type) {
    case TB_FILE_TYPE_DIRECTORY:
    case TB_FILE_TYPE_DOT:
    case TB_FILE_TYPE_DOT2:
      ret= true;
    }
  }
  return s7_make_boolean (sc, ret);
}

static void
glue_isdir (s7_scheme* sc) {
  const char* name= "g_isdir";
  const char* desc= "(g_isdir string) => boolean";
  glue_define (sc, name, desc, f_isdir, 1, 0);
}

static s7_pointer
f_isfile (s7_scheme* sc, s7_pointer args) {
  s7_pointer dir_arg= s7_car (args);
  if (!s7_is_string (dir_arg)) {
    return string_type_error (sc, "isfile: path must be a string", dir_arg);
  }
  const char*    dir_c= s7_string (dir_arg);
  tb_file_info_t info;
  bool           ret= false;
  if (tb_file_info (dir_c, &info)) {
    switch (info.type) {
    case TB_FILE_TYPE_FILE:
      ret= true;
    }
  }
  return s7_make_boolean (sc, ret);
}

static void
glue_isfile (s7_scheme* sc) {
  const char* name= "g_isfile";
  const char* desc= "(g_isfile string) => boolean";
  glue_define (sc, name, desc, f_isfile, 1, 0);
}

static s7_pointer
f_path_getsize (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "path-getsize: path must be a string", path_arg);
  }
  const char*    path_c= s7_string (path_arg);
  tb_file_info_t info;
  if (tb_file_info (path_c, &info)) {
    return s7_make_integer (sc, (int) info.size);
  }
  else {
    return s7_make_integer (sc, (int) -1);
  }
}

static void
glue_path_getsize (s7_scheme* sc) {
  const char* name= "g_path-getsize";
  const char* desc= "(g_path_getsize string): string => integer";
  glue_define (sc, name, desc, f_path_getsize, 1, 0);
}

static s7_pointer
f_path_read_text (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "path-read-text: path must be a string", path_arg);
  }
  const char* path= s7_string (path_arg);
  if (!path) {
    return s7_make_boolean (sc, false);
  }

  try {
    tb_file_info_t info;
    if (!tb_file_info (path, &info) || info.type != TB_FILE_TYPE_FILE) {
      return s7_make_boolean (sc, false);
    }

    tb_file_ref_t file= tb_file_init (path, TB_FILE_MODE_RO);
    if (file == tb_null) {
      return s7_make_boolean (sc, false);
    }

    tb_file_sync (file);

    tb_size_t size= tb_file_size (file);
    if (size == (tb_size_t) -1) {
      tb_file_exit (file);
      return s7_make_boolean (sc, false);
    }

    if (size == 0) {
      tb_file_exit (file);
      return s7_make_string (sc, "");
    }

    tb_byte_t* buffer= new (std::nothrow) tb_byte_t[size + 1];
    if (!buffer) {
      tb_file_exit (file);
      return s7_make_boolean (sc, false);
    }

    tb_size_t real_size= tb_file_read (file, buffer, size);
    tb_file_exit (file);

    if (real_size == (tb_size_t) -1) {
      delete[] buffer;
      return s7_make_boolean (sc, false);
    }

    buffer[real_size]= '\0';

    std::string content (reinterpret_cast<char*> (buffer), real_size);
    delete[] buffer;

    // Normalize line endings: convert \r\n to \n (also handles standalone \r)
    std::string normalized;
    normalized.reserve (content.size ());
    for (size_t i= 0; i < content.size (); ++i) {
      if (content[i] == '\r') {
        if (i + 1 < content.size () && content[i + 1] == '\n') {
          continue;
        }
        normalized.push_back ('\n');
      }
      else {
        normalized.push_back (content[i]);
      }
    }

    return s7_make_string (sc, normalized.c_str ());
  } catch (const std::exception& e) {
    return s7_make_boolean (sc, false);
  } catch (...) {
    return s7_make_boolean (sc, false);
  }
}

static void
glue_path_read_text (s7_scheme* sc) {
  const char* name= "g_path-read-text";
  const char* desc= "(g_path-read-text path) => string, read the content of the file at the given path";
  s7_define_function (sc, name, f_path_read_text, 1, 0, false, desc);
}

static s7_pointer
f_path_read_lines (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "path-read-lines: path must be a string", path_arg);
  }
  const char* path= s7_string (path_arg);
  if (!path) {
    return s7_make_boolean (sc, false);
  }

  bool       keepends = (s7_cadr (args) == s7_t (sc));
  s7_int     offset   = s7_integer (s7_caddr (args));
  s7_pointer limit_arg= s7_cadddr (args);
  s7_int     limit    = s7_is_integer (limit_arg) ? s7_integer (limit_arg) : -1;

  try {
    tb_file_info_t info;
    if (!tb_file_info (path, &info) || info.type != TB_FILE_TYPE_FILE) {
      return s7_make_boolean (sc, false);
    }

    tb_file_ref_t file= tb_file_init (path, TB_FILE_MODE_RO);
    if (file == tb_null) {
      return s7_make_boolean (sc, false);
    }

    tb_file_sync (file);
    tb_size_t size= tb_file_size (file);
    if (size == (tb_size_t) -1) {
      tb_file_exit (file);
      return s7_make_boolean (sc, false);
    }

    if (size == 0) {
      tb_file_exit (file);
      return s7_nil (sc);
    }

    std::string content;
    content.resize (size);
    tb_size_t real_size= tb_file_read (file, reinterpret_cast<tb_byte_t*> (&content[0]), size);
    tb_file_exit (file);

    if (real_size == (tb_size_t) -1) {
      return s7_make_boolean (sc, false);
    }
    content.resize (real_size);

    // 按行切分，\r\n 与孤立 \r 均归一化为 \n
    std::vector<std::string> lines;
    s7_int                   skipped  = 0;
    s7_int                   collected= 0;
    size_t                   n        = content.size ();
    size_t                   i        = 0;
    while (i < n && (limit < 0 || collected < limit)) {
      size_t j= i;
      while (j < n && content[j] != '\n' && content[j] != '\r') {
        j++;
      }
      bool   has_end= j < n;
      size_t next   = n;
      if (has_end) {
        if (content[j] == '\r' && j + 1 < n && content[j + 1] == '\n') {
          next= j + 2;
        }
        else {
          next= j + 1;
        }
      }
      if (skipped < offset) {
        skipped++;
      }
      else {
        std::string line= content.substr (i, j - i);
        if (keepends && has_end) {
          line.push_back ('\n');
        }
        lines.push_back (std::move (line));
        collected++;
      }
      i= next;
    }

    s7_pointer result= s7_nil (sc);
    for (auto it= lines.rbegin (); it != lines.rend (); ++it) {
      result= s7_cons (sc, s7_make_string_with_length (sc, it->data (), it->size ()), result);
    }
    return result;
  } catch (const std::exception& e) {
    return s7_make_boolean (sc, false);
  } catch (...) {
    return s7_make_boolean (sc, false);
  }
}

static void
glue_path_read_lines (s7_scheme* sc) {
  const char* name= "g_path-read-lines";
  const char* desc= "(g_path-read-lines path keepends offset limit) => list of strings, \
read the file at the given path line by line; limit -1 means read to end";
  glue_define (sc, name, desc, f_path_read_lines, 4, 0);
}

static s7_pointer
f_path_read_bytes (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "path-read-bytes: path must be a string", path_arg);
  }
  const char* path= s7_string (path_arg);
  if (!path) {
    return s7_make_boolean (sc, false);
  }

  try {
    tb_file_info_t info;
    if (!tb_file_info (path, &info) || info.type != TB_FILE_TYPE_FILE) {
      return s7_make_boolean (sc, false);
    }

    tb_file_ref_t file= tb_file_init (path, TB_FILE_MODE_RO);
    if (file == tb_null) {
      return s7_make_boolean (sc, false);
    }

    tb_file_sync (file);
    tb_size_t size= tb_file_size (file);
    if (size == (tb_size_t) -1) {
      tb_file_exit (file);
      return s7_make_boolean (sc, false);
    }

    if (size == 0) {
      tb_file_exit (file);
      return s7_make_byte_vector (sc, 0, 1, NULL);
    }

    tb_byte_t* buffer= new (std::nothrow) tb_byte_t[size];
    if (!buffer) {
      tb_file_exit (file);
      return s7_make_boolean (sc, false);
    }

    tb_size_t real_size= tb_file_read (file, buffer, size);
    tb_file_exit (file);

    if (real_size == (tb_size_t) -1 || real_size != size) {
      delete[] buffer;
      return s7_make_boolean (sc, false);
    }

    s7_pointer bytevector     = s7_make_byte_vector (sc, real_size, 1, NULL);
    tb_byte_t* bytevector_data= s7_byte_vector_elements (bytevector);
    memcpy (bytevector_data, buffer, real_size);

    delete[] buffer;
    return bytevector;
  } catch (const std::exception& e) {
    return s7_make_boolean (sc, false);
  } catch (...) {
    return s7_make_boolean (sc, false);
  }
}

static void
glue_path_read_bytes (s7_scheme* sc) {
  const char* name= "g_path-read-bytes";
  const char* desc= "(g_path-read-bytes path) => bytevector, read the binary content of the file at the given path";
  s7_define_function (sc, name, f_path_read_bytes, 1, 0, false, desc);
}

static s7_pointer
f_path_write_text (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "path-write-text: path must be a string", path_arg);
  }
  s7_pointer content_arg= s7_cadr (args);
  if (!s7_is_string (content_arg)) {
    return string_type_error (sc, "path-write-text: content must be a string", content_arg);
  }
  const char* path= s7_string (path_arg);
  if (!path) {
    return s7_make_integer (sc, -1);
  }

  const char* content= s7_string (content_arg);
  if (!content) {
    return s7_make_integer (sc, -1);
  }

  tb_file_ref_t file= tb_file_init (path, TB_FILE_MODE_WO | TB_FILE_MODE_CREAT | TB_FILE_MODE_TRUNC);
  if (file == tb_null) {
    return s7_make_integer (sc, -1);
  }

  tb_filelock_ref_t lock= tb_filelock_init (file);
  if (tb_filelock_enter (lock, TB_FILELOCK_MODE_EX) == tb_false) {
    tb_filelock_exit (lock);
    tb_file_exit (file);
    return s7_make_integer (sc, -1);
  }

  tb_size_t content_size= strlen (content);
  tb_size_t written_size= tb_file_writ (file, reinterpret_cast<const tb_byte_t*> (content), content_size);

  bool release_success= tb_filelock_leave (lock);
  tb_filelock_exit (lock);
  bool exit_success= tb_file_exit (file);

  if (written_size == content_size && release_success && exit_success) {
    return s7_make_integer (sc, written_size);
  }
  else {
    return s7_make_integer (sc, -1);
  }
}

static void
glue_path_write_text (s7_scheme* sc) {
  const char* name= "g_path-write-text";
  const char* desc= "(g_path-write-text path content) => integer,\
write content to the file at the given path and return the number of bytes written, or -1 on failure";
  s7_define_function (sc, name, f_path_write_text, 2, 0, false, desc);
}

static s7_pointer
f_path_write_bytes (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "path-write-bytes: path must be a string", path_arg);
  }
  s7_pointer bv= s7_cadr (args);
  if (!s7_is_byte_vector (bv)) {
    return string_type_error (sc, "path-write-bytes: content must be a bytevector", bv);
  }
  const char* path= s7_string (path_arg);
  if (!path) {
    return s7_make_integer (sc, -1);
  }

  tb_file_ref_t file= tb_file_init (path, TB_FILE_MODE_WO | TB_FILE_MODE_CREAT | TB_FILE_MODE_TRUNC);
  if (file == tb_null) {
    return s7_make_integer (sc, -1);
  }

  tb_filelock_ref_t lock= tb_filelock_init (file);
  if (tb_filelock_enter (lock, TB_FILELOCK_MODE_EX) == tb_false) {
    tb_filelock_exit (lock);
    tb_file_exit (file);
    return s7_make_integer (sc, -1);
  }

  tb_byte_t* data        = s7_byte_vector_elements (bv);
  tb_size_t  content_size= s7_vector_length (bv);
  tb_size_t  written_size= tb_file_writ (file, data, content_size);

  bool release_success= tb_filelock_leave (lock);
  tb_filelock_exit (lock);
  bool exit_success= tb_file_exit (file);

  if (written_size == content_size && release_success && exit_success) {
    return s7_make_integer (sc, written_size);
  }
  else {
    return s7_make_integer (sc, -1);
  }
}

static void
glue_path_write_bytes (s7_scheme* sc) {
  const char* name= "g_path-write-bytes";
  const char* desc= "(g_path-write-bytes path bytevector) => integer,\
write bytevector to the file at the given path in binary mode and return the number of bytes written, or -1 on failure";
  s7_define_function (sc, name, f_path_write_bytes, 2, 0, false, desc);
}

static s7_pointer
f_path_append_text (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "path-append-text: path must be a string", path_arg);
  }
  s7_pointer content_arg= s7_cadr (args);
  if (!s7_is_string (content_arg)) {
    return string_type_error (sc, "path-append-text: content must be a string", content_arg);
  }
  const char* path= s7_string (path_arg);
  if (!path) {
    return s7_make_integer (sc, -1);
  }

  const char* content= s7_string (content_arg);
  if (!content) {
    return s7_make_integer (sc, -1);
  }

  tb_file_ref_t file= tb_file_init (path, TB_FILE_MODE_WO | TB_FILE_MODE_CREAT | TB_FILE_MODE_APPEND);
  if (file == tb_null) {
    return s7_make_integer (sc, -1);
  }

  tb_filelock_ref_t lock= tb_filelock_init (file);
  if (tb_filelock_enter (lock, TB_FILELOCK_MODE_EX) == tb_false) {
    tb_filelock_exit (lock);
    tb_file_exit (file);
    return s7_make_integer (sc, -1);
  }

  tb_size_t content_size= strlen (content);
  tb_size_t written_size= tb_file_writ (file, reinterpret_cast<const tb_byte_t*> (content), content_size);

  bool release_success= tb_filelock_leave (lock);
  tb_filelock_exit (lock);
  bool exit_success= tb_file_exit (file);

  if (written_size == content_size && release_success && exit_success) {
    return s7_make_integer (sc, written_size);
  }
  else {
    return s7_make_integer (sc, -1);
  }
}

static void
glue_path_append_text (s7_scheme* sc) {
  const char* name= "g_path-append-text";
  const char* desc= "(g_path-append-text path content) => integer,\
append content to the file at the given path and return the number of bytes written, or -1 on failure";
  s7_define_function (sc, name, f_path_append_text, 2, 0, false, desc);
}

static s7_pointer
f_path_touch (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "path-touch: path must be a string", path_arg);
  }
  const char* path= s7_string (path_arg);
  if (!path) {
    return s7_make_boolean (sc, false);
  }

  tb_bool_t success= tb_file_touch (path, 0, 0);

  if (success == tb_true) {
    return s7_make_boolean (sc, true);
  }
  else {
    return s7_make_boolean (sc, false);
  }
}

static void
glue_path_touch (s7_scheme* sc) {
  const char* name= "g_path-touch";
  const char* desc= "(g_path-touch path) => boolean, create empty file or update modification time";
  s7_define_function (sc, name, f_path_touch, 1, 0, false, desc);
}

static s7_pointer
f_path_copy (s7_scheme* sc, s7_pointer args) {
  s7_pointer src_arg= s7_car (args);
  if (!s7_is_string (src_arg)) {
    return string_type_error (sc, "path-copy: src must be a string", src_arg);
  }
  s7_pointer dst_arg= s7_cadr (args);
  if (!s7_is_string (dst_arg)) {
    return string_type_error (sc, "path-copy: dst must be a string", dst_arg);
  }
  const char* source= s7_string (src_arg);
  const char* target= s7_string (dst_arg);

  if (!source || !target) {
    return s7_make_boolean (sc, false);
  }

  try {
    tb_file_info_t info;
    if (!tb_file_info (source, &info) || info.type != TB_FILE_TYPE_FILE) {
      return s7_make_boolean (sc, false);
    }

    tb_file_ref_t src_file= tb_file_init (source, TB_FILE_MODE_RO);
    if (src_file == tb_null) {
      return s7_make_boolean (sc, false);
    }

    tb_file_sync (src_file);
    tb_size_t size= tb_file_size (src_file);
    if (size == (tb_size_t) -1) {
      tb_file_exit (src_file);
      return s7_make_boolean (sc, false);
    }

    tb_file_ref_t dst_file= tb_file_init (target, TB_FILE_MODE_WO | TB_FILE_MODE_CREAT | TB_FILE_MODE_TRUNC);
    if (dst_file == tb_null) {
      tb_file_exit (src_file);
      return s7_make_boolean (sc, false);
    }

    bool success= true;

    if (size > 0) {
      tb_byte_t* buffer= new (std::nothrow) tb_byte_t[size];
      if (!buffer) {
        tb_file_exit (src_file);
        tb_file_exit (dst_file);
        return s7_make_boolean (sc, false);
      }
      tb_size_t read_size= tb_file_read (src_file, buffer, size);

      if (read_size != size) {
        success= false;
      }
      else {
        tb_size_t written_size= tb_file_writ (dst_file, buffer, read_size);
        if (written_size != read_size) {
          success= false;
        }
      }

      delete[] buffer;
    }

    tb_file_exit (src_file);
    tb_file_exit (dst_file);

    return s7_make_boolean (sc, success);
  } catch (...) {
    return s7_make_boolean (sc, false);
  }
}

static void
glue_path_copy (s7_scheme* sc) {
  const char* name= "g_path-copy";
  const char* desc= "(g_path-copy source target) => boolean, copy file from source to target";
  s7_define_function (sc, name, f_path_copy, 2, 0, false, desc);
}

static char
fold_case (char c) {
#if defined(TB_CONFIG_OS_WINDOWS)
  return (char) tolower ((unsigned char) c);
#else
  return c;
#endif
}

static bool
charclass_match (const char* pat, size_t plen, size_t& p_idx, char ch) {
  // pat[p_idx] is '['
  p_idx++;
  bool negate= false;
  if (p_idx < plen && (pat[p_idx] == '^' || pat[p_idx] == '!')) {
    negate= true;
    p_idx++;
  }
  bool matched  = false;
  char target_ch= fold_case (ch);

  while (p_idx < plen && pat[p_idx] != ']') {
    if (p_idx + 2 < plen && pat[p_idx + 1] == '-') {
      char lo= fold_case (pat[p_idx]);
      char hi= fold_case (pat[p_idx + 2]);
      if (lo > hi) {
        std::swap (lo, hi);
      }
      if (target_ch >= lo && target_ch <= hi) {
        matched= true;
      }
      p_idx+= 3;
    }
    else {
      if (fold_case (pat[p_idx]) == target_ch) {
        matched= true;
      }
      p_idx++;
    }
  }
  if (p_idx < plen && pat[p_idx] == ']') {
    p_idx++;
  }
  return negate ? !matched : matched;
}

static bool
glob_match_rec (const char* pat, size_t plen, size_t p0, const char* str, size_t slen, size_t s0) {
  if (p0 == plen) {
    return s0 == slen;
  }
  if (pat[p0] == '*') {
    for (size_t n= 0; s0 + n <= slen; ++n) {
      if (glob_match_rec (pat, plen, p0 + 1, str, slen, s0 + n)) {
        return true;
      }
    }
    return false;
  }
  if (s0 == slen) {
    return false;
  }
  if (pat[p0] == '?') {
    return glob_match_rec (pat, plen, p0 + 1, str, slen, s0 + 1);
  }
  if (pat[p0] == '[') {
    size_t next_p= p0;
    if (charclass_match (pat, plen, next_p, str[s0])) {
      return glob_match_rec (pat, plen, next_p, str, slen, s0 + 1);
    }
    return false;
  }
  if (fold_case (pat[p0]) == fold_case (str[s0])) {
    return glob_match_rec (pat, plen, p0 + 1, str, slen, s0 + 1);
  }
  return false;
}

static std::vector<std::string>
split_segments (const std::string& s) {
  std::vector<std::string> segs;
  size_t                   i= 0;
  size_t                   n= s.size ();
  while (i < n) {
    while (i < n && (s[i] == '/' || s[i] == '\\')) {
      i++;
    }
    if (i >= n) {
      break;
    }
    size_t j= i;
    while (j < n && s[j] != '/' && s[j] != '\\') {
      j++;
    }
    std::string seg= s.substr (i, j - i);
    if (seg != ".") {
      segs.push_back (seg);
    }
    i= j;
  }
  return segs;
}

static bool
path_match_segments (const std::vector<std::string>& pat_segs, const std::vector<std::string>& path_segs,
                     bool is_absolute_pattern) {
  if (pat_segs.empty ()) {
    return path_segs.empty ();
  }
  if (path_segs.size () < pat_segs.size ()) {
    return false;
  }
  if (is_absolute_pattern && path_segs.size () != pat_segs.size ()) {
    return false;
  }
  int pi= (int) pat_segs.size () - 1;
  int si= (int) path_segs.size () - 1;
  while (pi >= 0) {
    const std::string& pat= pat_segs[pi];
    const std::string& seg= path_segs[si];
    if (!glob_match_rec (pat.c_str (), pat.size (), 0, seg.c_str (), seg.size (), 0)) {
      return false;
    }
    pi--;
    si--;
  }
  return true;
}

struct path_glob_context {
  std::string              orig_dir;
  std::vector<std::string> pat_segs;
  bool                     is_absolute_pattern;
  std::vector<std::string> results;
};

static tb_long_t
path_glob_walk_cb (tb_char_t const* item_path, tb_file_info_t const* info, tb_cpointer_t priv) {
  tb_assert_and_check_return_val (item_path && priv, TB_DIRECTORY_WALK_CODE_END);

  path_glob_context* ctx= (path_glob_context*) priv;

  // tb_directory_walk 保证 item_path 以起始目录为前缀; 等于前缀的是起始目录自身
  std::string item_s (item_path);
  if (item_s.size () <= ctx->orig_dir.size ()) {
    return TB_DIRECTORY_WALK_CODE_CONTINUE;
  }
  std::string rel_path= item_s.substr (ctx->orig_dir.size ());
  while (!rel_path.empty () && (rel_path.front () == '/' || rel_path.front () == '\\')) {
    rel_path.erase (0, 1);
  }
  if (rel_path.empty ()) {
    return TB_DIRECTORY_WALK_CODE_CONTINUE;
  }

  size_t seg_N= 1;
  for (char c : rel_path) {
    if (c == '/' || c == '\\') {
      seg_N++;
    }
  }
  if (seg_N < ctx->pat_segs.size ()) {
    return TB_DIRECTORY_WALK_CODE_CONTINUE;
  }

  std::vector<std::string> path_segs= split_segments (rel_path);
  if (path_match_segments (ctx->pat_segs, path_segs, ctx->is_absolute_pattern)) {
    ctx->results.push_back (rel_path);
  }

  return TB_DIRECTORY_WALK_CODE_CONTINUE;
}

static s7_pointer
f_path_glob (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "path-glob: path must be a string", path_arg);
  }
  s7_pointer pat_arg= s7_cadr (args);
  if (!s7_is_string (pat_arg)) {
    return string_type_error (sc, "path-glob: pattern must be a string", pat_arg);
  }

  s7_int recursion= 0;
  if (s7_is_pair (s7_cddr (args))) {
    recursion= (s7_caddr (args) == s7_t (sc)) ? -1 : 0;
  }

  const char* path_c= s7_string (path_arg);
  const char* pat_c = s7_string (pat_arg);

  // 起始目录存在性与类型校验（单次 stat）
  tb_file_info_t base_info;
  if (!tb_file_info (path_c, &base_info)) {
    return s7_error (
        sc, s7_make_symbol (sc, "file-not-found-error"),
        s7_list (sc, 1, s7_make_string (sc, (string ("No such file or directory: '") + path_c + "'").c_str ())));
  }
  if (base_info.type != TB_FILE_TYPE_DIRECTORY && base_info.type != TB_FILE_TYPE_DOT &&
      base_info.type != TB_FILE_TYPE_DOT2) {
    return s7_error (sc, s7_make_symbol (sc, "not-a-directory-error"),
                     s7_list (sc, 1, s7_make_string (sc, (string ("Not a directory: '") + path_c + "'").c_str ())));
  }

  path_glob_context ctx;
  ctx.orig_dir= std::string (path_c);
  while (ctx.orig_dir.size () > 1 && (ctx.orig_dir.back () == '/' || ctx.orig_dir.back () == '\\')) {
    ctx.orig_dir.pop_back ();
  }

  std::string pat_s (pat_c);
  ctx.is_absolute_pattern= (!pat_s.empty () && (pat_s[0] == '/' || pat_s[0] == '\\'));
  ctx.pat_segs           = split_segments (pat_s);

  tb_directory_walk (path_c, (tb_long_t) recursion, tb_true, path_glob_walk_cb, &ctx);

  std::sort (ctx.results.begin (), ctx.results.end ());

  s7_pointer ret= s7_nil (sc);
  for (auto it= ctx.results.rbegin (); it != ctx.results.rend (); ++it) {
    ret= s7_cons (sc, s7_make_string_with_length (sc, it->data (), it->size ()), ret);
  }
  return ret;
}

static void
glue_path_glob (s7_scheme* sc) {
  const char* name= "g_path-glob";
  const char* desc= "(g_path-glob path pattern [recursion]) => list of relative path strings";
  glue_define (sc, name, desc, f_path_glob, 2, 1);
}

// ; realpath 语义对齐 os.path.realpath / pathlib.Path.resolve：
// ; 解析符号链接并折叠 . / .. 段，返回物理绝对路径。
// ; strict=true 时路径不存在抛 file-not-found-error；
// ; strict=false 时尽可能解析已存在的前缀，剩余段词法折叠后附加。
// ; 算法对齐 CPython _joinrealpath：逐段拼接，遇 .. 时对已解析部分
// ; 回溯（遇到符号链接先解析再回溯）。
// ; 依赖 POSIX realpath(3)/lstat(2)，Windows 下不参与编译
// ; （f_realpath 在 Windows 走 _fullpath 词法折叠分支）。
#if !defined(TB_CONFIG_OS_WINDOWS)
static std::string
path_realpath_join (const std::string& base, const std::string& rest) {
  std::vector<std::string> parts;
  // base 已是物理路径，拆段压栈
  {
    size_t i= 0;
    while (i < base.size ()) {
      while (i < base.size () && base[i] == '/')
        i++;
      size_t j= i;
      while (j < base.size () && base[j] != '/')
        j++;
      if (j > i) parts.push_back (base.substr (i, j - i));
      i= j;
    }
  }

  char        resolved[PATH_MAX];
  std::string new_base= "/";
  bool        has_base= false;
  // ; 先解析 base 本身(可能含符号链接)：base 为空字符串时从 cwd 开始
  if (!base.empty ()) {
    if (realpath (base.c_str (), resolved) != NULL) {
      new_base= resolved;
      has_base= true;
    }
    else {
      return ""; // base 不存在，无法解析
    }
  }
  else {
    if (getcwd (resolved, sizeof (resolved)) == NULL) return "";
    new_base= resolved;
    has_base= true;
  }

  // ; 对 new_base 重新拆段
  parts.clear ();
  {
    size_t i= 0;
    while (i < new_base.size ()) {
      while (i < new_base.size () && new_base[i] == '/')
        i++;
      size_t j= i;
      while (j < new_base.size () && new_base[j] != '/')
        j++;
      if (j > i) parts.push_back (new_base.substr (i, j - i));
      i= j;
    }
  }
  (void) has_base;

  // ; 逐段拼接 rest
  {
    size_t i= 0;
    while (i < rest.size ()) {
      while (i < rest.size () && rest[i] == '/')
        i++;
      size_t j= i;
      while (j < rest.size () && rest[j] != '/')
        j++;
      if (j > i) {
        std::string seg= rest.substr (i, j - i);
        if (seg == ".") {
          // 跳过
        }
        else if (seg == "..") {
          if (!parts.empty ()) parts.pop_back ();
        }
        else {
          // ; 拼接后检查是否符号链接，是则解析并重新拆段
          std::string joined= "/";
          for (auto& p : parts)
            joined+= p + "/";
          joined+= seg;
          struct stat st;
          if (lstat (joined.c_str (), &st) == 0 && S_ISLNK (st.st_mode)) {
            if (realpath (joined.c_str (), resolved) != NULL) {
              parts.clear ();
              std::string nb (resolved);
              size_t      k= 0;
              while (k < nb.size ()) {
                while (k < nb.size () && nb[k] == '/')
                  k++;
                size_t m= k;
                while (m < nb.size () && nb[m] != '/')
                  m++;
                if (m > k) parts.push_back (nb.substr (k, m - k));
                k= m;
              }
              i= j; // ; 已处理完本段，跳过尾部 i=j
              continue;
            }
          }
          parts.push_back (seg);
        }
      }
      i= j;
    }
  }

  std::string result= "/";
  for (size_t k= 0; k < parts.size (); k++) {
    result+= parts[k];
    if (k + 1 < parts.size ()) result+= "/";
  }
  return result;
}
#endif // !TB_CONFIG_OS_WINDOWS

static s7_pointer
f_realpath (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "realpath: path must be a string", path_arg);
  }
  s7_pointer strict_arg= s7_car (s7_cdr (args));
  bool       strict    = (strict_arg == s7_t (sc));

  const char* path_c= s7_string (path_arg);

#ifdef TB_CONFIG_OS_WINDOWS
  // Windows 无 realpath(3)：以 _fullpath 折叠 . / .. 段，
  // 不解析符号链接与盘符映射（与 Python 在 Windows 的行为一致）
  char resolved[TB_PATH_MAXN];
  if (_fullpath (resolved, path_c, sizeof (resolved)) == NULL) {
    if (strict)
      return s7_error (
          sc, s7_make_symbol (sc, "file-not-found-error"),
          s7_list (sc, 1, s7_make_string (sc, (string ("No such file or directory: '") + path_c + "'").c_str ())));
    return s7_make_string (sc, path_c);
  }
  return s7_make_string (sc, resolved);
#else
  // ; strict 模式：要求路径本身存在（对齐 pathlib.resolve(strict=True)）
  if (strict) {
    tb_file_info_t info;
    if (!tb_file_info (path_c, &info)) {
      return s7_error (
          sc, s7_make_symbol (sc, "file-not-found-error"),
          s7_list (sc, 1, s7_make_string (sc, (string ("No such file or directory: '") + path_c + "'").c_str ())));
    }
  }

  // ; 相对路径以 cwd 为种子，对齐 pathlib.Path.resolve
  std::string path_s (path_c);
  if (!path_s.empty () && path_s[0] != '/') {
    char cwd[PATH_MAX];
    if (getcwd (cwd, sizeof (cwd)) == NULL) {
      if (strict)
        return s7_error (
            sc, s7_make_symbol (sc, "file-not-found-error"),
            s7_list (sc, 1, s7_make_string (sc, (string ("No such file or directory: '") + path_c + "'").c_str ())));
      return s7_make_string (sc, path_c);
    }
    path_s= std::string (cwd) + "/" + path_s;
  }

  // ; 拆出最深的已存在前缀 base 与剩余部分 rest，
  // ; 对 base+rest 整体做逐段解析（对齐 CPython _joinrealpath）
  std::string base= path_s;
  std::string rest;
  {
    std::string    cur= path_s;
    tb_file_info_t info;
    while (!cur.empty () && !tb_file_info (cur.c_str (), &info)) {
      size_t pos= cur.find_last_of ('/');
      if (pos == std::string::npos) {
        base= "";
        rest= path_s;
        break;
      }
      rest= cur.substr (pos + 1) + (rest.empty () ? "" : "/" + rest);
      cur = cur.substr (0, pos);
      if (cur.empty ()) cur= "/";
      base= cur;
    }
    if (cur.empty ()) base= "";
  }

  std::string result= path_realpath_join (base, rest);
  if (result.empty ()) {
    if (strict)
      return s7_error (
          sc, s7_make_symbol (sc, "file-not-found-error"),
          s7_list (sc, 1, s7_make_string (sc, (string ("No such file or directory: '") + path_c + "'").c_str ())));
    // ; base 不存在且无法回溯到根：直接词法归一
    result= path_realpath_join ("/", path_s);
    if (result.empty ()) return s7_make_string (sc, path_c);
  }
  return s7_make_string (sc, result.c_str ());
#endif
}

static void
glue_realpath (s7_scheme* sc) {
  const char* name= "g_realpath";
  const char* desc= "(g_realpath path [strict]) => string, resolve symlinks and collapse . / .. segments";
  glue_define (sc, name, desc, f_realpath, 1, 1);
}

void
glue_liii_path (s7_scheme* sc) {
  glue_isfile (sc);
  glue_isdir (sc);
  glue_path_getsize (sc);
  glue_path_read_text (sc);
  glue_path_read_lines (sc);
  glue_path_read_bytes (sc);
  glue_path_write_text (sc);
  glue_path_write_bytes (sc);
  glue_path_append_text (sc);
  glue_path_touch (sc);
  glue_path_copy (sc);
  glue_path_glob (sc);
  glue_realpath (sc);
}

} // namespace goldfish
