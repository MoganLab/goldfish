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

#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE
#endif

#include "s7.h"
#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <random>
#include <string>
#include <vector>

#include <tbox/tbox.h>

#if !defined(TB_CONFIG_OS_WINDOWS)
#include <sys/stat.h>
#include <unistd.h>
#else
#include <windows.h>
#endif

namespace goldfish {

namespace fs= std::filesystem;

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

static std::string
generate_random_alphanumeric (std::size_t len) {
  static const char                  charset[]= "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  std::random_device                 rd;
  std::mt19937                       gen (rd ());
  std::uniform_int_distribution<int> dis (0, sizeof (charset) - 2);

  std::string s;
  s.reserve (len);
  for (std::size_t i= 0; i < len; ++i) {
    s+= charset[dis (gen)];
  }
  return s;
}

static s7_pointer
f_mkstemp (s7_scheme* sc, s7_pointer args) {
  s7_pointer prefix_arg= s7_car (args);
  s7_pointer suffix_arg= s7_cadr (args);
  s7_pointer dir_arg   = s7_caddr (args);

  if (!s7_is_string (prefix_arg)) {
    return string_type_error (sc, "mkstemp: prefix must be a string", prefix_arg);
  }
  if (!s7_is_string (suffix_arg)) {
    return string_type_error (sc, "mkstemp: suffix must be a string", suffix_arg);
  }
  if (!s7_is_string (dir_arg)) {
    return string_type_error (sc, "mkstemp: dir must be a string", dir_arg);
  }

  const char* prefix_c= s7_string (prefix_arg);
  const char* suffix_c= s7_string (suffix_arg);
  const char* dir_c   = s7_string (dir_arg);

  std::string base= dir_c;
  if (!base.empty () && base.back () != '/' && base.back () != '\\') {
#if defined(TB_CONFIG_OS_WINDOWS)
    base+= '\\';
#else
    base+= '/';
#endif
  }

#if !defined(TB_CONFIG_OS_WINDOWS)
  std::string tpl       = base + prefix_c + "XXXXXX" + suffix_c;
  int         suffix_len= static_cast<int> (std::strlen (suffix_c));
  int         fd        = mkstemps (&tpl[0], suffix_len);
  if (fd < 0) {
    return s7_error (sc, s7_make_symbol (sc, "os-error"),
                     s7_list (sc, 2, s7_make_string (sc, strerror (errno)), s7_make_string (sc, tpl.c_str ())));
  }
  close (fd);
  s7_pointer port= s7_open_output_file (sc, tpl.c_str (), "w");
  return s7_values (sc, s7_list (sc, 2, port, s7_make_string (sc, tpl.c_str ())));
#else
  std::string chosen_path;
  bool        created= false;
  for (int i= 0; i < 100; ++i) {
    std::string cand = base + prefix_c + generate_random_alphanumeric (6) + suffix_c;
    HANDLE      hFile= CreateFileA (cand.c_str (), GENERIC_READ | GENERIC_WRITE,
                                    FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL, CREATE_NEW,
                                    FILE_ATTRIBUTE_NORMAL, NULL);
    if (hFile != INVALID_HANDLE_VALUE) {
      CloseHandle (hFile);
      chosen_path= cand;
      created    = true;
      break;
    }
    DWORD err= GetLastError ();
    if (err != ERROR_FILE_EXISTS && err != ERROR_ALREADY_EXISTS) {
      return s7_error (sc, s7_make_symbol (sc, "os-error"),
                       s7_list (sc, 2, s7_make_string (sc, "CreateFileA failed"), s7_make_string (sc, cand.c_str ())));
    }
  }
  if (!created) {
    return s7_error (
        sc, s7_make_symbol (sc, "os-error"),
        s7_list (sc, 1, s7_make_string (sc, "mkstemp: failed to create unique temporary file after 100 attempts")));
  }
  s7_pointer port= s7_open_output_file (sc, chosen_path.c_str (), "w");
  return s7_values (sc, s7_list (sc, 2, port, s7_make_string (sc, chosen_path.c_str ())));
#endif
}

static s7_pointer
f_mkdtemp (s7_scheme* sc, s7_pointer args) {
  s7_pointer prefix_arg= s7_car (args);
  s7_pointer suffix_arg= s7_cadr (args);
  s7_pointer dir_arg   = s7_caddr (args);

  if (!s7_is_string (prefix_arg)) {
    return string_type_error (sc, "mkdtemp: prefix must be a string", prefix_arg);
  }
  if (!s7_is_string (suffix_arg)) {
    return string_type_error (sc, "mkdtemp: suffix must be a string", suffix_arg);
  }
  if (!s7_is_string (dir_arg)) {
    return string_type_error (sc, "mkdtemp: dir must be a string", dir_arg);
  }

  const char* prefix_c= s7_string (prefix_arg);
  const char* suffix_c= s7_string (suffix_arg);
  const char* dir_c   = s7_string (dir_arg);

  std::string base= dir_c;
  if (!base.empty () && base.back () != '/' && base.back () != '\\') {
#if defined(TB_CONFIG_OS_WINDOWS)
    base+= '\\';
#else
    base+= '/';
#endif
  }

#if !defined(TB_CONFIG_OS_WINDOWS)
  std::string chosen_path;
  if (std::strlen (suffix_c) == 0) {
    std::string tpl= base + prefix_c + "XXXXXX";
    char*       res= mkdtemp (&tpl[0]);
    if (!res) {
      return s7_error (sc, s7_make_symbol (sc, "os-error"),
                       s7_list (sc, 2, s7_make_string (sc, strerror (errno)), s7_make_string (sc, tpl.c_str ())));
    }
    chosen_path= tpl;
  }
  else {
    bool created= false;
    for (int i= 0; i < 100; ++i) {
      std::string cand= base + prefix_c + generate_random_alphanumeric (6) + suffix_c;
      if (mkdir (cand.c_str (), 0700) == 0) {
        chosen_path= cand;
        created    = true;
        break;
      }
      if (errno != EEXIST) {
        return s7_error (sc, s7_make_symbol (sc, "os-error"),
                         s7_list (sc, 2, s7_make_string (sc, strerror (errno)), s7_make_string (sc, cand.c_str ())));
      }
    }
    if (!created) {
      return s7_error (
          sc, s7_make_symbol (sc, "os-error"),
          s7_list (sc, 1,
                   s7_make_string (sc, "mkdtemp: failed to create unique temporary directory after 100 attempts")));
    }
  }
  return s7_make_string (sc, chosen_path.c_str ());
#else
  bool        created= false;
  std::string chosen_path;
  for (int i= 0; i < 100; ++i) {
    std::string cand= base + prefix_c + generate_random_alphanumeric (6) + suffix_c;
    if (CreateDirectoryA (cand.c_str (), NULL)) {
      chosen_path= cand;
      created    = true;
      break;
    }
    DWORD err= GetLastError ();
    if (err != ERROR_ALREADY_EXISTS) {
      return s7_error (
          sc, s7_make_symbol (sc, "os-error"),
          s7_list (sc, 2, s7_make_string (sc, "CreateDirectoryA failed"), s7_make_string (sc, cand.c_str ())));
    }
  }
  if (!created) {
    return s7_error (
        sc, s7_make_symbol (sc, "os-error"),
        s7_list (sc, 1,
                 s7_make_string (sc, "mkdtemp: failed to create unique temporary directory after 100 attempts")));
  }
  return s7_make_string (sc, chosen_path.c_str ());
#endif
}

static s7_pointer
f_rmtree (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "rmtree: path must be a string", path_arg);
  }
  const char*     path_c= s7_string (path_arg);
  std::error_code ec;
  fs::remove_all (fs::u8path (path_c), ec);
  return s7_make_boolean (sc, !ec);
}

static s7_pointer
f_file_mode (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "file-mode: path must be a string", path_arg);
  }
  const char* path_c= s7_string (path_arg);
#if !defined(TB_CONFIG_OS_WINDOWS)
  struct stat st;
  if (stat (path_c, &st) != 0) {
    return s7_make_integer (sc, -1);
  }
  return s7_make_integer (sc, st.st_mode & 0777);
#else
  return s7_make_integer (sc, 0600);
#endif
}

static s7_pointer
f_chmod (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "chmod: path must be a string", path_arg);
  }
  s7_pointer mode_arg= s7_cadr (args);
  if (!s7_is_integer (mode_arg)) {
    return string_type_error (sc, "chmod: mode must be an integer", mode_arg);
  }
  const char* path_c= s7_string (path_arg);
  int         mode  = s7_integer (mode_arg);
#if !defined(TB_CONFIG_OS_WINDOWS)
  return s7_make_boolean (sc, chmod (path_c, static_cast<mode_t> (mode)) == 0);
#else
  return s7_make_boolean (sc, true);
#endif
}

void
glue_liii_tempfile (s7_scheme* sc) {
  glue_define (sc, "g_mkstemp", "(g_mkstemp prefix suffix dir) => (values port path)", f_mkstemp, 3, 0);
  glue_define (sc, "g_mkdtemp", "(g_mkdtemp prefix suffix dir) => string", f_mkdtemp, 3, 0);
  glue_define (sc, "g_rmtree", "(g_rmtree path) => boolean", f_rmtree, 1, 0);
  glue_define (sc, "g_file-mode", "(g_file-mode path) => integer", f_file_mode, 1, 0);
  glue_define (sc, "g_chmod", "(g_chmod path mode) => boolean", f_chmod, 2, 0);
}

} // namespace goldfish
