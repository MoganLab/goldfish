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
#include <cstdlib>
#include <filesystem>
#include <string>
#include <vector>

#include <tbox/tbox.h>

namespace goldfish {

namespace fs= std::filesystem;

using std::string;
using std::vector;

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

// ---- which ----

static bool
shutil_which_access_check (const char* path) {
#ifdef TB_CONFIG_OS_WINDOWS
  tb_file_info_t info;
  return tb_file_info (path, &info) && info.type == TB_FILE_TYPE_FILE;
#else
  return tb_file_access (path, TB_FILE_MODE_EXEC);
#endif
}

static s7_pointer
f_which (s7_scheme* sc, s7_pointer args) {
  s7_pointer cmd_arg= s7_car (args);
  if (!s7_is_string (cmd_arg)) {
    return string_type_error (sc, "which: cmd must be a string", cmd_arg);
  }
  const char* cmd_c        = s7_string (cmd_arg);
  s7_pointer  path_arg     = s7_cdr (args);
  const char* path_override= nullptr;

  if (s7_is_pair (path_arg)) {
    s7_pointer path= s7_car (path_arg);
    if (!s7_is_string (path)) {
      return string_type_error (sc, "which: path must be a string", path);
    }
    path_override= s7_string (path);
  }

  string         cmd_str (cmd_c);
  vector<string> search_dirs;
  string         cmd_name;

  bool has_dir_sep= (cmd_str.find ('/') != string::npos) || (cmd_str.find ('\\') != string::npos);

  if (has_dir_sep) {
    size_t last_sep= cmd_str.find_last_of ("/\\");
    string dir     = cmd_str.substr (0, last_sep);
    cmd_name       = cmd_str.substr (last_sep + 1);
    if (dir.empty ()) {
      dir= ".";
    }
    search_dirs.push_back (dir);
  }
  else {
    cmd_name= cmd_str;

    string path_env;
    if (path_override != nullptr) {
      path_env= path_override;
    }
    else {
      const char* env= getenv ("PATH");
      if (env != nullptr) {
        path_env= env;
      }
    }

    if (path_env.empty ()) {
      return s7_make_boolean (sc, false);
    }

    char path_sep= ':';
#ifdef TB_CONFIG_OS_WINDOWS
    path_sep= ';';
#endif

    size_t start= 0;
    size_t end  = path_env.find (path_sep);
    while (end != string::npos) {
      string dir= path_env.substr (start, end - start);
      if (!dir.empty ()) {
        search_dirs.push_back (dir);
      }
      start= end + 1;
      end  = path_env.find (path_sep, start);
    }
    string last_dir= path_env.substr (start);
    if (!last_dir.empty ()) {
      search_dirs.push_back (last_dir);
    }
  }

  vector<string> files_to_check;

#ifdef TB_CONFIG_OS_WINDOWS
  vector<string> exts;
  const char*    pathext= getenv ("PATHEXT");
  if (pathext != nullptr) {
    string ext_str (pathext);
    size_t start= 0;
    size_t end  = ext_str.find (';');
    while (end != string::npos) {
      string ext= ext_str.substr (start, end - start);
      if (!ext.empty ()) {
        if (ext[0] == '.') ext= ext.substr (1);
        exts.push_back (ext);
      }
      start= end + 1;
      end  = ext_str.find (';', start);
    }
    string last_ext= ext_str.substr (start);
    if (!last_ext.empty ()) {
      if (last_ext[0] == '.') last_ext= last_ext.substr (1);
      exts.push_back (last_ext);
    }
  }

  files_to_check.push_back (cmd_name);
  for (const string& ext : exts) {
    files_to_check.push_back (cmd_name + "." + ext);
  }
#else
  files_to_check.push_back (cmd_name);
#endif

  for (const string& dir : search_dirs) {
    for (const string& file : files_to_check) {
#ifdef TB_CONFIG_OS_WINDOWS
      string full_path= dir + "\\" + file;
#else
      string full_path= dir + "/" + file;
#endif
      if (shutil_which_access_check (full_path.c_str ())) {
        return s7_make_string (sc, full_path.c_str ());
      }
    }
  }

  return s7_make_boolean (sc, false);
}

// ---- rmtree ----

// 递归删除目录树。默认在路径不存在/删除失败时抛出 (liii error)
// 约定的错误；ignore-errors 为 #t 时吞掉错误并返回 #f。
static s7_pointer
f_rmtree (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "rmtree: path must be a string", path_arg);
  }
  bool       ignore_errors= false;
  s7_pointer opt          = s7_cdr (args);
  if (s7_is_pair (opt)) {
    ignore_errors= s7_boolean (sc, s7_car (opt));
  }
  const char* path_c= s7_string (path_arg);

  std::error_code ec;
  const fs::path  p= fs::u8path (path_c);

  if (!fs::exists (p, ec)) {
    if (ignore_errors) return s7_make_boolean (sc, false);
    return s7_error (sc, s7_make_symbol (sc, "file-not-found-error"),
                     s7_list (sc, 2,
                              s7_make_string (sc, (string ("No such file or directory: '") + path_c + "'").c_str ()),
                              s7_make_string (sc, path_c)));
  }

  fs::remove_all (p, ec);
  if (ec) {
    if (ignore_errors) return s7_make_boolean (sc, false);
    return s7_error (sc, s7_make_symbol (sc, "os-error"),
                     s7_list (sc, 2, s7_make_string (sc, ec.message ().c_str ()), s7_make_string (sc, path_c)));
  }

  return s7_make_boolean (sc, true);
}

void
glue_liii_shutil (s7_scheme* sc) {
  glue_define (sc, "g_which", "(g_which cmd [path]) => string or #f, locate a command in PATH or given search path",
               f_which, 1, 1);
  glue_define (sc, "g_rmtree", "(g_rmtree path [ignore-errors]) => boolean, recursively delete a directory tree",
               f_rmtree, 1, 1);
}

} // namespace goldfish
