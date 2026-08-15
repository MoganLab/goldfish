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
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations
// under the License.
//

#include "s7.h"
#include "gf_glue.hpp"

#include <cstdlib>
#include <filesystem>
#include <string>
#include <vector>

#include <tbox/platform/file.h>
#include <tbox/platform/path.h>
#include <tbox/tbox.h>

#ifdef TB_CONFIG_OS_WINDOWS
#include <windows.h>
#elif TB_CONFIG_OS_MACOSX
#include <limits.h>
#elif defined(__EMSCRIPTEN__)
#include <limits.h>
#else
#include <linux/limits.h>
#endif

#if !defined(TB_CONFIG_OS_WINDOWS)
#include <errno.h>
#include <pwd.h>
#include <unistd.h>
#if !defined(__EMSCRIPTEN__)
#include <wordexp.h>
#endif
#endif

#define GOLDFISH_PATH_MAXN TB_PATH_MAXN

namespace goldfish {

using std::string;
using std::vector;

namespace fs= std::filesystem;

// ---------------------------------------------------------------------------
// Plain C++ implementations (no s7 dependency).
// ---------------------------------------------------------------------------

string
os_arch () {
  return TB_ARCH_STRING;
}

string
os_type () {
#ifdef TB_CONFIG_OS_LINUX
  return "Linux";
#elif defined(TB_CONFIG_OS_MACOSX)
  return "Darwin";
#elif defined(TB_CONFIG_OS_WINDOWS)
  return "Windows";
#else
  return "";
#endif
}

int
os_call (const string& cmd) {
  tb_process_attr_t attr= {tb_null};
  attr.flags            = TB_PROCESS_FLAG_NO_WINDOW;
  int ret;

#if (defined(_MSC_VER) || defined(__MINGW32__))
  ret= (int) std::system (cmd.c_str ());
#elif defined(__EMSCRIPTEN__)
  tb_char_t* argv[]= {(tb_char_t*) cmd.c_str (), tb_null};
  ret              = (int) tb_process_run (argv[0], (tb_char_t const**) argv, &attr);
#else
  wordexp_t p;
  ret= wordexp (cmd.c_str (), &p, 0);
  if (ret != 0) {
    // failed after calling wordexp
  }
  else if (p.we_wordc == 0) {
    wordfree (&p);
    ret= EINVAL;
  }
  else {
    ret= (int) tb_process_run (p.we_wordv[0], (tb_char_t const**) p.we_wordv, &attr);
    wordfree (&p);
  }
#endif
  return ret;
}

int
system_call (const string& cmd) {
  return (int) std::system (cmd.c_str ());
}

bool
access_check (const string& path, int mode) {
  bool ret= false;
  if (mode == 0) {
    tb_file_info_t info;
    ret= tb_file_info (path.c_str (), &info);
  }
  else {
    ret= tb_file_access (path.c_str (), mode);
  }
  return ret;
}

bool
set_environment_variable (const string& key, const string& value) {
  return tb_environment_set (key.c_str (), value.c_str ());
}

bool
unset_environment_variable (const string& env_name) {
  return tb_environment_remove (env_name.c_str ());
}

string
os_temp_dir () {
  tb_char_t path[GOLDFISH_PATH_MAXN];
  tb_directory_temporary (path, GOLDFISH_PATH_MAXN);
  return string (path);
}

bool
mkdir (const string& dir) {
  return tb_directory_create (dir.c_str ());
}

bool
rmdir (const string& dir) {
  return tb_directory_remove (dir.c_str ());
}

bool
remove_file (const string& path) {
  return tb_file_remove (path.c_str ());
}

bool
rename_file (const string& src, const string& dst) {
  try {
    fs::rename (src, dst);
    return true;
  } catch (const fs::filesystem_error& e) {
    return false;
  }
}

bool
chdir (const string& dir) {
  return tb_directory_current_set (dir.c_str ());
}

static tb_long_t
tb_directory_walk_func (tb_char_t const* path, tb_file_info_t const* info, tb_cpointer_t priv) {
  tb_assert_and_check_return_val (path && info, TB_DIRECTORY_WALK_CODE_END);

  vector<string>* p_v_result= (vector<string>*) priv;
  p_v_result->push_back (string (path));
  return TB_DIRECTORY_WALK_CODE_CONTINUE;
}

vector<string>
listdir (const string& path) {
  vector<string> entries;
  tb_directory_walk (path.c_str (), 0, tb_false, tb_directory_walk_func, &entries);

  int    entries_N   = (int) entries.size ();
  string path_s      = string (path);
  int    path_N      = (int) path_s.size ();
  int    path_slash_N= path_N;
  char   last_ch     = path_s[path_N - 1];
#if defined(TB_CONFIG_OS_WINDOWS)
  if (last_ch != '/' && last_ch != '\\') {
    path_slash_N= path_slash_N + 1;
  }
#else
  if (last_ch != '/') {
    path_slash_N= path_slash_N + 1;
  }
#endif
  for (int i= 0; i < entries_N; i++) {
    entries[i]= entries[i].substr (path_slash_N);
  }
  return entries;
}

string
getcwd () {
  tb_char_t path[GOLDFISH_PATH_MAXN];
  tb_directory_current (path, GOLDFISH_PATH_MAXN);
  return string (path);
}

string
getlogin () {
#ifdef TB_CONFIG_OS_WINDOWS
  return "";
#else
  uid_t          uid= getuid ();
  struct passwd* pwd= getpwuid (uid);
  return string (pwd->pw_name);
#endif
}

int
getpid () {
#ifdef TB_CONFIG_OS_WINDOWS
  return (int) GetCurrentProcessId ();
#else
  return (int) ::getpid ();
#endif
}

// ---------------------------------------------------------------------------
// Declarative glue: each GF_GLUE generates the s7_function wrapper and the
// registration, deriving the arity and types from the plain C++ signature.
// ---------------------------------------------------------------------------

GF_GLUE ("g_os-arch", "(g_os-arch) => string",              os_arch);
GF_GLUE ("g_os-type", "(g_os-type) => string",              os_type);
GF_GLUE ("g_os-call", "(g_os-call string) => int",          os_call);
GF_GLUE ("g_system",  "(g_system string) => int",           system_call);
GF_GLUE ("g_access",  "(g_access string integer) => boolean", access_check);
GF_GLUE ("g_setenv",  "(g_setenv key value) => boolean",    set_environment_variable);
GF_GLUE ("g_unsetenv", "(g_unsetenv string) => boolean",    unset_environment_variable);
GF_GLUE ("g_os-temp-dir", "(g_os-temp-dir) => string",      os_temp_dir);
GF_GLUE ("g_mkdir",   "(g_mkdir string) => boolean",        mkdir);
GF_GLUE ("g_rmdir",   "(g_rmdir string) => boolean",        rmdir);
GF_GLUE ("g_remove-file", "(g_remove-file path) => boolean", remove_file);
GF_GLUE ("g_rename",  "(g_rename src dst) => boolean",      rename_file);
GF_GLUE ("g_chdir",   "(g_chdir string) => boolean",        chdir);
GF_GLUE ("g_listdir", "(g_listdir string) => vector",       listdir);
GF_GLUE ("g_getcwd",  "(g_getcwd) => string",               getcwd);
GF_GLUE ("g_getlogin", "(g_getlogin) => string",            getlogin);
GF_GLUE ("g_getpid",  "(g_getpid) => integer",              getpid);

void
glue_liii_os (s7_scheme* sc) {
  glue_os_arch (sc);
  glue_os_type (sc);
  glue_os_call (sc);
  glue_system_call (sc);
  glue_access_check (sc);
  glue_set_environment_variable (sc);
  glue_unset_environment_variable (sc);
  glue_getcwd (sc);
  glue_os_temp_dir (sc);
  glue_mkdir (sc);
  glue_rmdir (sc);
  glue_remove_file (sc);
  glue_rename_file (sc);
  glue_chdir (sc);
  glue_listdir (sc);
  glue_getlogin (sc);
  glue_getpid (sc);
}

} // namespace goldfish
