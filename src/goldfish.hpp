//
// Copyright (C) 2024 The Goldfish Scheme Authors
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

#include "gf.h"
#include <algorithm>
#include <argh.h>
#include <cctype>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <limits>
#include <memory>
#include <mutex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <thread>
#include <unordered_map>
#include <vector>

#include <tbox/platform/file.h>
#include <tbox/platform/path.h>
#include <tbox/tbox.h>

#ifdef TB_CONFIG_OS_WINDOWS
#include <io.h>
#include <windows.h>
#elif TB_CONFIG_OS_MACOSX
#include <limits.h>
#include <mach-o/dyld.h>
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

#ifdef GOLDFISH_WITH_REPL
#include <functional>
#include <isocline.h>
#endif

#define GOLDFISH_VERSION "18.11.20"

#define GOLDFISH_PATH_MAXN TB_PATH_MAXN

static std::vector<std::string> command_args= std::vector<std::string> ();

// Declare environ for non-Windows platforms (needed for f_getenvs)
#if !defined(TB_CONFIG_OS_WINDOWS)
extern char** environ;
#endif

namespace goldfish {
using std::cerr;
using std::cout;
using std::endl;
using std::string;
using std::vector;

namespace fs= std::filesystem;

// True once the B4 boot chain (expander artifact + lib layer) has loaded;
// gates C++->Scheme call-ins that need expander-resolved modules like
// (liii project).  Stays false under --mode s7.
static bool g_expander_online= false;

inline void glue_define (gf::scheme* sc, const char* name, const char* desc, gf::function f, gf::int_ required,
                         gf::int_ optional);

static gf::pointer f_goldfish_library (gf::scheme* sc, gf::pointer args);
static gf::pointer f_load_path (gf::scheme* sc, gf::pointer args);


static string find_goldfish_library ();


void glue_njson (gf::scheme* sc);
#ifdef GOLDFISH_ENABLE_HTTP
void glue_http (gf::scheme* sc);
void glue_http_async (gf::scheme* sc);
#endif
void glue_liii_base64 (gf::scheme* sc);
void glue_liii_reader (gf::scheme* sc);
void bootstrap_scheme_reader (gf::scheme* sc, const char* gf_lib);
void glue_scheme_base (gf::scheme* sc);
void glue_scheme_char (gf::scheme* sc);
void glue_liii_hashlib (gf::scheme* sc);
void glue_liii_os (gf::scheme* sc);
void glue_liii_path (gf::scheme* sc);
void glue_liii_string (gf::scheme* sc);
void glue_subprocess_run_values (gf::scheme* sc);
void glue_vm (gf::scheme* sc);

inline gf::pointer
string_vector_to_s7_vector (gf::scheme* sc, vector<string> v) {
  int        N  = v.size ();
  gf::pointer ret= gf::make_vector (sc, N);
  for (int i= 0; i < N; i++) {
    gf::vector_set (sc, ret, i, gf::make_string (sc, v[i].c_str ()));
  }
  return ret;
}

inline void
glue_define (gf::scheme* sc, const char* name, const char* desc, gf::function f, gf::int_ required, gf::int_ optional) {
  gf::pointer cur_env= gf::curlet (sc);
  gf::pointer func   = gf::make_typed_function (sc, name, f, required, optional, false, desc, NULL);
  gf::define (sc, cur_env, gf::make_symbol (sc, name), func);
}

static gf::pointer
f_version (gf::scheme* sc, gf::pointer args) {
  return gf::make_string (sc, GOLDFISH_VERSION);
}

static gf::pointer
f_delete_file (gf::scheme* sc, gf::pointer args) {
  const char* path_c= gf::string (gf::car (args));
  return gf::make_boolean (sc, tb_file_remove (path_c));
}

inline void
glue_goldfish (gf::scheme* sc) {
  gf::pointer cur_env= gf::curlet (sc);

  const char* s_version           = "version";
  const char* d_version           = "(version) => string";
  const char* s_delete_file       = "g_delete-file";
  const char* d_delete_file       = "(g_delete-file string) => boolean";
  const char* s_goldfish_library  = "g_goldfish-library";
  const char* d_goldfish_library  = "(g_goldfish-library) => string or #f, goldfish library root";
  const char* s_load_path         = "g_load-path";
  const char* d_load_path         = "(g_load-path) => list of strings, current load-path";

  gf::define (sc, cur_env, gf::make_symbol (sc, s_version),
             gf::make_typed_function (sc, s_version, f_version, 0, 0, false, d_version, NULL));

  gf::define (sc, cur_env, gf::make_symbol (sc, s_delete_file),
             gf::make_typed_function (sc, s_delete_file, f_delete_file, 1, 0, false, d_delete_file, NULL));

  gf::define (sc, cur_env, gf::make_symbol (sc, s_goldfish_library),
             gf::make_typed_function (sc, s_goldfish_library, f_goldfish_library, 0, 0, false, d_goldfish_library, NULL));

  gf::define (sc, cur_env, gf::make_symbol (sc, s_load_path),
             gf::make_typed_function (sc, s_load_path, f_load_path, 0, 0, false, d_load_path, NULL));
}

// old `f_current_second` TODO: use std::chrono::tai_clock::now() when using C++ 20
//                        NOTE(jinser): use a new name for tai
// `current-second` impl by g_get-time-of-day now
static gf::pointer
f_get_time_of_day (gf::scheme* sc, gf::pointer args) {
  using namespace std::chrono;
  auto now        = time_point_cast<microseconds> (system_clock::now ());
  auto since_epoch= now.time_since_epoch ();
  auto sec        = duration_cast<seconds> (since_epoch);

  gf::pointer vs=
      gf::list (sc, gf::make_integer (sc, sec.count ()), gf::make_integer (sc, (since_epoch - sec).count ()));
  return gf::values (sc, vs);
}

static gf::pointer
f_monotonic_nanosecond (gf::scheme* sc, gf::pointer args) {
  using namespace std::chrono;
  auto now     = steady_clock::now ();
  auto duration= now.time_since_epoch ();
  auto count   = duration_cast<std::chrono::nanoseconds> (duration).count ();
  return gf::make_integer (sc, count);
}

template <typename Clock>
constexpr int64_t
clock_resolution_ns () {
  typedef std::chrono::duration<double, std::nano> NS;
  NS                                               ns= typename Clock::duration (1);
  return ns.count ();
}

inline void
glue_scheme_time (gf::scheme* sc) {
  gf::pointer cur_env= gf::curlet (sc);

  const char* s_get_time_of_day= "g_get-time-of-day";
  const char* d_get_time_of_day= "(g_get-time-of-day): () => (integer, integer), return the "
                                 "current second and microsecond in integer";
  gf::define (sc, cur_env, gf::make_symbol (sc, s_get_time_of_day),
             gf::make_typed_function (sc, s_get_time_of_day, f_get_time_of_day, 0, 0, false, d_get_time_of_day, NULL));

  const char* s_monotonic_nanosecond= "g_monotonic-nanosecond";
  const char* d_monotonic_nanosecond= "(g_monotonic-nanosecond): () => integer, returns the steady clock's monotonic "
                                      "nanoseconds since an unspecified epoch";
  gf::define (sc, cur_env, gf::make_symbol (sc, s_monotonic_nanosecond),
             gf::make_typed_function (sc, s_monotonic_nanosecond, f_monotonic_nanosecond, 0, 0, false,
                                     d_monotonic_nanosecond, NULL));

  gf::define_constant_with_environment (sc, cur_env, "g_system-clock-resolution",
                                       gf::make_integer (sc, clock_resolution_ns<std::chrono::system_clock> ()));
  gf::define_constant_with_environment (sc, cur_env, "g_steady-clock-resolution",
                                       gf::make_integer (sc, clock_resolution_ns<std::chrono::steady_clock> ()));
}

static gf::pointer
f_get_environment_variable (gf::scheme* sc, gf::pointer args) {
#ifdef _MSC_VER
  std::string path_sep= ";";
#else
  std::string path_sep= ":";
#endif
  std::string          ret;
  tb_size_t            size       = 0;
  const char*          key        = gf::string (gf::car (args));
  tb_environment_ref_t environment= tb_environment_init ();
  if (environment) {
    size= tb_environment_load (environment, key);
    if (size >= 1) {
      tb_for_all_if (tb_char_t const*, value, environment, value) { ret.append (value).append (path_sep); }
    }
  }
  tb_environment_exit (environment);
  if (size == 0) { // env key not found
    return gf::make_boolean (sc, false);
  }
  else {
    return gf::make_string (sc, ret.substr (0, ret.size () - 1).c_str ());
  }
}

static gf::pointer
f_command_line (gf::scheme* sc, gf::pointer args) {
  gf::pointer ret = gf::nil (sc);
  int        size= command_args.size ();
  for (int i= size - 1; i >= 0; i--) {
    ret= gf::cons (sc, gf::make_string (sc, command_args[i].c_str ()), ret);
  }
  return ret;
}

static gf::pointer
f_getenvs (gf::scheme* sc, gf::pointer args) {
  gf::pointer p= gf::nil (sc);

#ifdef TB_CONFIG_OS_WINDOWS
  // Windows: use GetEnvironmentStrings
  LPCH env_strings= GetEnvironmentStrings ();
  if (env_strings) {
    LPCH env= env_strings;
    while (*env) {
      const char* eq= strchr (env, '=');
      if (eq && eq != env) { // skip empty variable names
        gf::pointer name = gf::make_string_with_length (sc, env, eq - env);
        gf::pointer value= gf::make_string (sc, eq + 1);
        p               = gf::cons (sc, gf::cons (sc, name, value), p);
      }
      env+= strlen (env) + 1;
    }
    FreeEnvironmentStrings (env_strings);
  }
#else
  // Unix/Linux/macOS: use environ (declared at global scope)
  for (int32_t i= 0; environ[i]; i++) {
    const char* eq= strchr (environ[i], '=');
    if (eq) {
      gf::pointer name = gf::make_string_with_length (sc, environ[i], eq - environ[i]);
      gf::pointer value= gf::make_string (sc, eq + 1);
      p               = gf::cons (sc, gf::cons (sc, name, value), p);
    }
  }
#endif

  return p;
}

inline void
glue_scheme_process_context (gf::scheme* sc) {
  gf::pointer cur_env= gf::curlet (sc);

  const char* s_get_environment_variable= "g_get-environment-variable";
  const char* d_get_environment_variable= "(g_get-environemt-variable string) => string";
  const char* s_command_line            = "g_command-line";
  const char* d_command_line            = "(g_command-line) => string";
  const char* s_getenvs                 = "g_getenvs";
  const char* d_getenvs                 = "(g_getenvs) => alist, returns all environment variables as an alist";

  gf::define (sc, cur_env, gf::make_symbol (sc, s_get_environment_variable),
             gf::make_typed_function (sc, s_get_environment_variable, f_get_environment_variable, 1, 0, false,
                                     d_get_environment_variable, NULL));
  gf::define (sc, cur_env, gf::make_symbol (sc, s_command_line),
             gf::make_typed_function (sc, s_command_line, f_command_line, 0, 0, false, d_command_line, NULL));
  gf::define (sc, cur_env, gf::make_symbol (sc, s_getenvs),
             gf::make_typed_function (sc, s_getenvs, f_getenvs, 0, 0, false, d_getenvs, NULL));
}

string
goldfish_exe () {
#ifdef TB_CONFIG_OS_WINDOWS
  char buffer[GOLDFISH_PATH_MAXN];
  GetModuleFileName (NULL, buffer, GOLDFISH_PATH_MAXN);
  return string (buffer);
#elif TB_CONFIG_OS_MACOSX
  char     buffer[PATH_MAX];
  uint32_t size= sizeof (buffer);
  if (_NSGetExecutablePath (buffer, &size) == 0) {
    char real_path[GOLDFISH_PATH_MAXN];
    if (realpath (buffer, real_path) != NULL) {
      return string (real_path);
    }
  }
#elif TB_CONFIG_OS_LINUX
  char    buffer[GOLDFISH_PATH_MAXN];
  ssize_t len= readlink ("/proc/self/exe", buffer, sizeof (buffer) - 1);
  if (len != -1) {
    buffer[len]= '\0';
    return std::string (buffer);
  }
#endif
  return "";
}

static gf::pointer
f_executable (gf::scheme* sc, gf::pointer args) {
  string exe_path= goldfish_exe ();
  return gf::make_string (sc, exe_path.c_str ());
}

static bool
which_access_check (const char* path) {
#ifdef TB_CONFIG_OS_WINDOWS
  tb_file_info_t info;
  return tb_file_info (path, &info) && info.type == TB_FILE_TYPE_FILE;
#else
  return tb_file_access (path, TB_FILE_MODE_EXEC);
#endif
}

static gf::pointer
f_which (gf::scheme* sc, gf::pointer args) {
  const char* cmd_c        = gf::string (gf::car (args));
  gf::pointer  path_arg     = gf::cdr (args);
  const char* path_override= nullptr;

  if (gf::is_pair (path_arg)) {
    path_override= gf::string (gf::car (path_arg));
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
      return gf::make_boolean (sc, false);
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
      if (which_access_check (full_path.c_str ())) {
        return gf::make_string (sc, full_path.c_str ());
      }
    }
  }

  return gf::make_boolean (sc, false);
}

inline void
glue_which (gf::scheme* sc) {
  const char* name= "g_which";
  const char* desc= "(g_which cmd [path]) => string or #f, locate a command in PATH or given search path";
  glue_define (sc, name, desc, f_which, 1, 1);
}

inline void
glue_executable (gf::scheme* sc) {
  const char* name= "g_executable";
  const char* desc= "(g_executable) => string";
  glue_define (sc, name, desc, f_executable, 0, 0);
}

inline void
glue_liii_sys (gf::scheme* sc) {
  glue_which (sc);
  glue_executable (sc);
}

static gf::pointer
f_sleep (gf::scheme* sc, gf::pointer args) {
  gf::double_ seconds= gf::real (gf::car (args));

  // 使用 tbox 的 tb_sleep 函数，参数是毫秒
  tb_msleep ((tb_long_t) (seconds * 1000));

  return gf::nil (sc);
}

inline void
glue_sleep (gf::scheme* sc) {
  const char* name= "g_sleep";
  const char* desc= "(g_sleep seconds) => nil, sleep for the specified number of seconds";
  glue_define (sc, name, desc, f_sleep, 1, 0);
}

static gf::pointer
f_uuid4 (gf::scheme* sc, gf::pointer args) {
  tb_char_t        uuid[37];
  const tb_char_t* ret= tb_uuid4_make_cstr (uuid, tb_null);
  return gf::make_string (sc, ret);
}

inline void
glue_uuid4 (gf::scheme* sc) {
  const char* name= "g_uuid4";
  const char* desc= "(g_uuid4) => string";
  glue_define (sc, name, desc, f_uuid4, 0, 0);
}

inline void
glue_liii_uuid (gf::scheme* sc) {
  glue_uuid4 (sc);
}

static gf::pointer
f_datetime_now (gf::scheme* sc, gf::pointer args) {
  // Get current time using tbox for year, month, day, etc.
  tb_time_t now= tb_time ();

  // Get local time
  tb_tm_t lt= {0};
  if (!tb_localtime (now, &lt)) {
    return gf::f (sc);
  }

  // Use C++ chrono to get microseconds
  std::uint64_t micros= 0;
#ifdef TB_CONFIG_OS_WINDOWS
  // On Windows, ensure we properly handle chrono
  FILETIME       ft;
  ULARGE_INTEGER uli;
  GetSystemTimeAsFileTime (&ft);
  uli.LowPart = ft.dwLowDateTime;
  uli.HighPart= ft.dwHighDateTime;
  // Convert to microseconds and get modulo
  micros= (uli.QuadPart / 10) % 1000000; // Convert from 100-nanosecond intervals to microseconds
#else
  // Standard approach for other platforms
  auto now_chrono= std::chrono::system_clock::now ();
  auto duration  = now_chrono.time_since_epoch ();
  micros         = std::chrono::duration_cast<std::chrono::microseconds> (duration).count () % 1000000;
#endif

  // Create a vector with the time components - vector is easier to index than list in Scheme
  gf::pointer time_vec= gf::make_vector (sc, 7);

  // Fill the vector with values
  gf::vector_set (sc, time_vec, 0, gf::make_integer (sc, lt.year));   // year
  gf::vector_set (sc, time_vec, 1, gf::make_integer (sc, lt.month));  // month
  gf::vector_set (sc, time_vec, 2, gf::make_integer (sc, lt.mday));   // day
  gf::vector_set (sc, time_vec, 3, gf::make_integer (sc, lt.hour));   // hour
  gf::vector_set (sc, time_vec, 4, gf::make_integer (sc, lt.minute)); // minute
  gf::vector_set (sc, time_vec, 5, gf::make_integer (sc, lt.second)); // second
  gf::vector_set (sc, time_vec, 6, gf::make_integer (sc, micros));    // micro-second

  return time_vec;
}

inline void
glue_datetime_now (gf::scheme* sc) {
  const char* name= "g_datetime-now";
  const char* desc= "(g_datetime-now) => datetime, create a datetime object with current time";
  gf::define_function (sc, name, f_datetime_now, 0, 0, false, desc);
}

static gf::pointer
f_date_now (gf::scheme* sc, gf::pointer args) {
  // Get current time using tbox for year, month, day, etc.
  tb_time_t now= tb_time ();

  // Get local time
  tb_tm_t lt= {0};
  if (!tb_localtime (now, &lt)) {
    return gf::f (sc);
  }

  // Create a vector with the time components - vector is easier to index than list in Scheme
  gf::pointer time_vec= gf::make_vector (sc, 3);

  // Fill the vector with values
  gf::vector_set (sc, time_vec, 0, gf::make_integer (sc, lt.year));  // year
  gf::vector_set (sc, time_vec, 1, gf::make_integer (sc, lt.month)); // month
  gf::vector_set (sc, time_vec, 2, gf::make_integer (sc, lt.mday));  // day

  return time_vec;
}

inline void
glue_date_now (gf::scheme* sc) {
  const char* name= "g_date-now";
  const char* desc= "(g_date-now) => date, create a date object with current date";
  gf::define_function (sc, name, f_date_now, 0, 0, false, desc);
}

inline void
glue_liii_time (gf::scheme* sc) {
  glue_sleep (sc);
}

inline void
glue_liii_datetime (gf::scheme* sc) {
  glue_datetime_now (sc);
  glue_date_now (sc);
}

// -------------------------------- iota --------------------------------
static inline gf::pointer
iota_list (gf::scheme* sc, gf::int_ count, gf::pointer start, gf::int_ step) {
  gf::pointer res= gf::nil (sc);
  gf::int_     val;
  for (val= gf::integer (start) + step * (count - 1); count > 0; count--) {
    res= gf::cons (sc, gf::make_integer (sc, val), res);
    val-= step;
  }
  return res;
}

static gf::pointer
iota_list_p_ppp (gf::scheme* sc, gf::pointer count, gf::pointer start, gf::pointer step) {
  if (!gf::is_integer (count)) {
    return gf::error (sc, gf::make_symbol (sc, "type-error"),
                     gf::list (sc, gf::make_string (sc, "iota: count must be an integer"), count));
  }
  if (!gf::is_integer (start)) {
    return gf::error (sc, gf::make_symbol (sc, "type-error"),
                     gf::list (sc, gf::make_string (sc, "iota: start must be an integer"), start));
  }
  if (!gf::is_integer (step)) {
    return gf::error (sc, gf::make_symbol (sc, "type-error"),
                     gf::list (sc, gf::make_string (sc, "iota: step must be an integer"), step));
  }
  gf::int_ cnt= gf::integer (count);
  if (cnt < 0) {
    return gf::error (sc, gf::make_symbol (sc, "value-error"),
                     gf::list (sc, gf::make_string (sc, "iota: count is negative"), count));
  }
  gf::int_ st = gf::integer (start);
  gf::int_ stp= gf::integer (step);
  return iota_list (sc, cnt, start, stp);
}

static gf::pointer
g_iota_list (gf::scheme* sc, gf::pointer args) {
  gf::pointer arg1 = gf::car (args); // count
  gf::pointer rest1= gf::cdr (args);
  gf::pointer arg2 = (gf::is_pair (rest1)) ? gf::car (rest1) : gf::make_integer (sc, 0); // start value, default 0
  gf::pointer rest2= gf::cdr (rest1);
  gf::pointer arg3 = (gf::is_pair (rest2)) ? gf::car (rest2) : gf::make_integer (sc, 1); // step size, default 1
  return iota_list_p_ppp (sc, arg1, arg2, arg3);
}

inline void
glue_iota_list (gf::scheme* sc) {
  const char* name= "iota";
  const char* desc= "(iota count [start [step]]) => list, returns a list of count elements starting from start "
                    "(default 0) with step (default 1)";
  gf::define_function (sc, name, g_iota_list, 1, 2, false, desc);
}

inline void
glue_liii_list (gf::scheme* sc) {
  glue_iota_list (sc);
}

void
glue_for_community_edition (gf::scheme* sc) {
  glue_goldfish (sc);
  glue_scheme_time (sc);
  glue_scheme_process_context (sc);
  glue_liii_sys (sc);
  glue_liii_os (sc);
  glue_subprocess_run_values (sc);
  glue_vm (sc);
  glue_liii_path (sc);
  glue_liii_list (sc);
  glue_liii_string (sc);
  glue_liii_time (sc);
  glue_liii_datetime (sc);
  glue_liii_uuid (sc);
  glue_liii_hashlib (sc);
  glue_liii_base64 (sc);
  glue_liii_reader (sc);
  glue_scheme_base (sc);
  glue_scheme_char (sc);
  glue_njson (sc);
#ifdef GOLDFISH_ENABLE_HTTP
  glue_http (sc);
  glue_http_async (sc);
#endif
}

static void
display_help () {
  cout << "Goldfish Scheme " << GOLDFISH_VERSION << " by LiiiLabs" << endl;
  cout << endl;
  cout << "Commands:" << endl;
  cout << "  help               Display this help message" << endl;
  cout << "  version            Display version" << endl;
  cout << "  eval CODE          Evaluate Scheme code" << endl;
  cout << "                     Example: gf eval '(+ 1 2)'" << endl;
  cout
      << "                     Prefer single quotes so double quotes inside Scheme strings usually do not need escaping"
      << endl;
  cout << "  load FILE          Load Scheme code from FILE, then enter REPL" << endl;
  cout << "  fix [options] PATH Format PATH (PATH can be a .scm file or directory)" << endl;
  cout << "                     Options:" << endl;
  cout << "                       --dry-run  Print formatted result to stdout" << endl;
  cout << "  source ORG/LIB     Print the exact source of ORG/LIB from current *load-path*" << endl;
  cout << "                     Reads the real library file, not tests/ or generated docs" << endl;
  cout << "                     Example: gf source liii/path" << endl;
  cout << "  doc ORG/LIB        Show the library overview for ORG/LIB from tests/" << endl;
  cout << "                     Usually reads tests/ORG/LIB-test.scm" << endl;
  cout << "                     Example: gf doc liii/path" << endl;
  cout << "  doc ORG/LIB FUNC   Show the function doc/test file for FUNC under a specific library" << endl;
  cout << "                     Best when you already know the library, or the name is ambiguous" << endl;
  cout << "                     Example: gf doc liii/path \"path-read-text\"" << endl;
  cout << "                     Quote FUNC for names like \"bag-delete!\", \"path?\", \"alist->fxmapping\", or "
          "\"bag<=?\""
       << endl;
  cout << "                     This preserves symbols such as ! ? > < and keeps FUNC as one shell argument" << endl;
  cout << "  doc FUNC           Search visible libraries for exported FUNC, then show its doc/test file" << endl;
  cout << "                     If multiple libraries export it, candidates are listed" << endl;
  cout << "                     Example: gf doc \"string-split\"" << endl;
  cout << "                     Quote FUNC for names like \"bag-delete!\", \"path?\", \"alist->fxmapping\", or "
          "\"bag<=?\""
       << endl;
  cout << "                     This keeps shell-sensitive symbols intact and makes it clear FUNC is one argument"
       << endl;
  cout << "  doc --build-json   Rebuild tests/function-library-index.json for global gf doc FUNC lookup" << endl;
  cout << "                     Needed by function-name search and fuzzy suggestions" << endl;
  cout << "                     Run this after changing exports, or before packaging" << endl;
  cout << "  test [PATTERN]     Run tests (all *-test.scm files under tests/)" << endl;
  cout << "                     PATTERN can be:" << endl;
  cout << "                       (none)          Run all tests" << endl;
  cout << "                       FILE.scm        Run specific test file" << endl;
  cout << "                       DIR/            Run tests in directory" << endl;
  cout << "                       name-test.scm   Match by file name" << endl;
  cout << "                       substring       Match by path substring" << endl;
  cout << "  run TARGET         Run main function from TARGET" << endl;
  cout << "                     TARGET can be:" << endl;
  cout << "                       FILE.scm       Load file and run main" << endl;
  cout << "                       x/y/z.scm      Load file and run main" << endl;
  cout << "                       module.name    Import (module name) and run main" << endl;
#ifdef GOLDFISH_WITH_REPL
  cout << "  repl               Enter interactive REPL mode" << endl;
#endif
  cout << "  FILE               Load and evaluate Scheme code from FILE" << endl;
  cout << endl;
  cout << "Options:" << endl;
  cout << "  --mode, -m MODE    Set mode: default, liii, sicp, r7rs, s7" << endl;
  cout << "  -I DIR             Prepend DIR to library search path" << endl;
  cout << "  -A DIR             Append DIR to library search path" << endl;
  cout << "  -e CODE            Alias for eval CODE" << endl;
  cout << endl;
  cout << "If no command is specified, help is displayed by default." << endl;
}

static void
display_version () {
  cout << "Goldfish Scheme " << GOLDFISH_VERSION << " by LiiiLabs" << endl;
  cout << "based on S7 Scheme " << gf::host_version () << " (" << gf::host_date () << ")" << endl;
}

static void
display_for_invalid_options (const std::vector<std::string>& invalid_opts) {
  for (const auto& opt : invalid_opts) {
    std::cerr << "Invalid option: " << opt << "\n";
  }
  std::cerr << "\n";
  display_help ();
}

static void
goldfish_eval_file (gf::scheme* sc, string path, bool quiet) {
  // evaluate (load path): s7_eval processes the deferred read frames that a
  // C-implemented load pushes; s7_call/s7_apply_function would skip them.
  gf::pointer result= gf::eval (sc, gf::list (sc, gf::make_symbol (sc, "load"),
                                           gf::make_string (sc, path.c_str ())),
                              gf::rootlet (sc));
  if (!result) {
    cerr << "Failed to load " << path << endl;
    exit (-1);
  }
  if (!quiet) {
    cout << path << " => " << gf::object_to_c_string (sc, result) << endl;
  }
}

static string
goldfish_cli_program_name () {
  if (!command_args.empty ()) {
    string program= fs::path (command_args.front ()).filename ().string ();
    if (!program.empty ()) {
      return program;
    }
  }
  return "gf";
}

static bool
goldfish_is_fix_hint_candidate_error (const string& errmsg) {
  return errmsg.find ("unexpected close paren") != string::npos || errmsg.find ("missing close paren") != string::npos;
}

static string
goldfish_extract_scheme_path_from_error (const string& errmsg) {
  const char* marker_str= ".scm";
  size_t      marker   = errmsg.find (marker_str);
  while (marker != string::npos) {
    size_t after= marker + 4;
    char   next = (after < errmsg.size ()) ? errmsg[after] : '\0';
    // path.scm[...] (s7 file-location style) or path.scm at end/whitespace
    // (the R7RS reader's "in <file>" annotation)
    if (next == '[' || next == '\0' || next == '\n' || next == ')' || next == ';' || next == '"' || next == '\'' ||
        std::isspace (static_cast<unsigned char> (next))) {
      size_t start= marker;
      while (start > 0) {
        unsigned char ch= static_cast<unsigned char> (errmsg[start - 1]);
        if (std::isspace (ch) || ch == '"' || ch == '\'' || ch == '`' || ch == '(' || ch == ')' || ch == ',' ||
            ch == ';') {
          break;
        }
        --start;
      }

      string candidate= errmsg.substr (start, marker + 4 - start);
      if (!candidate.empty ()) {
        return candidate;
      }
    }

    marker= errmsg.find (marker_str, marker + 1);
  }

  return "";
}

static string
goldfish_extract_error_expression (const string& errmsg, size_t search_start) {
  const string infix= " in ";
  size_t       start= errmsg.find (infix, search_start);
  if (start == string::npos) {
    return "";
  }

  start+= infix.size ();
  size_t end= errmsg.find ('\n', start);
  if (end == string::npos) {
    end= errmsg.size ();
  }

  return errmsg.substr (start, end - start);
}

static bool
goldfish_form_contains_called_symbol (gf::scheme* sc, gf::pointer form, const string& function_name) {
  if (gf::is_pair (form)) {
    gf::pointer operator_form= gf::car (form);
    if (gf::is_symbol (operator_form) && (function_name == gf::symbol_name (operator_form))) {
      return true;
    }

    for (gf::pointer iter= form; gf::is_pair (iter); iter= gf::cdr (iter)) {
      if (goldfish_form_contains_called_symbol (sc, gf::car (iter), function_name)) {
        return true;
      }
    }

    gf::pointer tail= form;
    while (gf::is_pair (tail)) {
      tail= gf::cdr (tail);
    }
    if ((!gf::is_null (sc, tail)) && goldfish_form_contains_called_symbol (sc, tail, function_name)) {
      return true;
    }
  }

  return false;
}

// read one datum from port through the registered reader (tiny or Scheme)
static gf::pointer
goldfish_read_datum (gf::scheme* sc, gf::pointer port) {
  return gf::eval (sc, gf::list (sc, gf::make_symbol (sc, "read"), port), gf::rootlet (sc));
}

static bool
goldfish_error_expression_contains_function_call (gf::scheme* sc, const string& expression,
                                                  const string& function_name) {
  if (expression.empty ()) {
    return false;
  }

  gf::pointer port      = gf::open_input_string (sc, expression.c_str ());
  gf::pointer eof_object= gf::eof_object (sc);
  gf::pointer form      = goldfish_read_datum (sc, port);
  gf::close_input_port (sc, port);

  if ((form == eof_object) || (!form)) {
    return expression.find ("(" + function_name) != string::npos;
  }

  return goldfish_form_contains_called_symbol (sc, form, function_name);
}

static string
goldfish_extract_unbound_function_name_from_error (gf::scheme* sc, const string& errmsg) {
  const string prefix= "unbound variable ";
  size_t       start = errmsg.find (prefix);
  if (start == string::npos) {
    return "";
  }

  start+= prefix.size ();
  size_t end= start;
  while (end < errmsg.size ()) {
    unsigned char ch= static_cast<unsigned char> (errmsg[end]);
    if (std::isspace (ch) || ch == '(' || ch == ')' || ch == ';') {
      break;
    }
    ++end;
  }

  if (end == start) {
    return "";
  }

  string function_name= errmsg.substr (start, end - start);
  if (errmsg.find ("in (" + function_name, end) != string::npos) {
    return function_name;
  }

  string error_expression= goldfish_extract_error_expression (errmsg, end);
  if (!goldfish_error_expression_contains_function_call (sc, error_expression, function_name)) {
    return "";
  }

  return function_name;
}

static string
goldfish_format_scheme_error_message (const char* errmsg) {
  if ((!errmsg) || (!*errmsg)) {
    return "";
  }

  string formatted= errmsg;
  if (formatted.find ("Hint: try `") != string::npos) {
    return formatted;
  }
  if (!goldfish_is_fix_hint_candidate_error (formatted)) {
    return formatted;
  }

  string path= goldfish_extract_scheme_path_from_error (formatted);
  if (path.empty ()) {
    return formatted;
  }

  if ((!formatted.empty ()) && (formatted.back () != '\n')) {
    formatted+= '\n';
  }
  formatted+=
      "Hint: try `" + goldfish_cli_program_name () + " fix " + path + "` to repair common parenthesis issues.\n";
  return formatted;
}

static string
goldfish_shell_double_quote (const string& value) {
  string quoted= "\"";
  for (char ch : value) {
    switch (ch) {
    case '\\':
      quoted+= "\\\\";
      break;
    case '"':
      quoted+= "\\\"";
      break;
    case '$':
      quoted+= "\\$";
      break;
    case '`':
      quoted+= "\\`";
      break;
    default:
      quoted+= ch;
      break;
    }
  }
  quoted+= "\"";
  return quoted;
}

static gf::pointer
goldfish_eval_through_reader (gf::scheme* sc, const string& code);

static string
goldfish_append_doc_hint_if_needed (gf::scheme* sc, const string& errmsg) {
  if (errmsg.find ("Hint: try `") != string::npos) {
    return errmsg;
  }

  string function_name= goldfish_extract_unbound_function_name_from_error (sc, errmsg);
  if (function_name.empty ()) {
    return errmsg;
  }

  string formatted= errmsg;
  if ((!formatted.empty ()) && (formatted.back () != '\n')) {
    formatted+= '\n';
  }

  // Hint text is produced by (liii project)'s function-doc-hint; the C++
  // side only escapes literals and falls back when the module is missing.
  string escaped_name;
  for (char c : function_name) {
    if (c == '\\' || c == '"') escaped_name+= '\\';
    escaped_name+= c;
  }
  string program= goldfish_cli_program_name ();
  string escaped_program;
  for (char c : program) {
    if (c == '\\' || c == '"') escaped_program+= '\\';
    escaped_program+= c;
  }
  gf::pointer hint= goldfish_eval_through_reader (
      sc,
      "(import (liii project))"
      " (catch #t"
      "   (lambda () (function-doc-hint \"" + escaped_name + "\" \"" + escaped_program + "\"))"
      "   (lambda _ \"\"))");
  string hint_text= (gf::is_string (hint)) ? gf::string (hint) : "";
  formatted+= (hint_text.empty ())
      ? "Hint: try `" + goldfish_cli_program_name () + " doc " + goldfish_shell_double_quote (function_name) + "`\n"
      : hint_text;
  return formatted;
}

static void
goldfish_render_scheme_error_message (gf::scheme* sc, const char* errmsg, string& rendered) {
  rendered= goldfish_append_doc_hint_if_needed (sc, goldfish_format_scheme_error_message (errmsg));
  if ((!rendered.empty ()) && (rendered.back () != '\n')) {
    rendered+= '\n';
  }
}

static void
goldfish_print_scheme_error_message (gf::scheme* sc, const char* errmsg) {
  if ((errmsg) && (*errmsg)) {
    string rendered;
    goldfish_render_scheme_error_message (sc, errmsg, rendered);
    cout << rendered;
  }
}

static void
goldfish_print_prefixed_scheme_error_message (gf::scheme* sc, const string& prefix, const char* errmsg) {
  if ((errmsg) && (*errmsg)) {
    string rendered;
    goldfish_render_scheme_error_message (sc, errmsg, rendered);
    cerr << prefix;
    if ((!prefix.empty ()) && (prefix.back () != '\n')) {
      cerr << '\n';
    }
    cerr << rendered;
  }
}

// Parse CODE through the Scheme `read` (after bootstrap) and evaluate each
// form; returns the value of the last form. Uses s7_eval_c_string as the
// outer evaluator so error reporting is unchanged. Once the expander is
// loaded (expand-eval exists), each form goes through the expander and is
// evaluated in the expander library; otherwise (--mode s7, no expander) we
// fall back to the plain s7 eval.
static gf::pointer
goldfish_eval_through_reader (gf::scheme* sc, const string& code) {
  string escaped;
  for (char c : code) {
    if (c == '\\' || c == '"') escaped += '\\';
    escaped += c;
  }
  string expr= "(let ((p (open-input-string \"" + escaped +
               "\"))) (let loop ((r #f)) (let ((d (read p))) "
               "(if (eof-object? d) (begin (close-input-port p) r) "
               "(loop (if (defined? 'expand-eval) (expand-eval d) "
               "(eval d (rootlet))))))))";
  return gf::eval_c_string (sc, expr.c_str ());
}

static void
goldfish_eval_code (gf::scheme* sc, string code) {
  gf::pointer x= goldfish_eval_through_reader (sc, code);
  cout << gf::object_to_c_string (sc, x) << endl;
}

static string
find_golddoc_tool_root (const char* gf_lib) {
  std::error_code  ec;
  vector<fs::path> candidates= {fs::path (gf_lib) / "tools" / "doc",
                                fs::path (gf_lib).parent_path () / "tools" / "doc"};

  for (const auto& candidate : candidates) {
    if (fs::is_directory (candidate, ec)) {
      return candidate.string ();
    }
    ec.clear ();
  }

  return "";
}

static string
find_goldsource_tool_root (const char* gf_lib) {
  std::error_code  ec;
  vector<fs::path> candidates= {fs::path (gf_lib) / "tools" / "source",
                                fs::path (gf_lib).parent_path () / "tools" / "source"};

  for (const auto& candidate : candidates) {
    if (fs::is_directory (candidate, ec)) {
      return candidate.string ();
    }
    ec.clear ();
  }

  return "";
}

static string
find_goldhelp_tool_root (const char* gf_lib) {
  std::error_code  ec;
  vector<fs::path> candidates= {fs::path (gf_lib) / "tools" / "help",
                                fs::path (gf_lib).parent_path () / "tools" / "help"};

  for (const auto& candidate : candidates) {
    if (fs::is_directory (candidate, ec)) {
      return candidate.string ();
    }
    ec.clear ();
  }

  return "";
}

// Tool dispatch lives in its own header (depends on everything above).
#include "gf_tool_dispatch.hpp"

gf::scheme*
init_goldfish_scheme (const char* gf_lib) {
  gf::scheme* sc= gf::init ();
  gf::add_to_load_path (sc, gf_lib);

  if (!tb_init (tb_null, tb_null)) exit (-1);

  glue_for_community_edition (sc);
  return sc;
}

void
customize_goldfish_by_mode (gf::scheme* sc, string mode, const char* gf_lib) {
  if (mode != "s7") {
    // the tiny bootstrap read loads boot.scm, string-cursor.scm and reader.scm
    bootstrap_scheme_reader (sc, gf_lib);
  }

  // Phase 1: (removed) r7rs-small used to be host-imported here through
  // boot.scm's define-macro import (varlet into the curlet) because the
  // lib-layer install code needed scheme/base's let-values, which the s7
  // host lacks.  The seed (boot.scm) now provides let-values/let*-values
  // itself, so the lib-layer can be s7-evaluated without (scheme base), and
  // r7rs-small loads entirely through the expander (pure syntax, no varlet).

  // B4: load the Sets-of-Scopes expander core and its user-space macro library
  // so the expander is available to every gf invocation.  help/version return
  // before customize_goldfish_by_mode, so they stay fast.
  if (mode != "s7") {
    gf::eval_c_string (sc, "(load-source-file \"expander/kernel-combined.scm\")");
    // Guile boot-9 style: base library functions (map/for-each) implemented
    // in Scheme, evaluated into the rootlet like the kernel artifact so the
    // expander kernel, every library, and user programs all resolve the same
    // definition by name (and the VM needs no map/for-each fast paths).
    // Loaded AFTER the kernel (needs lambda/if/cons...) and BEFORE the
    // compiler preload, so compile-time map references resolve to it.
    gf::eval_c_string (sc, "(load-source-file \"expander/lib/base-functions.scm\")");
    // Load the minimal derived forms (let/cond/case/when/do/and/or plus
    // let-values/let*-values) into the base library, then the R7RS reader
    // THROUGH the expander (boot.scm's load-expanded), immediately after the
    // artifact: install.scm's lib-layer files use `(X ...)' ellipsis syntax
    // that s7's tiny reader collapses, so the R7RS reader must be up before
    // install.scm reads them, and the reader itself uses those derived forms
    // (which the lib layer would otherwise provide -- prelude.scm breaks that
    // cycle).  The reader's read/read-forms/load/expand-eval are re-bound in
    // the rootlet for the s7-side loader paths.  install.scm also loads
    // THROUGH the expander (its let-values/let*-values/when/unless desugar to
    // the prelude macros); only the artifact itself is still s7-evaluated.
    gf::eval_c_string (sc, "(load-expanded \"liii/prelude.scm\" 'base)");
    gf::eval_c_string (sc, "(load-expanded \"liii/reader.scm\")");
    gf::eval_c_string (sc, "(load-expanded \"expander/lib/install.scm\" '(expander lib install))");
    gf::eval_c_string (sc, "(install-standard-library!)");
    // Preload the compiler so library captures after this point compile
    // transformer definitions to VM bytecode programs (compiled once,
    // resolved references), instead of lowered forms that must be re-evaluated
    // and re-resolved by name at every warm start.  load-library! is a plain
    // function (resolvable in the rootlet), so this runs through s7 directly
    // -- NOT goldfish_eval_through_reader -- to avoid importing (goldfish)
    // into the session program library (which would leak the implementation
    // surface into a strict r7rs program).
    gf::eval_c_string (sc, "(load-library! '(goldfish compiler))");
    g_expander_online= true;
  }

  // Phase 2: mode-specific imports, now that the expander can handle
  // define-syntax in the library chains (liii base -> srfi-2, etc).  These
  // must go through the expander (expand-eval), not s7's host import macro:
  // the host macro imports only value bindings, so macros defined via
  // define-syntax in the library chain (and-let*, receive, ...) would be
  // missing after a host-side import, and s7's native eval would also fail
  // on the define-syntax forms themselves (s7 has no define-syntax).
  //
  // A mode is an IMPORT SHORTCUT (R7RS 5.1 program semantics): top-level
  // code runs in an empty program environment whose initial bindings are
  // exactly these imports.  r7rs is the foundation (scheme base); liii
  // builds the extension layer on top of the r7rs-small standard libraries
  // (cf. Guile's ice-9).  Nothing else is ambient: an identifier used
  // without an import is an error.
  {
    if (mode == "default" || mode == "liii") {
      goldfish_eval_through_reader (
          sc, "(import (goldfish) (scheme base) (scheme write) (scheme read)"
              " (scheme file) (scheme process-context) (scheme time)"
              " (scheme inexact) (scheme char) (scheme complex) (scheme cxr)"
              " (scheme eval) (scheme case-lambda) (liii base) (liii error)"
              " (liii string))");
    }
    else if (mode == "scheme") {
      goldfish_eval_through_reader (sc, "(import (scheme base) (liii base) (liii error))");
    }
    else if (mode == "sicp") {
      goldfish_eval_through_reader (sc, "(import (scheme base) (srfi sicp))");
    }
    else if (mode == "r7rs") {
      goldfish_eval_through_reader (sc, "(import (scheme base))");
    }
    else if (mode == "s7") {
    }
    else {
      cerr << "No such mode: " << mode << endl;
      exit (-1);
    }
  }
}

string
find_goldfish_library () {
  string exe_path= goldfish_exe ();

  tb_char_t        data_bin[TB_PATH_MAXN]= {0};
  tb_char_t const* ret_bin               = tb_path_directory (exe_path.c_str (), data_bin, sizeof (data_bin));

  tb_char_t        data_root[TB_PATH_MAXN]= {0};
  tb_char_t const* gf_root                = tb_path_directory (ret_bin, data_root, sizeof (data_root));

  tb_char_t        data_lib[TB_PATH_MAXN]= {0};
  tb_char_t const* gf_lib                = tb_path_absolute_to (gf_root, "share/goldfish", data_lib, sizeof (data_lib));
#ifdef TB_CONFIG_OS_LINUX
  if (strcmp (gf_root, "/") == 0) {
    gf_lib= "/usr/share/goldfish";
  }
#endif

  if (!tb_file_access (gf_lib, TB_FILE_MODE_RO)) {
    gf_lib= tb_path_absolute_to (gf_root, "goldfish", data_lib, sizeof (data_lib));
    if (!tb_file_access (gf_lib, TB_FILE_MODE_RO)) {
      cerr << "The load path for Goldfish standard library does not exist" << endl;
      exit (-1);
    }
  }

  return string (gf_lib);
}

string
find_goldfish_boot (const char* gf_lib) {
  tb_char_t        data_boot[TB_PATH_MAXN]= {0};
  tb_char_t const* gf_boot= tb_path_absolute_to (gf_lib, "liii/boot.scm", data_boot, sizeof (data_boot));

  if (!tb_file_access (gf_boot, TB_FILE_MODE_RO)) {
    cerr << "The boot.scm for Goldfish Scheme does not exist" << endl;
    exit (-1);
  }
  return string (gf_boot);
}

// REPL components live in their own header (isocline completion/history/
// meta commands); included under GOLDFISH_WITH_REPL.
#include "gf_repl.hpp"

struct StartupCliOptions {
  string         mode= "default";
  vector<string> prepend_dirs;
  vector<string> append_dirs;
  string         command;
  int            command_index= -1;
  string         error;
};

static std::string
parse_mode_option (int argc, char** argv, const std::string& default_mode= "default") {
  std::string mode= default_mode;
  for (int i= 1; i < argc; ++i) {
    string arg= argv[i];
    if ((arg == "--mode" || arg == "-m") && (i + 1) < argc) {
      mode= argv[++i];
    }
    else if (arg.rfind ("--mode=", 0) == 0) {
      mode= arg.substr (7);
    }
    else if (arg.rfind ("-m=", 0) == 0) {
      mode= arg.substr (3);
    }
  }
  return mode;
}

static bool
is_legacy_cli_command (const string& arg) {
  return arg == "--help" || arg == "-h" || arg == "--version" || arg == "-v" || arg == "-e";
}

static string
normalize_load_path_dir (const string& raw_dir) {
  fs::path path (raw_dir);
  string   normalized= path.lexically_normal ().string ();
  return normalized.empty () ? raw_dir : normalized;
}

static bool
load_path_directory_exists (const string& raw_dir) {
  std::error_code ec;
  fs::path        path (normalize_load_path_dir (raw_dir));
  return fs::exists (path, ec) && fs::is_directory (path, ec);
}

static bool
append_unique_string (vector<string>& items, const string& raw_item) {
  string item= normalize_load_path_dir (raw_item);
  if (item.empty ()) return false;
  if (std::find (items.begin (), items.end (), item) != items.end ()) return false;
  items.push_back (item);
  return true;
}

static bool
is_plugin_name_part (const string& value) {
  if (value.empty ()) return false;
  return std::all_of (value.begin (), value.end (),
                      [] (unsigned char ch) { return (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9'); });
}

static bool
is_auto_goldfish_plugin_dir_name (const string& name) {
  size_t dash_pos= name.find ('-');
  if (dash_pos == string::npos || dash_pos == 0 || dash_pos == name.length () - 1) {
    return false;
  }
  if (name.find ('-', dash_pos + 1) != string::npos) {
    return false;
  }
  return is_plugin_name_part (name.substr (0, dash_pos)) && is_plugin_name_part (name.substr (dash_pos + 1));
}

static bool
directory_contains_scheme_sources (const fs::path& dir) {
  std::error_code ec;
  for (fs::recursive_directory_iterator it (dir, fs::directory_options::skip_permission_denied, ec), end; it != end;
       it.increment (ec)) {
    if (ec) {
      ec.clear ();
      continue;
    }
    if (it->is_regular_file (ec) && it->path ().extension () == ".scm") {
      return true;
    }
    ec.clear ();
  }
  return false;
}

static vector<string>
discover_auto_goldfish_library_dirs () {
  vector<string> dirs;
  const char*    home= getenv ("HOME");
  if ((!home) || (!*home)) {
    return dirs;
  }

  std::error_code ec;
  fs::path        root= fs::path (home) / ".local" / "goldfish";
  if (!fs::exists (root, ec) || !fs::is_directory (root, ec)) {
    return dirs;
  }

  for (fs::directory_iterator it (root, fs::directory_options::skip_permission_denied, ec), end; it != end;
       it.increment (ec)) {
    if (ec) {
      ec.clear ();
      continue;
    }
    if (!it->is_directory (ec)) {
      ec.clear ();
      continue;
    }

    string name= it->path ().filename ().string ();
    if (!is_auto_goldfish_plugin_dir_name (name)) {
      ec.clear ();
      continue;
    }
    if (!directory_contains_scheme_sources (it->path ())) {
      ec.clear ();
      continue;
    }

    append_unique_string (dirs, it->path ().string ());
    ec.clear ();
  }

  std::sort (dirs.begin (), dirs.end ());
  return dirs;
}

static vector<string>
current_load_path_entries (gf::scheme* sc) {
  vector<string> entries;
  for (gf::pointer rest= gf::load_path (sc); gf::is_pair (rest); rest= gf::cdr (rest)) {
    gf::pointer entry= gf::car (rest);
    if (gf::is_string (entry)) {
      append_unique_string (entries, string (gf::string (entry)));
    }
  }
  return entries;
}

static void
set_load_path_entries (gf::scheme* sc, const vector<string>& entries) {
  gf::pointer list= gf::nil (sc);
  for (auto it= entries.rbegin (); it != entries.rend (); ++it) {
    list= gf::cons (sc, gf::make_string (sc, it->c_str ()), list);
  }
  gf::symbol_set_value (sc, gf::make_symbol (sc, "*load-path*"), list);
}

static void
prepend_load_path_entries (gf::scheme* sc, const vector<string>& prepend_dirs) {
  vector<string> seen= current_load_path_entries (sc);
  for (auto it= prepend_dirs.rbegin (); it != prepend_dirs.rend (); ++it) {
    string dir= normalize_load_path_dir (*it);
    if (dir.empty ()) continue;
    if (std::find (seen.begin (), seen.end (), dir) != seen.end ()) continue;
    gf::add_to_load_path (sc, dir.c_str ());
    seen.insert (seen.begin (), dir);
  }
}

static void
append_load_path_entries (gf::scheme* sc, const vector<string>& append_dirs) {
  vector<string> entries= current_load_path_entries (sc);
  bool           changed= false;
  for (const auto& raw_dir : append_dirs) {
    string dir= normalize_load_path_dir (raw_dir);
    if (dir.empty ()) continue;
    if (std::find (entries.begin (), entries.end (), dir) != entries.end ()) continue;
    entries.push_back (dir);
    changed= true;
  }
  if (changed) {
    set_load_path_entries (sc, entries);
  }
}

static StartupCliOptions
parse_startup_cli_options (int argc, char** argv) {
  StartupCliOptions opts;

  for (int i= 1; i < argc; ++i) {
    string arg= argv[i];

    if (arg == "--mode" || arg == "-m") {
      if ((i + 1) >= argc) {
        opts.error= "Error: '--mode' requires a MODE argument.";
        return opts;
      }
      opts.mode= argv[++i];
      continue;
    }
    if (arg == "--auto-compile") {
      setenv ("GOLDFISH_AUTO_COMPILE", "1", 1);
      continue;
    }
    if (arg == "--no-auto-compile") {
      setenv ("GOLDFISH_AUTO_COMPILE", "0", 1);
      continue;
    }
    if (arg.rfind ("--mode=", 0) == 0) {
      opts.mode= arg.substr (7);
      continue;
    }
    if (arg.rfind ("-m=", 0) == 0) {
      opts.mode= arg.substr (3);
      continue;
    }

    if (arg == "-I" || arg == "-A") {
      if ((i + 1) >= argc) {
        opts.error= "Error: '" + arg + "' requires a DIRECTORY argument.";
        return opts;
      }
      string dir= argv[++i];
      if (!load_path_directory_exists (dir)) {
        opts.error= "Error: directory does not exist: " + dir;
        return opts;
      }
      if (arg == "-I") {
        append_unique_string (opts.prepend_dirs, dir);
      }
      else {
        append_unique_string (opts.append_dirs, dir);
      }
      continue;
    }

    if (is_legacy_cli_command (arg) || arg.empty () || arg[0] != '-') {
      opts.command      = arg;
      opts.command_index= i;
      break;
    }

    opts.error= "Invalid option: " + arg;
    return opts;
  }

  return opts;
}

static void
apply_startup_load_path_options (gf::scheme* sc, const StartupCliOptions& opts) {
  vector<string> prepend_dirs= opts.prepend_dirs;
  for (const auto& dir : discover_auto_goldfish_library_dirs ()) {
    append_unique_string (prepend_dirs, dir);
  }
  prepend_load_path_entries (sc, prepend_dirs);
  append_load_path_entries (sc, opts.append_dirs);
}

int
repl_for_community_edition (gf::scheme* sc, int argc, char** argv) {
  string      gf_lib_dir  = find_goldfish_library ();
  const char* gf_lib      = gf_lib_dir.c_str ();

  // 供 goldfish `g_command-line` procedure 查询
  command_args.assign (argv, argv + argc);

  StartupCliOptions startup_opts= parse_startup_cli_options (argc, argv);
  if (!startup_opts.error.empty ()) {
    std::cerr << startup_opts.error << "\n\n";
    display_help ();
    exit (1);
  }

  string command      = startup_opts.command;
  int    command_index= startup_opts.command_index;

  if (argc <= 1 || command.empty ()) {
    command= "repl";
  }
  if (command == "-e") {
    command= "eval";
    if (command_index >= 0 && command_index < static_cast<int> (command_args.size ())) {
      command_args[command_index]= "eval";
    }
  }

  // 自动路由：如果参数是目录且第一级文件夹是 tests，自动视为 test 命令
  if (!command.empty () && command != "help" && command != "version" && command != "eval" && command != "load" &&
      command != "repl" && command != "run" && command != "test" && command != "-e") {
    std::error_code ec;
    if (fs::is_directory (command, ec)) {
      fs::path p (command);
      auto     it= p.begin ();
      if (it != p.end () && *it == "tests") {
        if (command_index >= 0 && command_index <= static_cast<int> (command_args.size ())) {
          command_args.insert (command_args.begin () + command_index, "test");
        }
        command= "test";
        std::cerr << "[gf] Auto-routing: detected tests directory, routing to test command" << "\n";
        std::cerr << "[gf] Executing: ";
        for (size_t i= 0; i < command_args.size (); ++i) {
          if (i > 0) std::cerr << " ";
          std::cerr << command_args[i];
        }
        std::cerr << std::endl;
      }
    }
  }

  // 根据命令类型确定默认模式：
  // - repl/load 命令默认使用 liii 模式
  // - 其他命令（eval, run, 直接执行脚本）默认使用 r7rs 模式
  string default_mode= "r7rs";
  if (command == "repl" || command == "load") {
    default_mode= "liii";
  }
  string mode= parse_mode_option (argc, argv, default_mode);

  // 处理旧版的 --help, -h, --version, -v（为了向后兼容）
  if (command == "--help" || command == "-h") {
    display_help ();
    return 0;
  }
  if (command == "--version" || command == "-v") {
    display_version ();
    return 0;
  }

  apply_startup_load_path_options (sc, startup_opts);
  customize_goldfish_by_mode (sc, mode, gf_lib);

  // start capture error output
  const char* errmsg  = NULL;
  gf::pointer  old_port= gf::set_current_error_port (sc, gf::open_output_string (sc));
  int         gc_loc  = -1;
  if (old_port != gf::nil (sc)) gc_loc= gf::gc_protect (sc, old_port);

  // 处理动态注册的工具（从 gfproject.scm 加载，DSL: (gfproject (tools ...))）
  // 候选查询与优先级排序由 (liii project) 完成；宿主逐个尝试 import 并执行
  {
    int tool_ret= goldfish_run_tool (sc, gf_lib, command, errmsg, old_port, gc_loc);
    if (tool_ret != -1) {
      // Tool was found and executed (or failed with an error)
      return tool_ret;
    }
    // tool_ret == -1: not a project tool -- fall through to built-in commands
  }

  if (command == "help") {
    gf::close_output_port (sc, gf::current_error_port (sc));
    gf::set_current_error_port (sc, old_port);
    if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
    display_help ();
    return 0;
  }

  if (command == "version") {
    gf::close_output_port (sc, gf::current_error_port (sc));
    gf::set_current_error_port (sc, old_port);
    if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
    display_version ();
    return 0;
  }

  // 处理 eval 子命令
  if (command == "eval") {
    if (argc < command_index + 1) {
      std::cerr << "Error: 'eval' requires CODE argument.\n" << std::endl;
      gf::close_output_port (sc, gf::current_error_port (sc));
      gf::set_current_error_port (sc, old_port);
      if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
      exit (1);
    }
    // 查找 CODE 参数（跳过 mode 选项，从命令位置之后开始）
    string code;
    for (int i= command_index + 1; i < argc; ++i) {
      string arg= argv[i];
      if (arg == "--mode" || arg == "-m") {
        i++; // skip mode value
        continue;
      }
      if (arg.rfind ("--mode=", 0) == 0 || arg.rfind ("-m=", 0) == 0) {
        continue;
      }
      code= arg;
      break;
    }
    if (code.empty ()) {
      std::cerr << "Error: 'eval' requires CODE argument.\n" << std::endl;
      gf::close_output_port (sc, gf::current_error_port (sc));
      gf::set_current_error_port (sc, old_port);
      if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
      exit (1);
    }
    goldfish_eval_code (sc, code);
    errmsg= gf::get_output_string (sc, gf::current_error_port (sc));
    goldfish_print_scheme_error_message (sc, errmsg);
    gf::close_output_port (sc, gf::current_error_port (sc));
    gf::set_current_error_port (sc, old_port);
    if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
    if ((errmsg) && (*errmsg)) return -1;
    return 0;
  }

  // 处理 load 子命令（加载文件后进入 REPL）
  if (command == "load") {
    if (argc < command_index + 1) {
      std::cerr << "Error: 'load' requires FILE argument.\n" << std::endl;
      gf::close_output_port (sc, gf::current_error_port (sc));
      gf::set_current_error_port (sc, old_port);
      if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
      exit (1);
    }
    // 查找 FILE 参数（跳过 mode 选项，从命令位置之后开始）
    string file;
    for (int i= command_index + 1; i < argc; ++i) {
      string arg= argv[i];
      if (arg == "--mode" || arg == "-m") {
        i++; // skip mode value
        continue;
      }
      if (arg.rfind ("--mode=", 0) == 0 || arg.rfind ("-m=", 0) == 0) {
        continue;
      }
      file= arg;
      break;
    }
    if (file.empty ()) {
      std::cerr << "Error: 'load' requires FILE argument.\n" << std::endl;
      gf::close_output_port (sc, gf::current_error_port (sc));
      gf::set_current_error_port (sc, old_port);
      if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
      exit (1);
    }
    // 加载文件
    goldfish_eval_file (sc, file, true);
    errmsg= gf::get_output_string (sc, gf::current_error_port (sc));
    if ((errmsg) && (*errmsg)) {
      goldfish_print_scheme_error_message (sc, errmsg);
      gf::close_output_port (sc, gf::current_error_port (sc));
      gf::set_current_error_port (sc, old_port);
      if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
      return -1;
    }
    // 加载成功后进入 REPL
#ifdef GOLDFISH_WITH_REPL
    errmsg= gf::get_output_string (sc, gf::current_error_port (sc));
    if ((errmsg) && (*errmsg)) {
      string rendered;
      goldfish_render_scheme_error_message (sc, errmsg, rendered);
      ic_printf ("[red]%s[/]", rendered.c_str ());
    }
    gf::close_output_port (sc, gf::current_error_port (sc));
    gf::set_current_error_port (sc, old_port);
    if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);

    goldfish_repl (sc, mode);
    return 0;
#else
    gf::close_output_port (sc, gf::current_error_port (sc));
    gf::set_current_error_port (sc, old_port);
    if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
    std::cerr << "Interactive REPL is not available in this build.\n" << std::endl;
    exit (-1);
#endif
  }

  // 处理 repl 子命令
  if (command == "repl") {
#ifdef GOLDFISH_WITH_REPL
    errmsg= gf::get_output_string (sc, gf::current_error_port (sc));
    if ((errmsg) && (*errmsg)) {
      string rendered;
      goldfish_render_scheme_error_message (sc, errmsg, rendered);
      ic_printf ("[red]%s[/]", rendered.c_str ());
    }
    gf::close_output_port (sc, gf::current_error_port (sc));
    gf::set_current_error_port (sc, old_port);
    if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);

    goldfish_repl (sc, mode);
    return 0;
#else
    gf::close_output_port (sc, gf::current_error_port (sc));
    gf::set_current_error_port (sc, old_port);
    if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
    std::cerr << "Interactive REPL is not available in this build.\n" << std::endl;
    exit (-1);
#endif
  }

  // 处理 run 子命令
  if (command == "run") {
    // 获取 TARGET 参数
    string target;
    for (int i= command_index + 1; i < argc; ++i) {
      string arg= argv[i];
      if (arg == "--mode" || arg == "-m") {
        i++; // skip mode value
        continue;
      }
      if (arg.rfind ("--mode=", 0) == 0 || arg.rfind ("-m=", 0) == 0) {
        continue;
      }
      target= arg;
      break;
    }
    if (target.empty ()) {
      std::cerr << "Error: 'run' requires TARGET argument.\n" << std::endl;
      gf::close_output_port (sc, gf::current_error_port (sc));
      gf::set_current_error_port (sc, old_port);
      if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
      exit (1);
    }

    // 判断类型并处理
    if (target.find ('/') != string::npos || target.rfind (".scm") == target.length () - 4) {
      // 包含 / 或以 .scm 结尾，按文件路径处理
      // 检查文件是否存在
      std::error_code ec;
      if (!fs::exists (target, ec) || !fs::is_regular_file (target, ec)) {
        gf::close_output_port (sc, gf::current_error_port (sc));
        gf::set_current_error_port (sc, old_port);
        if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
        std::cerr << "Error: File not found: " << target << std::endl;
        return 1;
      }
      goldfish_eval_file (sc, target, true);
    }
    else {
      // 按模块名处理，例如: liii.string -> (liii string)
      string import_expr= "(import (" + target + "))";
      // 将 . 替换为空格
      for (size_t i= 0; i < import_expr.length (); ++i) {
        if (import_expr[i] == '.') import_expr[i]= ' ';
      }
      gf::eval_c_string (sc, import_expr.c_str ());
    }

    errmsg= gf::get_output_string (sc, gf::current_error_port (sc));
    if ((errmsg) && (*errmsg)) {
      goldfish_print_scheme_error_message (sc, errmsg);
      gf::close_output_port (sc, gf::current_error_port (sc));
      gf::set_current_error_port (sc, old_port);
      if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
      return 1;
    }

    // 检查并调用 main 函数
    gf::pointer main_func= gf::name_to_value (sc, "main");
    if ((!main_func) || (!gf::is_procedure (main_func))) {
      // Module targets import through the expander, so `main` resolves in
      // the expander environment rather than the s7 rootlet.
      main_func= goldfish_eval_through_reader (sc, "main");
    }
    if ((!main_func) || (!gf::is_procedure (main_func))) {
      gf::close_output_port (sc, gf::current_error_port (sc));
      gf::set_current_error_port (sc, old_port);
      if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
      std::cerr << "Error: No main function found in target: " << target << std::endl;
      return 1;
    }

    // 调用 main 函数
    gf::call (sc, main_func, gf::nil (sc));

    errmsg= gf::get_output_string (sc, gf::current_error_port (sc));
    goldfish_print_scheme_error_message (sc, errmsg);
    gf::close_output_port (sc, gf::current_error_port (sc));
    gf::set_current_error_port (sc, old_port);
    if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
    return 0;
  }

  // 处理直接执行文件（以 .scm 结尾或存在的文件）
  // 检查是否是文件
  std::error_code ec;
  if (fs::exists (command, ec) && fs::is_regular_file (command, ec)) {
    goldfish_eval_file (sc, command, true);
    errmsg= gf::get_output_string (sc, gf::current_error_port (sc));
    goldfish_print_scheme_error_message (sc, errmsg);
    gf::close_output_port (sc, gf::current_error_port (sc));
    gf::set_current_error_port (sc, old_port);
    if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
    if ((errmsg) && (*errmsg)) return -1;
    return 0;
  }

  // 未知命令
  std::cerr << "Unknown command: " << command << "\n\n";
  display_help ();
  gf::close_output_port (sc, gf::current_error_port (sc));
  gf::set_current_error_port (sc, old_port);
  if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
  return 1;
}

} // namespace goldfish
