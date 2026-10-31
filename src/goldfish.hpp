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

#include "s7.h"
#include "s7_liii_record.h"
#include "s7_r7rs_library.h"
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

#include "liii_signal.hpp"
#include "liii_subprocess.hpp"

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

#define GOLDFISH_VERSION "18.11.49"

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

inline void glue_define (s7_scheme* sc, const char* name, const char* desc, s7_function f, s7_int required,
                         s7_int optional);

static s7_pointer f_function_libraries (s7_scheme* sc, s7_pointer args);

static s7_pointer f_goldfish_library_dir (s7_scheme* sc, s7_pointer args);

static s7_pointer f_project_root (s7_scheme* sc, s7_pointer args);

static vector<string> split_library_query_parts (const string& query);

static bool split_library_query (const string& query, string& group, string& library);

static string find_goldfish_library ();

static vector<string> find_function_libraries_in_load_path (s7_scheme* sc, const string& function_name);

#ifdef GOLDFISH_ENABLE_HTTP
void glue_http (s7_scheme* sc);
void glue_http_async (s7_scheme* sc);
#endif
void glue_liii_base64 (s7_scheme* sc);
void glue_liii_json (s7_scheme* sc);
void glue_liii_go (s7_scheme* sc);
void set_goldfish_lib_dir (const std::string& dir);
void glue_scheme_base (s7_scheme* sc);
void glue_scheme_char (s7_scheme* sc);
void glue_liii_hashlib (s7_scheme* sc);
void glue_liii_os (s7_scheme* sc);
void glue_liii_fs_watch (s7_scheme* sc);
void glue_liii_path (s7_scheme* sc);
void glue_liii_path_stat (s7_scheme* sc);
void glue_liii_psutil (s7_scheme* sc);
void glue_liii_time (s7_scheme* sc);
void glue_liii_sort (s7_scheme* sc);
void glue_liii_string (s7_scheme* sc);
void glue_liii_string_cursor (s7_scheme* sc);
void glue_liii_tempfile (s7_scheme* sc);
void glue_liii_shutil (s7_scheme* sc);
void glue_liii_date_time (s7_scheme* sc);
void glue_liii_uuid (s7_scheme* sc);

extern "C" void glue_syntax_rules (s7_scheme* sc);

inline s7_pointer
string_vector_to_s7_vector (s7_scheme* sc, vector<string> v) {
  int        N  = v.size ();
  s7_pointer ret= s7_make_vector (sc, N);
  for (int i= 0; i < N; i++) {
    s7_vector_set (sc, ret, i, s7_make_string (sc, v[i].c_str ()));
  }
  return ret;
}

inline void
glue_define (s7_scheme* sc, const char* name, const char* desc, s7_function f, s7_int required, s7_int optional) {
  s7_pointer cur_env= s7_curlet (sc);
  s7_pointer func   = s7_make_typed_function (sc, name, f, required, optional, false, desc, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, name), func);
}

// 抛出 (liii error) 约定的 type-error，irritant 为出错的参数
inline s7_pointer
string_type_error (s7_scheme* sc, const char* msg, s7_pointer arg) {
  return s7_error (sc, s7_make_symbol (sc, "type-error"), s7_list (sc, 2, s7_make_string (sc, msg), arg));
}

static s7_pointer
f_version (s7_scheme* sc, s7_pointer args) {
  return s7_make_string (sc, GOLDFISH_VERSION);
}

static s7_pointer
f_delete_file (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "delete-file: path must be a string", path_arg);
  }
  const char* path_c= s7_string (path_arg);
  return s7_make_boolean (sc, tb_file_remove (path_c));
}

inline void
glue_goldfish (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);

  const char* s_version           = "version";
  const char* d_version           = "(version) => string";
  const char* s_delete_file       = "g_delete-file";
  const char* d_delete_file       = "(g_delete-file string) => boolean";
  const char* s_function_libraries= "g_function-libraries";
  const char* d_function_libraries=
      "(g_function-libraries function-name) => list, returns visible library names such as '((liii string)) that "
      "export function-name in the current *load-path*";
  const char* s_goldfish_library_dir= "g_goldfish-library-dir";
  const char* d_goldfish_library_dir= "(g_goldfish-library-dir) => string, returns the goldfish library directory";
  const char* s_project_root        = "g_project-root";
  const char* d_project_root=
      "(g_project-root) => string or #f, returns the directory containing the local gfproject.json, "
      "or #f if none is found in the current working directory";

  s7_define (sc, cur_env, s7_make_symbol (sc, s_version),
             s7_make_typed_function (sc, s_version, f_version, 0, 0, false, d_version, NULL));

  s7_define (sc, cur_env, s7_make_symbol (sc, s_delete_file),
             s7_make_typed_function (sc, s_delete_file, f_delete_file, 1, 0, false, d_delete_file, NULL));

  s7_define (
      sc, cur_env, s7_make_symbol (sc, s_function_libraries),
      s7_make_typed_function (sc, s_function_libraries, f_function_libraries, 1, 0, false, d_function_libraries, NULL));

  s7_define (sc, cur_env, s7_make_symbol (sc, s_goldfish_library_dir),
             s7_make_typed_function (sc, s_goldfish_library_dir, f_goldfish_library_dir, 0, 0, false,
                                     d_goldfish_library_dir, NULL));

  s7_define (sc, cur_env, s7_make_symbol (sc, s_project_root),
             s7_make_typed_function (sc, s_project_root, f_project_root, 0, 0, false, d_project_root, NULL));
}

// old `f_current_second` TODO: use std::chrono::tai_clock::now() when using C++ 20
//                        NOTE(jinser): use a new name for tai
// `current-second` impl by g_get-time-of-day now
static s7_pointer
f_get_time_of_day (s7_scheme* sc, s7_pointer args) {
  using namespace std::chrono;
  auto now        = time_point_cast<microseconds> (system_clock::now ());
  auto since_epoch= now.time_since_epoch ();
  auto sec        = duration_cast<seconds> (since_epoch);

  s7_pointer vs=
      s7_list (sc, 2, s7_make_integer (sc, sec.count ()), s7_make_integer (sc, (since_epoch - sec).count ()));
  return s7_values (sc, vs);
}

static s7_pointer
f_monotonic_nanosecond (s7_scheme* sc, s7_pointer args) {
  using namespace std::chrono;
  auto now     = steady_clock::now ();
  auto duration= now.time_since_epoch ();
  auto count   = duration_cast<std::chrono::nanoseconds> (duration).count ();
  return s7_make_integer (sc, count);
}

template <typename Clock>
constexpr int64_t
clock_resolution_ns () {
  typedef std::chrono::duration<double, std::nano> NS;
  NS                                               ns= typename Clock::duration (1);
  return ns.count ();
}

inline void
glue_scheme_time (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);

  const char* s_get_time_of_day= "g_get-time-of-day";
  const char* d_get_time_of_day= "(g_get-time-of-day): () => (integer, integer), return the "
                                 "current second and microsecond in integer";
  s7_define (sc, cur_env, s7_make_symbol (sc, s_get_time_of_day),
             s7_make_typed_function (sc, s_get_time_of_day, f_get_time_of_day, 0, 0, false, d_get_time_of_day, NULL));

  const char* s_monotonic_nanosecond= "g_monotonic-nanosecond";
  const char* d_monotonic_nanosecond= "(g_monotonic-nanosecond): () => integer, returns the steady clock's monotonic "
                                      "nanoseconds since an unspecified epoch";
  s7_define (sc, cur_env, s7_make_symbol (sc, s_monotonic_nanosecond),
             s7_make_typed_function (sc, s_monotonic_nanosecond, f_monotonic_nanosecond, 0, 0, false,
                                     d_monotonic_nanosecond, NULL));

  s7_define_constant_with_environment (sc, cur_env, "g_system-clock-resolution",
                                       s7_make_integer (sc, clock_resolution_ns<std::chrono::system_clock> ()));
  s7_define_constant_with_environment (sc, cur_env, "g_steady-clock-resolution",
                                       s7_make_integer (sc, clock_resolution_ns<std::chrono::steady_clock> ()));
}

static s7_pointer
f_get_environment_variable (s7_scheme* sc, s7_pointer args) {
  // 类型检查先行：s7_error 是裸 longjmp，raise 时帧内不得有存活的 RAII 对象
  s7_pointer key_arg= s7_car (args);
  if (!s7_is_string (key_arg)) {
    return string_type_error (sc, "get-environment-variable: key must be a string", key_arg);
  }
#ifdef _MSC_VER
  std::string path_sep= ";";
#else
  std::string path_sep= ":";
#endif
  std::string          ret;
  tb_size_t            size       = 0;
  const char*          key        = s7_string (key_arg);
  tb_environment_ref_t environment= tb_environment_init ();
  if (environment) {
    size= tb_environment_load (environment, key);
    if (size >= 1) {
      tb_for_all_if (tb_char_t const*, value, environment, value) { ret.append (value).append (path_sep); }
    }
  }
  tb_environment_exit (environment);
  if (size == 0) { // env key not found
    return s7_make_boolean (sc, false);
  }
  else {
    return s7_make_string (sc, ret.substr (0, ret.size () - 1).c_str ());
  }
}

static s7_pointer
f_command_line (s7_scheme* sc, s7_pointer args) {
  s7_pointer ret = s7_nil (sc);
  int        size= command_args.size ();
  for (int i= size - 1; i >= 0; i--) {
    ret= s7_cons (sc, s7_make_string (sc, command_args[i].c_str ()), ret);
  }
  return ret;
}

static s7_pointer
f_getenvs (s7_scheme* sc, s7_pointer args) {
  s7_pointer p= s7_nil (sc);

#ifdef TB_CONFIG_OS_WINDOWS
  // Windows: use GetEnvironmentStrings
  LPCH env_strings= GetEnvironmentStrings ();
  if (env_strings) {
    LPCH env= env_strings;
    while (*env) {
      const char* eq= strchr (env, '=');
      if (eq && eq != env) { // skip empty variable names
        s7_pointer name = s7_make_string_with_length (sc, env, eq - env);
        s7_pointer value= s7_make_string (sc, eq + 1);
        p               = s7_cons (sc, s7_cons (sc, name, value), p);
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
      s7_pointer name = s7_make_string_with_length (sc, environ[i], eq - environ[i]);
      s7_pointer value= s7_make_string (sc, eq + 1);
      p               = s7_cons (sc, s7_cons (sc, name, value), p);
    }
  }
#endif

  return p;
}

inline void
glue_scheme_process_context (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);

  const char* s_get_environment_variable= "g_get-environment-variable";
  const char* d_get_environment_variable= "(g_get-environemt-variable string) => string";
  const char* s_command_line            = "g_command-line";
  const char* d_command_line            = "(g_command-line) => string";
  const char* s_getenvs                 = "g_getenvs";
  const char* d_getenvs                 = "(g_getenvs) => alist, returns all environment variables as an alist";

  s7_define (sc, cur_env, s7_make_symbol (sc, s_get_environment_variable),
             s7_make_typed_function (sc, s_get_environment_variable, f_get_environment_variable, 1, 0, false,
                                     d_get_environment_variable, NULL));
  s7_define (sc, cur_env, s7_make_symbol (sc, s_command_line),
             s7_make_typed_function (sc, s_command_line, f_command_line, 0, 0, false, d_command_line, NULL));
  s7_define (sc, cur_env, s7_make_symbol (sc, s_getenvs),
             s7_make_typed_function (sc, s_getenvs, f_getenvs, 0, 0, false, d_getenvs, NULL));
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

static s7_pointer
f_executable (s7_scheme* sc, s7_pointer args) {
  string exe_path= goldfish_exe ();
  return s7_make_string (sc, exe_path.c_str ());
}

inline void
glue_executable (s7_scheme* sc) {
  const char* name= "g_executable";
  const char* desc= "(g_executable) => string";
  glue_define (sc, name, desc, f_executable, 0, 0);
}

inline void
glue_liii_sys (s7_scheme* sc) {
  glue_executable (sc);
}

static s7_pointer
f_sleep (s7_scheme* sc, s7_pointer args) {
  s7_double seconds= s7_real (s7_car (args));

  tb_long_t total_ms= (tb_long_t) (seconds * 1000);
  if (signal_catch_active ()) {
    // 有被捕获的信号时才分片睡眠，以便及时派发
    tb_long_t elapsed= 0;
    while (elapsed < total_ms) {
      tb_long_t chunk= (total_ms - elapsed > 50) ? 50 : (total_ms - elapsed);
      tb_msleep (chunk);
      elapsed+= chunk;
      if (dispatch_pending_signals (sc)) {
        break;
      }
    }
  }
  else {
    tb_msleep (total_ms);
  }

  return s7_nil (sc);
}

inline void
glue_sleep (s7_scheme* sc) {
  const char* name= "g_sleep";
  const char* desc= "(g_sleep seconds) => nil, sleep for the specified number of seconds";
  glue_define (sc, name, desc, f_sleep, 1, 0);
}

static s7_pointer
f_datetime_now (s7_scheme* sc, s7_pointer args) {
  // Get current time using tbox for year, month, day, etc.
  tb_time_t now= tb_time ();

  // Get local time
  tb_tm_t lt= {0};
  if (!tb_localtime (now, &lt)) {
    return s7_f (sc);
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
  s7_pointer time_vec= s7_make_vector (sc, 7);

  // Fill the vector with values
  s7_vector_set (sc, time_vec, 0, s7_make_integer (sc, lt.year));   // year
  s7_vector_set (sc, time_vec, 1, s7_make_integer (sc, lt.month));  // month
  s7_vector_set (sc, time_vec, 2, s7_make_integer (sc, lt.mday));   // day
  s7_vector_set (sc, time_vec, 3, s7_make_integer (sc, lt.hour));   // hour
  s7_vector_set (sc, time_vec, 4, s7_make_integer (sc, lt.minute)); // minute
  s7_vector_set (sc, time_vec, 5, s7_make_integer (sc, lt.second)); // second
  s7_vector_set (sc, time_vec, 6, s7_make_integer (sc, micros));    // micro-second

  return time_vec;
}

inline void
glue_datetime_now (s7_scheme* sc) {
  const char* name= "g_datetime-now";
  const char* desc= "(g_datetime-now) => datetime, create a datetime object with current time";
  s7_define_function (sc, name, f_datetime_now, 0, 0, false, desc);
}

static s7_pointer
f_date_now (s7_scheme* sc, s7_pointer args) {
  // Get current time using tbox for year, month, day, etc.
  tb_time_t now= tb_time ();

  // Get local time
  tb_tm_t lt= {0};
  if (!tb_localtime (now, &lt)) {
    return s7_f (sc);
  }

  // Create a vector with the time components - vector is easier to index than list in Scheme
  s7_pointer time_vec= s7_make_vector (sc, 3);

  // Fill the vector with values
  s7_vector_set (sc, time_vec, 0, s7_make_integer (sc, lt.year));  // year
  s7_vector_set (sc, time_vec, 1, s7_make_integer (sc, lt.month)); // month
  s7_vector_set (sc, time_vec, 2, s7_make_integer (sc, lt.mday));  // day

  return time_vec;
}

inline void
glue_date_now (s7_scheme* sc) {
  const char* name= "g_date-now";
  const char* desc= "(g_date-now) => date, create a date object with current date";
  s7_define_function (sc, name, f_date_now, 0, 0, false, desc);
}

inline void
glue_liii_datetime (s7_scheme* sc) {
  glue_datetime_now (sc);
  glue_date_now (sc);
}

// -------------------------------- iota --------------------------------
#ifndef S7_INT64_MAX
#define S7_INT64_MAX 9223372036854775807LL
#endif
#ifndef S7_INT64_MIN
#define S7_INT64_MIN (int64_t) (-S7_INT64_MAX - 1LL)
#endif

static inline bool
safe_multiply (s7_int a, s7_int b, s7_int* res) {
#if defined(__GNUC__) || defined(__clang__)
  return __builtin_mul_overflow (a, b, res);
#else
  if (a == 0 || b == 0) {
    *res= 0;
    return false;
  }
  if (a > 0) {
    if (b > 0) {
      if (a > S7_INT64_MAX / b) return true;
    }
    else {
      if (b < S7_INT64_MIN / a) return true;
    }
  }
  else {
    if (b > 0) {
      if (a < S7_INT64_MIN / b) return true;
    }
    else {
      if (a == S7_INT64_MIN || b == S7_INT64_MIN) return true;
      if (-a > S7_INT64_MAX / (-b)) return true;
    }
  }
  *res= a * b;
  return false;
#endif
}

static inline bool
safe_add (s7_int a, s7_int b, s7_int* res) {
#if defined(__GNUC__) || defined(__clang__)
  return __builtin_add_overflow (a, b, res);
#else
  if (b > 0 && a > S7_INT64_MAX - b) return true;
  if (b < 0 && a < S7_INT64_MIN - b) return true;
  *res= a + b;
  return false;
#endif
}

static inline s7_pointer
iota_list (s7_scheme* sc, s7_int count, s7_int last_val, s7_int step) {
  s7_pointer res= s7_nil (sc);
  s7_int     val= last_val;
  while (true) {
    res= s7_cons (sc, s7_make_integer (sc, val), res);
    if (--count == 0) break;
    val-= step;
  }
  return res;
}

static s7_pointer
iota_list_p_ppp (s7_scheme* sc, s7_pointer count, s7_pointer start, s7_pointer step) {
  if (!s7_is_integer (count)) {
    return s7_error (sc, s7_make_symbol (sc, "type-error"),
                     s7_list (sc, 2, s7_make_string (sc, "iota: count must be an integer"), count));
  }
  if (!s7_is_integer (start)) {
    return s7_error (sc, s7_make_symbol (sc, "type-error"),
                     s7_list (sc, 2, s7_make_string (sc, "iota: start must be an integer"), start));
  }
  if (!s7_is_integer (step)) {
    return s7_error (sc, s7_make_symbol (sc, "type-error"),
                     s7_list (sc, 2, s7_make_string (sc, "iota: step must be an integer"), step));
  }
  s7_int cnt= s7_integer (count);
  if (cnt < 0) {
    return s7_error (sc, s7_make_symbol (sc, "value-error"),
                     s7_list (sc, 2, s7_make_string (sc, "iota: count is negative"), count));
  }
  if (cnt == 0) {
    return s7_nil (sc);
  }
  s7_int st = s7_integer (start);
  s7_int stp= s7_integer (step);
  s7_int mul_res;
  s7_int last_val;
  if (safe_multiply (stp, cnt - 1, &mul_res) || safe_add (st, mul_res, &last_val)) {
    return s7_error (sc, s7_make_symbol (sc, "value-error"),
                     s7_list (sc, 2, s7_make_string (sc, "iota: integer overflow"), count));
  }
  return iota_list (sc, cnt, last_val, stp);
}

static s7_pointer
g_iota_list (s7_scheme* sc, s7_pointer args) {
  s7_pointer arg1 = s7_car (args); // count
  s7_pointer rest1= s7_cdr (args);
  s7_pointer arg2 = (s7_is_pair (rest1)) ? s7_car (rest1) : s7_make_integer (sc, 0); // start value, default 0
  s7_pointer rest2= (s7_is_pair (rest1)) ? s7_cdr (rest1) : s7_nil (sc);
  s7_pointer arg3 = (s7_is_pair (rest2)) ? s7_car (rest2) : s7_make_integer (sc, 1); // step size, default 1
  return iota_list_p_ppp (sc, arg1, arg2, arg3);
}

inline void
glue_iota_list (s7_scheme* sc) {
  const char* name= "iota";
  const char* desc= "(iota count [start [step]]) => list, returns a list of count elements starting from start "
                    "(default 0) with step (default 1)";
  s7_define_function (sc, name, g_iota_list, 1, 2, false, desc);
}

inline void
glue_liii_list (s7_scheme* sc) {
  glue_iota_list (sc);
}

inline void
glue_for_community_edition (s7_scheme* sc) {
  glue_goldfish (sc);
  glue_scheme_time (sc);
  glue_scheme_process_context (sc);
  glue_liii_sys (sc);
  glue_liii_os (sc);
  glue_liii_fs_watch (sc);
  glue_subprocess (sc);
  glue_liii_signal (sc);
  glue_liii_path (sc);
  glue_liii_path_stat (sc);
  glue_liii_psutil (sc);
  glue_liii_sort (sc);
  glue_liii_list (sc);
  glue_liii_string (sc);
  glue_liii_string_cursor (sc);
  glue_sleep (sc);
  glue_liii_time (sc);
  glue_liii_datetime (sc);
  glue_liii_date_time (sc);
  glue_liii_uuid (sc);
  glue_liii_hashlib (sc);
  glue_liii_base64 (sc);
  glue_liii_json (sc);
  glue_liii_go (sc);
  glue_liii_tempfile (sc);
  glue_liii_shutil (sc);
  glue_scheme_base (sc);
  glue_scheme_char (sc);
  glue_r7rs_library (sc);
  glue_liii_record (sc);
  glue_syntax_rules (sc);
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
  cout << "  check [options] [PATH...] Static code metrics check (line count and tree depth limits)" << endl;
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
  cout << "  FILE               Load and evaluate Scheme code from FILE" << endl;
  cout << endl;
  cout << "Options:" << endl;
  cout << "  --mode, -m MODE    Set mode: default, liii, sicp, r7rs, s7" << endl;
  cout << "  -I DIR             Prepend DIR to library search path" << endl;
  cout << "  -A DIR             Append DIR to library search path" << endl;
  cout << "  -e CODE            Alias for eval CODE" << endl;
  cout << endl;
  cout << "If no command is specified, gf agent is run by default." << endl;
}

static void
display_version () {
  cout << "Goldfish Scheme " << GOLDFISH_VERSION << " by LiiiLabs" << endl;
  cout << "based on S7 Scheme " << S7_VERSION << " (" << S7_DATE << ")" << endl;
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
goldfish_eval_file (s7_scheme* sc, string path, bool quiet) {
  s7_pointer result= s7_load (sc, path.c_str ());
  if (!result) {
    cerr << "Failed to load " << path << endl;
    exit (-1);
  }
  if (!quiet) {
    cout << path << " => " << s7_object_to_c_string (sc, result) << endl;
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
  size_t marker= errmsg.find (".scm[");
  while (marker != string::npos) {
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

    marker= errmsg.find (".scm[", marker + 1);
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
goldfish_is_symbol_boundary_char (char ch) {
  return std::isspace (static_cast<unsigned char> (ch)) || ch == '(' || ch == ')' || ch == '[' || ch == ']' ||
         ch == '"' || ch == ';' || ch == '\'' || ch == '`' || ch == ',';
}

// 纯文本判断 name 是否出现在调用位置（前面（跳过空白后）是左括号）。
// 提示路径不允许调用 s7_read：错误处理路径里再触发 reader 错误会让 s7 在
// 构造 stacktrace 时段错误。
static bool
goldfish_error_expression_contains_function_call (const string& expression, const string& function_name) {
  if (expression.empty () || function_name.empty ()) {
    return false;
  }

  size_t pos= 0;
  while ((pos= expression.find (function_name, pos)) != string::npos) {
    size_t end= pos + function_name.size ();

    bool starts_ok= (pos == 0) || goldfish_is_symbol_boundary_char (expression[pos - 1]);
    bool ends_ok  = (end == expression.size ()) || goldfish_is_symbol_boundary_char (expression[end]);
    if (starts_ok && ends_ok) {
      size_t before= pos;
      while ((before > 0) && (std::isspace (static_cast<unsigned char> (expression[before - 1])))) {
        --before;
      }
      if ((before > 0) && (expression[before - 1] == '(')) {
        return true;
      }
    }

    pos= end;
  }

  return false;
}

static string
goldfish_extract_unbound_function_name_from_error (const string& errmsg) {
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
  if (!goldfish_error_expression_contains_function_call (error_expression, function_name)) {
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

static string
goldfish_library_display_name (const string& library_query) {
  vector<string> parts= split_library_query_parts (library_query);
  if (parts.size () < 2) {
    return library_query;
  }
  string result= "(";
  for (size_t i= 0; i < parts.size (); ++i) {
    if (i > 0) result+= " ";
    result+= parts[i];
  }
  result+= ")";
  return result;
}

static string
goldfish_library_import_form (const string& library_query) {
  vector<string> parts= split_library_query_parts (library_query);
  if (parts.size () < 2) {
    return "";
  }
  return "(import " + goldfish_library_display_name (library_query) + ")";
}

static string
goldfish_library_doc_command (const string& library_query, const string& function_name) {
  return goldfish_cli_program_name () + " doc " + library_query + " " + goldfish_shell_double_quote (function_name);
}

static string
goldfish_append_doc_hint_if_needed (s7_scheme* sc, const string& errmsg) {
  if (errmsg.find ("Hint: try `") != string::npos) {
    return errmsg;
  }

  string function_name= goldfish_extract_unbound_function_name_from_error (errmsg);
  if (function_name.empty ()) {
    return errmsg;
  }

  string formatted= errmsg;
  if ((!formatted.empty ()) && (formatted.back () != '\n')) {
    formatted+= '\n';
  }

  vector<string> library_queries;
  try {
    library_queries= find_function_libraries_in_load_path (sc, function_name);
  } catch (const std::exception&) {
    library_queries.clear ();
  }

  if (library_queries.empty ()) {
    formatted+=
        "Hint: try `" + goldfish_cli_program_name () + " doc " + goldfish_shell_double_quote (function_name) + "`\n";
    formatted+=
        "`" + goldfish_cli_program_name () + " doc` may show similarly named functions when there is no exact match.\n";
    formatted+= "If it finds nothing similar, try searching the codebase with `git grep " +
                goldfish_shell_double_quote (function_name) +
                "`, implement that function yourself, or stop using it.\n";
    return formatted;
  }

  if (library_queries.size () == 1) {
    string import_form= goldfish_library_import_form (library_queries.front ());
    formatted+= "Hint: function `" + function_name + "` exists in library `" +
                goldfish_library_display_name (library_queries.front ()) + "`.\n";
    if (!import_form.empty ()) {
      formatted+= "Please import that library first: `" + import_form + "`.\n";
    }
    return formatted;
  }

  formatted+= "Hint: function `" + function_name + "` exists in multiple visible libraries:\n";
  for (const auto& library_query : library_queries) {
    formatted+= "  " + goldfish_library_display_name (library_query) + "\n";
  }
  formatted+= "Try one of these commands to decide which library to use:\n";
  for (const auto& library_query : library_queries) {
    formatted+= "  " + goldfish_library_doc_command (library_query, function_name) + "\n";
  }
  return formatted;
}

static void
goldfish_render_scheme_error_message (s7_scheme* sc, const char* errmsg, string& rendered) {
  rendered= goldfish_append_doc_hint_if_needed (sc, goldfish_format_scheme_error_message (errmsg));
  if ((!rendered.empty ()) && (rendered.back () != '\n')) {
    rendered+= '\n';
  }
}

static void
goldfish_print_scheme_error_message (s7_scheme* sc, const char* errmsg) {
  if ((errmsg) && (*errmsg)) {
    string rendered;
    goldfish_render_scheme_error_message (sc, errmsg, rendered);
    cout << rendered;
  }
}

static void
goldfish_eval_code (s7_scheme* sc, string code) {
  string     wrapped_code= "(begin " + code + " )";
  s7_pointer x           = s7_eval_c_string (sc, wrapped_code.c_str ());
  cout << s7_object_to_c_string (sc, x) << endl;
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

// Tool registration system - dynamically load tools from gfproject.json

static fs::path
find_local_gfproject_json () {
  std::error_code ec;
  fs::path        cwd= fs::current_path (ec);
  if (!ec) {
    fs::path local_path= cwd / "gfproject.json";
    if (fs::exists (local_path, ec) && fs::is_regular_file (local_path, ec)) {
      return local_path;
    }
  }
  return "";
}

static s7_pointer
f_goldfish_library_dir (s7_scheme* sc, s7_pointer args) {
  (void) args;
  string gf_lib_dir= find_goldfish_library ();
  return s7_make_string (sc, gf_lib_dir.c_str ());
}

static s7_pointer
f_project_root (s7_scheme* sc, s7_pointer args) {
  (void) args;
  fs::path local= find_local_gfproject_json ();
  if (local.empty ()) {
    return s7_f (sc);
  }
  // local points at gfproject.json; its parent is the project root.
  std::string s= local.parent_path ().generic_string ();
  if (s.empty ()) {
    return s7_f (sc);
  }
  return s7_make_string (sc, s.c_str ());
}

static int
goldfish_run_tool (s7_scheme* sc, const char* gf_lib, const string& command, const char*& errmsg, s7_pointer old_port,
                   int gc_loc) {
  s7_eval_c_string (sc, "(import (liii gfproject tools))");
  s7_pointer func= s7_name_to_value (sc, "gfproject-run-tool");
  if (!func || !s7_is_procedure (func)) {
    return -1;
  }
  s7_pointer result=
      s7_call (sc, func, s7_list (sc, 2, s7_make_string (sc, command.c_str ()), s7_make_string (sc, gf_lib)));
  if (result == s7_f (sc)) {
    s7_close_output_port (sc, s7_current_error_port (sc));
    s7_set_current_error_port (sc, s7_open_output_string (sc));
    return -1;
  }
  if (s7_is_integer (result)) {
    errmsg= s7_get_output_string (sc, s7_current_error_port (sc));
    goldfish_print_scheme_error_message (sc, errmsg);
    s7_close_output_port (sc, s7_current_error_port (sc));
    s7_set_current_error_port (sc, old_port);
    if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);
    return static_cast<int> (s7_integer (result));
  }
  return -1;
}

static string
current_scheme_error_output (s7_scheme* sc) {
  const char* errmsg= s7_get_output_string (sc, s7_current_error_port (sc));
  if ((errmsg) && (*errmsg)) {
    return string (errmsg);
  }
  return "";
}

static string
read_text_file_exact (const fs::path& path) {
  std::ifstream input (path, std::ios::binary);
  if (!input.is_open ()) {
    throw std::runtime_error ("Failed to open file for reading: " + path.string ());
  }

  std::ostringstream buffer;
  buffer << input.rdbuf ();
  if (input.bad ()) {
    throw std::runtime_error ("Failed to read file: " + path.string ());
  }

  return buffer.str ();
}

s7_scheme*
init_goldfish_scheme (const char* gf_lib) {
  set_goldfish_lib_dir (gf_lib);
  s7_scheme* sc= s7_init ();
  s7_add_to_load_path (sc, gf_lib);

  if (!tb_init (tb_null, tb_null)) exit (-1);

  glue_for_community_edition (sc);
  return sc;
}

void
customize_goldfish_by_mode (s7_scheme* sc, string mode, const char* boot_file_path) {
  if (mode != "s7") {
    s7_load (sc, boot_file_path);
  }

  if (mode == "default" || mode == "liii") {
    s7_eval_c_string (sc, "(import (scheme base) (liii base) (liii error) (liii string))");
  }
  else if (mode == "scheme") {
    s7_eval_c_string (sc, "(import (liii base) (liii error))");
  }
  else if (mode == "sicp") {
    s7_eval_c_string (sc, "(import (scheme base) (srfi sicp))");
  }
  else if (mode == "r7rs") {
    s7_eval_c_string (sc, "(import (scheme base))");
  }
  else if (mode == "s7") {
  }
  else {
    cerr << "No such mode: " << mode << endl;
    exit (-1);
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
  tb_char_t const* gf_boot= tb_path_absolute_to (gf_lib, "scheme/boot.scm", data_boot, sizeof (data_boot));

  if (!tb_file_access (gf_boot, TB_FILE_MODE_RO)) {
    cerr << "The boot.scm for Goldfish Scheme does not exist" << endl;
    exit (-1);
  }
  return string (gf_boot);
}

struct StartupCliOptions {
  string         mode= "default";
  vector<string> prepend_dirs;
  vector<string> append_dirs;
  string         command;
  int            command_index= -1;
  string         error;
};

static std::string
parse_mode_option (int argc, char** argv, int command_index, const std::string& default_mode= "default") {
  std::string mode = default_mode;
  int         limit= (command_index >= 0) ? command_index : argc;
  for (int i= 1; i < limit; ++i) {
    string arg= argv[i];
    if ((arg == "--mode" || arg == "-m") && (i + 1) < limit) {
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
current_load_path_entries (s7_scheme* sc) {
  vector<string> entries;
  for (s7_pointer rest= s7_load_path (sc); s7_is_pair (rest); rest= s7_cdr (rest)) {
    s7_pointer entry= s7_car (rest);
    if (s7_is_string (entry)) {
      append_unique_string (entries, string (s7_string (entry)));
    }
  }
  return entries;
}

static void
set_load_path_entries (s7_scheme* sc, const vector<string>& entries) {
  s7_pointer list= s7_nil (sc);
  for (auto it= entries.rbegin (); it != entries.rend (); ++it) {
    list= s7_cons (sc, s7_make_string (sc, it->c_str ()), list);
  }
  s7_symbol_set_value (sc, s7_make_symbol (sc, "*load-path*"), list);
}

static void
prepend_load_path_entries (s7_scheme* sc, const vector<string>& prepend_dirs) {
  vector<string> seen= current_load_path_entries (sc);
  for (auto it= prepend_dirs.rbegin (); it != prepend_dirs.rend (); ++it) {
    string dir= normalize_load_path_dir (*it);
    if (dir.empty ()) continue;
    if (std::find (seen.begin (), seen.end (), dir) != seen.end ()) continue;
    s7_add_to_load_path (sc, dir.c_str ());
    seen.insert (seen.begin (), dir);
  }
}

static void
append_load_path_entries (s7_scheme* sc, const vector<string>& append_dirs) {
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

static bool
append_unique_exact_string (vector<string>& items, const string& value) {
  if (value.empty ()) return false;
  if (std::find (items.begin (), items.end (), value) != items.end ()) return false;
  items.push_back (value);
  return true;
}

static bool
string_is_decimal_integer (const string& value) {
  if (value.empty ()) return false;

  size_t index= 0;
  if (value[0] == '+' || value[0] == '-') {
    index= 1;
  }
  if (index >= value.size ()) return false;

  return std::all_of (value.begin () + static_cast<std::ptrdiff_t> (index), value.end (),
                      [] (unsigned char ch) { return std::isdigit (ch) != 0; });
}

static s7_pointer
make_library_name_part (s7_scheme* sc, const string& part) {
  if (string_is_decimal_integer (part)) {
    try {
      return s7_make_integer (sc, std::stoll (part));
    } catch (const std::exception&) {
    }
  }
  return s7_make_symbol (sc, part.c_str ());
}

static vector<string>
split_library_query_parts (const string& query) {
  vector<string> parts;
  if (query.empty ()) return parts;
  size_t start= 0;
  while (start < query.size ()) {
    size_t slash_pos= query.find ('/', start);
    if (slash_pos == string::npos) {
      string part= query.substr (start);
      if (part.empty ()) return {};
      parts.push_back (part);
      break;
    }
    if (slash_pos == start) return {};
    parts.push_back (query.substr (start, slash_pos - start));
    start= slash_pos + 1;
    if (start == query.size ()) return {};
  }
  if (parts.size () < 2) return {};
  return parts;
}

static bool
split_library_query (const string& query, string& group, string& library) {
  vector<string> parts= split_library_query_parts (query);
  if (parts.size () < 2) {
    return false;
  }
  group  = parts[0];
  library= query.substr (parts[0].size () + 1);
  return true;
}

// 提示（hint）扫描器：只做文本层面的 s-expression 扫描，不使用 s7 reader。
// 原因：在错误处理路径里调用 s7_read，一旦被扫描文件里出现当前 reader 不认识的
// 语法（例如未导入 (liii raw-string) 时的 #"" 原始字符串），就会在错误处理中再次
// 抛错，s7 在构造 stacktrace 时段错误。
enum class SchemeSourceTokenKind { Atom, Open, Close, Quote, DatumComment };

struct SchemeSourceToken {
  SchemeSourceTokenKind kind;
  string                text;
};

struct SchemeSourceNode {
  bool                     is_list= false;
  string                   atom;
  vector<SchemeSourceNode> items;
};

static bool
scheme_source_delimiter (char ch) {
  return std::isspace (static_cast<unsigned char> (ch)) || ch == '(' || ch == ')' || ch == '[' || ch == ']' ||
         ch == '"' || ch == ';' || ch == '\'' || ch == '`' || ch == ',';
}

static bool
skip_scheme_string_literal (const string& text, size_t& pos) {
  // pos 指向开引号
  ++pos;
  while (pos < text.size ()) {
    if (text[pos] == '\\') {
      pos+= 2;
      continue;
    }
    if (text[pos] == '"') {
      ++pos;
      return true;
    }
    ++pos;
  }
  return false;
}

static bool
skip_scheme_block_comment (const string& text, size_t& pos) {
  // pos 指向 "#|"
  pos+= 2;
  int depth= 1;
  while (pos < text.size ()) {
    if ((text[pos] == '#') && (pos + 1 < text.size ()) && (text[pos + 1] == '|')) {
      ++depth;
      pos+= 2;
      continue;
    }
    if ((text[pos] == '|') && (pos + 1 < text.size ()) && (text[pos + 1] == '#')) {
      --depth;
      pos+= 2;
      if (depth == 0) {
        return true;
      }
      continue;
    }
    ++pos;
  }
  return false;
}

static string
read_scheme_character_literal (const string& text, size_t& pos) {
  // pos 指向 "#\\"
  size_t start= pos;
  pos+= 2;
  if ((pos < text.size ()) && (scheme_source_delimiter (text[pos]))) {
    ++pos;
  }
  else {
    while ((pos < text.size ()) && (!scheme_source_delimiter (text[pos]))) {
      ++pos;
    }
  }
  return text.substr (start, pos - start);
}

static bool
tokenize_scheme_source (const string& text, vector<SchemeSourceToken>& tokens) {
  size_t pos= 0;

  while (pos < text.size ()) {
    char ch= text[pos];

    if (std::isspace (static_cast<unsigned char> (ch))) {
      ++pos;
      continue;
    }

    if (ch == ';') {
      while ((pos < text.size ()) && (text[pos] != '\n')) {
        ++pos;
      }
      continue;
    }

    if (ch == '#') {
      if ((pos + 1 < text.size ()) && (text[pos + 1] == '|')) {
        if (!skip_scheme_block_comment (text, pos)) {
          return false;
        }
        continue;
      }

      if ((pos + 1 < text.size ()) && (text[pos + 1] == ';')) {
        pos+= 2;
        tokens.push_back ({SchemeSourceTokenKind::DatumComment, ""});
        continue;
      }

      if ((pos + 1 < text.size ()) && (text[pos + 1] == '!')) {
        // #!fold-case / #!no-fold-case / shebang 行
        while ((pos < text.size ()) && (text[pos] != '\n')) {
          ++pos;
        }
        continue;
      }

      if ((pos + 2 < text.size ()) && (text[pos + 1] == '"')) {
        // 原始字符串：# 后跟 N 个引号（SRFI-267 / (liii raw-string)），
        // 以同样数量的引号结束；例如 #""..."" 或 #"""..."""。
        size_t quote_count= 0;
        while ((pos + 1 + quote_count < text.size ()) && (text[pos + 1 + quote_count] == '"')) {
          ++quote_count;
        }
        if (quote_count >= 2) {
          string delimiter (quote_count, '"');
          size_t body_start= pos + 1 + quote_count;
          size_t end       = text.find (delimiter, body_start);
          if (end == string::npos) {
            return false;
          }
          pos= end + quote_count;
          continue;
        }
      }

      if ((pos + 1 < text.size ()) && (text[pos + 1] == '\\')) {
        tokens.push_back ({SchemeSourceTokenKind::Atom, read_scheme_character_literal (text, pos)});
        continue;
      }

      if ((pos + 1 < text.size ()) && ((text[pos + 1] == '(') || (text[pos + 1] == '['))) {
        tokens.push_back ({SchemeSourceTokenKind::Open, ""});
        pos+= 2;
        continue;
      }

      // #t / #f / #x1f / #u8 等 dispatch 常量走通用 atom 分支
    }

    if ((ch == '(') || (ch == '[')) {
      tokens.push_back ({SchemeSourceTokenKind::Open, ""});
      ++pos;
      continue;
    }

    if ((ch == ')') || (ch == ']')) {
      tokens.push_back ({SchemeSourceTokenKind::Close, ""});
      ++pos;
      continue;
    }

    if (ch == '"') {
      size_t start= pos;
      if (!skip_scheme_string_literal (text, pos)) {
        return false;
      }
      tokens.push_back ({SchemeSourceTokenKind::Atom, text.substr (start, pos - start)});
      continue;
    }

    if (ch == '|') {
      size_t start= pos;
      ++pos;
      while (pos < text.size ()) {
        if (text[pos] == '\\') {
          pos+= 2;
          continue;
        }
        if (text[pos] == '|') {
          ++pos;
          break;
        }
        ++pos;
      }
      tokens.push_back ({SchemeSourceTokenKind::Atom, text.substr (start, pos - start)});
      continue;
    }

    if ((ch == '\'') || (ch == '`') || (ch == ',')) {
      string prefix (1, ch);
      ++pos;
      if ((ch == ',') && (pos < text.size ()) && (text[pos] == '@')) {
        ++pos;
        prefix= ",@";
      }
      tokens.push_back ({SchemeSourceTokenKind::Quote, prefix});
      continue;
    }

    size_t start= pos;
    while ((pos < text.size ()) && (!scheme_source_delimiter (text[pos]))) {
      ++pos;
    }
    if (pos == start) {
      ++pos;
      continue;
    }
    tokens.push_back ({SchemeSourceTokenKind::Atom, text.substr (start, pos - start)});
  }

  return true;
}

static bool
parse_scheme_source_node (const vector<SchemeSourceToken>& tokens, size_t& pos, SchemeSourceNode& node, int depth) {
  constexpr int kMaxDepth= 200;

  if (depth > kMaxDepth) {
    return false;
  }
  if (pos >= tokens.size ()) {
    return false;
  }

  const SchemeSourceToken& token= tokens[pos];
  if (token.kind == SchemeSourceTokenKind::Close) {
    return false;
  }
  if (token.kind == SchemeSourceTokenKind::Atom) {
    node.is_list= false;
    node.atom   = token.text;
    ++pos;
    return true;
  }
  if (token.kind == SchemeSourceTokenKind::DatumComment) {
    ++pos;
    SchemeSourceNode skipped;
    return parse_scheme_source_node (tokens, pos, skipped, depth + 1);
  }
  if (token.kind == SchemeSourceTokenKind::Quote) {
    // 引用形式（'x / `x / ,x）包成一个列表，避免把被引用的
    // (define-library ...) 数据误当成真正的库定义。
    ++pos;
    SchemeSourceNode quoted;
    if (!parse_scheme_source_node (tokens, pos, quoted, depth + 1)) {
      return false;
    }
    node.is_list= true;
    node.items.push_back ({false, "quote", {}});
    node.items.push_back (std::move (quoted));
    return true;
  }

  node.is_list= true;
  ++pos;
  while ((pos < tokens.size ()) && (tokens[pos].kind != SchemeSourceTokenKind::Close)) {
    if (tokens[pos].kind == SchemeSourceTokenKind::DatumComment) {
      ++pos;
      SchemeSourceNode skipped;
      if (!parse_scheme_source_node (tokens, pos, skipped, depth + 1)) {
        return false;
      }
      continue;
    }
    SchemeSourceNode child;
    if (!parse_scheme_source_node (tokens, pos, child, depth + 1)) {
      return false;
    }
    node.items.push_back (std::move (child));
  }
  if (pos >= tokens.size ()) {
    return false;
  }
  ++pos;
  return true;
}

static bool
scheme_source_node_is_named_atom (const SchemeSourceNode& node, const char* name) {
  return (!node.is_list) && (node.atom == name);
}

static bool
scheme_source_node_as_library_part (const SchemeSourceNode& node, string& out) {
  if (node.is_list || node.atom.empty ()) {
    return false;
  }
  // 字符串常量、字符常量不是库名的一部分
  if ((node.atom[0] == '"') || (node.atom[0] == '#')) {
    return false;
  }
  out= node.atom;
  return true;
}

static bool
extract_library_name_from_node (const SchemeSourceNode& library_name, string& group, string& library) {
  if ((!library_name.is_list) || (library_name.items.size () < 2)) {
    return false;
  }

  string first_part;
  if (!scheme_source_node_as_library_part (library_name.items[0], first_part)) {
    return false;
  }
  group= first_part;

  string remaining;
  for (size_t i= 1; i < library_name.items.size (); ++i) {
    string part;
    if (!scheme_source_node_as_library_part (library_name.items[i], part)) {
      return false;
    }
    if (!remaining.empty ()) {
      remaining+= "/";
    }
    remaining+= part;
  }
  library= remaining;
  return true;
}

static bool
export_spec_node_matches (const SchemeSourceNode& export_spec, const string& function_name) {
  if (!export_spec.is_list) {
    return export_spec.atom == function_name;
  }

  if ((export_spec.items.size () == 3) && scheme_source_node_is_named_atom (export_spec.items[0], "rename")) {
    const SchemeSourceNode& renamed_export= export_spec.items[2];
    return (!renamed_export.is_list) && (renamed_export.atom == function_name);
  }

  return false;
}

static bool
define_library_node_exports_function (const SchemeSourceNode& form, const string& function_name, string& group,
                                      string& library) {
  if ((!form.is_list) || (form.items.size () < 3)) {
    return false;
  }
  if (!scheme_source_node_is_named_atom (form.items[0], "define-library")) {
    return false;
  }

  string form_group;
  string form_library;
  if (!extract_library_name_from_node (form.items[1], form_group, form_library)) {
    return false;
  }

  for (size_t i= 2; i < form.items.size (); ++i) {
    const SchemeSourceNode& declaration= form.items[i];
    if ((!declaration.is_list) || declaration.items.empty ()) {
      continue;
    }
    if (!scheme_source_node_is_named_atom (declaration.items[0], "export")) {
      continue;
    }

    for (size_t j= 1; j < declaration.items.size (); ++j) {
      if (export_spec_node_matches (declaration.items[j], function_name)) {
        group  = form_group;
        library= form_library;
        return true;
      }
    }
  }

  return false;
}

static bool
source_file_exports_function (const fs::path& source_file, const string& function_name, string& group,
                              string& library) {
  string source_text= read_text_file_exact (source_file);

  // 快速跳过绝大多数与本次查找无关的源码文件
  if (source_text.find ("define-library") == string::npos) {
    return false;
  }
  if (source_text.find (function_name) == string::npos) {
    return false;
  }

  vector<SchemeSourceToken> tokens;
  if (!tokenize_scheme_source (source_text, tokens)) {
    return false;
  }

  size_t pos= 0;
  while (pos < tokens.size ()) {
    if (tokens[pos].kind == SchemeSourceTokenKind::DatumComment) {
      ++pos;
      SchemeSourceNode skipped;
      if (!parse_scheme_source_node (tokens, pos, skipped, 0)) {
        return false;
      }
      continue;
    }
    if (tokens[pos].kind == SchemeSourceTokenKind::Close) {
      return false;
    }

    SchemeSourceNode form;
    if (!parse_scheme_source_node (tokens, pos, form, 0)) {
      return false;
    }
    if (define_library_node_exports_function (form, function_name, group, library)) {
      return true;
    }
  }

  return false;
}

static s7_pointer
make_library_name_list (s7_scheme* sc, const vector<string>& parts) {
  s7_pointer result= s7_nil (sc);
  for (auto it= parts.rbegin (); it != parts.rend (); ++it) {
    result= s7_cons (sc, make_library_name_part (sc, *it), result);
  }
  return result;
}

static s7_pointer
make_library_name_list (s7_scheme* sc, const string& group, const string& library) {
  vector<string> parts= split_library_query_parts (group + "/" + library);
  if (parts.empty ()) {
    parts= {group, library};
  }
  return make_library_name_list (sc, parts);
}

static vector<fs::path>
sorted_child_directories (const fs::path& root) {
  vector<fs::path> directories;
  std::error_code  ec;

  for (fs::directory_iterator it (root, fs::directory_options::skip_permission_denied, ec), end; it != end;
       it.increment (ec)) {
    if (ec) {
      ec.clear ();
      continue;
    }
    if (it->is_directory (ec)) {
      directories.push_back (it->path ());
    }
    ec.clear ();
  }

  std::sort (directories.begin (), directories.end (),
             [] (const fs::path& lhs, const fs::path& rhs) { return lhs.string () < rhs.string (); });
  return directories;
}

static vector<fs::path>
sorted_scheme_source_files (const fs::path& dir) {
  vector<fs::path> files;
  std::error_code  ec;

  for (fs::directory_iterator it (dir, fs::directory_options::skip_permission_denied, ec), end; it != end;
       it.increment (ec)) {
    if (ec) {
      ec.clear ();
      continue;
    }
    if (it->is_regular_file (ec) && (it->path ().extension () == ".scm")) {
      files.push_back (it->path ());
    }
    ec.clear ();
  }

  std::sort (files.begin (), files.end (),
             [] (const fs::path& lhs, const fs::path& rhs) { return lhs.string () < rhs.string (); });
  return files;
}

static vector<fs::path>
sorted_recursive_scheme_source_files (const fs::path& dir) {
  vector<fs::path> files;
  std::error_code  ec;

  for (fs::recursive_directory_iterator it (dir, fs::directory_options::skip_permission_denied, ec), end; it != end;
       it.increment (ec)) {
    if (ec) {
      ec.clear ();
      continue;
    }
    if (it->is_regular_file (ec) && (it->path ().extension () == ".scm")) {
      files.push_back (it->path ());
    }
    ec.clear ();
  }

  std::sort (files.begin (), files.end (),
             [] (const fs::path& lhs, const fs::path& rhs) { return lhs.string () < rhs.string (); });
  return files;
}

static vector<string>
find_function_libraries_in_load_path (s7_scheme* sc, const string& function_name) {
  vector<string>  library_queries;
  std::error_code ec;

  for (const auto& load_root_string : current_load_path_entries (sc)) {
    fs::path load_root (load_root_string);
    if ((!fs::exists (load_root, ec)) || (!fs::is_directory (load_root, ec))) {
      ec.clear ();
      continue;
    }
    ec.clear ();

    for (const auto& group_dir : sorted_child_directories (load_root)) {
      for (const auto& source_file : sorted_recursive_scheme_source_files (group_dir)) {
        string group;
        string library;
        if (source_file_exports_function (source_file, function_name, group, library)) {
          append_unique_exact_string (library_queries, group + "/" + library);
        }
      }
    }
  }

  std::sort (library_queries.begin (), library_queries.end ());
  return library_queries;
}

static s7_pointer
make_library_name_list_list (s7_scheme* sc, const vector<string>& library_queries) {
  s7_pointer result= s7_nil (sc);
  for (auto it= library_queries.rbegin (); it != library_queries.rend (); ++it) {
    vector<string> parts= split_library_query_parts (*it);
    if (parts.size () < 2) continue;
    result= s7_cons (sc, make_library_name_list (sc, parts), result);
  }
  return result;
}

// try/catch 收进独立函数：s7_error 是裸 longjmp，MSVC 下从"函数体内含 try/catch
// （带 SEH 展开信息）的帧" raise 会损坏 EH 状态；f_function_libraries 本体不得含 EH 内容
static bool
find_libraries_safe (s7_scheme* sc, const string& function_name, vector<string>& out, std::string& err_msg) {
  try {
    out= find_function_libraries_in_load_path (sc, function_name);
    return true;
  } catch (const std::exception& ex) {
    err_msg= string ("g_function-libraries: failed to inspect libraries: ") + ex.what ();
    return false;
  }
}

// raise 同样放进无 EH、无 RAII 的独立函数
static s7_pointer
raise_function_libraries_error (s7_scheme* sc, const char* msg, s7_pointer arg) {
  return s7_error (sc, s7_make_symbol (sc, "read-error"), s7_list (sc, 2, s7_make_string (sc, msg), arg));
}

static s7_pointer
f_function_libraries (s7_scheme* sc, s7_pointer args) {
  s7_pointer function_name_arg= s7_car (args);
  if (!s7_is_string (function_name_arg)) {
    return s7_error (
        sc, s7_make_symbol (sc, "type-error"),
        s7_list (sc, 2, s7_make_string (sc, "g_function-libraries: function-name must be string?"), function_name_arg));
  }

  // 帧内只有平凡局部变量：RAII 与 EH 帧都在内层作用域/辅助函数内
  static thread_local std::string err_msg;
  bool                            has_err= false;
  s7_pointer                      result = s7_nil (sc);
  {
    string         function_name= s7_string (function_name_arg);
    vector<string> visible_library_queries;
    has_err= !find_libraries_safe (sc, function_name, visible_library_queries, err_msg);
    if (!has_err) {
      result= make_library_name_list_list (sc, visible_library_queries);
    }
  }
  if (has_err) {
    return raise_function_libraries_error (sc, err_msg.c_str (), function_name_arg);
  }
  return result;
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
apply_startup_load_path_options (s7_scheme* sc, const StartupCliOptions& opts) {
  vector<string> prepend_dirs= opts.prepend_dirs;
  for (const auto& dir : discover_auto_goldfish_library_dirs ()) {
    append_unique_string (prepend_dirs, dir);
  }
  prepend_load_path_entries (sc, prepend_dirs);
  append_load_path_entries (sc, opts.append_dirs);

  fs::path local= find_local_gfproject_json ();
  if (!local.empty ()) {
    append_load_path_entries (sc, {local.parent_path ().generic_string ()});
  }
}

int
repl_for_community_edition (s7_scheme* sc, int argc, char** argv) {
  string      gf_lib_dir  = find_goldfish_library ();
  const char* gf_lib      = gf_lib_dir.c_str ();
  string      gf_boot_path= find_goldfish_boot (gf_lib);
  const char* gf_boot     = gf_boot_path.c_str ();

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

  // 如果没有找到命令或没有参数，使用 pi 命令
  if (argc <= 1 || command.empty ()) {
    command= "pi";
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
  string mode= parse_mode_option (argc, argv, command_index, default_mode);

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
  customize_goldfish_by_mode (sc, mode, gf_boot);

  // start capture error output
  const char* errmsg  = NULL;
  s7_pointer  old_port= s7_set_current_error_port (sc, s7_open_output_string (sc));
  int         gc_loc  = -1;
  if (old_port != s7_nil (sc)) gc_loc= s7_gc_protect (sc, old_port);

  // 处理动态注册的工具（从 gfproject.json 加载）
  int tool_ret= goldfish_run_tool (sc, gf_lib, command, errmsg, old_port, gc_loc);
  if (tool_ret != -1) {
    // Tool was found and executed (or failed with an error)
    return tool_ret;
  }

  if (command == "help") {
    s7_close_output_port (sc, s7_current_error_port (sc));
    s7_set_current_error_port (sc, old_port);
    if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);
    display_help ();
    return 0;
  }

  if (command == "version") {
    s7_close_output_port (sc, s7_current_error_port (sc));
    s7_set_current_error_port (sc, old_port);
    if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);
    display_version ();
    return 0;
  }

  // 处理 eval 子命令
  if (command == "eval") {
    if (argc < command_index + 1) {
      std::cerr << "Error: 'eval' requires CODE argument.\n" << std::endl;
      s7_close_output_port (sc, s7_current_error_port (sc));
      s7_set_current_error_port (sc, old_port);
      if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);
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
      s7_close_output_port (sc, s7_current_error_port (sc));
      s7_set_current_error_port (sc, old_port);
      if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);
      exit (1);
    }
    goldfish_eval_code (sc, code);
    errmsg= s7_get_output_string (sc, s7_current_error_port (sc));
    goldfish_print_scheme_error_message (sc, errmsg);
    s7_close_output_port (sc, s7_current_error_port (sc));
    s7_set_current_error_port (sc, old_port);
    if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);
    if ((errmsg) && (*errmsg)) return -1;
    return 0;
  }

  // 处理 load 子命令（加载文件后进入 REPL）
  if (command == "load") {
    if (argc < command_index + 1) {
      std::cerr << "Error: 'load' requires FILE argument.\n" << std::endl;
      s7_close_output_port (sc, s7_current_error_port (sc));
      s7_set_current_error_port (sc, old_port);
      if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);
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
      s7_close_output_port (sc, s7_current_error_port (sc));
      s7_set_current_error_port (sc, old_port);
      if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);
      exit (1);
    }
    // 加载文件
    goldfish_eval_file (sc, file, true);
    errmsg= s7_get_output_string (sc, s7_current_error_port (sc));
    if ((errmsg) && (*errmsg)) {
      goldfish_print_scheme_error_message (sc, errmsg);
      s7_close_output_port (sc, s7_current_error_port (sc));
      s7_set_current_error_port (sc, old_port);
      if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);
      return -1;
    }
    // 加载成功后进入 REPL
    s7_close_output_port (sc, s7_current_error_port (sc));
    s7_set_current_error_port (sc, old_port);
    if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);
    return goldfish_run_tool (sc, gf_lib, "repl", errmsg, old_port, gc_loc);
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
      s7_close_output_port (sc, s7_current_error_port (sc));
      s7_set_current_error_port (sc, old_port);
      if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);
      exit (1);
    }

    // 判断类型并处理
    if (target.find ('/') != string::npos || target.rfind (".scm") == target.length () - 4) {
      // 包含 / 或以 .scm 结尾，按文件路径处理
      // 检查文件是否存在
      std::error_code ec;
      if (!fs::exists (target, ec) || !fs::is_regular_file (target, ec)) {
        s7_close_output_port (sc, s7_current_error_port (sc));
        s7_set_current_error_port (sc, old_port);
        if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);
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
      s7_eval_c_string (sc, import_expr.c_str ());
    }

    errmsg= s7_get_output_string (sc, s7_current_error_port (sc));
    if ((errmsg) && (*errmsg)) {
      goldfish_print_scheme_error_message (sc, errmsg);
      s7_close_output_port (sc, s7_current_error_port (sc));
      s7_set_current_error_port (sc, old_port);
      if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);
      return 1;
    }

    // 检查并调用 main 函数
    s7_pointer main_func= s7_name_to_value (sc, "main");
    if ((!main_func) || (!s7_is_procedure (main_func))) {
      s7_close_output_port (sc, s7_current_error_port (sc));
      s7_set_current_error_port (sc, old_port);
      if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);
      std::cerr << "Error: No main function found in target: " << target << std::endl;
      return 1;
    }

    // 调用 main 函数
    s7_call (sc, main_func, s7_nil (sc));

    errmsg= s7_get_output_string (sc, s7_current_error_port (sc));
    goldfish_print_scheme_error_message (sc, errmsg);
    s7_close_output_port (sc, s7_current_error_port (sc));
    s7_set_current_error_port (sc, old_port);
    if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);
    return 0;
  }

  // 处理直接执行文件（以 .scm 结尾或存在的文件）
  // 检查是否是文件
  std::error_code ec;
  if (fs::exists (command, ec) && fs::is_regular_file (command, ec)) {
    goldfish_eval_file (sc, command, true);
    errmsg= s7_get_output_string (sc, s7_current_error_port (sc));
    goldfish_print_scheme_error_message (sc, errmsg);
    s7_close_output_port (sc, s7_current_error_port (sc));
    s7_set_current_error_port (sc, old_port);
    if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);
    if ((errmsg) && (*errmsg)) return -1;
    return 0;
  }

  // 未知命令
  std::cerr << "Unknown command: " << command << "\n\n";
  display_help ();
  s7_close_output_port (sc, s7_current_error_port (sc));
  s7_set_current_error_port (sc, old_port);
  if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);
  return 1;
}

} // namespace goldfish
