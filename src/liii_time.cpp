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
// See the License for the specific language governing permissions and limitations
// under the License.
//

#include "s7.h"
#include <cstdint>
#include <limits>
#include <string>

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#elif defined(__EMSCRIPTEN__)
// WASM has no supported process or thread CPU-time interface.
#elif defined(__APPLE__)
#include <mach/mach.h>
#include <sys/resource.h>
#else
#include <sys/resource.h>
#include <time.h>
#endif

namespace goldfish {

inline void
glue_define (s7_scheme* sc, const char* name, const char* desc, s7_function f, s7_int required, s7_int optional) {
  s7_pointer function= s7_make_typed_function (sc, name, f, required, optional, false, desc, NULL);
  s7_define (sc, s7_curlet (sc), s7_make_symbol (sc, name), function);
}

static bool
make_nanoseconds (std::uint64_t seconds, std::uint64_t subsecond_nanoseconds, std::uint64_t& result) {
  const std::uint64_t billion= 1000000000ULL;
  if (seconds > (std::numeric_limits<std::uint64_t>::max () - subsecond_nanoseconds) / billion) return false;
  result= seconds * billion + subsecond_nanoseconds;
  return result <= static_cast<std::uint64_t> (std::numeric_limits<s7_int>::max ());
}

static bool
timeval_nanoseconds (std::int64_t seconds, std::int64_t microseconds, std::uint64_t& result) {
  return make_nanoseconds (static_cast<std::uint64_t> (seconds), static_cast<std::uint64_t> (microseconds) * 1000ULL,
                           result);
}

static bool
add_cpu_times (std::uint64_t user, std::uint64_t system, std::uint64_t& result) {
  if (user > std::numeric_limits<std::uint64_t>::max () - system) return false;
  result= user + system;
  return result <= static_cast<std::uint64_t> (std::numeric_limits<s7_int>::max ());
}

static bool
read_process_time_ns (std::uint64_t& result) {
#if defined(_WIN32)
  FILETIME creation, exit, kernel, user;
  if (!GetProcessTimes (GetCurrentProcess (), &creation, &exit, &kernel, &user)) return false;
  ULARGE_INTEGER kernel_time, user_time;
  kernel_time.LowPart      = kernel.dwLowDateTime;
  kernel_time.HighPart     = kernel.dwHighDateTime;
  user_time.LowPart        = user.dwLowDateTime;
  user_time.HighPart       = user.dwHighDateTime;
  const std::uint64_t ticks= kernel_time.QuadPart + user_time.QuadPart;
  if (ticks > std::numeric_limits<std::uint64_t>::max () / 100ULL) return false;
  result= ticks * 100ULL;
  return result <= static_cast<std::uint64_t> (std::numeric_limits<s7_int>::max ());
#elif defined(__EMSCRIPTEN__)
  return false;
#elif defined(__APPLE__)
  rusage usage;
  if (getrusage (RUSAGE_SELF, &usage) != 0) return false;
  std::uint64_t user_ns, system_ns;
  return timeval_nanoseconds (usage.ru_utime.tv_sec, usage.ru_utime.tv_usec, user_ns) &&
         timeval_nanoseconds (usage.ru_stime.tv_sec, usage.ru_stime.tv_usec, system_ns) &&
         add_cpu_times (user_ns, system_ns, result);
#else
#if defined(CLOCK_PROCESS_CPUTIME_ID)
  timespec time;
  if (clock_gettime (CLOCK_PROCESS_CPUTIME_ID, &time) != 0) return false;
  return make_nanoseconds (static_cast<std::uint64_t> (time.tv_sec), static_cast<std::uint64_t> (time.tv_nsec), result);
#else
  rusage usage;
  if (getrusage (RUSAGE_SELF, &usage) != 0) return false;
  std::uint64_t user_ns, system_ns;
  return timeval_nanoseconds (usage.ru_utime.tv_sec, usage.ru_utime.tv_usec, user_ns) &&
         timeval_nanoseconds (usage.ru_stime.tv_sec, usage.ru_stime.tv_usec, system_ns) &&
         add_cpu_times (user_ns, system_ns, result);
#endif
#endif
}

static bool
read_thread_time_ns (std::uint64_t& result) {
#if defined(_WIN32)
  FILETIME creation, exit, kernel, user;
  if (!GetThreadTimes (GetCurrentThread (), &creation, &exit, &kernel, &user)) return false;
  ULARGE_INTEGER kernel_time, user_time;
  kernel_time.LowPart      = kernel.dwLowDateTime;
  kernel_time.HighPart     = kernel.dwHighDateTime;
  user_time.LowPart        = user.dwLowDateTime;
  user_time.HighPart       = user.dwHighDateTime;
  const std::uint64_t ticks= kernel_time.QuadPart + user_time.QuadPart;
  if (ticks > std::numeric_limits<std::uint64_t>::max () / 100ULL) return false;
  result= ticks * 100ULL;
  return result <= static_cast<std::uint64_t> (std::numeric_limits<s7_int>::max ());
#elif defined(__EMSCRIPTEN__)
  return false;
#elif defined(__APPLE__)
  const mach_port_t        thread= mach_thread_self ();
  thread_basic_info_data_t info;
  mach_msg_type_number_t   count= THREAD_BASIC_INFO_COUNT;
  const kern_return_t status= thread_info (thread, THREAD_BASIC_INFO, reinterpret_cast<thread_info_t> (&info), &count);
  mach_port_deallocate (mach_task_self (), thread);
  if (status != KERN_SUCCESS || count != THREAD_BASIC_INFO_COUNT) return false;
  std::uint64_t user_ns, system_ns;
  return timeval_nanoseconds (info.user_time.seconds, info.user_time.microseconds, user_ns) &&
         timeval_nanoseconds (info.system_time.seconds, info.system_time.microseconds, system_ns) &&
         add_cpu_times (user_ns, system_ns, result);
#else
#if defined(CLOCK_THREAD_CPUTIME_ID)
  timespec time;
  if (clock_gettime (CLOCK_THREAD_CPUTIME_ID, &time) != 0) return false;
  return make_nanoseconds (static_cast<std::uint64_t> (time.tv_sec), static_cast<std::uint64_t> (time.tv_nsec), result);
#else
  return false;
#endif
#endif
}

static s7_pointer
cpu_time_error (s7_scheme* sc, const char* operation) {
  const std::string message= std::string (operation) + " failed or is unsupported on this platform";
  return s7_error (sc, s7_make_symbol (sc, "os-error"), s7_list (sc, 1, s7_make_string (sc, message.c_str ())));
}

typedef bool (*cpu_time_reader) (std::uint64_t&);

static s7_pointer
cpu_time_seconds (s7_scheme* sc, cpu_time_reader read, const char* operation) {
  std::uint64_t nanoseconds;
  if (!read (nanoseconds)) return cpu_time_error (sc, operation);
  return s7_make_real (sc, static_cast<double> (nanoseconds) / 1000000000.0);
}

static s7_pointer
cpu_time_nanoseconds (s7_scheme* sc, cpu_time_reader read, const char* operation) {
  std::uint64_t nanoseconds;
  if (!read (nanoseconds)) return cpu_time_error (sc, operation);
  return s7_make_integer (sc, static_cast<s7_int> (nanoseconds));
}

static s7_pointer
f_process_time (s7_scheme* sc, s7_pointer args) {
  return cpu_time_seconds (sc, read_process_time_ns, "process-time");
}

static s7_pointer
f_thread_time (s7_scheme* sc, s7_pointer args) {
  return cpu_time_seconds (sc, read_thread_time_ns, "thread-time");
}

static s7_pointer
f_process_time_ns (s7_scheme* sc, s7_pointer args) {
  return cpu_time_nanoseconds (sc, read_process_time_ns, "process-time-ns");
}

static s7_pointer
f_thread_time_ns (s7_scheme* sc, s7_pointer args) {
  return cpu_time_nanoseconds (sc, read_thread_time_ns, "thread-time-ns");
}

void
glue_liii_time (s7_scheme* sc) {
  glue_define (sc, "g_process-time", "(g_process-time) => non-negative-real, process CPU time in seconds",
               f_process_time, 0, 0);
  glue_define (sc, "g_thread-time", "(g_thread-time) => non-negative-real, current thread CPU time in seconds",
               f_thread_time, 0, 0);
  glue_define (sc, "g_process-time-ns", "(g_process-time-ns) => non-negative-integer, process CPU time in nanoseconds",
               f_process_time_ns, 0, 0);
  glue_define (sc, "g_thread-time-ns",
               "(g_thread-time-ns) => non-negative-integer, current thread CPU time in nanoseconds", f_thread_time_ns,
               0, 0);
}

} // namespace goldfish
