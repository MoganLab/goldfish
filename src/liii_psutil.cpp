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

#include "s7.h"
#include <cerrno>
#include <cstdint>
#include <cstdio>
#include <limits>
#include <string>

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#ifndef NOMINMAX
#define NOMINMAX
#endif
// windows.h 必须先于 psapi.h，提供后者依赖的 Win32 类型和调用约定。
// clang-format off
#include <windows.h>
#include <psapi.h>
#include <tlhelp32.h>
// clang-format on
#elif defined(__APPLE__)
#include <libproc.h>
#include <unistd.h>
#elif defined(__linux__)
#include <unistd.h>
#endif

namespace goldfish {

inline void
glue_define (s7_scheme* sc, const char* name, const char* desc, s7_function f, s7_int required, s7_int optional) {
  s7_pointer function= s7_make_typed_function (sc, name, f, required, optional, false, desc, NULL);
  s7_define (sc, s7_curlet (sc), s7_make_symbol (sc, name), function);
}

static s7_pointer
psutil_error (s7_scheme* sc, const char* type, const char* operation, s7_int pid) {
  std::string message= std::string (operation) + " failed for process " + std::to_string (pid);
  return s7_error (sc, s7_make_symbol (sc, type), s7_list (sc, 1, s7_make_string (sc, message.c_str ())));
}

static s7_pointer
psutil_errno_error (s7_scheme* sc, const char* operation, s7_int pid, int error) {
  if (error == ENOENT || error == ESRCH) {
    return psutil_error (sc, "process-not-found-error", operation, pid);
  }
  if (error == EACCES || error == EPERM) {
    return psutil_error (sc, "permission-denied-error", operation, pid);
  }
  return psutil_error (sc, "os-error", operation, pid);
}

#if defined(_WIN32)
static s7_pointer
psutil_windows_error (s7_scheme* sc, const char* operation, s7_int pid, unsigned long error) {
  if (error == ERROR_ACCESS_DENIED) {
    return psutil_error (sc, "permission-denied-error", operation, pid);
  }
  if (error == ERROR_INVALID_PARAMETER || error == ERROR_NOT_FOUND) {
    return psutil_error (sc, "process-not-found-error", operation, pid);
  }
  return psutil_error (sc, "os-error", operation, pid);
}
#endif

static s7_pointer
psutil_pid (s7_scheme* sc, s7_pointer args, const char* function, s7_int& pid) {
  if (s7_is_null (sc, args)) {
#if defined(_WIN32)
    pid= (s7_int) GetCurrentProcessId ();
#elif !defined(__EMSCRIPTEN__)
    pid= (s7_int) getpid ();
#else
    pid= 1;
#endif
    return NULL;
  }
  s7_pointer argument= s7_car (args);
  if (!s7_is_integer (argument)) {
    return s7_error (
        sc, s7_make_symbol (sc, "type-error"),
        s7_list (sc, 2, s7_make_string (sc, (std::string (function) + ": pid must be an integer").c_str ()), argument));
  }
  pid= s7_integer (argument);
  if (pid <= 0) {
    return psutil_error (sc, "value-error", function, pid);
  }
  return NULL;
}

#if defined(__linux__)
static FILE*
open_proc_file (s7_int pid, const char* name) {
  std::string path= "/proc/" + std::to_string (pid) + "/" + name;
  return fopen (path.c_str (), "r");
}

static bool
linux_memory_info (s7_int pid, uint64_t& rss, uint64_t& vms) {
  FILE* file= open_proc_file (pid, "statm");
  if (!file) return false;
  unsigned long long virtual_pages = 0;
  unsigned long long resident_pages= 0;
  int                fields        = fscanf (file, "%llu %llu", &virtual_pages, &resident_pages);
  fclose (file);
  if (fields != 2) {
    errno= EIO;
    return false;
  }
  long page_size= sysconf (_SC_PAGESIZE);
  if (page_size <= 0) {
    errno= EIO;
    return false;
  }
  vms= (uint64_t) virtual_pages * (uint64_t) page_size;
  rss= (uint64_t) resident_pages * (uint64_t) page_size;
  return true;
}

static bool
linux_proportional_set_memory_size (s7_int pid, uint64_t& pss) {
  FILE* file= open_proc_file (pid, "smaps_rollup");
  if (!file) return false;
  char line[256];
  bool found= false;
  while (fgets (line, sizeof (line), file)) {
    unsigned long long kib= 0;
    if (sscanf (line, "Pss: %llu kB", &kib) == 1) {
      pss  = (uint64_t) kib * 1024;
      found= true;
      break;
    }
  }
  fclose (file);
  return found;
}

static bool
linux_num_threads (s7_int pid, uint64_t& count) {
  FILE* file= open_proc_file (pid, "status");
  if (!file) return false;
  char line[256];
  bool found= false;
  while (fgets (line, sizeof (line), file)) {
    unsigned long long value= 0;
    if (sscanf (line, "Threads: %llu", &value) == 1) {
      count= (uint64_t) value;
      found= true;
      break;
    }
  }
  fclose (file);
  if (!found) errno= EIO;
  return found;
}
#endif

static s7_pointer
f_process_memory_info (s7_scheme* sc, s7_pointer args) {
  s7_int     pid  = 0;
  s7_pointer error= psutil_pid (sc, args, "process-memory-info", pid);
  if (error) return error;

  uint64_t rss          = 0;
  uint64_t vms          = 0;
  uint64_t pss          = 0;
  bool     pss_available= false;

#if defined(__linux__)
  if (pid > std::numeric_limits<pid_t>::max ()) {
    return psutil_error (sc, "process-not-found-error", "process-memory-info", pid);
  }
  if (!linux_memory_info (pid, rss, vms)) {
    return psutil_errno_error (sc, "process-memory-info", pid, errno);
  }
  pss_available= linux_proportional_set_memory_size (pid, pss);
#elif defined(__APPLE__)
  if (pid > std::numeric_limits<int>::max ()) {
    return psutil_error (sc, "process-not-found-error", "process-memory-info", pid);
  }
  struct proc_taskinfo info;
  int                  result= proc_pidinfo ((int) pid, PROC_PIDTASKINFO, 0, &info, PROC_PIDTASKINFO_SIZE);
  if (result <= 0) return psutil_errno_error (sc, "process-memory-info", pid, errno);
  if (result < PROC_PIDTASKINFO_SIZE) return psutil_error (sc, "os-error", "process-memory-info", pid);
  rss= info.pti_resident_size;
  vms= info.pti_virtual_size;
#elif defined(_WIN32)
  if ((uint64_t) pid > (std::numeric_limits<DWORD>::max) ()) {
    return psutil_error (sc, "process-not-found-error", "process-memory-info", pid);
  }
  HANDLE process= OpenProcess (PROCESS_QUERY_LIMITED_INFORMATION, FALSE, (DWORD) pid);
  if (!process) {
    DWORD windows_error= GetLastError ();
    return psutil_windows_error (sc, "process-memory-info", pid, windows_error);
  }
  PROCESS_MEMORY_COUNTERS counters= {};
  counters.cb                     = sizeof (counters);
  BOOL  success                   = GetProcessMemoryInfo (process, &counters, sizeof (counters));
  DWORD windows_error             = success ? ERROR_SUCCESS : GetLastError ();
  CloseHandle (process);
  if (!success) {
    const char* type= windows_error == ERROR_ACCESS_DENIED ? "permission-denied-error" : "os-error";
    return psutil_error (sc, type, "process-memory-info", pid);
  }
  rss= counters.WorkingSetSize;
  vms= counters.PagefileUsage;
#elif defined(__EMSCRIPTEN__)
  return psutil_error (sc, "os-error", "process-memory-info is unsupported on WASM", pid);
#else
  return psutil_error (sc, "os-error", "process-memory-info is unsupported on this platform", pid);
#endif

  s7_pointer rss_value= s7_make_integer (sc, (s7_int) rss);
  s7_int     rss_loc  = s7_gc_protect (sc, rss_value);
  s7_pointer vms_value= s7_make_integer (sc, (s7_int) vms);
  s7_int     vms_loc  = s7_gc_protect (sc, vms_value);
  s7_pointer pss_value= pss_available ? s7_make_integer (sc, (s7_int) pss) : s7_make_boolean (sc, false);
  s7_int     pss_loc  = s7_gc_protect (sc, pss_value);
  s7_pointer result   = s7_list (sc, 3, rss_value, vms_value, pss_value);
  s7_gc_unprotect_at (sc, pss_loc);
  s7_gc_unprotect_at (sc, vms_loc);
  s7_gc_unprotect_at (sc, rss_loc);
  return result;
}

static s7_pointer
f_process_num_threads (s7_scheme* sc, s7_pointer args) {
  s7_int     pid  = 0;
  s7_pointer error= psutil_pid (sc, args, "process-num-threads", pid);
  if (error) return error;
  uint64_t count= 0;

#if defined(__linux__)
  if (pid > std::numeric_limits<pid_t>::max ()) {
    return psutil_error (sc, "process-not-found-error", "process-num-threads", pid);
  }
  if (!linux_num_threads (pid, count)) {
    return psutil_errno_error (sc, "process-num-threads", pid, errno);
  }
#elif defined(__APPLE__)
  if (pid > std::numeric_limits<int>::max ()) {
    return psutil_error (sc, "process-not-found-error", "process-num-threads", pid);
  }
  struct proc_taskinfo info;
  int                  result= proc_pidinfo ((int) pid, PROC_PIDTASKINFO, 0, &info, PROC_PIDTASKINFO_SIZE);
  if (result <= 0) return psutil_errno_error (sc, "process-num-threads", pid, errno);
  if (result < PROC_PIDTASKINFO_SIZE) return psutil_error (sc, "os-error", "process-num-threads", pid);
  count= info.pti_threadnum;
#elif defined(_WIN32)
  if ((uint64_t) pid > (std::numeric_limits<DWORD>::max) ()) {
    return psutil_error (sc, "process-not-found-error", "process-num-threads", pid);
  }
  HANDLE process= OpenProcess (PROCESS_QUERY_LIMITED_INFORMATION, FALSE, (DWORD) pid);
  if (!process) {
    DWORD windows_error= GetLastError ();
    return psutil_windows_error (sc, "process-num-threads", pid, windows_error);
  }
  CloseHandle (process);
  HANDLE snapshot= CreateToolhelp32Snapshot (TH32CS_SNAPTHREAD, 0);
  if (snapshot == INVALID_HANDLE_VALUE) return psutil_error (sc, "os-error", "process-num-threads", pid);
  THREADENTRY32 entry;
  entry.dwSize= sizeof (entry);
  BOOL success= Thread32First (snapshot, &entry);
  while (success) {
    if (entry.th32OwnerProcessID == (DWORD) pid) count++;
    entry.dwSize= sizeof (entry);
    success     = Thread32Next (snapshot, &entry);
  }
  CloseHandle (snapshot);
  if (count == 0) return psutil_error (sc, "process-not-found-error", "process-num-threads", pid);
#elif defined(__EMSCRIPTEN__)
  return psutil_error (sc, "os-error", "process-num-threads is unsupported on WASM", pid);
#else
  return psutil_error (sc, "os-error", "process-num-threads is unsupported on this platform", pid);
#endif

  if (count == 0) return psutil_error (sc, "os-error", "process-num-threads", pid);
  return s7_make_integer (sc, (s7_int) count);
}

void
glue_liii_psutil (s7_scheme* sc) {
  glue_define (sc, "g_process-memory-info", "(g_process-memory-info [pid]) => (rss vms pss), sizes in bytes",
               f_process_memory_info, 0, 1);
  glue_define (sc, "g_process-num-threads", "(g_process-num-threads [pid]) => positive integer", f_process_num_threads,
               0, 1);
}

} // namespace goldfish
