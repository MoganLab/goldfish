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
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations
// under the License.
//

#include "s7.h"
#include <cstdlib>
#include <deque>
#include <filesystem>
#include <string>
#include <vector>

#include <tbox/platform/file.h>
#include <tbox/platform/path.h>
#include <tbox/tbox.h>

#ifdef TB_CONFIG_OS_WINDOWS
#include <conio.h>
#include <io.h>
#include <windows.h>
#ifdef _MSC_VER
#include <crtdbg.h>
#endif
#elif TB_CONFIG_OS_MACOSX
#include <limits.h>
#elif defined(__EMSCRIPTEN__)
#include <limits.h>
#else
#include <linux/limits.h>
#endif

#if !defined(TB_CONFIG_OS_WINDOWS)
#include <errno.h>
#include <poll.h>
#include <pwd.h>
#include <sys/ioctl.h>
#include <termios.h>
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

inline void
glue_define (s7_scheme* sc, const char* name, const char* desc, s7_function f, s7_int required, s7_int optional) {
  s7_pointer cur_env= s7_curlet (sc);
  s7_pointer func   = s7_make_typed_function (sc, name, f, required, optional, false, desc, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, name), func);
}

inline s7_pointer
string_vector_to_s7_vector (s7_scheme* sc, vector<string> v) {
  int        N  = v.size ();
  s7_pointer ret= s7_make_vector (sc, N);
  // 循环内 s7_make_string 可能触发 GC，构造中的 vector 必须保护，
  // 否则大目录（数千条目）时返回悬挂对象 <free cell!>
  s7_gc_protect_via_stack (sc, ret);
  for (int i= 0; i < N; i++) {
    s7_vector_set (sc, ret, i, s7_make_string (sc, v[i].c_str ()));
  }
  s7_gc_unprotect_via_stack (sc, ret);
  return ret;
}

// 抛出 (liii error) 约定的 type-error，irritant 为出错的参数
inline s7_pointer
string_type_error (s7_scheme* sc, const char* msg, s7_pointer arg) {
  return s7_error (sc, s7_make_symbol (sc, "type-error"), s7_list (sc, 2, s7_make_string (sc, msg), arg));
}

static s7_pointer
f_os_arch (s7_scheme* sc, s7_pointer args) {
  return s7_make_string (sc, TB_ARCH_STRING);
}

static void
glue_os_arch (s7_scheme* sc) {
  const char* name= "g_os-arch";
  const char* desc= "(g_os-arch) => string";
  glue_define (sc, name, desc, f_os_arch, 0, 0);
}

static s7_pointer
f_os_type (s7_scheme* sc, s7_pointer args) {
#ifdef TB_CONFIG_OS_LINUX
  return s7_make_string (sc, "Linux");
#endif
#ifdef TB_CONFIG_OS_MACOSX
  return s7_make_string (sc, "Darwin");
#endif
#ifdef TB_CONFIG_OS_WINDOWS
  return s7_make_string (sc, "Windows");
#endif
  return s7_make_boolean (sc, false);
}

static void
glue_os_type (s7_scheme* sc) {
  const char* name= "g_os-type";
  const char* desc= "(g_os-type) => string";
  glue_define (sc, name, desc, f_os_type, 0, 0);
}

static s7_pointer
f_os_call (s7_scheme* sc, s7_pointer args) {
  s7_pointer cmd_arg= s7_car (args);
  if (!s7_is_string (cmd_arg)) {
    return string_type_error (sc, "os-call: command must be a string", cmd_arg);
  }
  const char*       cmd_c= s7_string (cmd_arg);
  tb_process_attr_t attr = {tb_null};
  attr.flags             = TB_PROCESS_FLAG_NO_WINDOW;
  int ret;

#if (defined(_MSC_VER) || defined(__MINGW32__))
  ret= (int) std::system (cmd_c);
#elif defined(__EMSCRIPTEN__)
  tb_char_t* argv[]= {(tb_char_t*) cmd_c, tb_null};
  ret              = (int) tb_process_run (argv[0], (tb_char_t const**) argv, &attr);
#else
  wordexp_t p;
  ret= wordexp (cmd_c, &p, 0);
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
  return s7_make_integer (sc, ret);
}

static void
glue_os_call (s7_scheme* sc) {
  const char* name= "g_os-call";
  const char* desc= "(g_os-call string) => int, execute a shell command and return the exit code";
  glue_define (sc, name, desc, f_os_call, 1, 0);
}

static s7_pointer
f_system (s7_scheme* sc, s7_pointer args) {
  s7_pointer cmd_arg= s7_car (args);
  if (!s7_is_string (cmd_arg)) {
    return string_type_error (sc, "system: command must be a string", cmd_arg);
  }
  const char* cmd_c= s7_string (cmd_arg);
  int         ret  = (int) std::system (cmd_c);
  return s7_make_integer (sc, ret);
}

static void
glue_system (s7_scheme* sc) {
  const char* name= "g_system";
  const char* desc= "(g_system string) => int, execute a shell command and return the exit code";
  glue_define (sc, name, desc, f_system, 1, 0);
}

static s7_pointer
f_access (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "access: path must be a string", path_arg);
  }
  s7_pointer mode_arg= s7_cadr (args);
  if (!s7_is_integer (mode_arg)) {
    return string_type_error (sc, "access: mode must be an integer", mode_arg);
  }
  const char* path_c= s7_string (path_arg);
  int         mode  = s7_integer (mode_arg);
  bool        ret   = false;
  if (mode == 0) {
    tb_file_info_t info;
    ret= tb_file_info (path_c, &info);
  }
  else {
    ret= tb_file_access (path_c, mode);
  }

  return s7_make_boolean (sc, ret);
}

static void
glue_access (s7_scheme* sc) {
  const char* name= "g_access";
  const char* desc= "(g_access string integer) => boolean, check file access permissions";
  glue_define (sc, name, desc, f_access, 2, 0);
}

static s7_pointer
f_set_environment_variable (s7_scheme* sc, s7_pointer args) {
  s7_pointer key_arg= s7_car (args);
  if (!s7_is_string (key_arg)) {
    return string_type_error (sc, "setenv: key must be a string", key_arg);
  }
  s7_pointer value_arg= s7_cadr (args);
  if (!s7_is_string (value_arg)) {
    return string_type_error (sc, "setenv: value must be a string", value_arg);
  }
  const char* key  = s7_string (key_arg);
  const char* value= s7_string (value_arg);
  return s7_make_boolean (sc, tb_environment_set (key, value));
}

static void
glue_setenv (s7_scheme* sc) {
  const char* name= "g_setenv";
  const char* desc= "(g_setenv key value) => boolean, set an environment variable";
  glue_define (sc, name, desc, f_set_environment_variable, 2, 0);
}

static s7_pointer
f_unset_environment_variable (s7_scheme* sc, s7_pointer args) {
  s7_pointer env_arg= s7_car (args);
  if (!s7_is_string (env_arg)) {
    return string_type_error (sc, "unsetenv: key must be a string", env_arg);
  }
  const char* env_name= s7_string (env_arg);
  return s7_make_boolean (sc, tb_environment_remove (env_name));
}

static void
glue_unsetenv (s7_scheme* sc) {
  const char* name= "g_unsetenv";
  const char* desc= "(g_unsetenv string): string => boolean";
  glue_define (sc, name, desc, f_unset_environment_variable, 1, 0);
}

static s7_pointer
f_os_temp_dir (s7_scheme* sc, s7_pointer args) {
  tb_char_t path[GOLDFISH_PATH_MAXN];
  tb_directory_temporary (path, GOLDFISH_PATH_MAXN);
  return s7_make_string (sc, path);
}

static void
glue_os_temp_dir (s7_scheme* sc) {
  const char* name= "g_os-temp-dir";
  const char* desc= "(g_os-temp-dir) => string, get the temporary directory path";
  glue_define (sc, name, desc, f_os_temp_dir, 0, 0);
}

static s7_pointer
f_mkdir (s7_scheme* sc, s7_pointer args) {
  s7_pointer dir_arg= s7_car (args);
  if (!s7_is_string (dir_arg)) {
    return string_type_error (sc, "mkdir: path must be a string", dir_arg);
  }
  const char* dir_c= s7_string (dir_arg);
  return s7_make_boolean (sc, tb_directory_create (dir_c));
}

static void
glue_mkdir (s7_scheme* sc) {
  const char* name= "g_mkdir";
  const char* desc= "(g_mkdir string) => boolean, create a directory";
  glue_define (sc, name, desc, f_mkdir, 1, 0);
}

static s7_pointer
f_rmdir (s7_scheme* sc, s7_pointer args) {
  s7_pointer dir_arg= s7_car (args);
  if (!s7_is_string (dir_arg)) {
    return string_type_error (sc, "rmdir: path must be a string", dir_arg);
  }
  const char* dir_c= s7_string (dir_arg);
  return s7_make_boolean (sc, tb_directory_remove (dir_c));
}

static void
glue_rmdir (s7_scheme* sc) {
  const char* name= "g_rmdir";
  const char* desc= "(g_rmdir string) => boolean, remove a directory";
  glue_define (sc, name, desc, f_rmdir, 1, 0);
}

static s7_pointer
f_remove_file (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "remove-file: path must be a string", path_arg);
  }
  const char* path   = s7_string (path_arg);
  bool        success= tb_file_remove (path);
  return s7_make_boolean (sc, success);
}

static void
glue_remove_file (s7_scheme* sc) {
  const char* name= "g_remove-file";
  const char* desc= "(g_remove-file path) => boolean, delete a file";
  glue_define (sc, name, desc, f_remove_file, 1, 0);
}

static s7_pointer
f_rename (s7_scheme* sc, s7_pointer args) {
  s7_pointer src_arg= s7_car (args);
  if (!s7_is_string (src_arg)) {
    return string_type_error (sc, "rename: src must be a string", src_arg);
  }
  s7_pointer dst_arg= s7_cadr (args);
  if (!s7_is_string (dst_arg)) {
    return string_type_error (sc, "rename: dst must be a string", dst_arg);
  }
  const char* src= s7_string (src_arg);
  const char* dst= s7_string (dst_arg);
  try {
    fs::rename (src, dst);
    return s7_make_boolean (sc, true);
  } catch (const fs::filesystem_error& e) {
    return s7_make_boolean (sc, false);
  }
}

static void
glue_rename (s7_scheme* sc) {
  const char* name= "g_rename";
  const char* desc= "(g_rename src dst) => boolean, rename file or directory from src to dst";
  glue_define (sc, name, desc, f_rename, 2, 0);
}

static s7_pointer
f_chdir (s7_scheme* sc, s7_pointer args) {
  s7_pointer dir_arg= s7_car (args);
  if (!s7_is_string (dir_arg)) {
    return string_type_error (sc, "chdir: path must be a string", dir_arg);
  }
  const char* dir_c= s7_string (dir_arg);
  return s7_make_boolean (sc, tb_directory_current_set (dir_c));
}

static void
glue_chdir (s7_scheme* sc) {
  const char* name= "g_chdir";
  const char* desc= "(g_chdir string) => boolean, change the current working directory";
  glue_define (sc, name, desc, f_chdir, 1, 0);
}

static tb_long_t
tb_directory_walk_func (tb_char_t const* path, tb_file_info_t const* info, tb_cpointer_t priv) {
  tb_assert_and_check_return_val (path && info, TB_DIRECTORY_WALK_CODE_END);

  vector<string>* p_v_result= (vector<string>*) priv;
  p_v_result->push_back (string (path));
  return TB_DIRECTORY_WALK_CODE_CONTINUE;
}

static s7_pointer
f_listdir (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  if (!s7_is_string (path_arg)) {
    return string_type_error (sc, "listdir: path must be a string", path_arg);
  }
  const char*    path_c= s7_string (path_arg);
  vector<string> entries;
  s7_pointer     ret= s7_make_vector (sc, 0);
  tb_directory_walk (path_c, 0, tb_false, tb_directory_walk_func, &entries);

  int    entries_N   = entries.size ();
  string path_s      = string (path_c);
  int    path_N      = path_s.size ();
  int    path_slash_N= path_N;
  char   last_ch     = (path_N > 0) ? path_s[path_N - 1] : '\0';
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
  return string_vector_to_s7_vector (sc, entries);
}

static void
glue_listdir (s7_scheme* sc) {
  const char* name= "g_listdir";
  const char* desc= "(g_listdir string) => vector, list the contents of a directory";
  glue_define (sc, name, desc, f_listdir, 1, 0);
}

static s7_pointer
f_getcwd (s7_scheme* sc, s7_pointer args) {
  tb_char_t path[GOLDFISH_PATH_MAXN];
  tb_directory_current (path, GOLDFISH_PATH_MAXN);
  return s7_make_string (sc, path);
}

static void
glue_getcwd (s7_scheme* sc) {
  const char* name= "g_getcwd";
  const char* desc= "(g_getcwd) => string, get the current working directory";
  glue_define (sc, name, desc, f_getcwd, 0, 0);
}

static s7_pointer
f_getlogin (s7_scheme* sc, s7_pointer args) {
#ifdef TB_CONFIG_OS_WINDOWS
  return s7_make_boolean (sc, false);
#else
  uid_t          uid= getuid ();
  struct passwd* pwd= getpwuid (uid);
  return s7_make_string (sc, pwd->pw_name);
#endif
}

static void
glue_getlogin (s7_scheme* sc) {
  const char* name= "g_getlogin";
  const char* desc= "(g_getlogin) => string, get the current user's login name";
  glue_define (sc, name, desc, f_getlogin, 0, 0);
}

static s7_pointer
f_getpid (s7_scheme* sc, s7_pointer args) {
#ifdef TB_CONFIG_OS_WINDOWS
  return s7_make_integer (sc, (int) GetCurrentProcessId ());
#else
  return s7_make_integer (sc, getpid ());
#endif
}

static void
glue_getpid (s7_scheme* sc) {
  const char* name= "g_getpid";
  const char* desc= "(g_getpid) => integer";
  glue_define (sc, name, desc, f_getpid, 0, 0);
}

static s7_pointer
f_os_tty_p (s7_scheme* sc, s7_pointer args) {
  s7_pointer fd_arg= s7_car (args);
  if (!s7_is_integer (fd_arg)) {
    return string_type_error (sc, "os-tty?: fd must be an integer", fd_arg);
  }
  s7_int fd= s7_integer (fd_arg);
#if defined(TB_CONFIG_OS_WINDOWS) && defined(_MSC_VER)
  _invalid_parameter_handler old_handler= _set_thread_local_invalid_parameter_handler (
      [] (const wchar_t*, const wchar_t*, const wchar_t*, unsigned int, uintptr_t) {});
  _CrtSetReportMode (_CRT_ASSERT, 0);
  int ret= (fd >= 0) ? _isatty ((int) fd) : 0;
  _set_thread_local_invalid_parameter_handler (old_handler);
#elif defined(TB_CONFIG_OS_WINDOWS)
  int ret= (fd >= 0) ? _isatty ((int) fd) : 0;
#else
  int ret= (fd >= 0) ? isatty ((int) fd) : 0;
#endif
  return s7_make_boolean (sc, ret != 0);
}

#ifdef TB_CONFIG_OS_WINDOWS
static HANDLE g_hIn_orig     = INVALID_HANDLE_VALUE;
static DWORD  g_orig_in_mode = 0;
static bool   g_has_orig_mode= false;
// 当前是否处于 raw mode（与 POSIX 分支的同名变量语义一致）
static bool            g_raw_mode_enabled= false;
static std::deque<int> g_console_key_queue;
static uint32_t        g_surrogate_hi= 0;
// 上一次处于按下状态的鼠标按钮（SGR 编码：0=左 1=中 2=右，-1=无），
// 用于在释放事件中还原按钮号
static int g_mouse_prev_button= -1;

static void
restore_windows_console_mode_at_exit () {
  if (g_has_orig_mode && g_hIn_orig != INVALID_HANDLE_VALUE) {
    SetConsoleMode (g_hIn_orig, g_orig_in_mode);
    g_raw_mode_enabled= false;
  }
}

static void
push_unicode_utf8 (uint32_t cp) {
  if (cp < 0x80) {
    g_console_key_queue.push_back ((int) cp);
  }
  else if (cp < 0x800) {
    g_console_key_queue.push_back ((int) (0xC0 | (cp >> 6)));
    g_console_key_queue.push_back ((int) (0x80 | (cp & 0x3F)));
  }
  else if (cp < 0x10000) {
    g_console_key_queue.push_back ((int) (0xE0 | (cp >> 12)));
    g_console_key_queue.push_back ((int) (0x80 | ((cp >> 6) & 0x3F)));
    g_console_key_queue.push_back ((int) (0x80 | (cp & 0x3F)));
  }
  else if (cp <= 0x10FFFF) {
    g_console_key_queue.push_back ((int) (0xF0 | (cp >> 18)));
    g_console_key_queue.push_back ((int) (0x80 | ((cp >> 12) & 0x3F)));
    g_console_key_queue.push_back ((int) (0x80 | ((cp >> 6) & 0x3F)));
    g_console_key_queue.push_back ((int) (0x80 | (cp & 0x3F)));
  }
}

static void
push_seq (int prefix, int code) {
  g_console_key_queue.push_back (prefix);
  g_console_key_queue.push_back (code);
}

/* 把 Windows MOUSE_EVENT_RECORD 合成为 XTerm SGR 鼠标序列（1006 模式）：
 * ESC [ < B ; X ; Y M/m，B 为按钮码（bit0-1 按钮 0/1/2，bit3 Alt，
 * bit4 Shift，bit5 Ctrl，bit6 motion，bit7 滚轮），X/Y 为 1-based 坐标。
 * 滚轮：WHEEL_UP=64，WHEEL_DOWN=65。按钮释放统一报 3（无按钮）。
 * Scheme 侧由 (liii tui mouse) 的 parse-mouse-event 解析。 */
static void
push_sgr_mouse (DWORD button_code, DWORD x, DWORD y, bool release) {
  std::string seq= "\x1b[<";
  seq+= std::to_string ((long long) button_code);
  seq+= ';';
  seq+= std::to_string ((long long) (x + 1));
  seq+= ';';
  seq+= std::to_string ((long long) (y + 1));
  seq+= release ? 'm' : 'M';
  for (unsigned char c : seq) {
    g_console_key_queue.push_back ((int) c);
  }
}

static bool
pump_console_input (HANDLE hIn, long timeout_ms) {
  if (!g_console_key_queue.empty ()) {
    return true;
  }
  DWORD mode;
  if (!GetConsoleMode (hIn, &mode)) {
    DWORD avail= 0;
    if (!PeekNamedPipe (hIn, NULL, 0, NULL, &avail, NULL) || avail == 0) {
      return false;
    }
    unsigned char ch       = 0;
    DWORD         bytesRead= 0;
    if (ReadFile (hIn, &ch, 1, &bytesRead, NULL) && bytesRead == 1) {
      g_console_key_queue.push_back ((int) ch);
      return true;
    }
    return false;
  }

  while (g_console_key_queue.empty ()) {
    DWORD count= 0;
    if (!GetNumberOfConsoleInputEvents (hIn, &count)) {
      return false;
    }
    if (count == 0) {
      if (timeout_ms <= 0) {
        return false;
      }
      ULONGLONG start_ms= GetTickCount64 ();
      DWORD     res     = WaitForSingleObject (hIn, (DWORD) timeout_ms);
      if (res != WAIT_OBJECT_0) {
        return false;
      }
      ULONGLONG waited= GetTickCount64 () - start_ms;
      if ((long) waited >= timeout_ms) {
        timeout_ms= 0;
      }
      else {
        timeout_ms-= (long) waited;
      }
    }

    INPUT_RECORD rec;
    DWORD        read_count= 0;
    if (!ReadConsoleInputW (hIn, &rec, 1, &read_count) || read_count != 1) {
      return false;
    }

    if (rec.EventType == MOUSE_EVENT) {
      const MOUSE_EVENT_RECORD& me= rec.Event.MouseEvent;
      DWORD ctrl                = me.dwControlKeyState;
      // dwButtonState 低 5 位：1=左 2=右 4=中 8/16=XBUTTON
      // SGR 按钮码：0=左 1=中 2=右，需要重映射
      DWORD win_button         = me.dwButtonState & 0xF;
      int    sgr_button        = -1; // -1 = 无按钮
      if (win_button == FROM_LEFT_1ST_BUTTON_PRESSED)
        sgr_button= 0;
      else if (win_button == RIGHTMOST_BUTTON_PRESSED)
        sgr_button= 2;
      else if (win_button == FROM_LEFT_2ND_BUTTON_PRESSED)
        sgr_button= 1;
      else if (win_button != 0)
        continue; // XBUTTON 忽略

      DWORD button_code= 0;
      if (me.dwEventFlags & MOUSE_WHEELED) {
        // 高字为有符号滚轮增量，负数表示向上滚；按钮码 64=上，65=下
        SHORT delta= (SHORT) ((me.dwButtonState >> 16) & 0xFFFF);
        button_code= delta < 0 ? 64 : 65;
      }
      else if (me.dwEventFlags & MOUSE_MOVED) {
        if (sgr_button < 0 && g_mouse_prev_button < 0) {
          continue; // 无按钮移动（1003 才上报，这里忽略）
        }
        // 拖动：motion 位 32 + 按钮号（若当前无按钮则用上一次的）
        button_code= 32 | (DWORD) (sgr_button >= 0 ? sgr_button : g_mouse_prev_button);
      }
      else if (sgr_button >= 0) {
        button_code= (DWORD) sgr_button; // 按下（含双击，重复上报无害）
        g_mouse_prev_button= sgr_button;
      }
      else {
        // 释放：final m，报告上一次按下的按钮号
        DWORD rel= g_mouse_prev_button >= 0 ? (DWORD) g_mouse_prev_button : 3;
        g_mouse_prev_button= -1;
        push_sgr_mouse (rel, me.dwMousePosition.X, me.dwMousePosition.Y, true);
        return true;
      }
      // 修饰位：Shift(4) Alt(8) Ctrl(16)，与 SGR 按钮码位定义一致
      if (ctrl & SHIFT_PRESSED) button_code|= 4;
      if (ctrl & (LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED)) button_code|= 8;
      if (ctrl & (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)) button_code|= 16;
      push_sgr_mouse (button_code, me.dwMousePosition.X, me.dwMousePosition.Y, false);
      return true;
    }

    if (rec.EventType != KEY_EVENT || !rec.Event.KeyEvent.bKeyDown) {
      continue;
    }

    const KEY_EVENT_RECORD& ke      = rec.Event.KeyEvent;
    WORD                    virt    = ke.wVirtualKeyCode;
    DWORD                   ctrl    = ke.dwControlKeyState;
    uint32_t                chr     = (uint32_t) ke.uChar.UnicodeChar;
    bool                    is_ctrl = (ctrl & (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)) != 0;
    bool                    is_shift= (ctrl & SHIFT_PRESSED) != 0;

    // Shift-Tab
    if (virt == VK_TAB && is_shift) {
      g_console_key_queue.push_back (353); // curses KEY_BTAB
      return true;
    }

    // Ctrl-Enter -> 10 (LineFeed)
    if (virt == VK_RETURN && is_ctrl) {
      g_console_key_queue.push_back (10);
      return true;
    }

    // 功能键/光标键处理
    if (chr == 0) {
      switch (virt) {
      case VK_UP:
        push_seq (224, is_ctrl ? 141 : 72);
        return true;
      case VK_DOWN:
        push_seq (224, is_ctrl ? 145 : 80);
        return true;
      case VK_LEFT:
        push_seq (224, is_ctrl ? 115 : 75);
        return true;
      case VK_RIGHT:
        push_seq (224, is_ctrl ? 116 : 77);
        return true;
      case VK_HOME:
        push_seq (224, is_ctrl ? 119 : 71);
        return true;
      case VK_END:
        push_seq (224, is_ctrl ? 117 : 79);
        return true;
      case VK_PRIOR:
        push_seq (224, 73);
        return true;
      case VK_NEXT:
        push_seq (224, 81);
        return true;
      case VK_DELETE:
        push_seq (224, is_ctrl ? 147 : 83);
        return true;
      case VK_INSERT:
        push_seq (224, 82);
        return true;
      case VK_F1:
      case VK_F2:
      case VK_F3:
      case VK_F4:
      case VK_F5:
      case VK_F6:
      case VK_F7:
      case VK_F8:
      case VK_F9:
      case VK_F10:
        push_seq (0, 59 + (virt - VK_F1));
        return true;
      case VK_F11:
        push_seq (224, 133);
        return true;
      case VK_F12:
        push_seq (224, 134);
        return true;
      default:
        break;
      }
      continue;
    }

    // 代理对处理 (Surrogate pairs)
    if (chr >= 0xD800 && chr <= 0xDBFF) {
      g_surrogate_hi= (chr - 0xD800);
      continue;
    }
    if (chr >= 0xDC00 && chr <= 0xDFFF) {
      uint32_t full_cp= ((g_surrogate_hi << 10) + (chr - 0xDC00) + 0x10000);
      g_surrogate_hi  = 0;
      push_unicode_utf8 (full_cp);
      return true;
    }

    // 常规 Unicode 字符 / ASCII 控制码
    push_unicode_utf8 (chr);
    return true;
  }

  return !g_console_key_queue.empty ();
}
#else
static struct termios g_orig_termios;
static bool           g_raw_mode_enabled= false;
static bool           g_has_orig_termios= false;

static void
restore_terminal_mode_at_exit () {
  if (g_raw_mode_enabled && g_has_orig_termios) {
    tcsetattr (STDIN_FILENO, TCSANOW, &g_orig_termios);
    g_raw_mode_enabled= false;
  }
}
#endif

static s7_pointer
f_console_raw_mode (s7_scheme* sc, s7_pointer args) {
  s7_pointer enable_arg= s7_car (args);
  if (!s7_is_boolean (enable_arg)) {
    return string_type_error (sc, "console-raw-mode!: enable must be a boolean", enable_arg);
  }
#ifdef TB_CONFIG_OS_WINDOWS
  HANDLE hIn= GetStdHandle (STD_INPUT_HANDLE);
  if (hIn == INVALID_HANDLE_VALUE) {
    return s7_make_boolean (sc, false);
  }
  bool enable= (enable_arg == s7_t (sc));
  if (enable) {
    if (!g_has_orig_mode && GetConsoleMode (hIn, &g_orig_in_mode)) {
      g_has_orig_mode= true;
      g_hIn_orig     = hIn;
      atexit (restore_windows_console_mode_at_exit);
    }
    DWORD raw_mode= g_orig_in_mode;
    raw_mode&= ~(ENABLE_ECHO_INPUT | ENABLE_LINE_INPUT | ENABLE_PROCESSED_INPUT);
    raw_mode|= ENABLE_WINDOW_INPUT;
    /* 鼠标事件上报：需同时清 QUICK_EDIT（否则点击被终端自带的文本选择占用），
     * MOUSE_EVENT 在 pump_console_input 中合成为 SGR 字节序列（1006 模式），
     * 与 POSIX 终端的 ESC[<B;X;YM/m 序列统一，Scheme 层只需一条解析路径。 */
    raw_mode&= ~ENABLE_QUICK_EDIT_MODE;
    raw_mode|= ENABLE_EXTENDED_FLAGS | ENABLE_MOUSE_INPUT;
    SetConsoleMode (hIn, raw_mode);
    g_raw_mode_enabled= true;
  }
  else if (g_has_orig_mode) {
    SetConsoleMode (hIn, g_orig_in_mode);
    g_raw_mode_enabled= false;
    g_console_key_queue.clear ();
  }
  return s7_make_boolean (sc, true);
#else
  bool enable= (enable_arg == s7_t (sc));
  if (enable) {
    if (!isatty (STDIN_FILENO)) {
      return s7_make_boolean (sc, false);
    }
    if (!g_has_orig_termios) {
      if (tcgetattr (STDIN_FILENO, &g_orig_termios) != 0) {
        return s7_make_boolean (sc, false);
      }
      g_has_orig_termios= true;
      atexit (restore_terminal_mode_at_exit);
    }
    struct termios raw= g_orig_termios;
    raw.c_iflag&= ~(ICRNL | INLCR);
    /*
     * 必须同时清除 ISIG：否则 tty 驱动会把 Ctrl+C/Ctrl+Z 等按键转成
     * SIGINT/SIGTSTP 信号而不会作为字节（0x03 等）交给上层。TUI 依赖
     * 读到 0x03 来识别 'ctrl-c（见 terminal-poll-key），若保留 ISIG，
     * 按键会直接杀死进程，Pi TUI 永远收不到 ctrl-c。
     * IXON 一并清除，避免 Ctrl+S/Ctrl+Q 被当成流控吞掉。
     */
    raw.c_lflag&= ~(ICANON | ECHO | ISIG | IEXTEN);
    raw.c_iflag&= ~(IXON);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME]= 0;
    if (tcsetattr (STDIN_FILENO, TCSANOW, &raw) != 0) {
      return s7_make_boolean (sc, false);
    }
    g_raw_mode_enabled= true;
    return s7_make_boolean (sc, true);
  }
  else {
    if (g_raw_mode_enabled && g_has_orig_termios) {
      tcsetattr (STDIN_FILENO, TCSANOW, &g_orig_termios);
      g_raw_mode_enabled= false;
    }
    return s7_make_boolean (sc, true);
  }
#endif
}

static void
glue_console_raw_mode (s7_scheme* sc) {
  const char* name= "g_console_raw_mode";
  const char* desc= "(g_console_raw_mode enable) => boolean, set console raw mode";
  glue_define (sc, name, desc, f_console_raw_mode, 1, 0);
}

/*
 * 诊断接口：(g_console_raw_mode_state) => alist
 * 用于验证 raw mode 是否真正关闭了把按键转成信号的位。
 * TUI 依赖读到 Ctrl+C 的 0x03 字节而不是收到 SIGINT；若 ISIG 未清除，
 * 按键会在 tty 层被转成 SIGINT 直接杀死进程，上层永远收不到 'ctrl-c。
 * 返回字段：
 *   (isig-disabled . bool)   —— c_lflag 中 ISIG 是否已清除（关键）
 *   (icanon-disabled . bool) —— 行缓冲是否已关闭
 *   (echo-disabled . bool)   —— 回显是否已关闭
 *   (ixon-disabled . bool)   —— 流控是否已关闭
 *   (raw-active . bool)      —— 当前是否处于 raw mode
 * Windows 下控制台不走 termios，统一返回 #t（ENABLE_PROCESSED_INPUT
 * 已关闭，Ctrl+C 不会作为信号处理）。
 */
static s7_pointer
f_console_raw_mode_state (s7_scheme* sc, s7_pointer args) {
  (void) args;
#ifdef TB_CONFIG_OS_WINDOWS
  bool isig_off  = true;
  bool icanon_off= true;
  bool echo_off  = true;
  bool ixon_off  = true;
  bool active    = g_raw_mode_enabled;
#else
  struct termios cur;
  bool           ok        = (tcgetattr (STDIN_FILENO, &cur) == 0);
  bool           isig_off  = ok && ((cur.c_lflag & ISIG) == 0);
  bool           icanon_off= ok && ((cur.c_lflag & ICANON) == 0);
  bool           echo_off  = ok && ((cur.c_lflag & ECHO) == 0);
  bool           ixon_off  = ok && ((cur.c_iflag & IXON) == 0);
  bool           active    = g_raw_mode_enabled;
#endif
  s7_pointer res= s7_nil (sc);
  // 逐项转换为 alist；数组遍历避免五段重复 cons 链与超长行造成的对齐跳变
  const struct {
    const char* key;
    bool        val;
  } fields[]= {{"ixon-disabled", ixon_off},
               {"echo-disabled", echo_off},
               {"icanon-disabled", icanon_off},
               {"isig-disabled", isig_off},
               {"raw-active", active}};
  for (const auto& f : fields) {
    res= s7_cons (sc, s7_cons (sc, s7_make_symbol (sc, f.key), s7_make_boolean (sc, f.val)), res);
  }
  return res;
}; /* f_console_raw_mode_state */

static void
glue_console_raw_mode_state (s7_scheme* sc) {
  const char* name= "g_console_raw_mode_state";
  const char* desc= "(g_console_raw_mode_state) => alist describing raw-mode termios flags";
  glue_define (sc, name, desc, f_console_raw_mode_state, 0, 0);
}

static s7_pointer
f_terminal_size (s7_scheme* sc, s7_pointer args) {
#ifdef TB_CONFIG_OS_WINDOWS
  CONSOLE_SCREEN_BUFFER_INFO csbi;
  HANDLE                     h= GetStdHandle (STD_OUTPUT_HANDLE);
  if (GetConsoleScreenBufferInfo (h, &csbi)) {
    int cols= csbi.srWindow.Right - csbi.srWindow.Left + 1;
    int rows= csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
    return s7_list (sc, 2, s7_make_integer (sc, cols), s7_make_integer (sc, rows));
  }
#else
  struct winsize ws;
  if (ioctl (STDIN_FILENO, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0 && ws.ws_row > 0) {
    return s7_list (sc, 2, s7_make_integer (sc, ws.ws_col), s7_make_integer (sc, ws.ws_row));
  }
  if (ioctl (STDOUT_FILENO, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0 && ws.ws_row > 0) {
    return s7_list (sc, 2, s7_make_integer (sc, ws.ws_col), s7_make_integer (sc, ws.ws_row));
  }
#endif
  return s7_make_boolean (sc, false);
}

static void
glue_terminal_size (s7_scheme* sc) {
  const char* name= "g_terminal_size";
  const char* desc= "(g_terminal_size) => (cols rows) or #f, detect terminal dimensions";
  glue_define (sc, name, desc, f_terminal_size, 0, 0);
}

static s7_pointer
f_console_kbhit (s7_scheme* sc, s7_pointer args) {
  int timeout_ms= 0;
  if (s7_is_pair (args)) {
    s7_pointer t_arg= s7_car (args);
    if (!s7_is_integer (t_arg)) {
      return string_type_error (sc, "console-kbhit?: timeout_ms must be an integer", t_arg);
    }
    timeout_ms= (int) s7_integer (t_arg);
    if (timeout_ms < 0) {
      timeout_ms= 0;
    }
  }
#ifdef TB_CONFIG_OS_WINDOWS
  HANDLE hIn  = GetStdHandle (STD_INPUT_HANDLE);
  bool   ready= pump_console_input (hIn, timeout_ms);
  return s7_make_boolean (sc, ready);
#else
  if (!isatty (STDIN_FILENO)) {
    return s7_make_boolean (sc, false);
  }
  struct pollfd pfd;
  pfd.fd     = STDIN_FILENO;
  pfd.events = POLLIN;
  pfd.revents= 0;
  int ret    = poll (&pfd, 1, timeout_ms);
  return s7_make_boolean (sc, ret > 0 && (pfd.revents & POLLIN));
#endif
}

static void
glue_console_kbhit (s7_scheme* sc) {
  const char* name= "g_console_kbhit";
  const char* desc= "(g_console_kbhit [timeout-ms]) => boolean, check if console key is ready";
  glue_define (sc, name, desc, f_console_kbhit, 0, 1);
}

static s7_pointer
f_console_getch (s7_scheme* sc, s7_pointer args) {
#ifdef TB_CONFIG_OS_WINDOWS
  HANDLE hIn= GetStdHandle (STD_INPUT_HANDLE);
  if (!pump_console_input (hIn, 0)) {
    DWORD mode;
    if (!GetConsoleMode (hIn, &mode)) {
      unsigned char ch       = 0;
      DWORD         bytesRead= 0;
      if (ReadFile (hIn, &ch, 1, &bytesRead, NULL) && bytesRead == 1) {
        return s7_make_integer (sc, (int) ch);
      }
    }
    return s7_make_boolean (sc, false);
  }
  int ch= g_console_key_queue.front ();
  g_console_key_queue.pop_front ();
  return s7_make_integer (sc, ch);
#else
  unsigned char ch= 0;
  ssize_t       n = read (STDIN_FILENO, &ch, 1);
  if (n == 1) {
    return s7_make_integer (sc, (int) ch);
  }
  return s7_make_boolean (sc, false);
#endif
}

static void
glue_console_getch (s7_scheme* sc) {
  const char* name= "g_console_getch";
  const char* desc= "(g_console_getch) => integer, read char from console without echo";
  glue_define (sc, name, desc, f_console_getch, 0, 0);
}

static void
glue_os_tty_p (s7_scheme* sc) {
  const char* name= "g_os_tty_p";
  const char* desc= "(g_os_tty_p fd) => boolean, check if fd is a tty";
  glue_define (sc, name, desc, f_os_tty_p, 1, 0);
}

void
glue_liii_os (s7_scheme* sc) {
  glue_os_arch (sc);
  glue_os_type (sc);
  glue_os_call (sc);
  glue_system (sc);
  glue_access (sc);
  glue_setenv (sc);
  glue_unsetenv (sc);
  glue_getcwd (sc);
  glue_os_temp_dir (sc);
  glue_mkdir (sc);
  glue_rmdir (sc);
  glue_remove_file (sc);
  glue_rename (sc);
  glue_chdir (sc);
  glue_listdir (sc);
  glue_getlogin (sc);
  glue_getpid (sc);
  glue_os_tty_p (sc);
  glue_console_raw_mode (sc);
  glue_console_raw_mode_state (sc);
  glue_terminal_size (sc);
  glue_console_kbhit (sc);
  glue_console_getch (sc);
}

} // namespace goldfish
