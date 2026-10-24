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

#include "goldfish.hpp"
#include <clocale>
#include <string>

#ifdef TB_CONFIG_OS_WINDOWS
#include <shellapi.h>

// Windows 上 CRT 的 argv 使用系统 ANSI 代码页（中文系统为 GBK），中文参数会
// 以非法 UTF-8 字节进入 Scheme 层（如拼入 HTTP 请求体触发 JSON 解析错误）。
// 这里改用 Wide API 获取 Unicode 命令行并转为 UTF-8，使 (command-line)
// 在任何系统代码页下都返回合法 UTF-8 字符串。
static std::vector<std::string> g_utf8_args;
static std::vector<char*>       g_utf8_argv;

static void
init_windows_utf8_argv (int& argc, char**& argv) {
  int     argc_w= 0;
  LPWSTR* argv_w= CommandLineToArgvW (GetCommandLineW (), &argc_w);
  if (!argv_w || argc_w <= 0) {
    if (argv_w) LocalFree (argv_w);
    return;
  }
  g_utf8_args.reserve ((size_t) argc_w);
  for (int i= 0; i < argc_w; ++i) {
    int len= WideCharToMultiByte (CP_UTF8, 0, argv_w[i], -1, NULL, 0, NULL, NULL);
    if (len <= 0) continue;
    std::string s ((size_t) (len - 1), '\0');
    WideCharToMultiByte (CP_UTF8, 0, argv_w[i], -1, &s[0], len, NULL, NULL);
    g_utf8_args.push_back (std::move (s));
  }
  LocalFree (argv_w);
  g_utf8_argv.reserve (g_utf8_args.size ());
  for (auto& s : g_utf8_args)
    g_utf8_argv.push_back (&s[0]);
  argc= (int) g_utf8_argv.size ();
  argv= g_utf8_argv.data ();
}
#endif

int
main (int argc, char** argv) {
#ifdef TB_CONFIG_OS_WINDOWS
  SetConsoleCP (65001);
  SetConsoleOutputCP (65001);
  init_windows_utf8_argv (argc, argv);
#endif
  setlocale (LC_ALL, "C.UTF-8");
  std::string gf_lib_dir= goldfish::find_goldfish_library ();
  const char* gf_lib    = gf_lib_dir.c_str ();
  s7_scheme*  sc        = goldfish::init_goldfish_scheme (gf_lib);
  int         ret       = goldfish::repl_for_community_edition (sc, argc, argv);
  s7_pointer  exit_hook = s7_name_to_value (sc, "*exit-hook*");
  s7_pointer  funcs     = s7_hook_functions (sc, exit_hook);
  if (s7_is_pair (funcs)) {
    s7_pointer args= s7_cons (sc, s7_make_integer (sc, ret == 0 ? EXIT_SUCCESS : EXIT_FAILURE), s7_nil (sc));
    s7_apply_function (sc, exit_hook, args);
  }
  return ret;
}
