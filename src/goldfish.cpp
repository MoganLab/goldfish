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
#include <cstdlib>
#include <string>

using namespace goldfish;

int
main (int argc, char** argv) {
#ifdef TB_CONFIG_OS_WINDOWS
  SetConsoleOutputCP (65001);
#endif
  setlocale (LC_ALL, "C.UTF-8");
  if (argc >= 2 && std::string(argv[1]) == "test") {
    setenv("GOLDFISH_NO_VM_DEFS", "1", 1);
  }
  std::string gf_lib_dir= goldfish::find_goldfish_library ();
  const char* gf_lib    = gf_lib_dir.c_str ();
  gf::scheme*  sc        = goldfish::init_goldfish_scheme (gf_lib);
  int         ret       = goldfish::repl_for_community_edition (sc, argc, argv);
  gf::pointer  exit_hook = gf::name_to_value (sc, "*exit-hook*");
  gf::pointer  funcs     = gf::hook_functions (sc, exit_hook);
  if (gf::is_pair (funcs)) {
    gf::pointer args= gf::cons (sc, gf::make_integer (sc, ret == 0 ? EXIT_SUCCESS : EXIT_FAILURE), gf::nil (sc));
    gf::apply_function (sc, exit_hook, args);
  }
  return ret;
}
