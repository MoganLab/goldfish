// Tool registration system - dynamically load tools from gfproject.scm (DSL: (gfproject (tools ...)))
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

// gf_tool_dispatch.hpp -- project tool dispatch (the `gf <tool>` path).
// Candidate lookup and ranking live in (liii project); this file tries
// each candidate import expression and runs the tool's main.
//
// Included INSIDE namespace goldfish, after goldfish_eval_through_reader;
// it expects those declarations to be visible.  Do not include directly.

static void
goldfish_reset_captured_error_port (gf::scheme* sc) {
  gf::close_output_port (sc, gf::current_error_port (sc));
  gf::set_current_error_port (sc, gf::open_output_string (sc));
}

// Tool registration system - dynamically load tools from gfproject.scm (DSL: (gfproject (tools ...)))
// Config parsing and candidate ranking live in (liii project) (pure Scheme):
// gfproject-tool-imports returns the import expressions of candidate tools,
// best first, '() when the command is not a project tool.

static vector<string>
gfproject_tool_imports (gf::scheme* sc, const string& command) {
  vector<string> out;
  if (!g_expander_online) {
    return out;
  }
  // Embed the command as a Scheme string literal: backslash and double
  // quote are the only characters special inside a string.  NB: the forms
  // stay top-level -- the expander rejects `import` inside begin/lambda.
  string lit= "\"";
  for (char c : command) {
    if (c == '\\' || c == '"') lit+= '\\';
    lit+= c;
  }
  lit+= '"';
  gf::pointer r= goldfish_eval_through_reader (
      sc,
      "(import (liii project))"
      " (catch #t"
      "   (lambda () (gfproject-tool-imports " + lit + "))"
      "   (lambda _ '()))");
  for (gf::pointer p= r; gf::is_pair (p); p= gf::cdr (p)) {
    if (gf::is_string (gf::car (p))) out.push_back (gf::string (gf::car (p)));
  }
  return out;
}

enum class gfproject_tool_prepare_error {
  none,
  incomplete_config,
  invalid_config_value,
  missing_tool_root,
  import_failed,
  missing_main,
};

struct gfproject_tool_prepare_result {
  gfproject_tool_prepare_error error= gfproject_tool_prepare_error::none;
  string                       message;
  gf::pointer                   main_func= nullptr;
};

static string find_tool_root_by_command (const char* gf_lib, const string& command);

static gfproject_tool_prepare_result
goldfish_prepare_tool_main (gf::scheme* sc, const char* gf_lib, const string& command,
                            const string& import_expr) {
  gfproject_tool_prepare_result result;

  string tool_root= find_tool_root_by_command (gf_lib, command);

  if (tool_root.empty ()) {
    result.error  = gfproject_tool_prepare_error::missing_tool_root;
    result.message= "Error: tools/" + command + "/ directory not found.";
    return result;
  }

  gf::add_to_load_path (sc, tool_root.c_str ());

  // Tools share utility libraries (e.g. (liii goldtool-changed)) in the
  // sibling tools/common directory.  The expander resolves a tool's imports
  // at compile time, before any top-level form of the tool file runs (the
  // old per-tool `(set! *load-path* ...)` header is therefore dead), so the
  // common directory must be on the load path from the start.
  fs::path   common_dir= fs::path (tool_root).parent_path () / "common";
  std::error_code common_ec;
  if (fs::is_directory (common_dir, common_ec)) {
    gf::add_to_load_path (sc, common_dir.string ().c_str ());
  }

  gf::pointer  import_result= goldfish_eval_through_reader (sc, import_expr);
  const char* errmsg       = gf::get_output_string (sc, gf::current_error_port (sc));
  if (!import_result || ((errmsg) && (*errmsg))) {
    result.error  = gfproject_tool_prepare_error::import_failed;
    result.message= "Error " + import_expr + ":";
    return result;
  }

  gf::pointer main_func= gf::name_to_value (sc, "main");
  if ((!main_func) || (!gf::is_procedure (main_func))) {
    // The imported library's bindings live in the expander environment, not
    // the s7 rootlet: the host import macro (varlet into the rootlet) cannot
    // handle the library chain, so the expander imports into the base
    // library and subsequent expressions evaluate there.  Resolve `main`
    // through the expander too.
    main_func= goldfish_eval_through_reader (sc, "main");
  }
  if ((!main_func) || (!gf::is_procedure (main_func))) {
    result.error  = gfproject_tool_prepare_error::missing_main;
    result.message= "Error: Failed to find main function via " + import_expr + ".";
    return result;
  }

  result.main_func= main_func;
  return result;
}

static int
goldfish_finish_tool_error (gf::scheme* sc, const string& message, const char*& errmsg, gf::pointer old_port, int gc_loc,
                            bool include_scheme_error) {
  errmsg= gf::get_output_string (sc, gf::current_error_port (sc));
  if (!message.empty ()) {
    if (include_scheme_error && (errmsg) && (*errmsg)) {
      goldfish_print_prefixed_scheme_error_message (sc, message, errmsg);
    }
    else {
      cerr << message << endl;
    }
  }
  else if ((errmsg) && (*errmsg)) {
    goldfish_print_scheme_error_message (sc, errmsg);
  }
  gf::close_output_port (sc, gf::current_error_port (sc));
  gf::set_current_error_port (sc, old_port);
  if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
  return 1;
}

static int
goldfish_finish_tool_success (gf::scheme* sc, gf::pointer result, const char*& errmsg, gf::pointer old_port, int gc_loc) {
  errmsg= gf::get_output_string (sc, gf::current_error_port (sc));
  goldfish_print_scheme_error_message (sc, errmsg);
  gf::close_output_port (sc, gf::current_error_port (sc));
  gf::set_current_error_port (sc, old_port);
  if (gc_loc != -1) gf::gc_unprotect_at (sc, gc_loc);
  if (gf::is_integer (result)) {
    return static_cast<int> (gf::integer (result));
  }
  return 0;
}

static int
goldfish_run_tool_with_config (gf::scheme* sc, const char* gf_lib, const string& command, const string& import_expr,
                               const char*& errmsg, gf::pointer old_port, int gc_loc, bool allow_fallback) {
  gfproject_tool_prepare_result prepared= goldfish_prepare_tool_main (sc, gf_lib, command, import_expr);
  if (prepared.error != gfproject_tool_prepare_error::none) {
    if (allow_fallback) {
      goldfish_reset_captured_error_port (sc);
      return -1;
    }

    bool include_scheme_error= prepared.error == gfproject_tool_prepare_error::import_failed;
    return goldfish_finish_tool_error (sc, prepared.message, errmsg, old_port, gc_loc, include_scheme_error);
  }

  gf::pointer result= gf::call (sc, prepared.main_func, gf::nil (sc));
  return goldfish_finish_tool_success (sc, result, errmsg, old_port, gc_loc);
}

static gf::pointer
f_goldfish_library (gf::scheme* sc, gf::pointer args) {
  (void) args;
  string s= find_goldfish_library ();
  if (s.empty ()) return gf::f (sc);
  return gf::make_string (sc, s.c_str ());
}

static gf::pointer
f_load_path (gf::scheme* sc, gf::pointer args) {
  (void) args;
  return gf::load_path (sc);
}

static int
goldfish_run_tool (gf::scheme* sc, const char* gf_lib, const string& command, const char*& errmsg, gf::pointer old_port,
                   int gc_loc) {
  // Cheap guards before touching (liii project): paths and flags are never
  // tool commands.  Built-in names stay dispatchable -- a project may
  // override them.
  if (!g_expander_online || command.empty () || command[0] == '-' || command.find ('/') != string::npos) {
    return -1;
  }

  vector<string> imports= gfproject_tool_imports (sc, command);
  if (imports.empty ()) {
    return -1;
  }

  bool allow_builtin_fallback= command == "help" || command == "version" || command == "eval" || command == "load" ||
                               command == "repl" || command == "run";

  // Candidates are priority-ordered (local override first, library second):
  // try each in turn; the first that prepares and runs wins.
  for (size_t i= 0; i < imports.size (); ++i) {
    const bool last= i + 1 == imports.size ();
    int ret= goldfish_run_tool_with_config (sc, gf_lib, command, imports[i], errmsg, old_port, gc_loc,
                                            last ? allow_builtin_fallback : true);
    if (ret != -1) {
      return ret;
    }
    goldfish_reset_captured_error_port (sc);
  }
  return -1;
}

static string
find_tool_root_by_command (const char* gf_lib, const string& command) {
  std::error_code  ec;
  fs::path         cwd= fs::current_path (ec);
  vector<fs::path> candidates;
  if (!ec) {
    candidates.push_back (cwd / "tools" / command);
  }
  candidates.push_back (fs::path (gf_lib) / "tools" / command);
  candidates.push_back (fs::path (gf_lib).parent_path () / "tools" / command);

  for (const auto& candidate : candidates) {
    if (fs::is_directory (candidate, ec)) {
      return candidate.string ();
    }
    ec.clear ();
  }
  return "";
}

