//
// Copyright (C) 2025 The Goldfish Scheme Authors
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
#include <tbox/tbox.h>
#include <cstring>
#include <string>
#include <vector>

using std::string;
using std::vector;

namespace goldfish {

inline void
glue_define (s7_scheme* sc, const char* name, const char* desc, s7_function f, s7_int required,
             s7_int optional) {
  s7_pointer cur_env= s7_curlet (sc);
  s7_pointer func   = s7_make_typed_function (sc, name, f, required, optional, false, desc, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, name), func);
}

static s7_pointer
f_subprocess_run (s7_scheme* sc, s7_pointer args) {
  s7_pointer cmd_p= s7_car (args);
  if (!s7_is_string (cmd_p))
    return s7_wrong_type_arg_error (sc, "g_subprocess-run", 1, cmd_p, "a string");
  const char* cmd= s7_string (cmd_p);

  s7_pointer args_list= s7_cadr (args);
  vector<string> argv_storage;
  argv_storage.push_back (cmd);
  s7_pointer iter= args_list;
  while (s7_is_pair (iter)) {
    s7_pointer arg= s7_car (iter);
    if (!s7_is_string (arg))
      return s7_wrong_type_arg_error (sc, "g_subprocess-run", 2, arg, "a string in args list");
    argv_storage.push_back (s7_string (arg));
    iter= s7_cdr (iter);
  }
  if (!s7_is_null (sc, iter))
    return s7_wrong_type_arg_error (sc, "g_subprocess-run", 2, args_list, "a list of strings");

  vector<const char*> argv;
  for (auto& s : argv_storage) argv.push_back (s.c_str ());
  argv.push_back (nullptr);

  s7_pointer options= s7_caddr (args);
  if (!s7_is_list (sc, options))
    return s7_wrong_type_arg_error (sc, "g_subprocess-run", 3, options, "an alist");

  string cwd;
  vector<string> env_storage;
  vector<const char*> envp;
  string input_data;
  bool has_input= false;
  enum class output_mode_t { out_inherit, out_discard, out_string } output_mode= output_mode_t::out_inherit;
  enum class stderr_mode_t { err_inherit, err_discard, err_stdout, err_string } stderr_mode= stderr_mode_t::err_inherit;
  double timeout_sec= -1;
  bool search= true;

  iter= options;
  while (s7_is_pair (iter)) {
    s7_pointer pair= s7_car (iter);
    if (!s7_is_pair (pair)) {
      return s7_error (sc, s7_make_symbol (sc, "type-error"),
                       s7_list (sc, 2, s7_make_string (sc, "g_subprocess-run: options must be an alist of pairs"),
                                pair));
    }
    s7_pointer key_p= s7_car (pair);
    s7_pointer val= s7_cdr (pair);

    const char* key= nullptr;
    if (s7_is_keyword (key_p)) {
      s7_pointer sym= s7_keyword_to_symbol (sc, key_p);
      key= s7_symbol_name (sym);
    } else if (s7_is_symbol (key_p)) {
      key= s7_symbol_name (key_p);
    } else {
      return s7_error (sc, s7_make_symbol (sc, "type-error"),
                       s7_list (sc, 2, s7_make_string (sc, "g_subprocess-run: option key must be keyword or symbol"),
                                key_p));
    }

    if (strcmp (key, "cwd") == 0) {
      if (s7_is_string (val)) cwd= s7_string (val);
      else if (!(s7_is_boolean (val) && !s7_boolean (sc, val))) {
        return s7_error (sc, s7_make_symbol (sc, "type-error"),
                         s7_list (sc, 2, s7_make_string (sc, "g_subprocess-run: :cwd must be string or #f"), val));
      }
    } else if (strcmp (key, "env") == 0) {
      if (s7_is_list (sc, val)) {
        s7_pointer eiter= val;
        while (s7_is_pair (eiter)) {
          s7_pointer epair= s7_car (eiter);
          if (!s7_is_pair (epair)) {
            return s7_error (sc, s7_make_symbol (sc, "type-error"),
                             s7_list (sc, 2, s7_make_string (sc, "g_subprocess-run: env entry must be a pair"),
                                      epair));
          }
          s7_pointer ekey= s7_car (epair);
          s7_pointer eval= s7_cdr (epair);
          if (!s7_is_string (ekey) || !s7_is_string (eval)) {
            return s7_error (sc, s7_make_symbol (sc, "type-error"),
                             s7_list (sc, 2,
                                      s7_make_string (sc, "g_subprocess-run: env entry must be (string . string)"),
                                      epair));
          }
          env_storage.push_back (string (s7_string (ekey)) + "=" + s7_string (eval));
          eiter= s7_cdr (eiter);
        }
      } else if (!(s7_is_boolean (val) && !s7_boolean (sc, val))) {
        return s7_error (sc, s7_make_symbol (sc, "type-error"),
                         s7_list (sc, 2, s7_make_string (sc, "g_subprocess-run: :env must be alist or #f"), val));
      }
    } else if (strcmp (key, "input") == 0) {
      if (s7_is_string (val)) {
        input_data= s7_string (val);
        has_input= true;
      } else if (!(s7_is_boolean (val) && !s7_boolean (sc, val))) {
        return s7_error (sc, s7_make_symbol (sc, "type-error"),
                         s7_list (sc, 2, s7_make_string (sc, "g_subprocess-run: :input must be string or #f"), val));
      }
    } else if (strcmp (key, "output") == 0) {
      if (s7_is_symbol (val)) {
        const char* s= s7_symbol_name (val);
        if (strcmp (s, "inherit") == 0) output_mode= output_mode_t::out_inherit;
        else if (strcmp (s, "discard") == 0) output_mode= output_mode_t::out_discard;
        else if (strcmp (s, "string") == 0) output_mode= output_mode_t::out_string;
        else {
          return s7_error (sc, s7_make_symbol (sc, "type-error"),
                           s7_list (sc, 2,
                                    s7_make_string (sc, "g_subprocess-run: :output must be 'inherit, 'discard, or 'string"),
                                    val));
        }
      } else if (s7_is_boolean (val) && !s7_boolean (sc, val)) {
        output_mode= output_mode_t::out_discard;
      } else {
        return s7_error (sc, s7_make_symbol (sc, "type-error"),
                         s7_list (sc, 2, s7_make_string (sc, "g_subprocess-run: :output must be symbol or #f"), val));
      }
    } else if (strcmp (key, "stderr") == 0) {
      if (s7_is_symbol (val)) {
        const char* s= s7_symbol_name (val);
        if (strcmp (s, "inherit") == 0) stderr_mode= stderr_mode_t::err_inherit;
        else if (strcmp (s, "discard") == 0) stderr_mode= stderr_mode_t::err_discard;
        else if (strcmp (s, "stdout") == 0) stderr_mode= stderr_mode_t::err_stdout;
        else if (strcmp (s, "string") == 0) stderr_mode= stderr_mode_t::err_string;
        else {
          return s7_error (sc, s7_make_symbol (sc, "type-error"),
                           s7_list (sc, 2,
                                    s7_make_string (
                                        sc, "g_subprocess-run: :stderr must be 'inherit, 'discard, 'stdout, or 'string"),
                                    val));
        }
      } else if (s7_is_boolean (val) && !s7_boolean (sc, val)) {
        stderr_mode= stderr_mode_t::err_discard;
      } else {
        return s7_error (sc, s7_make_symbol (sc, "type-error"),
                         s7_list (sc, 2, s7_make_string (sc, "g_subprocess-run: :stderr must be symbol or #f"), val));
      }
    } else if (strcmp (key, "timeout") == 0) {
      if (s7_is_number (val)) timeout_sec= s7_number_to_real (sc, val);
      else if (!(s7_is_boolean (val) && !s7_boolean (sc, val))) {
        return s7_error (sc, s7_make_symbol (sc, "type-error"),
                         s7_list (sc, 2, s7_make_string (sc, "g_subprocess-run: :timeout must be number or #f"), val));
      }
    } else if (strcmp (key, "search") == 0) {
      if (s7_is_boolean (val)) search= s7_boolean (sc, val);
      else {
        return s7_error (sc, s7_make_symbol (sc, "type-error"),
                         s7_list (sc, 2, s7_make_string (sc, "g_subprocess-run: :search must be boolean"), val));
      }
    }

    iter= s7_cdr (iter);
  }

  if (!search) {
    bool is_absolute= false;
#ifdef TB_CONFIG_OS_WINDOWS
    is_absolute= (cmd[0] == '\\' || cmd[0] == '/' || (strlen (cmd) > 1 && cmd[1] == ':'));
#else
    is_absolute= (cmd[0] == '/');
#endif
    if (!is_absolute) {
      return s7_error (sc, s7_make_symbol (sc, "value-error"),
                       s7_list (sc, 2, s7_make_string (sc, "g_subprocess-run: :search #f requires absolute path"),
                                cmd_p));
    }
  }

  tb_process_attr_t attr= {0};
  attr.flags= TB_PROCESS_FLAG_NO_WINDOW;

  string temp_input;
  string temp_stdout;
  string temp_stderr;
  static int temp_counter= 0;

  if (has_input) {
    tb_char_t temp_dir[TB_PATH_MAXN];
    tb_directory_temporary (temp_dir, TB_PATH_MAXN);
    temp_input= string (temp_dir) + "/gf_subprocess_" + std::to_string (++temp_counter) + ".in";
    tb_file_ref_t f= tb_file_init (temp_input.c_str (),
                                   TB_FILE_MODE_WO | TB_FILE_MODE_TRUNC | TB_FILE_MODE_CREAT);
    if (f) {
      tb_file_writ (f, (tb_byte_t const*) input_data.data (), input_data.size ());
      tb_file_exit (f);
    }
    attr.intype= TB_PROCESS_REDIRECT_TYPE_FILEPATH;
    attr.in.path= temp_input.c_str ();
    attr.inmode= TB_FILE_MODE_RO;
  }

  if (output_mode == output_mode_t::out_discard) {
    attr.outtype= TB_PROCESS_REDIRECT_TYPE_FILEPATH;
#ifdef TB_CONFIG_OS_WINDOWS
    attr.out.path= "NUL";
#else
    attr.out.path= "/dev/null";
#endif
    attr.outmode= TB_FILE_MODE_WO | TB_FILE_MODE_TRUNC | TB_FILE_MODE_CREAT;
  } else if (output_mode == output_mode_t::out_string) {
    tb_char_t temp_dir[TB_PATH_MAXN];
    tb_directory_temporary (temp_dir, TB_PATH_MAXN);
    temp_stdout= string (temp_dir) + "/gf_subprocess_" + std::to_string (++temp_counter) + ".out";
    attr.outtype= TB_PROCESS_REDIRECT_TYPE_FILEPATH;
    attr.out.path= temp_stdout.c_str ();
    attr.outmode= TB_FILE_MODE_WO | TB_FILE_MODE_TRUNC | TB_FILE_MODE_CREAT;
  }

  if (stderr_mode == stderr_mode_t::err_discard) {
    attr.errtype= TB_PROCESS_REDIRECT_TYPE_FILEPATH;
#ifdef TB_CONFIG_OS_WINDOWS
    attr.err.path= "NUL";
#else
    attr.err.path= "/dev/null";
#endif
    attr.errmode= TB_FILE_MODE_WO | TB_FILE_MODE_TRUNC | TB_FILE_MODE_CREAT;
  } else if (stderr_mode == stderr_mode_t::err_stdout) {
    if (output_mode == output_mode_t::out_string) {
      // Use a separate temp file for stderr to avoid Windows file-pointer
      // overwrite when two handles open the same file. We'll merge later.
      tb_char_t temp_dir[TB_PATH_MAXN];
      tb_directory_temporary (temp_dir, TB_PATH_MAXN);
      temp_stderr= string (temp_dir) + "/gf_subprocess_" + std::to_string (++temp_counter) + ".err";
      attr.errtype= TB_PROCESS_REDIRECT_TYPE_FILEPATH;
      attr.err.path= temp_stderr.c_str ();
      attr.errmode= TB_FILE_MODE_WO | TB_FILE_MODE_TRUNC | TB_FILE_MODE_CREAT;
    } else if (output_mode == output_mode_t::out_discard) {
      attr.errtype= TB_PROCESS_REDIRECT_TYPE_FILEPATH;
#ifdef TB_CONFIG_OS_WINDOWS
      attr.err.path= "NUL";
#else
      attr.err.path= "/dev/null";
#endif
      attr.errmode= TB_FILE_MODE_WO | TB_FILE_MODE_TRUNC | TB_FILE_MODE_CREAT;
    }
  } else if (stderr_mode == stderr_mode_t::err_string) {
    tb_char_t temp_dir[TB_PATH_MAXN];
    tb_directory_temporary (temp_dir, TB_PATH_MAXN);
    temp_stderr= string (temp_dir) + "/gf_subprocess_" + std::to_string (++temp_counter) + ".err";
    attr.errtype= TB_PROCESS_REDIRECT_TYPE_FILEPATH;
    attr.err.path= temp_stderr.c_str ();
    attr.errmode= TB_FILE_MODE_WO | TB_FILE_MODE_TRUNC | TB_FILE_MODE_CREAT;
  }

  if (!cwd.empty ()) attr.curdir= cwd.c_str ();

  if (!env_storage.empty ()) {
    for (auto& s : env_storage) envp.push_back (s.c_str ());
    envp.push_back (nullptr);
    attr.envp= (tb_char_t const**) envp.data ();
  }

  tb_process_ref_t process= tb_process_init (cmd, argv.data (), &attr);
  if (!process) {
    if (!temp_input.empty ()) tb_file_remove (temp_input.c_str ());
    if (!temp_stdout.empty ()) tb_file_remove (temp_stdout.c_str ());
    if (!temp_stderr.empty ()) tb_file_remove (temp_stderr.c_str ());
    return s7_error (sc, s7_make_symbol (sc, "os-error"),
                     s7_list (sc, 2, s7_make_string (sc, "failed to start process"), cmd_p));
  }

  tb_long_t status= 0;
  tb_long_t timeout_ms= timeout_sec > 0 ? (tb_long_t) (timeout_sec * 1000) : -1;
  tb_long_t wait_result= tb_process_wait (process, &status, timeout_ms);

  if (wait_result == 0) {
    tb_process_kill (process);
    tb_process_wait (process, &status, -1);
    tb_process_exit (process);
    if (!temp_input.empty ()) tb_file_remove (temp_input.c_str ());
    if (!temp_stdout.empty ()) tb_file_remove (temp_stdout.c_str ());
    if (!temp_stderr.empty ()) tb_file_remove (temp_stderr.c_str ());
    return s7_error (sc, s7_make_symbol (sc, "timeout-error"),
                     s7_list (sc, 2, s7_make_string (sc, "process timed out"), cmd_p));
  }

  tb_process_exit (process);

  if (!temp_input.empty ()) tb_file_remove (temp_input.c_str ());

  string stdout_str;
  if (output_mode == output_mode_t::out_string && !temp_stdout.empty ()) {
    tb_file_ref_t f= tb_file_init (temp_stdout.c_str (), TB_FILE_MODE_RO);
    if (f) {
      char buf[4096];
      tb_long_t n;
      while ((n= tb_file_read (f, (tb_byte_t*) buf, sizeof (buf))) > 0) {
        stdout_str.append (buf, n);
      }
      tb_file_exit (f);
    }
    tb_file_remove (temp_stdout.c_str ());
  }

  string stderr_str;
  if ((stderr_mode == stderr_mode_t::err_string || stderr_mode == stderr_mode_t::err_stdout) && !temp_stderr.empty ()) {
    tb_file_ref_t f= tb_file_init (temp_stderr.c_str (), TB_FILE_MODE_RO);
    if (f) {
      char buf[4096];
      tb_long_t n;
      while ((n= tb_file_read (f, (tb_byte_t*) buf, sizeof (buf))) > 0) {
        stderr_str.append (buf, n);
      }
      tb_file_exit (f);
    }
    tb_file_remove (temp_stderr.c_str ());
  }

  // Merge stderr into stdout when :stderr 'stdout was requested
  if (stderr_mode == stderr_mode_t::err_stdout && output_mode == output_mode_t::out_string) {
    stdout_str += stderr_str;
    stderr_str.clear ();
  }

  s7_pointer vs= s7_list (sc, 3,
                          s7_make_integer (sc, status),
                          s7_make_string_with_length (sc, stdout_str.data (), stdout_str.size ()),
                          s7_make_string_with_length (sc, stderr_str.data (), stderr_str.size ()));
  return s7_values (sc, vs);
}

inline void
glue_subprocess_run (s7_scheme* sc) {
  const char* name= "g_subprocess-run";
  const char* desc= "(g_subprocess-run cmd args opts) => (values exit-code stdout stderr)";
  glue_define (sc, name, desc, f_subprocess_run, 3, 0);
}

void
glue_liii_subprocess (s7_scheme* sc) {
  glue_subprocess_run (sc);
}

} // namespace goldfish
