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
#include <tbox/tbox.h>
#include <cstring>
#include <string>
#include <vector>

namespace goldfish {

using std::string;
using std::vector;

s7_pointer
f_subprocess_run_values (s7_scheme* sc, s7_pointer args) {
  const char* cmd_c = s7_string (s7_car (args));
  args = s7_cdr (args);

  const char* cwd = nullptr;
  if (s7_is_pair (args) && s7_is_string (s7_car (args))) {
    cwd = s7_string (s7_car (args));
    args = s7_cdr (args);
  }
  else if (s7_is_pair (args)) {
    args = s7_cdr (args);
  }

  vector<string> env_storage;
  vector<const char*> envp;
  if (s7_is_pair (args) && s7_is_pair (s7_car (args))) {
    s7_pointer env_alist = s7_car (args);
    while (s7_is_pair (env_alist)) {
      s7_pointer item = s7_car (env_alist);
      if (s7_is_pair (item)) {
        const char* key = s7_string (s7_car (item));
        s7_pointer val = s7_cdr (item);
        const char* val_c = s7_is_string (val) ? s7_string (val) : "";
        env_storage.push_back (string (key) + "=" + val_c);
      }
      env_alist = s7_cdr (env_alist);
    }
    for (auto& s : env_storage) {
      envp.push_back (s.c_str ());
    }
    envp.push_back (nullptr);
    args = s7_cdr (args);
  }
  else if (s7_is_pair (args)) {
    args = s7_cdr (args);
  }

  const char* input = nullptr;
  size_t input_len = 0;
  if (s7_is_pair (args) && s7_is_string (s7_car (args))) {
    input = s7_string (s7_car (args));
    input_len = strlen (input);
    args = s7_cdr (args);
  }
  else if (s7_is_pair (args)) {
    args = s7_cdr (args);
  }

  tb_long_t timeout_ms = -1;
  if (s7_is_pair (args) && s7_is_integer (s7_car (args))) {
    timeout_ms = s7_integer (s7_car (args)) * 1000;
    args = s7_cdr (args);
  }
  else if (s7_is_pair (args)) {
    args = s7_cdr (args);
  }

  bool stdout_discard = false;
  const char* stdout_path = nullptr;
  if (s7_is_pair (args)) {
    s7_pointer stdout_val = s7_car (args);
    if (s7_is_symbol (stdout_val) && strcmp (s7_symbol_name (stdout_val), "discard") == 0) {
      stdout_discard = true;
    }
    else if (s7_is_string (stdout_val)) {
      stdout_path = s7_string (stdout_val);
    }
    args = s7_cdr (args);
  }

  bool stderr_to_stdout = false;
  bool stderr_discard = false;
  const char* stderr_path = nullptr;
  if (s7_is_pair (args)) {
    s7_pointer stderr_val = s7_car (args);
    if (s7_is_symbol (stderr_val)) {
      const char* sym = s7_symbol_name (stderr_val);
      if (strcmp (sym, "stdout") == 0) {
        stderr_to_stdout = true;
      }
      else if (strcmp (sym, "discard") == 0) {
        stderr_discard = true;
      }
    }
    else if (s7_is_string (stderr_val)) {
      stderr_path = s7_string (stderr_val);
    }
    args = s7_cdr (args);
  }

  const char* stdin_path = nullptr;
  bool stdin_null = false;
  if (s7_is_pair (args)) {
    s7_pointer stdin_val = s7_car (args);
    if (s7_is_symbol (stdin_val) && strcmp (s7_symbol_name (stdin_val), "null") == 0) {
      stdin_null = true;
    }
    else if (s7_is_string (stdin_val)) {
      stdin_path = s7_string (stdin_val);
    }
  }

  tb_process_attr_t attr = {tb_null};
  attr.flags = TB_PROCESS_FLAG_NO_WINDOW;

  if (cwd) attr.curdir = cwd;
  if (!envp.empty ()) attr.envp = (tb_char_t const**) envp.data ();

  tb_pipe_file_ref_t out_pipe[2] = {tb_null};
  if (stdout_path) {
    attr.outtype = TB_PROCESS_REDIRECT_TYPE_FILEPATH;
    attr.out.path = stdout_path;
    attr.outmode = TB_FILE_MODE_RW | TB_FILE_MODE_CREAT | TB_FILE_MODE_TRUNC;
  }
  else if (!stdout_discard) {
    tb_size_t mode[2] = {TB_PIPE_MODE_RO, TB_PIPE_MODE_WO};
    tb_pipe_file_init_pair (out_pipe, mode, 0);
    attr.outtype = TB_PROCESS_REDIRECT_TYPE_PIPE;
    attr.out.pipe = out_pipe[1];
  }

  tb_pipe_file_ref_t err_pipe[2] = {tb_null};
  if (stderr_path) {
    attr.errtype = TB_PROCESS_REDIRECT_TYPE_FILEPATH;
    attr.err.path = stderr_path;
    attr.errmode = TB_FILE_MODE_RW | TB_FILE_MODE_CREAT | TB_FILE_MODE_TRUNC;
  }
  else if (stderr_to_stdout && out_pipe[1]) {
    attr.errtype = TB_PROCESS_REDIRECT_TYPE_PIPE;
    attr.err.pipe = out_pipe[1];
  }
  else if (!stderr_discard) {
    tb_size_t mode[2] = {TB_PIPE_MODE_RO, TB_PIPE_MODE_WO};
    tb_pipe_file_init_pair (err_pipe, mode, 0);
    attr.errtype = TB_PROCESS_REDIRECT_TYPE_PIPE;
    attr.err.pipe = err_pipe[1];
  }

  tb_pipe_file_ref_t in_pipe[2] = {tb_null};
  if (stdin_path) {
    attr.intype = TB_PROCESS_REDIRECT_TYPE_FILEPATH;
    attr.in.path = stdin_path;
    attr.inmode = TB_FILE_MODE_RO;
  }
  else if (stdin_null) {
    tb_size_t mode[2] = {TB_PIPE_MODE_RO, TB_PIPE_MODE_WO};
    tb_pipe_file_init_pair (in_pipe, mode, 0);
    attr.intype = TB_PROCESS_REDIRECT_TYPE_PIPE;
    attr.in.pipe = in_pipe[0];
    tb_pipe_file_exit (in_pipe[1]);
  }
  else if (input) {
    tb_size_t mode[2] = {TB_PIPE_MODE_RO, TB_PIPE_MODE_WO};
    tb_pipe_file_init_pair (in_pipe, mode, 0);
    attr.intype = TB_PROCESS_REDIRECT_TYPE_PIPE;
    attr.in.pipe = in_pipe[0];
    tb_pipe_file_write (in_pipe[1], (tb_byte_t*) input, input_len);
    tb_pipe_file_exit (in_pipe[1]);
  }

  tb_char_t const* sh_argv[] = {"sh", "-c", cmd_c, tb_null};
  tb_process_ref_t process = tb_process_init ("/bin/sh", sh_argv, &attr);

  if (out_pipe[1]) tb_pipe_file_exit (out_pipe[1]);
  if (err_pipe[1]) tb_pipe_file_exit (err_pipe[1]);

  string stdout_str;
  string stderr_str;
  tb_long_t status = -1;

  if (process) {
    tb_long_t wait_result = tb_process_wait (process, &status, timeout_ms);

    if (wait_result == 0) {
      tb_process_kill (process);
      tb_process_wait (process, &status, -1);
      status = -1;
    }

    if (out_pipe[0]) {
      char buf[4096];
      tb_long_t n;
      while ((n = tb_pipe_file_read (out_pipe[0], (tb_byte_t*) buf, sizeof (buf) - 1)) > 0) {
        buf[n] = '\0';
        stdout_str.append (buf);
      }
      tb_pipe_file_exit (out_pipe[0]);
    }

    if (err_pipe[0]) {
      char buf[4096];
      tb_long_t n;
      while ((n = tb_pipe_file_read (err_pipe[0], (tb_byte_t*) buf, sizeof (buf) - 1)) > 0) {
        buf[n] = '\0';
        stderr_str.append (buf);
      }
      tb_pipe_file_exit (err_pipe[0]);
    }

    tb_process_exit (process);
  }

  s7_pointer out_s7 = s7_make_string (sc, stdout_str.c_str ());
  s7_pointer err_s7 = s7_make_string (sc, stderr_str.c_str ());
  s7_pointer code_s7 = s7_make_integer (sc, (s7_int) status);
  return s7_values (sc, s7_cons (sc, out_s7, s7_cons (sc, err_s7, s7_cons (sc, code_s7, s7_nil (sc)))));
}

} // namespace goldfish
