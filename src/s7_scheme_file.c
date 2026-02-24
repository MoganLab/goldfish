/* s7_scheme_file.c - file library implementations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#include "s7_scheme_file.h"

static s7_pointer wrong_type_string(s7_scheme *sc, const char *caller, s7_int argn, s7_pointer arg)
{
  return s7_wrong_type_arg_error(sc, caller, argn, arg, "a string");
}

static s7_pointer wrong_type_procedure(s7_scheme *sc, const char *caller, s7_int argn, s7_pointer arg)
{
  return s7_wrong_type_arg_error(sc, caller, argn, arg, "a procedure");
}

s7_pointer g_open_input_file(s7_scheme *sc, s7_pointer args)
{
  s7_pointer name = s7_car(args);
  const char *mode = "r";
  if (!s7_is_string(name))
    return wrong_type_string(sc, "open-input-file", 1, name);
  if (!s7_is_null(sc, s7_cdr(args)))
    {
      s7_pointer m = s7_cadr(args);
      if (!s7_is_string(m))
        return wrong_type_string(sc, "open-input-file", 2, m);
      mode = s7_string(m);
    }
  return s7_open_input_file(sc, s7_string(name), mode);
}

s7_pointer g_open_output_file(s7_scheme *sc, s7_pointer args)
{
  s7_pointer name = s7_car(args);
  const char *mode = "w";
  if (!s7_is_string(name))
    return wrong_type_string(sc, "open-output-file", 1, name);
  if (!s7_is_null(sc, s7_cdr(args)))
    {
      s7_pointer m = s7_cadr(args);
      if (!s7_is_string(m))
        return wrong_type_string(sc, "open-output-file", 2, m);
      mode = s7_string(m);
    }
  return s7_open_output_file(sc, s7_string(name), mode);
}

s7_pointer g_call_with_input_file(s7_scheme *sc, s7_pointer args)
{
  s7_pointer file = s7_car(args), proc = s7_cadr(args);
  s7_pointer port, result;
  if (!s7_is_string(file))
    return wrong_type_string(sc, "call-with-input-file", 1, file);
  if (!s7_is_procedure(proc))
    return wrong_type_procedure(sc, "call-with-input-file", 2, proc);
  port = s7_open_input_file(sc, s7_string(file), "r");
  result = s7_call(sc, proc, s7_list(sc, 1, port));
  s7_close_input_port(sc, port);
  return result;
}

s7_pointer g_with_input_from_file(s7_scheme *sc, s7_pointer args)
{
  s7_pointer file = s7_car(args), thunk = s7_cadr(args);
  s7_pointer old_port, port, result;
  if (!s7_is_string(file))
    return wrong_type_string(sc, "with-input-from-file", 1, file);
  if (!s7_is_procedure(thunk))
    return wrong_type_procedure(sc, "with-input-from-file", 2, thunk);
  old_port = s7_current_input_port(sc);
  port = s7_open_input_file(sc, s7_string(file), "r");
  s7_set_current_input_port(sc, port);
  result = s7_call(sc, thunk, s7_nil(sc));
  s7_set_current_input_port(sc, old_port);
  s7_close_input_port(sc, port);
  return result;
}

s7_pointer g_call_with_output_file(s7_scheme *sc, s7_pointer args)
{
  s7_pointer file = s7_car(args), proc = s7_cadr(args);
  s7_pointer port, result;
  if (!s7_is_string(file))
    return wrong_type_string(sc, "call-with-output-file", 1, file);
  if (!s7_is_procedure(proc))
    return wrong_type_procedure(sc, "call-with-output-file", 2, proc);
  port = s7_open_output_file(sc, s7_string(file), "w");
  result = s7_call(sc, proc, s7_list(sc, 1, port));
  s7_close_output_port(sc, port);
  return result;
}

s7_pointer g_with_output_to_file(s7_scheme *sc, s7_pointer args)
{
  s7_pointer file = s7_car(args), thunk = s7_cadr(args);
  s7_pointer old_port, port, result;
  if (!s7_is_string(file))
    return wrong_type_string(sc, "with-output-to-file", 1, file);
  if (!s7_is_procedure(thunk))
    return wrong_type_procedure(sc, "with-output-to-file", 2, thunk);
  old_port = s7_current_output_port(sc);
  port = s7_open_output_file(sc, s7_string(file), "w");
  s7_set_current_output_port(sc, port);
  result = s7_call(sc, thunk, s7_nil(sc));
  s7_set_current_output_port(sc, old_port);
  s7_close_output_port(sc, port);
  return result;
}
