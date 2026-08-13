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
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations
// under the License.
//

/* s7_r7rs_library.c - R7RS library registry for Goldfish Scheme
 *
 * The registry maps an R7RS library name (a proper list of symbols and
 * non-negative integers, e.g. (liii base)) to the let environment holding
 * the library's exported bindings.  It lives in the rootlet variable
 * *r7rs-libraries* so that it is reachable by the garbage collector.
 */

#include "s7_r7rs_library.h"

#include <stddef.h>

#define R7RS_LIBRARIES_NAME "*r7rs-libraries*"

static s7_pointer
r7rs_library_error (s7_scheme* sc, const char* kind, const char* msg, s7_pointer arg) {
  return s7_error (sc, s7_make_symbol (sc, kind), s7_list (sc, 2, s7_make_string (sc, msg), arg));
}

static s7_pointer
r7rs_library_registry (s7_scheme* sc) {
  return s7_name_to_value (sc, R7RS_LIBRARIES_NAME);
}

/* R7RS: a library name is a proper list whose elements are symbols or
 * exact non-negative integers.  Returns true iff name is valid. */
static bool
r7rs_library_name_valid (s7_scheme* sc, s7_pointer name) {
  if ((!s7_is_null (sc, name)) && (!s7_is_pair (name))) return false; /* not a list at all */
  if (s7_list_length (sc, name) < 0) return false;                    /* improper or circular list */
  for (s7_pointer p= name; s7_is_pair (p); p= s7_cdr (p)) {
    s7_pointer elt= s7_car (p);
    if (s7_is_symbol (elt)) continue;
    if (s7_is_integer (elt) && s7_integer (elt) >= 0) continue;
    return false;
  }
  return true;
}

static s7_pointer
r7rs_library_check_name (s7_scheme* sc, const char* caller, s7_pointer name) {
  if (!r7rs_library_name_valid (sc, name))
    return r7rs_library_error (sc, "wrong-type-arg",
                               "library name should be a proper list of symbols or non-negative integers, but got ~S",
                               name);
  return NULL;
}

static s7_pointer
g_library_defined_p (s7_scheme* sc, s7_pointer args) {
  s7_pointer name= s7_car (args);
  s7_pointer err = r7rs_library_check_name (sc, "g_library-defined?", name);
  if (err) return err;
  return s7_make_boolean (sc, s7_is_let (s7_hash_table_ref (sc, r7rs_library_registry (sc), name)));
}

static s7_pointer
g_library_ref (s7_scheme* sc, s7_pointer args) {
  s7_pointer name= s7_car (args);
  s7_pointer err = r7rs_library_check_name (sc, "g_library-ref", name);
  if (err) return err;
  s7_pointer env= s7_hash_table_ref (sc, r7rs_library_registry (sc), name);
  return s7_is_let (env) ? env : s7_f (sc);
}

static s7_pointer
g_library_register (s7_scheme* sc, s7_pointer args) {
  s7_pointer name= s7_car (args);
  s7_pointer err = r7rs_library_check_name (sc, "g_library-register!", name);
  if (err) return err;
  s7_pointer env= s7_cadr (args);
  if (!s7_is_let (env))
    return r7rs_library_error (sc, "wrong-type-arg", "library environment should be a let, but got ~S", env);
  s7_hash_table_set (sc, r7rs_library_registry (sc), name, env);
  return env;
}

static s7_pointer
g_library_unregister (s7_scheme* sc, s7_pointer args) {
  s7_pointer name= s7_car (args);
  s7_pointer err = r7rs_library_check_name (sc, "g_library-unregister!", name);
  if (err) return err;
  /* s7 hash tables have no delete; storing #f marks the entry as absent
   * (g_library-defined?/g_library-ref only accept let values). */
  s7_hash_table_set (sc, r7rs_library_registry (sc), name, s7_f (sc));
  return s7_unspecified (sc);
}

void
glue_r7rs_library (s7_scheme* sc) {
  s7_define_variable (sc, R7RS_LIBRARIES_NAME, s7_make_hash_table (sc, 64));
  s7_define_safe_function (sc, "g_library-defined?", g_library_defined_p, 1, 0, false,
                           "(g_library-defined? libname) returns #t if the R7RS library named libname is registered");
  s7_define_safe_function (sc, "g_library-ref", g_library_ref, 1, 0, false,
                           "(g_library-ref libname) returns the exported environment of the R7RS library named libname, "
                           "or #f if it is not registered");
  s7_define_safe_function (sc, "g_library-register!", g_library_register, 2, 0, false,
                           "(g_library-register! libname env) registers env as the exported environment of the R7RS "
                           "library named libname, replacing any previous registration");
  s7_define_safe_function (sc, "g_library-unregister!", g_library_unregister, 1, 0, false,
                           "(g_library-unregister! libname) removes the R7RS library named libname from the registry");
}
