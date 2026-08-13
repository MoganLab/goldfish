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
#include <stdlib.h>
#include <string.h>

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

/* -------- define-library -------- */

static bool
r7rs_decl_named (s7_pointer decl, const char* name) {
  return (s7_is_pair (decl)) && (s7_is_symbol (s7_car (decl))) &&
         (strcmp (s7_symbol_name (s7_car (decl)), name) == 0);
}

/* An export spec is either a symbol (external == internal) or (rename old new).
 * On success stores the internal name in *internal and returns the external name;
 * on a malformed spec an error is signalled. */
static s7_pointer
r7rs_export_spec_names (s7_scheme* sc, s7_pointer spec, s7_pointer* internal) {
  if (s7_is_symbol (spec)) {
    *internal= spec;
    return spec;
  }
  if ((s7_is_pair (spec)) && (r7rs_decl_named (spec, "rename")) && (s7_list_length (sc, spec) == 3) &&
      (s7_is_symbol (s7_cadr (spec))) && (s7_is_symbol (s7_caddr (spec)))) {
    *internal= s7_cadr (spec);
    return s7_caddr (spec);
  }
  return r7rs_library_error (sc, "syntax-error", "define-library: invalid export spec ~S", spec);
}

/* Find the binding entry (a (symbol . value) pair of s7_let_to_list) for sym
 * among the own slots of env, or NULL if sym is not bound in env itself
 * (outlets are deliberately not searched: only definitions and imports of the
 * library body count). */
static s7_pointer
r7rs_entries_find (s7_pointer entries, s7_pointer sym) {
  for (s7_pointer p= entries; s7_is_pair (p); p= s7_cdr (p)) {
    s7_pointer entry= s7_car (p);
    if (s7_car (entry) == sym) return entry;
  }
  return NULL;
}

static s7_pointer
g_define_library (s7_scheme* sc, s7_pointer args) {
  s7_gc_protect_via_stack (sc, args);
  s7_pointer libname= s7_car (args);
  if (!r7rs_library_name_valid (sc, libname))
    return r7rs_library_error (sc, "wrong-type-arg",
                               "define-library: invalid library name ~S (a proper list of symbols or non-negative "
                               "integers expected)",
                               libname);

  /* the working environment: definitions and imports of the library body land here */
  s7_pointer lib_env= s7_sublet (sc, s7_rootlet (sc), s7_nil (sc));
  s7_gc_protect_via_stack (sc, lib_env);

  /* pass 1: evaluate every declaration except export in lib_env.
   * import uses the current (Scheme or C) import implementation via s7_eval;
   * begin and any other form are evaluated directly. */
  for (s7_pointer body= s7_cdr (args); s7_is_pair (body); body= s7_cdr (body)) {
    s7_pointer decl= s7_car (body);
    if (r7rs_decl_named (decl, "export")) continue;
    s7_eval (sc, decl, lib_env);
  }

  /* the own slots of lib_env, as (symbol . value) entries */
  s7_pointer entries= s7_let_to_list (sc, lib_env);
  s7_gc_protect_via_stack (sc, entries);

  /* pass 2: validate export specs before mutating any visible state.
   * A name may come from the library body's own slots, or fall through to the
   * rootlet (R7RS libraries such as (scheme base) re-export core bindings). */
  for (s7_pointer body= s7_cdr (args); s7_is_pair (body); body= s7_cdr (body)) {
    s7_pointer decl= s7_car (body);
    if (!r7rs_decl_named (decl, "export")) continue;
    for (s7_pointer specs= s7_cdr (decl); s7_is_pair (specs); specs= s7_cdr (specs)) {
      s7_pointer internal= NULL;
      r7rs_export_spec_names (sc, s7_car (specs), &internal);
      if ((!r7rs_entries_find (entries, internal)) && (!s7_is_defined (sc, s7_symbol_name (internal))))
        return r7rs_library_error (sc, "unbound-variable", "define-library: cannot export ~S: it is not defined in "
                                   "the library body",
                                   internal);
    }
  }

  s7_pointer export_env= s7_inlet (sc, s7_nil (sc));
  s7_gc_protect_via_stack (sc, export_env);

  /* make the library reachable before populating it: the registry entry and the
   * compatibility global symbol (used by the Scheme import implementation) */
  s7_hash_table_set (sc, r7rs_library_registry (sc), libname, export_env);
  char* name_str= s7_object_to_c_string (sc, libname);
  s7_define (sc, s7_rootlet (sc), s7_make_symbol (sc, name_str), export_env);
  free (name_str);

  /* populate: with no export declaration every binding is exported */
  bool export_all= true;
  for (s7_pointer body= s7_cdr (args); s7_is_pair (body); body= s7_cdr (body))
    if (r7rs_decl_named (s7_car (body), "export")) {
      export_all= false;
      break;
    }
  if (export_all) {
    for (s7_pointer p= entries; s7_is_pair (p); p= s7_cdr (p)) {
      s7_pointer entry= s7_car (p);
      s7_varlet (sc, export_env, s7_car (entry), s7_cdr (entry));
    }
  }
  else {
    for (s7_pointer body= s7_cdr (args); s7_is_pair (body); body= s7_cdr (body)) {
      s7_pointer decl= s7_car (body);
      if (!r7rs_decl_named (decl, "export")) continue;
      for (s7_pointer specs= s7_cdr (decl); s7_is_pair (specs); specs= s7_cdr (specs)) {
        s7_pointer internal= NULL;
        s7_pointer external= r7rs_export_spec_names (sc, s7_car (specs), &internal);
        s7_pointer entry   = r7rs_entries_find (entries, internal);
        /* pass 2 guarantees the name exists: in the library body's own slots,
         * or via the fall-through to the rootlet */
        s7_pointer value= entry ? s7_cdr (entry) : s7_let_ref (sc, lib_env, internal);
        /* syntactic rootlet bindings (define* etc.) cannot be let slots in s7;
         * skip them, matching the old Scheme implementation */
        if (!s7_is_syntax (value)) s7_varlet (sc, export_env, external, value);
      }
    }
  }

  s7_gc_unprotect_via_stack (sc, export_env);
  s7_gc_unprotect_via_stack (sc, entries);
  s7_gc_unprotect_via_stack (sc, lib_env);
  s7_gc_unprotect_via_stack (sc, args);
  return s7_t (sc);
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
  s7_define_macro (sc, "define-library", g_define_library, 1, 0, true,
                   "(define-library libname decl ...) defines the R7RS library libname from the given declarations "
                   "(export, import, begin, ...) and registers its exported environment");
}
