//
// gf.cpp -- Goldfish Foundation backend (currently s7).
//
// Every goldfish::gf::* function is a thin call through to the host Scheme
// runtime.  Replacing s7 later means rewriting this file's bodies against
// the new runtime; nothing outside goldfish::gf changes.
//

#include "gf.h"

namespace goldfish {
namespace gf {

// ---------------------------------------------------------------------------
// Constants.
// ---------------------------------------------------------------------------

pointer f (scheme* sc) { return s7_f (sc); }
pointer t (scheme* sc) { return s7_t (sc); }
pointer nil (scheme* sc) { return s7_nil (sc); }
pointer undefined (scheme* sc) { return s7_undefined (sc); }
pointer unspecified (scheme* sc) { return s7_unspecified (sc); }
pointer eof_object (scheme* sc) { return s7_eof_object (sc); }
pointer make_undefined (scheme* sc, const char* name) { return s7_make_undefined (sc, name); }

// ---------------------------------------------------------------------------
// Predicates.
// ---------------------------------------------------------------------------

bool is_eq (pointer a, pointer b) { return s7_is_eq (a, b); }
bool is_pair (pointer p) { return s7_is_pair (p); }
bool is_null (scheme* sc, pointer p) { return s7_is_null (sc, p); }
bool is_list (scheme* sc, pointer p) { return s7_is_list (sc, p); }
bool is_proper_list (scheme* sc, pointer p) { return s7_is_proper_list (sc, p); }
bool is_boolean (pointer p) { return s7_is_boolean (p); }
bool is_character (pointer p) { return s7_is_character (p); }
bool is_number (pointer p) { return s7_is_number (p); }
bool is_integer (pointer p) { return s7_is_integer (p); }
bool is_real (pointer p) { return s7_is_real (p); }
bool is_string (pointer p) { return s7_is_string (p); }
bool is_symbol (pointer p) { return s7_is_symbol (p); }
bool is_vector (pointer p) { return s7_is_vector (p); }
bool is_let (pointer p) { return s7_is_let (p); }
bool is_procedure (pointer p) { return s7_is_procedure (p); }
bool is_closure (pointer p) { return s7_gf_is_closure (p); }
bool is_multiple_value (pointer p) { return s7_is_multiple_value (p); }

// ---------------------------------------------------------------------------
// Pair / list.
// ---------------------------------------------------------------------------

pointer cons (scheme* sc, pointer a, pointer b) { return s7_cons (sc, a, b); }
pointer car (pointer p) { return s7_car (p); }
pointer cdr (pointer p) { return s7_cdr (p); }
pointer cadr (pointer p) { return s7_cadr (p); }
pointer caddr (pointer p) { return s7_caddr (p); }
pointer cadddr (pointer p) { return s7_cadddr (p); }
pointer cddddr (pointer p) { return s7_cddddr (p); }
void set_cdr (pointer p, pointer b) { s7_set_cdr (p, b); }
pointer reverse (scheme* sc, pointer a) { return s7_reverse (sc, a); }
int_ list_length (scheme* sc, pointer a) { return s7_list_length (sc, a); }
pointer list_ref (scheme* sc, pointer lst, int_ num) { return s7_list_ref (sc, lst, num); }
pointer array_to_list (scheme* sc, int_ num_values, pointer* array) {
  return s7_array_to_list (sc, num_values, array);
}

// ---------------------------------------------------------------------------
// Booleans / characters / numbers.
// ---------------------------------------------------------------------------

bool boolean (scheme* sc, pointer x) { return s7_boolean (sc, x); }
pointer make_boolean (scheme* sc, bool x) { return s7_make_boolean (sc, x); }
uint32_t character (pointer p) { return s7_character (p); }
pointer make_character (scheme* sc, uint32_t c) { return s7_make_character (sc, c); }
int_ integer (pointer p) { return s7_integer (p); }
pointer make_integer (scheme* sc, int_ num) { return s7_make_integer (sc, num); }
double_ real (pointer p) { return s7_real (p); }
pointer make_real (scheme* sc, double_ num) { return s7_make_real (sc, num); }
double_ number_to_real (scheme* sc, pointer x) { return s7_number_to_real (sc, x); }

// ---------------------------------------------------------------------------
// Strings / symbols.
// ---------------------------------------------------------------------------

const char* string (pointer p) { return s7_string (p); }
pointer make_string (scheme* sc, const char* str) { return s7_make_string (sc, str); }
pointer make_string_with_length (scheme* sc, const char* str, int_ len) {
  return s7_make_string_with_length (sc, str, len);
}
int_ string_length (pointer str) { return s7_string_length (str); }
char* object_to_c_string (scheme* sc, pointer obj) { return s7_object_to_c_string (sc, obj); }
const char* symbol_name (pointer p) { return s7_symbol_name (p); }
pointer make_symbol (scheme* sc, const char* name) { return s7_make_symbol (sc, name); }

// ---------------------------------------------------------------------------
// Vectors / byte vectors.
// ---------------------------------------------------------------------------

pointer make_vector (scheme* sc, int_ len) { return s7_make_vector (sc, len); }
pointer vector_ref (scheme* sc, pointer vec, int_ index) { return s7_vector_ref (sc, vec, index); }
pointer vector_set (scheme* sc, pointer vec, int_ index, pointer a) {
  return s7_vector_set (sc, vec, index, a);
}
int_ vector_length (pointer vec) { return s7_vector_length (vec); }
pointer* vector_elements (pointer vec) { return s7_vector_elements (vec); }
pointer make_byte_vector (scheme* sc, int_ len, int_ dims, int_* dim_info) {
  return s7_make_byte_vector (sc, len, dims, dim_info);
}
uint8_t* byte_vector_elements (pointer vec) { return s7_byte_vector_elements (vec); }

// ---------------------------------------------------------------------------
// Hash tables.
// ---------------------------------------------------------------------------

pointer make_hash_table (scheme* sc, int_ size) { return s7_make_hash_table (sc, size); }
pointer hash_table_set (scheme* sc, pointer table, pointer key, pointer value) {
  return s7_hash_table_set (sc, table, key, value);
}

// ---------------------------------------------------------------------------
// Environments / globals.
// ---------------------------------------------------------------------------

pointer rootlet (scheme* sc) { return s7_rootlet (sc); }
pointer curlet (scheme* sc) { return s7_curlet (sc); }
pointer inlet (scheme* sc, pointer bindings) { return s7_inlet (sc, bindings); }
pointer varlet (scheme* sc, pointer env, pointer symbol, pointer value) {
  return s7_varlet (sc, env, symbol, value);
}
pointer let_ref (scheme* sc, pointer env, pointer sym) { return s7_let_ref (sc, env, sym); }
void define (scheme* sc, pointer env, pointer symbol, pointer value) {
  s7_define (sc, env, symbol, value);
}
void define_variable (scheme* sc, const char* name, pointer value) {
  s7_define_variable (sc, name, value);
}
pointer global_value (scheme* sc, pointer sym) { return s7_gf_global_value (sc, sym); }
pointer name_to_value (scheme* sc, const char* name) { return s7_name_to_value (sc, name); }

// ---------------------------------------------------------------------------
// Functions / calls.
// ---------------------------------------------------------------------------

pointer make_closure (scheme* sc, pointer args, pointer body, int32_t arity) {
  return s7_gf_make_closure (sc, args, body, arity);
}
pointer closure_body (scheme* sc, pointer p) { return s7_closure_body (sc, p); }
pointer apply_function (scheme* sc, pointer fnc, pointer args) {
  return s7_apply_function (sc, fnc, args);
}
pointer call (scheme* sc, pointer fnc, pointer args) { return s7_call (sc, fnc, args); }
pointer values (scheme* sc, pointer args) { return s7_values (sc, args); }
pointer make_typed_function (scheme* sc, const char* name, function f,
                             int_ required_args, int_ optional_args, bool rest_arg,
                             const char* doc, pointer signature) {
  return s7_make_typed_function (sc, name, f, required_args, optional_args, rest_arg,
                                 doc, signature);
}
pointer define_function (scheme* sc, const char* name, function fnc,
                         int_ required_args, int_ optional_args, bool rest_arg,
                         const char* doc) {
  return s7_define_function (sc, name, fnc, required_args, optional_args, rest_arg, doc);
}

// ---------------------------------------------------------------------------
// C objects.
// ---------------------------------------------------------------------------

int_ make_c_type (scheme* sc, const char* name) { return s7_make_c_type (sc, name); }
pointer make_c_object_with_let (scheme* sc, int_ type, void* value, pointer let) {
  return s7_make_c_object_with_let (sc, type, value, let);
}
void* c_object_value (pointer obj) { return s7_c_object_value (obj); }

// ---------------------------------------------------------------------------
// GC.
// ---------------------------------------------------------------------------

int_ gc_protect (scheme* sc, pointer x) { return s7_gc_protect (sc, x); }
void gc_unprotect_at (scheme* sc, int_ loc) { s7_gc_unprotect_at (sc, loc); }
bool gc_enabled (scheme* sc) { return s7_gc_enabled (sc); }
pointer gc_on (scheme* sc, bool on) { return s7_gc_on (sc, on); }

// ---------------------------------------------------------------------------
// Error / evaluation / misc.
// ---------------------------------------------------------------------------

pointer error (scheme* sc, pointer type, pointer info) { return s7_error (sc, type, info); }
pointer eval (scheme* sc, pointer code, pointer e) { return s7_eval (sc, code, e); }
pointer eval_c_string (scheme* sc, const char* str) { return s7_eval_c_string (sc, str); }
void initialize_misc (scheme* sc) { s7_initialize_misc (sc); }

// ---------------------------------------------------------------------------
// Ports.
// ---------------------------------------------------------------------------

pointer open_input_file (scheme* sc, const char* name, const char* mode) {
  return s7_open_input_file (sc, name, mode);
}
void close_input_port (scheme* sc, pointer port) { s7_close_input_port (sc, port); }
pointer read_char (scheme* sc, pointer port) { return s7_read_char (sc, port); }
pointer peek_char (scheme* sc, pointer port) { return s7_peek_char (sc, port); }
pointer current_input_port (scheme* sc) { return s7_current_input_port (sc); }

// ---------------------------------------------------------------------------
// Hooks.
// ---------------------------------------------------------------------------

pointer hook_functions (scheme* sc, pointer hook) { return s7_hook_functions (sc, hook); }

} // namespace gf
} // namespace goldfish
