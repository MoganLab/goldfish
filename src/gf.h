//
// gf.h -- Goldfish Foundation interface.
//
// The single C++ API surface between the goldfish runtime (VM, C++
// extension libraries, CLI) and the host Scheme runtime it is built on.
// Today the backend is s7 (src/gf.cpp calls through to s7); replacing s7
// later means swapping src/gf.cpp (and this header's types) without
// touching any caller.  Callers must include gf.h, never s7.h directly.
//
// The interface is intentionally thin and C++17-idiomatic: everything
// lives in goldfish::gf, values are held as gf::pointer (opaque), and a
// backend migration only rewrites the function bodies in gf.cpp.
//

#ifndef GOLDFISH_GF_H
#define GOLDFISH_GF_H

#include "s7.h"

#include <cstdint>
#include <cstddef>

namespace goldfish {
namespace gf {

// ---------------------------------------------------------------------------
// Types (aliases so callers do not spell s7 type names).
// ---------------------------------------------------------------------------

using scheme = s7_scheme;
using pointer = s7_pointer;
using int_ = s7_int;
using double_ = s7_double;
using function = s7_function;

// ---------------------------------------------------------------------------
// Constants.
// ---------------------------------------------------------------------------

pointer f (scheme* sc);
pointer t (scheme* sc);
pointer nil (scheme* sc);
pointer undefined (scheme* sc);
pointer unspecified (scheme* sc);
pointer eof_object (scheme* sc);
pointer make_undefined (scheme* sc, const char* name);

// ---------------------------------------------------------------------------
// Predicates.
// ---------------------------------------------------------------------------

bool is_eq (pointer a, pointer b);
bool is_pair (pointer p);
bool is_null (scheme* sc, pointer p);
bool is_list (scheme* sc, pointer p);
bool is_proper_list (scheme* sc, pointer p);
bool is_boolean (pointer p);
bool is_character (pointer p);
bool is_number (pointer p);
bool is_integer (pointer p);
bool is_real (pointer p);
bool is_string (pointer p);
bool is_symbol (pointer p);
bool is_vector (pointer p);
bool is_let (pointer p);
bool is_procedure (pointer p);
bool is_closure (pointer p);
bool is_multiple_value (pointer p);

// ---------------------------------------------------------------------------
// Pair / list.
// ---------------------------------------------------------------------------

pointer cons (scheme* sc, pointer a, pointer b);
pointer car (pointer p);
pointer cdr (pointer p);
pointer cadr (pointer p);
pointer caddr (pointer p);
pointer cadddr (pointer p);
pointer cddddr (pointer p);
void set_cdr (pointer p, pointer b);
pointer reverse (scheme* sc, pointer a);

// Build a proper list from its elements (variadic; the element count is
// inferred from the argument pack).
template <typename... Args>
inline pointer list (scheme* sc, Args... args) {
  return s7_list (sc, (int_)sizeof...(args), args...);
}

int_ list_length (scheme* sc, pointer a);
pointer list_ref (scheme* sc, pointer lst, int_ num);
pointer array_to_list (scheme* sc, int_ num_values, pointer* array);

// ---------------------------------------------------------------------------
// Booleans / characters / numbers.
// ---------------------------------------------------------------------------

bool boolean (scheme* sc, pointer x);
pointer make_boolean (scheme* sc, bool x);
uint32_t character (pointer p);
pointer make_character (scheme* sc, uint32_t c);
int_ integer (pointer p);
pointer make_integer (scheme* sc, int_ num);
double_ real (pointer p);
pointer make_real (scheme* sc, double_ num);
double_ number_to_real (scheme* sc, pointer x);

// ---------------------------------------------------------------------------
// Strings / symbols.
// ---------------------------------------------------------------------------

const char* string (pointer p);
pointer make_string (scheme* sc, const char* str);
pointer make_string_with_length (scheme* sc, const char* str, int_ len);
int_ string_length (pointer str);
char* object_to_c_string (scheme* sc, pointer obj);
const char* symbol_name (pointer p);
pointer make_symbol (scheme* sc, const char* name);

// ---------------------------------------------------------------------------
// Vectors / byte vectors.
// ---------------------------------------------------------------------------

pointer make_vector (scheme* sc, int_ len);
pointer vector_ref (scheme* sc, pointer vec, int_ index);
pointer vector_set (scheme* sc, pointer vec, int_ index, pointer a);
int_ vector_length (pointer vec);
pointer* vector_elements (pointer vec);
pointer make_byte_vector (scheme* sc, int_ len, int_ dims, int_* dim_info);
uint8_t* byte_vector_elements (pointer vec);

// ---------------------------------------------------------------------------
// Hash tables.
// ---------------------------------------------------------------------------

pointer make_hash_table (scheme* sc, int_ size);
pointer hash_table_set (scheme* sc, pointer table, pointer key, pointer value);

// ---------------------------------------------------------------------------
// Environments / globals.
// ---------------------------------------------------------------------------

pointer rootlet (scheme* sc);
pointer curlet (scheme* sc);
pointer inlet (scheme* sc, pointer bindings);
pointer varlet (scheme* sc, pointer env, pointer symbol, pointer value);
pointer let_ref (scheme* sc, pointer env, pointer sym);
void define (scheme* sc, pointer env, pointer symbol, pointer value);
void define_variable (scheme* sc, const char* name, pointer value);
pointer global_value (scheme* sc, pointer sym);
pointer name_to_value (scheme* sc, const char* name);

// ---------------------------------------------------------------------------
// Functions / calls.
// ---------------------------------------------------------------------------

pointer make_closure (scheme* sc, pointer args, pointer body, int32_t arity);
pointer closure_body (scheme* sc, pointer p);
pointer apply_function (scheme* sc, pointer fnc, pointer args);
pointer call (scheme* sc, pointer fnc, pointer args);
pointer values (scheme* sc, pointer args);
pointer make_typed_function (scheme* sc, const char* name, function f,
                             int_ required_args, int_ optional_args, bool rest_arg,
                             const char* doc, pointer signature);
pointer define_function (scheme* sc, const char* name, function fnc,
                         int_ required_args, int_ optional_args, bool rest_arg,
                         const char* doc);

// ---------------------------------------------------------------------------
// C objects.
// ---------------------------------------------------------------------------

int_ make_c_type (scheme* sc, const char* name);
pointer make_c_object_with_let (scheme* sc, int_ type, void* value, pointer let);
void* c_object_value (pointer obj);

// ---------------------------------------------------------------------------
// GC.
// ---------------------------------------------------------------------------

int_ gc_protect (scheme* sc, pointer x);
void gc_unprotect_at (scheme* sc, int_ loc);
bool gc_enabled (scheme* sc);
pointer gc_on (scheme* sc, bool on);

// ---------------------------------------------------------------------------
// Error / evaluation / misc.
// ---------------------------------------------------------------------------

pointer error (scheme* sc, pointer type, pointer info);
pointer eval (scheme* sc, pointer code, pointer e);
pointer eval_c_string (scheme* sc, const char* str);
void initialize_misc (scheme* sc);

// ---------------------------------------------------------------------------
// Ports.
// ---------------------------------------------------------------------------

pointer open_input_file (scheme* sc, const char* name, const char* mode);
void close_input_port (scheme* sc, pointer port);
pointer read_char (scheme* sc, pointer port);
pointer peek_char (scheme* sc, pointer port);
pointer current_input_port (scheme* sc);

// ---------------------------------------------------------------------------
// Hooks.
// ---------------------------------------------------------------------------

pointer hook_functions (scheme* sc, pointer hook);

} // namespace gf
} // namespace goldfish

#endif // GOLDFISH_GF_H
