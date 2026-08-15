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
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied See the
// License for the specific language governing permissions and limitations
// under the License.
//

//
// gf_glue.hpp - declarative s7 glue for C++ library modules.
//
// Turns the repetitive "unpack s7_pointer args, call a plain C++ function,
// pack the result back into s7" boilerplate into a single declaration:
//
//   namespace goldfish {
//     std::string os_arch () { return TB_ARCH_STRING; }       // pure C++
//     int os_call (const std::string& cmd) { ...; return ret; }
//     bool access (const std::string& path, int mode) { ...; }
//     std::vector<std::string> listdir (const std::string& path) { ...; }
//   }
//
//   GF_GLUE (g_os-arch, "(g_os-arch) => string",            os_arch);
//   GF_GLUE (g_os-call, "(g_os-call string) => int",        os_call);
//   GF_GLUE (g_access,  "(g_access string integer) => bool", access);
//   GF_GLUE (g_listdir, "(g_listdir string) => vector",     listdir);
//
// GF_GLUE expands to two static functions:
//   * f_##fn: s7_function that unpacks args (via s7_traits<T>::in, with
//     an s7 error on type mismatch), calls fn, packs the result (via
//     s7_traits<R>::out).  Exceptions thrown by fn become s7 errors.
//   * glue_##fn: registers the function under Scheme-Name with the exact
//     arity of fn (required = number of parameters, no optionals).
// The module's glue entry point then calls each glue_##fn, exactly as
// before, so existing call sites in goldfish.hpp are unchanged.
//
// Supported argument/return types (extend s7_traits below as needed):
//   std::string, s7_int, int, bool, double,
//   std::vector<std::string> (-> s7 vector of strings),
//   s7_pointer (pass-through), void (-> (values)),
//   std::vector<s7_byte> (-> bytevector).
//
// The plain C++ functions must live in namespace goldfish so the macro
// expands inside that namespace.  Functions needing the host (http
// callbacks, reader eval) fall back to a handwritten s7_function.

#ifndef GOLDFISH_GLUE_HPP
#define GOLDFISH_GLUE_HPP

#include "s7.h"

#include <cstring>
#include <cstdint>
#include <functional>
#include <optional>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

namespace goldfish {

// ---------------------------------------------------------------------------
// gf_error: convert a C++ exception (or a direct call) into an s7 error.
// ---------------------------------------------------------------------------

inline s7_pointer
gf_error (s7_scheme* sc, const char* kind, const std::string& msg) {
  return s7_error (sc, s7_make_symbol (sc, kind), s7_make_string (sc, msg.c_str ()));
}

// ---------------------------------------------------------------------------
// s7_traits<T>: convert T <-> s7_pointer.
// ---------------------------------------------------------------------------

template <typename T> struct s7_traits;

template <> struct s7_traits<std::string> {
  static const char* type_name () { return "string"; }
  static bool accepts (s7_pointer p) { return s7_is_string (p); }
  static std::string in (s7_scheme* sc, s7_pointer p) {
    if (!s7_is_string (p))
      gf_error (sc, "type-error", "expected a string argument");
    return std::string (s7_string (p));
  }
  static s7_pointer out (s7_scheme* sc, const std::string& v) {
    return s7_make_string (sc, v.c_str ());
  }
};

template <> struct s7_traits<char32_t> {
  static const char* type_name () { return "character"; }
  static bool accepts (s7_pointer p) { return s7_is_character (p); }
  static char32_t in (s7_scheme* sc, s7_pointer p) {
    if (!s7_is_character (p))
      gf_error (sc, "type-error", "expected a character argument");
    return (char32_t) s7_character (p);
  }
  static s7_pointer out (s7_scheme* sc, char32_t v) {
    return s7_make_character (sc, (uint32_t) v);
  }
};

template <> struct s7_traits<s7_int> {
  static const char* type_name () { return "integer"; }
  static bool accepts (s7_pointer p) { return s7_is_integer (p); }
  static s7_int in (s7_scheme* sc, s7_pointer p) {
    if (!s7_is_integer (p))
      gf_error (sc, "type-error", "expected an integer argument");
    return s7_integer (p);
  }
  static s7_pointer out (s7_scheme* sc, s7_int v) { return s7_make_integer (sc, v); }
};

template <> struct s7_traits<int> {
  static const char* type_name () { return "integer"; }
  static bool accepts (s7_pointer p) { return s7_is_integer (p); }
  static int in (s7_scheme* sc, s7_pointer p) {
    return (int) s7_traits<s7_int>::in (sc, p);
  }
  static s7_pointer out (s7_scheme* sc, int v) { return s7_make_integer (sc, v); }
};

template <> struct s7_traits<bool> {
  static const char* type_name () { return "boolean"; }
  static bool accepts (s7_pointer p) { return s7_is_boolean (p); }
  static bool in (s7_scheme* sc, s7_pointer p) {
    if (!s7_is_boolean (p))
      gf_error (sc, "type-error", "expected a boolean argument");
    return s7_boolean (sc, p);
  }
  static s7_pointer out (s7_scheme* sc, bool v) { return s7_make_boolean (sc, v); }
};

template <> struct s7_traits<double> {
  static const char* type_name () { return "number"; }
  static bool accepts (s7_pointer p) { return s7_is_number (p); }
  static double in (s7_scheme* sc, s7_pointer p) {
    if (!s7_is_number (p))
      gf_error (sc, "type-error", "expected a number argument");
    return s7_number_to_real (sc, p);
  }
  static s7_pointer out (s7_scheme* sc, double v) { return s7_make_real (sc, v); }
};

template <> struct s7_traits<std::vector<std::string>> {
  static const char* type_name () { return "vector-of-strings"; }
  static bool accepts (s7_pointer p) { return s7_is_vector (p); }
  static std::vector<std::string> in (s7_scheme* sc, s7_pointer p) {
    std::vector<std::string> r;
    if (!s7_is_vector (p))
      gf_error (sc, "type-error", "expected a vector argument");
    s7_int n= s7_vector_length (p);
    r.reserve (n);
    for (s7_int i= 0; i < n; i++)
      r.push_back (std::string (s7_string (s7_vector_ref (sc, p, i))));
    return r;
  }
  static s7_pointer out (s7_scheme* sc, const std::vector<std::string>& v) {
    s7_pointer r= s7_make_vector (sc, (s7_int) v.size ());
    for (size_t i= 0; i < v.size (); i++)
      s7_vector_set (sc, r, (s7_int) i, s7_make_string (sc, v[i].c_str ()));
    return r;
  }
};

// std::vector<uint8_t>: bytevector <-> C++ byte vector (copy semantics, so
// the pure C++ functions never touch s7 memory).
template <> struct s7_traits<std::vector<uint8_t>> {
  static const char* type_name () { return "bytevector"; }
  static bool accepts (s7_pointer p) { return s7_is_byte_vector (p); }
  static std::vector<uint8_t> in (s7_scheme* sc, s7_pointer p) {
    std::vector<uint8_t> r;
    if (!s7_is_byte_vector (p))
      gf_error (sc, "type-error", "expected a bytevector argument");
    s7_int n= s7_vector_length (p);
    const uint8_t* elems= s7_byte_vector_elements (p);
    r.assign (elems, elems + n);
    return r;
  }
  static s7_pointer out (s7_scheme* sc, const std::vector<uint8_t>& v) {
    s7_int n= (s7_int) v.size ();
    s7_pointer r= s7_make_byte_vector (sc, n, 1, NULL);
    if (n > 0)
      memcpy (s7_byte_vector_elements (r), v.data (), n);
    return r;
  }
};

// s7_pointer: pass-through (no conversion).  For functions that need the raw
// s7 object (e.g. to hand it to s7_call later).
template <> struct s7_traits<s7_pointer> {
  static const char* type_name () { return "object"; }
  static s7_pointer in (s7_scheme* sc, s7_pointer p) { return p; }
  static s7_pointer out (s7_scheme* sc, s7_pointer v) { return v; }
};

// void: s7_unspecified (the (values) value).
template <> struct s7_traits<void> {
  static const char* type_name () { return "void"; }
  static s7_pointer out (s7_scheme* sc) { return s7_unspecified (sc); }
};

// std::optional<T>: absent -> #f, present -> s7_traits<T>::out.
template <typename T>
struct s7_traits<std::optional<T>> {
  static const char* type_name () { return s7_traits<T>::type_name (); }
  static bool accepts (s7_pointer p) { return s7_traits<T>::accepts (p); }
  static std::optional<T> in (s7_scheme* sc, s7_pointer p) {
    if (s7_is_boolean (p) && !s7_boolean (sc, p)) return std::nullopt;
    return std::make_optional (s7_traits<T>::in (sc, p));
  }
  static s7_pointer out (s7_scheme* sc, const std::optional<T>& v) {
    if (!v.has_value ()) return s7_f (sc);
    return s7_traits<T>::out (sc, *v);
  }
};

// gf_strlist: std::vector<std::string> mapped to an s7 LIST of strings
// (distinct from the vector-of-strings trait above).  Output only.
struct gf_strlist {
  std::vector<std::string> items;
};

template <> struct s7_traits<gf_strlist> {
  static const char* type_name () { return "list-of-strings"; }
  static s7_pointer out (s7_scheme* sc, const gf_strlist& v) {
    s7_pointer head= s7_cons (sc, s7_nil (sc), s7_nil (sc));
    s7_gc_protect_via_stack (sc, head);
    s7_pointer tail= head;
    for (const auto& s : v.items) {
      s7_set_cdr (tail, s7_cons (sc, s7_make_string (sc, s.c_str ()), s7_nil (sc)));
      tail= s7_cdr (tail);
    }
    s7_gc_unprotect_via_stack (sc, head);
    return s7_cdr (head);
  }
};

// std::variant<A, B, ...>: accept any of the alternatives; the first
// alternative whose type check succeeds wins.
template <typename... Ts>
struct s7_traits<std::variant<Ts...>> {
  static const char* type_name () { return "one-of"; }
  static std::variant<Ts...> in (s7_scheme* sc, s7_pointer p) {
    std::variant<Ts...> result;
    bool ok= false;
    (void) std::initializer_list<int>{
        (try_alternative<Ts> (sc, p, result, ok), 0)...};
    if (!ok) {
      std::string msg= "expected one of: ";
      bool first= true;
      ((msg += (first ? "" : " / "), msg += s7_traits<Ts>::type_name (), first= false), ...);
      gf_error (sc, "type-error", msg);
    }
    return result;
  }
  template <typename T>
  static void try_alternative (s7_scheme* sc, s7_pointer p, std::variant<Ts...>& result, bool& ok) {
    if (ok) return;
    if (s7_traits<T>::accepts (p)) {
      result= std::variant<Ts...> (std::in_place_type<T>, s7_traits<T>::in (sc, p));
      ok= true;
    }
  }
};

// ---------------------------------------------------------------------------
// fn_traits<R(Args...)>: recover the parameter and result types of fn.
// ---------------------------------------------------------------------------

template <typename T> struct gf_fn_traits;

template <typename R, typename... Args>
struct gf_fn_traits<R (*) (Args...)> {
  using result = R;
  static constexpr size_t arity = sizeof...(Args);
  template <size_t I> using arg = std::tuple_element_t<I, std::tuple<Args...>>;
  using make_tuple = std::tuple<std::decay_t<Args>...>;
};

// ---------------------------------------------------------------------------
// Argument unpacking: walk the s7 arg list, converting each element via
// s7_traits<T>::in, building a tuple of plain C++ values.
// ---------------------------------------------------------------------------

template <typename T>
struct gf_arg_reader {
  static std::decay_t<T> read (s7_scheme* sc, s7_pointer p) {
    return s7_traits<std::decay_t<T>>::in (sc, p);
  }
};

// Reads a single argument after advancing the list I times.
template <size_t I, typename T>
std::decay_t<T> gf_read_arg (s7_scheme* sc, s7_pointer args) {
  s7_pointer p= args;
  for (size_t i= 0; i < I; i++)
    p= s7_cdr (p);
  return gf_arg_reader<T>::read (sc, s7_car (p));
}

template <typename Tuple, size_t... I>
Tuple gf_read_all (s7_scheme* sc, s7_pointer args, std::index_sequence<I...>) {
  return Tuple (gf_read_arg<I, std::tuple_element_t<I, Tuple>> (sc, args)...);
}

template <typename Fn>
auto gf_read_args (s7_scheme* sc, s7_pointer args) {
  using traits= gf_fn_traits<Fn>;
  using tuple = typename traits::make_tuple;
  return gf_read_all<tuple> (sc, args, std::make_index_sequence<traits::arity> ());
}

// ---------------------------------------------------------------------------
// Result packing: call fn(tuple...), convert the result via s7_traits<R>::out.
// Exceptions are translated into s7 errors.
// ---------------------------------------------------------------------------

template <typename Fn, typename Tuple, size_t... I>
s7_pointer gf_call (s7_scheme* sc, Fn fn, Tuple& t, std::index_sequence<I...>, std::false_type /*not void*/) {
  using R= typename gf_fn_traits<Fn>::result;
  try {
    return s7_traits<R>::out (sc, fn (std::get<I> (t)...));
  } catch (const std::exception& e) {
    return gf_error (sc, "value-error", e.what ());
  }
}

template <typename Fn, typename Tuple, size_t... I>
s7_pointer gf_call (s7_scheme* sc, Fn fn, Tuple& t, std::index_sequence<I...>, std::true_type /*void*/) {
  try {
    fn (std::get<I> (t)...);
    return s7_unspecified (sc);
  } catch (const std::exception& e) {
    return gf_error (sc, "value-error", e.what ());
  }
}

template <typename Fn, typename Tuple>
s7_pointer gf_dispatch (s7_scheme* sc, Fn fn, Tuple& t) {
  using traits = gf_fn_traits<Fn>;
  using is_void= std::is_same<typename traits::result, void>;
  return gf_call (sc, fn, t, std::make_index_sequence<traits::arity> (), is_void{});
}

// ---------------------------------------------------------------------------
// GF_GLUE(Scheme_Name, Desc, fn) and GF_GLUE0 (no-arg functions).
//
// Expands to:
//   static s7_pointer f_##fn (s7_scheme* sc, s7_pointer args);
//   static void glue_##fn (s7_scheme* sc);
//   f_##fn:   unpacks required args (traits::arity of them), calls fn, packs.
//   glue_##fn: s7_define_typed_function under Scheme_Name with that arity.
// ---------------------------------------------------------------------------

#define GF_GLUE_0(Scheme_Name, Desc, fn)                                           \
  static s7_pointer f_##fn (s7_scheme* sc, s7_pointer args) {                      \
    (void) args;                                                                   \
    using traits= goldfish::gf_fn_traits<decltype (&fn)>;                          \
    (void) sizeof (traits);                                                        \
    return goldfish::gf_call (sc, &fn, std::tuple<>{},                             \
                              std::make_index_sequence<0> (),                      \
                              std::is_same<typename traits::result, void>{});      \
  }                                                                                \
  static void glue_##fn (s7_scheme* sc) {                                          \
    s7_define_typed_function (sc, Scheme_Name, f_##fn, 0, 0, false, Desc, NULL);   \
  }

#define GF_GLUE(Scheme_Name, Desc, fn)                                             \
  static s7_pointer f_##fn (s7_scheme* sc, s7_pointer args) {                      \
    using traits = goldfish::gf_fn_traits<decltype (&fn)>;                         \
    using is_void= std::is_same<typename traits::result, void>;                    \
    auto t       = goldfish::gf_read_args<decltype (&fn)> (sc, args);              \
    return goldfish::gf_call (sc, &fn, t,                                          \
                              std::make_index_sequence<traits::arity> (), is_void{}); \
  }                                                                                \
  static void glue_##fn (s7_scheme* sc) {                                          \
    s7_define_typed_function (sc, Scheme_Name, f_##fn,                             \
                              (s7_int) goldfish::gf_fn_traits<decltype (&fn)>::arity, \
                              0, false, Desc, NULL);                               \
  }

} // namespace goldfish

#endif // GOLDFISH_GLUE_HPP
