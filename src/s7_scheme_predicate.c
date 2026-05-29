/* s7_scheme_predicate.c - predicate implementations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#include "s7_scheme_predicate.h"
#include "s7_internal_helpers.h"

s7_pointer g_not(s7_scheme *sc, s7_pointer args)
{
  return((s7_car(args) == s7_f(sc)) ? s7_t(sc) : s7_f(sc));
}

s7_pointer g_is_boolean(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_boolean(p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_boolean_symbol(sc)));
}

s7_pointer g_is_unspecified(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_unspecified(sc, p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_unspecified_symbol(sc)));
}

s7_pointer g_is_number(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_number(p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_number_symbol(sc)));
}

s7_pointer g_is_integer(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_integer(p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_integer_symbol(sc)));
}

s7_pointer g_is_real(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_real(p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_real_symbol(sc)));
}

s7_pointer g_is_complex(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_number(p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_complex_symbol(sc)));
}

s7_pointer g_is_rational(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_rational(p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_rational_symbol(sc)));
}

s7_pointer g_is_keyword(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_keyword(p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_keyword_symbol(sc)));
}

s7_pointer g_is_procedure(s7_scheme *sc, s7_pointer args)
{
  return(s7_make_boolean(sc, s7_is_procedure(s7_car(args))));
}

s7_pointer g_is_dilambda(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_dilambda(p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_dilambda_symbol(sc)));
}

s7_pointer g_is_sequence(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7i_is_sequence(p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_sequence_symbol(sc)));
}

s7_pointer g_is_symbol(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_symbol(p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_symbol_symbol(sc)));
}

s7_pointer g_is_input_port(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_input_port(sc, p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_input_port_symbol(sc)));
}

s7_pointer g_is_output_port(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_output_port(sc, p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_output_port_symbol(sc)));
}

s7_pointer g_is_macro(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_macro(sc, p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_macro_symbol(sc)));
}

s7_pointer g_is_undefined(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7i_is_undefined(p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_undefined_symbol(sc)));
}

s7_pointer g_is_eof_object(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7i_is_eof(p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_eof_object_symbol(sc)));
}

s7_pointer g_is_byte(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_integer(p) && s7_integer(p) >= 0 && s7_integer(p) < 256) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_byte_symbol(sc)));
}

s7_pointer g_is_float(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7i_is_t_real(p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_float_symbol(sc)));
}

s7_pointer g_is_random_state(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_random_state(p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_random_state_symbol(sc)));
}

s7_pointer g_is_continuation(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7i_is_continuation(p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_continuation_symbol(sc)));
}

s7_pointer g_is_iterator(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_iterator(p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_iterator_symbol(sc)));
}
