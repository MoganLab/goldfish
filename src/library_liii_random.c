/* library_liii_random.c - random and random-state implementations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#include "s7_internal.h"

#include "library_liii_random.h"
#include "s7_internal_helpers.h"
#include "s7_scheme_base.h"
#include "s7_scheme_let.h"

#include <math.h>

#ifndef make_integer
#define make_integer(Sc, N) s7_make_integer (Sc, N)
#endif
#ifndef make_integer_unchecked
#define make_integer_unchecked(Sc, N) s7_make_integer (Sc, N)
#endif
#ifndef make_real
#define make_real(Sc, X) s7_make_real (Sc, X)
#endif
#ifndef make_complex
#define make_complex(Sc, R, I) s7_make_complex (Sc, R, I)
#endif
#ifndef make_ratio
#define make_ratio(Sc, N, D) s7_make_ratio (Sc, N, D)
#endif
#ifndef wrap_integer
#define wrap_integer(Sc, N) s7i_wrap_integer (Sc, N)
#endif
#ifndef s7_integer_clamped_if_gmp
#define s7_integer_clamped_if_gmp(Sc, P) s7i_integer_clamped_if_gmp (Sc, P)
#endif
#ifndef int_one
#define int_one s7i_int_one (sc)
#endif
#ifndef int_two
#define int_two s7_make_integer (sc, 2)
#endif
#ifndef a_random_state_object_string
#define a_random_state_object_string s7i_a_random_state_object_string ()
#endif
#ifndef a_number_string
#define a_number_string s7i_a_number_string ()
#endif
#ifndef it_is_negative_string
#define it_is_negative_string s7i_it_is_negative_string ()
#endif
#ifndef opt3_int
#define opt3_int(P) s7i_opt3_int (P)
#endif
#ifndef list_2
#define list_2(Sc, A, B) list_2_unchecked (Sc, A, B)
#endif
#ifndef numerator
#define numerator(p) s7_numerator (p)
#endif
#ifndef denominator
#define denominator(p) s7_denominator (p)
#endif
#ifndef fraction
#define fraction(p) (((s7_double) numerator (p)) / ((s7_double) denominator (p)))
#endif
#ifndef real_part
#define real_part(p) s7_real_part (p)
#endif
#ifndef imag_part
#define imag_part(p) s7_imag_part (p)
#endif
#ifndef S7_INT64_MAX
#define S7_INT64_MAX 9223372036854775807LL
#endif

/* -------------------------------- random-state -------------------------------- */
/* random numbers.  The simple version used in clm.c is probably adequate, but here I'll use Marsaglia's MWC algorithm.
 *     (random num) -> a number (0..num), if num == 0 return 0, use global default state
 *     (random num state) -> same but use this state
 *     (random-state seed) -> make a new state
 *   to save the current seed, use copy, to save it across load, random-state->list and list->random-state.
 *   random-state? returns #t if its arg is one of these guys
 */

s7_pointer
random_state_copy (s7_scheme* sc, s7_pointer args) {
  s7_pointer new_r, obj= car (args);
  if (!is_random_state (obj)) return (sc->F);
  new_cell (sc, new_r, T_RANDOM_STATE);
  random_seed (new_r) = random_seed (obj);
  random_carry (new_r)= random_carry (obj);
  return (new_r);
}

bool
random_state_equal (s7_scheme* sc, s7_pointer x, s7_pointer y, shared_info_t* ci) {
  (void) sc;
  (void) ci;
  return ((x == y) ||
          ((is_random_state (y)) && (random_seed (x) == random_seed (y)) && (random_carry (x) == random_carry (y))));
}

s7_pointer
random_state_to_let (s7_scheme* sc, s7_pointer rs) {
  if (!sc->seed_symbol) {
    sc->seed_symbol = make_symbol (sc, "seed", 4);
    sc->carry_symbol= make_symbol (sc, "carry", 5);
  }
  return (internal_inlet (sc, 8, sc->value_symbol, rs, sc->type_symbol, sc->is_random_state_symbol, sc->seed_symbol,
                          make_integer (sc, random_seed (rs)), sc->carry_symbol, make_integer (sc, random_carry (rs))));
}

#ifndef MWC_32
#define MWC_32 1
#endif

s7_pointer
s7_random_state (s7_scheme* sc, s7_pointer args) {
  s7_pointer r1, r2, rs;
  s7_int     i1, i2; /* actually want s7_uint here -- we lose the sign bit? */
  if (is_null (args)) return (sc->default_random_state);

  r1= car (args);
  if (!s7_is_integer (r1))
    return (method_or_bust (sc, r1, sc->random_state_symbol, args, sc->type_names[T_INTEGER], 1));
  i1= integer (r1);
#if MWC_32
  if (i1 < 0) out_of_range_error_nr (sc, sc->random_state_symbol, int_one, r1, it_is_negative_string);
#endif
  if (is_null (cdr (args))) {
    new_cell (sc, rs, T_RANDOM_STATE);
    random_seed (rs) = (s7_uint) i1;
    random_carry (rs)= 1675393560; /* should this be dependent on the seed? */
    return (rs);
  }

  r2= cadr (args);
  if (!s7_is_integer (r2))
    return (method_or_bust (sc, r2, sc->random_state_symbol, args, sc->type_names[T_INTEGER], 2));
  i2= integer (r2);
#if MWC_32
  if (i2 < 0) out_of_range_error_nr (sc, sc->random_state_symbol, int_two, r2, it_is_negative_string);
#endif
  new_cell (sc, rs, T_RANDOM_STATE);
  random_seed (rs) = (s7_uint) i1;
  random_carry (rs)= (s7_uint) i2;
  return (rs);
}

s7_pointer
random_state_getter (s7_scheme* sc, s7_pointer r, s7_int loc) {
  if (loc == 0) return (make_integer (sc, random_seed (r)));
  if (loc == 1) return (make_integer (sc, random_carry (r)));
  return (sc->F);
}

s7_pointer
random_state_setter (s7_scheme* sc, s7_pointer r, s7_int loc, s7_pointer val) {
  if (is_t_integer (val)) {
    s7_int i= s7_integer_clamped_if_gmp (sc, val);
    if (loc == 0) random_seed (r)= i;
    if (loc == 1) random_carry (r)= i;
  }
  return (sc->F);
}

/* -------------------------------- random-state? -------------------------------- */
/* g_is_random_state is now defined in s7_scheme_predicate.c */

bool
s7_is_random_state (s7_pointer r) {
  return (type (r) == T_RANDOM_STATE);
}

/* -------------------------------- random-state->list -------------------------------- */
s7_pointer
s7_random_state_to_list (s7_scheme* sc, s7_pointer args) {
  s7_pointer r= (is_null (args)) ? sc->default_random_state : car (args);
  if (!is_random_state (r))
    return (method_or_bust (sc, r, sc->random_state_to_list_symbol, args, a_random_state_object_string, 1));
  return (list_2 (sc, make_integer (sc, random_seed (r)), make_integer_unchecked (sc, random_carry (r))));
}

void
s7_set_default_random_state (s7_scheme* sc, s7_int seed, s7_int carry) {
  s7_pointer rs;
  new_cell (sc, rs, T_RANDOM_STATE);
  random_seed (rs)        = (s7_uint) seed;
  random_carry (rs)       = (s7_uint) carry;
  sc->default_random_state= rs;
}

/* -------------------------------- random -------------------------------- */
double
s7_next_random (s7_pointer r) {
#if MWC_32
/* The multiply-with-carry generator for 32-bit integers:
 *        x(n)=a*x(n-1) + carry mod 2^32
 * Choose multiplier a from this list:
 *   1791398085 1929682203 1683268614 1965537969 1675393560 1967773755 1517746329 1447497129 1655692410 1606218150
 *   2051013963 1075433238 1557985959 1781943330 1893513180 1631296680 2131995753 2083801278 1873196400 1554115554
 * ( or any 'a' for which both a*2^32-1 and a*2^31-1 are prime)
 */
#define RAN_MULT 2131995753UL
  double  result;
  s7_uint temp    = random_seed (r) * RAN_MULT + random_carry (r);
  random_seed (r) = (temp & 0xffffffffUL);
  random_carry (r)= (temp >> 32);
  result          = (double) ((uint32_t) (random_seed (r))) / 4294967295.5;
#else
  /* 64-bit MWC from https://prng.di.unimi.it/#shootout */
  double            result;
#define MWC_A1 0xffebb71d94fcdaf9
  s7_uint           x= random_seed (r), c= random_carry (r);
  const __uint128_t t= MWC_A1 * (__uint128_t) x + c;
  random_seed (r)    = t;
  random_carry (r)   = t >> 64;
  result             = ((long_double) (random_seed (r)) / (long_double) 4294967296.0) / (long_double) 4294967295.5;
#endif

  return (result);
}

s7_pointer
g_random (s7_scheme* sc, s7_pointer args) {
  s7_pointer r, num;

  if (is_null (cdr (args))) r= sc->default_random_state;
  else {
    r= cadr (args);
    if (!is_random_state (r)) return (method_or_bust (sc, r, sc->random_symbol, args, a_random_state_object_string, 2));
  }
  num= car (args);
  switch (type (num)) {
  case T_INTEGER:
    return (make_integer (sc, (s7_int) (integer (num) * s7_next_random (r))));
  case T_RATIO: {
    const s7_double x= fraction (num);
    s7_double       error;
    s7_int          numer= 0, denom= 1;
    if ((x < 1.0e-10) && (x > -1.0e-10)) {
      if (numerator (num) < -10) numer= -(s7_int) (floor (-numerator (num) * s7_next_random (r)));
      else if (numerator (num) > 10) numer= (s7_int) floor (numerator (num) * s7_next_random (r));
      else {
        s7_int diff= S7_INT64_MAX - denominator (num);
        numer      = numerator (num);
        if (diff < 100) return (make_ratio (sc, numer, denominator (num)));
        denom= denominator (num) + (s7_int) floor (diff * s7_next_random (r));
        return (make_ratio_with_div_check (sc, sc->random_symbol, numer, denom));
      }
      return (make_ratio (sc, numer, denominator (num)));
    }
    error= ((x < 1e-6) && (x > -1e-6)) ? 1e-18 : 1e-12;
    c_rationalize (x * s7_next_random (r), error, &numer, &denom);
    return (make_simpler_ratio_or_integer (sc, numer, denom));
  }
  case T_REAL:
    return (make_real (sc, real (num) * s7_next_random (r)));
  case T_COMPLEX:
    return (make_complex (sc, real_part (num) * s7_next_random (r), imag_part (num) * s7_next_random (r)));
  default:
    return (method_or_bust (sc, num, sc->random_symbol, args, a_number_string, 1));
  }
  return (sc->F);
}

s7_double
s7_random (s7_scheme* sc, s7_pointer state) {
  return (s7_next_random ((state) ? state : sc->default_random_state));
}

s7_double
random_d_7d (s7_scheme* sc, s7_double x) {
  return (x * s7_next_random (sc->default_random_state));
}

s7_int
random_i_7i (s7_scheme* sc, s7_int i) {
  return ((s7_int) (i * s7_next_random (sc->default_random_state)));
}

s7_pointer
g_random_i (s7_scheme* sc, s7_pointer args) {
  return (make_integer (sc, (s7_int) (integer (car (args)) * s7_next_random (sc->default_random_state))));
}

s7_pointer
g_random_f (s7_scheme* sc, s7_pointer args) {
  return (make_real (sc, real (car (args)) * s7_next_random (sc->default_random_state)));
}

s7_pointer
g_random_1 (s7_scheme* sc, s7_pointer args) {
  s7_pointer num= car (args), r= sc->default_random_state;
  if (is_t_integer (num)) return (make_integer (sc, (s7_int) (integer (num) * s7_next_random (r))));
  if (is_t_real (num)) return (make_real (sc, real (num) * s7_next_random (r)));
  return (g_random (sc, args));
}

s7_pointer
random_p_p (s7_scheme* sc, s7_pointer num) {
  if (is_t_integer (num))
    return (make_integer (sc, (s7_int) (integer (num) * s7_next_random (sc->default_random_state))));
  if (is_t_real (num)) return (make_real (sc, real (num) * s7_next_random (sc->default_random_state)));
  return (g_random (sc, set_plist_1 (sc, num)));
}

s7_pointer
random_p_p_wrapped (s7_scheme* sc, s7_pointer num) {
  if (is_t_integer (num))
    return (wrap_integer (sc, (s7_int) (integer (num) * s7_next_random (sc->default_random_state))));
  if (is_t_real (num)) return (wrap_real (sc, real (num) * s7_next_random (sc->default_random_state)));
  return (g_random (sc, set_plist_1 (sc, num)));
}

s7_pointer
random_chooser (s7_scheme* sc, s7_pointer func, int32_t args, s7_pointer expr) {
  if (args == 1) {
    s7_pointer arg1= cadr (expr);
    if (is_t_integer (arg1)) return (sc->random_i);
    return ((is_t_real (arg1)) ? sc->random_f : sc->random_1);
  }
  return (func);
}

s7_pointer
g_add_i_random (s7_scheme* sc, s7_pointer args) {
  s7_int x= integer (car (args)), y= opt3_int (args); /* cadadr */
  return (make_integer (
      sc, x + (s7_int) (y * s7_next_random (sc->default_random_state)))); /* (+ -1 (random 1)) -- placement of the
                                                                             (s7_int) cast matters! */
}
