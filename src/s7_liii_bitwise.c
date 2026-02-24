/* s7_liii_bitwise.c - 
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#include "s7_liii_bitwise.h"

/* -------------------------------- logior -------------------------------- */

static bool has_two_int_args(s7_scheme *sc, s7_pointer expr)
{
  /* this needs to be split into 2 calls on has_one_int, and maybe support (apply int-func...), also the global business is wrong if it is currently shadowed */
  const s7_pointer arg1 = cadr(expr), arg2 = caddr(expr);
  if (is_t_integer(arg1))
    {
      if (is_t_integer(arg2)) return(true);
      if ((is_pair(arg2)) && (is_symbol(car(arg2))) && (is_defined_global(car(arg2))) && (is_c_function(global_value(car(arg2)))))
	{
	  s7_pointer sig = c_function_signature(global_value(car(arg2)));
	  if ((is_pair(sig)) && (car(sig) == sc->is_integer_symbol)) return(true);
	}
      return(false);
    }
  if ((is_pair(arg1)) && (is_symbol(car(arg1))) && (is_defined_global(car(arg1))) && (is_c_function(global_value(car(arg1)))))
    {
      s7_pointer sig = c_function_signature(global_value(car(arg1)));
      if ((is_pair(sig)) && ((car(sig) == sc->is_integer_symbol) || (car(sig) == sc->is_byte_symbol)))
	{
	  if (is_t_integer(arg2)) return(true);
	  if ((is_pair(arg2)) && (is_symbol(car(arg2))) && (is_defined_global(car(arg2))) && (is_c_function(global_value(car(arg2)))))
	    {
	      sig = c_function_signature(global_value(car(arg2)));
	      if ((is_pair(sig)) && ((car(sig) == sc->is_integer_symbol) || (car(sig) == sc->is_byte_symbol))) return(true);
	    }}}
  return(false);
}

#if WITH_GMP
static s7_pointer big_logior(s7_scheme *sc, s7_int start, s7_pointer args)
{
  mpz_set_si(sc->mpz_1, start);
  for (s7_pointer x = args; is_pair(x); x = cdr(x))
    {
      s7_pointer i = car(x);
      switch (type(i))
	{
	case T_BIG_INTEGER:
	  mpz_ior(sc->mpz_1, sc->mpz_1, big_integer(i));
	  break;
	case T_INTEGER:
	  mpz_set_si(sc->mpz_2, integer(i));
	  mpz_ior(sc->mpz_1, sc->mpz_1, sc->mpz_2);
	  break;
	default:
	  if (!is_integer_via_method(sc, i))
	    wrong_type_error_nr(sc, sc->logior_symbol, position_of(x, args), i, sc->type_names[T_INTEGER]);
	  return(method_or_bust(sc, i, sc->logior_symbol,
				set_ulist_1(sc, mpz_to_integer(sc, sc->mpz_1), x),
				sc->type_names[T_INTEGER], position_of(x, args)));
	}}
  return(mpz_to_integer(sc, sc->mpz_1));
}
#endif

s7_pointer g_logior(s7_scheme *sc, s7_pointer args)
{
  #define H_logior "(logior int32_t ...) returns the OR of its integer arguments (the bits that are on in any of the arguments)"
  #define Q_logior sc->pcl_i

  s7_int result = 0;
  for (s7_pointer x = args; is_pair(x); x = cdr(x))
    {
#if WITH_GMP
      if (is_t_big_integer(car(x)))
	return(big_logior(sc, result, x));
#endif
      if (!is_t_integer(car(x)))
	return(method_or_bust(sc, car(x), sc->logior_symbol,
			      (x == args) ? x : set_ulist_1(sc, make_integer(sc, result), x),
			      sc->type_names[T_INTEGER], position_of(x, args)));
      result |= integer(car(x));
    }
  return(make_integer(sc, result));
}

s7_int logior_i_ii(s7_int i1, s7_int i2) {return(i1 | i2);}
s7_int logior_i_iii(s7_int i1, s7_int i2, s7_int i3) {return(i1 | i2 | i3);}

s7_pointer g_logior_ii(s7_scheme *sc, s7_pointer args) {return(make_integer(sc, integer(car(args)) | integer(cadr(args))));}
s7_pointer g_logior_2(s7_scheme *sc, s7_pointer args)
{
  s7_pointer arg1 = car(args), arg2 = cadr(args);
  if ((is_t_integer(arg1)) && (is_t_integer(arg2)))
    return(make_integer(sc, integer(arg1) | integer(arg2)));
  return(g_logior(sc, args));
}

s7_pointer logior_chooser(s7_scheme *sc, s7_pointer func, int32_t args, s7_pointer expr)
{
  if (args == 2)
    {
      if (has_two_int_args(sc, expr)) return(sc->logior_ii);
      return(sc->logior_2);
    }
  return(func);
}


/* -------------------------------- logxor -------------------------------- */
#if WITH_GMP
static s7_pointer big_logxor(s7_scheme *sc, s7_int start, s7_pointer args)
{
  mpz_set_si(sc->mpz_1, start);
  for (s7_pointer x = args; is_pair(x); x = cdr(x))
    {
      const s7_pointer i = car(x);
      switch (type(i))
	{
	case T_BIG_INTEGER:
	  mpz_xor(sc->mpz_1, sc->mpz_1, big_integer(i));
	  break;
	case T_INTEGER:
	  mpz_set_si(sc->mpz_2, integer(i));
	  mpz_xor(sc->mpz_1, sc->mpz_1, sc->mpz_2);
	  break;
	default:
	  if (!is_integer_via_method(sc, i))
	    wrong_type_error_nr(sc, sc->logxor_symbol, position_of(x, args), i, sc->type_names[T_INTEGER]);
	  return(method_or_bust(sc, i, sc->logxor_symbol,
				set_ulist_1(sc, mpz_to_integer(sc, sc->mpz_1), x),
				sc->type_names[T_INTEGER], position_of(x, args)));
	}}
  return(mpz_to_integer(sc, sc->mpz_1));
}
#endif

s7_pointer g_logxor(s7_scheme *sc, s7_pointer args)
{
  #define H_logxor "(logxor int32_t ...) returns the XOR of its integer arguments (the bits that are on in an odd number of the arguments)"
  #define Q_logxor sc->pcl_i

  s7_int result = 0;
  for (s7_pointer x = args; is_pair(x); x = cdr(x))
    {
#if WITH_GMP
      if (is_t_big_integer(car(x)))
	return(big_logxor(sc, result, x));
#endif
      if (!is_t_integer(car(x)))
	return(method_or_bust(sc, car(x), sc->logxor_symbol,
			      (x == args) ? x : set_ulist_1(sc, make_integer(sc, result), x), /* not (result == 0), (logxor 1 1) is 0 */
			      sc->type_names[T_INTEGER], position_of(x, args)));
      result ^= integer(car(x));
    }
  return(make_integer(sc, result));
}

s7_int logxor_i_ii(s7_int i1, s7_int i2) {return(i1 ^ i2);}
s7_int logxor_i_iii(s7_int i1, s7_int i2, s7_int i3) {return(i1 ^ i2 ^ i3);}

s7_pointer g_logxor_2(s7_scheme *sc, s7_pointer args)
{
  s7_pointer arg1 = car(args), arg2 = cadr(args);
  if ((is_t_integer(arg1)) && (is_t_integer(arg2)))
    return(make_integer(sc, integer(arg1) ^ integer(arg2)));
  return(g_logxor(sc, args));
}

s7_pointer logxor_chooser(s7_scheme *sc, s7_pointer func, int32_t args, s7_pointer expr) {return((args == 2) ? sc->logxor_2 : func);}


/* -------------------------------- logand -------------------------------- */
#if WITH_GMP
static s7_pointer big_logand(s7_scheme *sc, s7_int start, s7_pointer args)
{
  mpz_set_si(sc->mpz_1, start);
  for (s7_pointer x = args; is_pair(x); x = cdr(x))
    {
      const s7_pointer i = car(x);
      switch (type(i))
	{
	case T_BIG_INTEGER:
	  mpz_and(sc->mpz_1, sc->mpz_1, big_integer(i));
	  break;
	case T_INTEGER:
	  mpz_set_si(sc->mpz_2, integer(i));
	  mpz_and(sc->mpz_1, sc->mpz_1, sc->mpz_2);
	  break;
	default:
	  if (!is_integer_via_method(sc, i))
	    wrong_type_error_nr(sc, sc->logand_symbol, position_of(x, args), i, sc->type_names[T_INTEGER]);
	  return(method_or_bust(sc, i, sc->logand_symbol,
				set_ulist_1(sc, mpz_to_integer(sc, sc->mpz_1), x),
				sc->type_names[T_INTEGER], position_of(x, args)));
	}}
  return(mpz_to_integer(sc, sc->mpz_1));
}
#endif

s7_pointer g_logand(s7_scheme *sc, s7_pointer args)
{
  #define H_logand "(logand int32_t ...) returns the AND of its integer arguments (the bits that are on in every argument)"
  #define Q_logand sc->pcl_i

  s7_int result = -1;
  for (s7_pointer x = args; is_pair(x); x = cdr(x))
    {
#if WITH_GMP
      if (is_t_big_integer(car(x)))
	return(big_logand(sc, result, x));
#endif
      if (!is_t_integer(car(x)))
	return(method_or_bust(sc, car(x), sc->logand_symbol,
			      (x == args) ? x : set_ulist_1(sc, make_integer(sc, result), x),
			      sc->type_names[T_INTEGER], position_of(x, args)));
      result &= integer(car(x));
    }
  return(make_integer(sc, result));
}

s7_int logand_i_ii(s7_int i1, s7_int i2) {return(i1 & i2);}
s7_int logand_i_iii(s7_int i1, s7_int i2, s7_int i3) {return(i1 & i2 & i3);}

s7_pointer g_logand_ii(s7_scheme *sc, s7_pointer args) {return(make_integer(sc, integer(car(args)) & integer(cadr(args))));}
s7_pointer g_logand_2(s7_scheme *sc, s7_pointer args)
{
  s7_pointer arg1 = car(args), arg2 = cadr(args);
  if ((is_t_integer(arg1)) && (is_t_integer(arg2)))
    return(make_integer(sc, integer(arg1) & integer(arg2)));
  return(g_logand(sc, args));
}

s7_pointer logand_chooser(s7_scheme *sc, s7_pointer func, int32_t args, s7_pointer expr)
{
  if (args == 2)
    {
      if (has_two_int_args(sc, expr)) return(sc->logand_ii);
      return(sc->logand_2);
    }
  return(func);
}


/* -------------------------------- lognot -------------------------------- */
s7_pointer g_lognot(s7_scheme *sc, s7_pointer args)
{
  #define H_lognot "(lognot num) returns the negation of num (its complement, the bits that are not on): (lognot 0) -> -1"
  #define Q_lognot sc->pcl_i

  const s7_pointer x = car(args);
  if (is_t_integer(x))
    return(make_integer(sc, ~integer(x)));

#if WITH_GMP
  if (is_t_big_integer(x))
    {
      mpz_com(sc->mpz_1, big_integer(x));
      return(mpz_to_integer(sc, sc->mpz_1));
    }
#endif
  return(sole_arg_method_or_bust(sc, x, sc->lognot_symbol, args, sc->type_names[T_INTEGER]));
}

s7_int lognot_i_i(s7_int i1) {return(~i1);}


/* -------------------------------- logbit? -------------------------------- */
/* logbit?  CL is (logbitp index int) using 2^index, but that order strikes me as backwards
 *   at least gmp got the arg order right!
 */

static s7_pointer g_logbit(s7_scheme *sc, s7_pointer args)
{
  #define H_logbit "(logbit? int index) returns #t if the index-th bit is on in int, otherwise #f. The argument \
order here follows gmp, and is the opposite of the CL convention.  (logbit? int bit) is the same as (not (zero? (logand int (ash 1 bit))))."
  #define Q_logbit s7_make_circular_signature(sc, 1, 2, sc->is_boolean_symbol, sc->is_integer_symbol)

  const s7_pointer x = car(args), y = cadr(args);
  s7_int index;      /* index in gmp is mp_bitcnt which is an unsigned long int */

  if (!s7_is_integer(x))
    return(method_or_bust(sc, x, sc->logbit_symbol, args, sc->type_names[T_INTEGER], 1));
  if (!s7_is_integer(y))
    return(method_or_bust(sc, y, sc->logbit_symbol, args, sc->type_names[T_INTEGER], 2));

  index = s7_integer_clamped_if_gmp(sc, y);
  if (index < 0)
    out_of_range_error_nr(sc, sc->logbit_symbol, int_two, y, it_is_negative_string);

#if WITH_GMP
  if (is_t_big_integer(x))
    return(make_boolean(sc, (mpz_tstbit(big_integer(x), index) != 0)));
#endif

  if (index >= S7_INT_BITS)           /* not sure about the >: (logbit? -1 64) ?? */
    return(make_boolean(sc, integer(x) < 0));
  /* (zero? (logand most-positive-fixnum (ash 1 63))) -> ash argument 2, 63, is out of range (shift is too large)
   *   so logbit? has a wider range than the logand/ash shuffle above.
   */

  /* all these s7_ints are necessary, else C turns it into an int, gets confused about signs etc */
  return(make_boolean(sc, ((((s7_int)(1LL << (s7_int)index)) & (s7_int)integer(x)) != 0)));
}

static bool logbit_b_7ii(s7_scheme *sc, s7_int i1, s7_int i2)
{
  if (i2 < 0)
    {
      out_of_range_error_nr(sc, sc->logbit_symbol, int_two, wrap_integer(sc, i1), it_is_negative_string);
      return(false);
    }
  if (i2 >= S7_INT_BITS) return(i1 < 0);
  return((((s7_int)(1LL << (s7_int)i2)) & (s7_int)i1) != 0);
}

static bool logbit_b_7pp(s7_scheme *sc, s7_pointer i1, s7_pointer i2)
{
  if (is_t_integer(i1))
    {
      if (is_t_integer(i2))
	return(logbit_b_7ii(sc, integer(i1), integer(i2)));
      return(method_or_bust(sc, i2, sc->logbit_symbol, set_plist_2(sc, i1, i2), sc->type_names[T_INTEGER], 2) != sc->F);
    }
#if WITH_GMP
  return(g_logbit(sc, set_plist_2(sc, i1, i2)));
#else
  return(method_or_bust(sc, i1, sc->logbit_symbol, set_plist_2(sc, i1, i2), sc->type_names[T_INTEGER], 1) != sc->F);
#endif
}


/* -------------------------------- ash -------------------------------- */
static s7_int c_ash(s7_scheme *sc, s7_int arg1, s7_int arg2)
{
  if (arg2 >= S7_INT_BITS)
    {
      if ((arg1 == -1) && (arg2 == 63))   /* (ash -1 63): most-negative-fixnum */
	return(S7_INT64_MIN);
      if (arg1 == 0) return(0);
      out_of_range_error_nr(sc, sc->ash_symbol, int_two, wrap_integer(sc, arg2), it_is_too_large_string);
    }
  if (arg2 < 0)
    {
      if (arg2 < -S7_INT_BITS)
	return((arg1 < 0) ? -1 : 0);      /* (ash -31 -100) */
      return(arg1 >> -arg2);
    }
  /* (ash 9223372036854775807 1) -> -2, anyone using ash must know something about bits */
  return(arg1 << arg2);
}

s7_pointer g_ash(s7_scheme *sc, s7_pointer args)
{
  #define H_ash "(ash i1 i2) returns i1 shifted right or left i2 times, i1 << i2, (ash 1 3) -> 8, (ash 8 -3) -> 1"
  #define Q_ash sc->pcl_i

#if WITH_GMP
  /* here, as in expt, there are cases like (ash 1 63) which need to be handled as bignums */
  const s7_pointer i1 = car(args), i2 = cadr(args);

  /* here, as in expt, there are cases like (ash 1 63) which need to be bignums so there's no easy way to tell when it's safe to drop into g_ash instead */
  if ((s7_is_integer(i1)) && /* this includes bignum ints... */
      (s7_is_integer(i2)))
    {
      s7_int shift;
      bool i1_is_big = is_big_number(i1);
      int32_t i1_compared_to_zero = 0;

      if (i1_is_big)
	i1_compared_to_zero = mpz_cmp_ui(big_integer(i1), 0);
      else
	if (integer(i1) > 0)
	  i1_compared_to_zero = 1;
	else i1_compared_to_zero = (integer(i1) < 0) ? -1 : 0;

      if (i1_compared_to_zero == 0)
	return(int_zero);

      if (is_big_number(i2))
	{
	  if (!mpz_fits_sint_p(big_integer(i2)))
	    {
	      if (mpz_cmp_ui(big_integer(i2), 0) > 0)
		out_of_range_error_nr(sc, sc->ash_symbol, int_two, i2, it_is_too_large_string);

	      /* here if i1 is negative, we need to return -1 */
	      return((i1_compared_to_zero == 1) ? int_zero : minus_one);
	    }
	  shift = mpz_get_si(big_integer(i2));
	}
      else
	{
	  shift = integer(i2);
	  if (shift < S7_INT32_MIN)
	    return((i1_compared_to_zero == 1) ? int_zero : minus_one);
	}
      if (shift > S7_INT32_MAX)
	out_of_range_error_nr(sc, sc->ash_symbol, int_two, i2, it_is_too_large_string); /* gmp calls abort if overflow here */

      if (is_t_big_integer(i1))
	mpz_set(sc->mpz_1, big_integer(i1));
      else mpz_set_si(sc->mpz_1, integer(i1));

      if (shift > 0)     /* left */
	mpz_mul_2exp(sc->mpz_1, sc->mpz_1, shift);
      else
	if (shift < 0) /* right */
	  mpz_fdiv_q_2exp(sc->mpz_1, sc->mpz_1, (uint32_t)(-shift));

      return(mpz_to_integer(sc, sc->mpz_1));
    }
#endif
  const s7_pointer x = car(args), y = cadr(args);

  if (!s7_is_integer(x))
    return(method_or_bust(sc, x, sc->ash_symbol, args, sc->type_names[T_INTEGER], 1));
  if (!s7_is_integer(y))
    return(method_or_bust(sc, y, sc->ash_symbol, args, sc->type_names[T_INTEGER], 2));
  return(make_integer(sc, c_ash(sc, s7_integer_clamped_if_gmp(sc, x), s7_integer_clamped_if_gmp(sc, y))));
}

static s7_int lsh_i_ii_unchecked(s7_int i1, s7_int i2) {return(i1 << i2);} /* this may need gmp special handling, and out-of-range as in c_ash */
static s7_int rsh_i_ii_unchecked(s7_int i1, s7_int i2) {return(i1 >> (-i2));}
static s7_int rsh_i_i2_direct(s7_int i1, s7_int unused_i2) {return(i1 >> 1);}

#if !WITH_GMP
s7_int ash_i_7ii(s7_scheme *sc, s7_int i1, s7_int i2) {return(c_ash(sc, i1, i2));}
/* this duplication (with c_ash) makes a big difference to callgrind -- why? */

s7_pointer g_ash_ii(s7_scheme *sc, s7_pointer args) {return(make_integer(sc, c_ash(sc, integer(car(args)), integer(cadr(args)))));}

s7_pointer g_ash_ic(s7_scheme *sc, s7_pointer args)
{
  s7_pointer x = car(args);
  s7_int y = integer(cadr(args));
  if (!s7_is_integer(x))
    return(method_or_bust(sc, x, sc->ash_symbol, args, sc->type_names[T_INTEGER], 1));
  return(make_integer(sc, c_ash(sc, integer(x), y)));
}

s7_pointer ash_chooser(s7_scheme *sc, s7_pointer func, int32_t args, s7_pointer expr)
{
  if (args == 2)
    {
      s7_pointer arg2 = caddr(expr);
      if (has_two_int_args(sc, expr)) return(sc->ash_ii);
      if ((is_t_integer(arg2)) && (integer(arg2) > 0) && (integer(arg2) < S7_INT_BITS)) return(sc->ash_ic);
    }
  return(func);
}
#endif
