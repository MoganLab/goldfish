/* ---------------------------------------- add ---------------------------------------- */
static inline s7_pointer add_if_overflow_to_real_or_big_integer(s7_scheme *sc, s7_int x, s7_int y)
{
#if HAVE_OVERFLOW_CHECKS
  s7_int val;
  if (add_overflow(x, y, &val))
    {
      if (WITH_WARNINGS) s7_warn(sc, 128, "integer add overflow: (+ %" ld64 " %" ld64 ")\n", x, y);
      return(make_real(sc, (long_double)x + (long_double)y));
    }
  return(make_integer(sc, val));
#else
  return(make_integer(sc, x + y));
#endif
}

static s7_pointer integer_ratio_add_if_overflow_to_real_or_rational(s7_scheme *sc, s7_pointer x, s7_pointer y) /* x: int, y:ratio */
{
#if HAVE_OVERFLOW_CHECKS
  s7_int z;
  if ((multiply_overflow(integer(x), denominator(y), &z)) ||
      (add_overflow(z, numerator(y), &z)))
    {
      if (WITH_WARNINGS) s7_warn(sc, 128, "integer + ratio overflow: (+ %" ld64 " %" ld64 "/%" ld64 ")\n", integer(x), numerator(y), denominator(y));
      return(make_real(sc, (long_double)integer(x) + fraction(y)));
    }
    return(make_ratio(sc, z, denominator(y)));
#else
  return(make_ratio(sc, integer(x) * denominator(y) + numerator(y), denominator(y)));
#endif
}

#define parcel_out_fractions(X, Y) do {d1 = denominator(x); n1 = numerator(x); d2 = denominator(y); n2 = numerator(y);} while (0)
/* add_out_x|y here (as in lt_out_x|y) gives a small speed-up, say 3-7 callgrind units, about 2% */

static s7_pointer add_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  /* an experiment: try to avoid the switch statement */
  /* this wins in most s7 cases, not so much elsewhere? parallel subtract/multiply code is slower */
  if (is_t_integer(x))
    {
      if (is_t_integer(y))
	return(add_if_overflow_to_real_or_big_integer(sc, integer(x), integer(y)));
      if (is_t_real(y))
	return(make_real(sc, (long_double)integer(x) + real(y)));
    }
  else
    if (is_t_real(x))
      {
	if (is_t_real(y))
	  return(make_real(sc, real(x) + real(y)));
      }
    else
      if ((is_t_complex(x)) && (is_t_complex(y)))
	return(make_complex(sc, real_part(x) + real_part(y), imag_part(x) + imag_part(y)));

  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  return(add_if_overflow_to_real_or_big_integer(sc, integer(x), integer(y)));
	case T_RATIO:
	  return(integer_ratio_add_if_overflow_to_real_or_rational(sc, x, y));
	case T_REAL:
	  return(make_real(sc, (long_double)integer(x) + real(y)));
	case T_COMPLEX:
	  return(make_complex_not_0i(sc, (long_double)integer(x) + (long_double)real_part(y), imag_part(y)));
	default:
	  return(method_or_bust_with_type_and_loc_pp(sc, y, sc->add_symbol, x, y, a_number_string, 2));
	}

    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  return(integer_ratio_add_if_overflow_to_real_or_rational(sc, y, x));
	case T_RATIO:
	  {
	    s7_int d1, d2, n1, n2;
	    parcel_out_fractions(x, y);
	    if (d1 == d2)
	      {
#if HAVE_OVERFLOW_CHECKS
		s7_int q;
		if (add_overflow(n1, n2, &q))
		  {
 		    if (WITH_WARNINGS) s7_warn(sc, 128, "ratio + ratio overflow: (/ (+ %" ld64 " %" ld64 ") %" ld64 ")\n", n1, n2, d1);
		    return(make_real(sc, ((long_double)n1 + (long_double)n2) / (long_double)d1));
		  }
	        return(make_ratio_with_div_check(sc, sc->add_symbol, q, d1));
#else
		return(make_ratio_with_div_check(sc, sc->add_symbol, n1 + n2, d1));
#endif
	      }

#if HAVE_OVERFLOW_CHECKS
	    {
	      s7_int n1d2, n2d1, d1d2, q;
	      if ((multiply_overflow(d1, d2, &d1d2)) ||
		  (multiply_overflow(n1, d2, &n1d2)) ||
		  (multiply_overflow(n2, d1, &n2d1)) ||
		  (add_overflow(n1d2, n2d1, &q)))
	        {
 		  if (WITH_WARNINGS) s7_warn(sc, 128, "ratio + ratio overflow: (+ %" ld64 "/%" ld64 " %" ld64 "/%" ld64 ")\n", n1, d1, n2, d2);
	          return(make_real(sc, ((long_double)n1 / (long_double)d1) + ((long_double)n2 / (long_double)d2)));
		}
	      return(make_ratio_with_div_check(sc, sc->add_symbol, q, d1d2));
	    }
#else
	    return(make_ratio_with_div_check(sc, sc->add_symbol, n1 * d2 + n2 * d1, d1 * d2));
#endif
	  }
	case T_REAL:
	  return(make_real(sc, fraction(x) + real(y)));
	case T_COMPLEX:
	  return(make_complex_not_0i(sc, (s7_double)fraction(x) + real_part(y), imag_part(y)));
	default:
	  return(method_or_bust_with_type_and_loc_pp(sc, y, sc->add_symbol, x, y, a_number_string, 2));
	}

    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER:
	  return(make_real(sc, real(x) + (long_double)integer(y)));
	case T_RATIO:
	  return(make_real(sc, real(x) + (s7_double)fraction(y)));
	case T_REAL:
	  return(make_real(sc, real(x) + real(y)));
	case T_COMPLEX:
	  return(make_complex_not_0i(sc, real(x) + real_part(y), imag_part(y)));
	default:
	  return(method_or_bust_with_type_and_loc_pp(sc, y, sc->add_symbol, x, y, a_number_string, 2));
	}

    case T_COMPLEX:
      switch (type(y))
	{
	case T_INTEGER:
	  return(make_complex_not_0i(sc, real_part(x) + integer(y), imag_part(x)));
	case T_RATIO:
	  return(make_complex_not_0i(sc, real_part(x) + (s7_double)fraction(y), imag_part(x)));
	case T_REAL:
	  return(make_complex_not_0i(sc, real_part(x) + real(y), imag_part(x)));
	case T_COMPLEX:
	  return(make_complex(sc, real_part(x) + real_part(y), imag_part(x) + imag_part(y)));
	default:
	  return(method_or_bust_with_type_and_loc_pp(sc, y, sc->add_symbol, x, y, a_number_string, 2));
	}

      default:
	return(method_or_bust_pp(sc, x, sc->add_symbol, x, y, a_number_string, 1));
    }
}

s7_pointer s7i_add_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(add_p_pp(sc, x, y));}

static inline s7_pointer add_if_overflow_to_real_wrapped(s7_scheme *sc, s7_int x, s7_int y)
{
#if HAVE_OVERFLOW_CHECKS
  s7_int val;
  if (add_overflow(x, y, &val))
    {
      if (WITH_WARNINGS) s7_warn(sc, 128, "integer add overflow: (+ %" ld64 " %" ld64 ")\n", x, y);
      return(wrap_real(sc, (long_double)x + (long_double)y));
    }
  return(wrap_integer(sc, val));
#else
  return(wrap_integer(sc, x + y));
#endif
}

static s7_pointer add_p_pp_wrapped(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  /* an experiment -- wraps rather than boxes results */
#if 1
  if (is_t_integer(x))
    {
      if (is_t_integer(y))
	return(add_if_overflow_to_real_wrapped(sc, integer(x), integer(y)));
      if (is_t_real(y))
	return(wrap_real(sc, (long_double)integer(x) + real(y)));
    }
  else
    if (is_t_real(x))
      {
	if (is_t_real(y))
	  return(wrap_real(sc, real(x) + real(y)));
      }
    else
      if ((is_t_complex(x)) && (is_t_complex(y)))
	return(wrap_real_or_complex(sc, real_part(x) + real_part(y), imag_part(x) + imag_part(y)));
#endif
  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  return(add_if_overflow_to_real_wrapped(sc, integer(x), integer(y)));
	case T_REAL:
	  return(wrap_real(sc, (long_double)integer(x) + real(y)));
	case T_COMPLEX:
	  return(wrap_complex(sc, (long_double)integer(x) + (long_double)real_part(y), imag_part(y)));
	}

    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER:
	  return(wrap_real(sc, real(x) + (long_double)integer(y)));
	case T_REAL:
	  return(make_real(sc, real(x) + real(y)));
	case T_COMPLEX:
	  return(wrap_complex(sc, real(x) + real_part(y), imag_part(y)));
	}

    case T_COMPLEX:
      switch (type(y))
	{
	case T_INTEGER:
	  return(wrap_complex(sc, real_part(x) + integer(y), imag_part(x)));
	case T_REAL:
	  return(wrap_complex(sc, real_part(x) + real(y), imag_part(x)));
	case T_COMPLEX:
	  return(wrap_real_or_complex(sc, real_part(x) + real_part(y), imag_part(x) + imag_part(y)));
	}}
  return(add_p_pp(sc, x, y));
}

s7_pointer s7i_add_p_pp_wrapped(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(add_p_pp_wrapped(sc, x, y));}

static s7_pointer add_p_ppp(s7_scheme *sc, s7_pointer x, s7_pointer y, s7_pointer z)
{
  if ((is_t_integer(x)) && (is_t_integer(y)) && (is_t_integer(z)))
    {
#if HAVE_OVERFLOW_CHECKS
      s7_int val;
      if ((!add_overflow(integer(x), integer(y), &val)) &&
	  (!add_overflow(val, integer(z), &val)))
	return(make_integer(sc, val));
      if (WITH_WARNINGS) s7_warn(sc, 128, "integer add overflow: (+ %" ld64 " %" ld64 " %" ld64 ")\n", integer(x), integer(y), integer(z));
      return(make_real(sc, (long_double)integer(x) + (long_double)integer(y) + (long_double)integer(z)));
#else
      return(make_integer(sc, integer(x) + integer(y) + integer(z)));
#endif
    }
  if ((is_t_real(x)) && (is_t_real(y)) && (is_t_real(z)))
    return(make_real(sc, real(x) + real(y) + real(z)));
  {
    s7_pointer num = add_p_pp_wrapped(sc, x, y);
    sc->error_argnum = 1;
    num = add_p_pp(sc, num, z);
    sc->error_argnum = 0;
    return(num);
  }
}

s7_pointer s7i_add_p_ppp(s7_scheme *sc, s7_pointer x, s7_pointer y, s7_pointer z) {return(add_p_ppp(sc, x, y, z));}

static s7_pointer add_p_ppp_wrapped(s7_scheme *sc, s7_pointer x, s7_pointer y, s7_pointer z)
{
  if ((is_t_integer(x)) && (is_t_integer(y)) && (is_t_integer(z)))
    {
#if HAVE_OVERFLOW_CHECKS
      s7_int val;
      if ((!add_overflow(integer(x), integer(y), &val)) &&
	  (!add_overflow(val, integer(z), &val)))
	return(wrap_integer(sc, val));
      if (WITH_WARNINGS) s7_warn(sc, 128, "integer add overflow: (+ %" ld64 " %" ld64 " %" ld64 ")\n", integer(x), integer(y), integer(z));
      return(wrap_real(sc, (long_double)integer(x) + (long_double)integer(y) + (long_double)integer(z)));
#else
      return(wrap_integer(sc, integer(x) + integer(y) + integer(z)));
#endif
    }
  if ((is_t_real(x)) && (is_t_real(y)) && (is_t_real(z)))
    return(wrap_real(sc, real(x) + real(y) + real(z)));
  {
    s7_pointer num = add_p_pp_wrapped(sc, x, y);
    sc->error_argnum = 1;
    num = add_p_pp_wrapped(sc, num, z);
    sc->error_argnum = 0;
    return(num);
  }
}


s7_pointer s7i_add_p_ppp_wrapped(s7_scheme *sc, s7_pointer x, s7_pointer y, s7_pointer z) {return(add_p_ppp_wrapped(sc, x, y, z));}


static s7_pointer g_add(s7_scheme *sc, s7_pointer args)
{
  #define H_add "(+ ...) adds its arguments"
  #define Q_add sc->pcl_n

  s7_pointer x, p;
  if (is_null(args))
    return(int_zero);
  x = car(args);
  p = cdr(args);
  if (is_null(p))
    {
      if (!is_number(x))
	return(method_or_bust_p(sc, x, sc->add_symbol, a_number_string));
      return(x);
    }
  if (is_null(cdr(p)))
    return(add_p_pp(sc, x, car(p)));
  for (sc->error_argnum = 0; is_pair(cdr(p)); p = cdr(p), sc->error_argnum++)
    x = add_p_pp_wrapped(sc, x, car(p));
  x = add_p_pp(sc, x, car(p));
  sc->error_argnum = 0;
  return(x);
}

static s7_pointer g_add_x1_1(s7_scheme *sc, s7_pointer x, int32_t pos)
{
  if (is_t_integer(x))
    return(add_if_overflow_to_real_or_big_integer(sc, integer(x), 1));

  switch (type(x))
    {
    case T_RATIO:   return(integer_ratio_add_if_overflow_to_real_or_rational(sc, int_one, x)); /* return(add_p_pp(sc, x, int_one)) */
    case T_REAL:    return(make_real(sc, real(x) + 1.0));
    case T_COMPLEX: return(make_complex_not_0i(sc, real_part(x) + 1.0, imag_part(x)));
    default:
      return(method_or_bust(sc, x, sc->add_symbol,
			    (pos == 1) ? set_plist_2(sc, x, int_one) : set_plist_2(sc, int_one, x),
			    a_number_string, pos));
    }
  return(x);
}

static s7_pointer g_add_x1(s7_scheme *sc, s7_pointer args)
{
  s7_pointer x = car(args);
  if (is_t_integer(x)) return(add_if_overflow_to_real_or_big_integer(sc, integer(x), 1)); /* return(make_integer(sc, integer(x) + 1)); */
  if (is_t_real(x)) return(make_real(sc, real(x) + 1.0));
  if (is_t_complex(x)) return(make_complex_not_0i(sc, real_part(x) + 1.0, imag_part(x)));
  return(add_p_pp(sc, x, int_one));
}
static s7_pointer g_add_1x(s7_scheme *sc, s7_pointer args) {return(g_add_x1_1(sc, cadr(args), 2));}

static s7_pointer g_add_xi(s7_scheme *sc, s7_pointer x, s7_int y, int32_t loc)
{
  if (is_t_integer(x))
    return(add_if_overflow_to_real_or_big_integer(sc, integer(x), y));

  switch (type(x))
    {
    case T_RATIO:   return(add_p_pp(sc, x, wrap_integer(sc, y)));
    case T_REAL:    return(make_real(sc, real(x) + y));
    case T_COMPLEX: return(make_complex_not_0i(sc, real_part(x) + y, imag_part(x)));
    default: return(method_or_bust_pp(sc, x, sc->add_symbol, x, make_integer(sc, y), a_number_string, loc));
    }
  return(x);
}

static s7_pointer g_add_xf(s7_scheme *sc, s7_pointer x, s7_double y, int32_t loc)
{
  if (is_t_real(x)) return(make_real(sc, real(x) + y));
  switch (type(x))
    {
    case T_INTEGER: return(make_real(sc, integer(x) + y));
    case T_RATIO:   return(make_real(sc, (s7_double)fraction(x) + y));
    case T_COMPLEX: return(make_complex_not_0i(sc, real_part(x) + y, imag_part(x)));
    default: return(method_or_bust_pp(sc, x, sc->add_symbol, x, make_real(sc, y), a_number_string, loc));
    }
  return(x);
}

static s7_pointer add_p_pi(s7_scheme *sc, s7_pointer p1, s7_int i1) {return(g_add_xi(sc, p1, i1, 1));}
static s7_pointer add_p_dd(s7_scheme *sc, s7_double x1, s7_double x2) {return(make_real(sc, x1 + x2));} /* very few calls */
static s7_pointer add_p_ii(s7_scheme *sc, s7_int x1, s7_int x2) {return(make_integer(sc, x1 + x2));}    /* no calls */
static s7_double add_d_d(s7_double x) {return(x);}
static s7_double add_d_dd(s7_double x1, s7_double x2) {return(x1 + x2);}
static s7_double add_d_id(s7_int x1, s7_double x2) {return(x1 + x2);}
static s7_double add_d_ddd(s7_double x1, s7_double x2, s7_double x3) {return(x1 + x2 + x3);}
static s7_double add_d_dddd(s7_double x1, s7_double x2, s7_double x3, s7_double x4) {return(x1 + x2 + x3 + x4);}
static s7_int add_i_ii(s7_int i1, s7_int i2) {return(i1 + i2);}
static s7_int add_i_iii(s7_int i1, s7_int i2, s7_int i3) {return(i1 + i2 + i3);}

static s7_pointer argument_type(s7_scheme *sc, s7_pointer arg1)
{
  if (is_pair(arg1))
    {
      if (is_quote(sc, car(arg1)))
	return((is_pair(cdr(arg1))) ? s7_type_of(sc, cadr(arg1)) : NULL);    /* arg1 = (quote) */

      if ((is_h_optimized(arg1)) &&
	  (is_safe_c_op(optimize_op(arg1))) &&
	  (is_c_function(opt1_cfunc(arg1))))
	{
	  s7_pointer sig = c_function_signature(opt1_cfunc(arg1));
	  if ((sig) &&
	      (is_pair(sig)) &&
	      (is_symbol(car(sig))))
	    return(car(sig));
	}
      /* perhaps add closure sig if we can depend on it (immutable func etc) */
    }
  else
    if (!is_symbol(arg1))
      return(s7_type_of(sc, arg1));
  return(NULL);
}

static s7_pointer g_random_i(s7_scheme *sc, s7_pointer args);

static s7_pointer add_chooser(s7_scheme *sc, s7_pointer func, int32_t args, s7_pointer expr)
{
  /* (+ s f) (+ (* s s) s) (+ s s) (+ s (* s s)) */
  if (args == 2)
    {
      const s7_pointer arg1 = cadr(expr), arg2 = caddr(expr);
      if ((is_pair(arg1)) && (has_fn(arg1)) && (fn_proc(arg1) == g_multiply_2)) set_fn_direct(arg1, g_multiply_2_wrapped);
      if ((is_pair(arg2)) && (has_fn(arg2)))
	{
	  if (fn_proc(arg2) == g_multiply_2) set_fn_direct(arg2, g_multiply_2_wrapped);
	  if (fn_proc(arg2) == g_subtract_2) set_fn_direct(arg2, g_subtract_2_wrapped);
	}
      if (arg2 == int_one)    /* (+ ... 1) */
	return(sc->add_x1);
      if ((is_t_integer(arg1)) && ((is_pair(arg2)) && (is_optimized(arg2)) && (is_h_safe_c_nc(arg2)) && (fn_proc(arg2) == g_random_i)))
	{
	  set_opt3_int(cdr(expr), integer(cadr(arg2)));
	  set_safe_optimize_op(expr, HOP_SAFE_C_NC); /* i.e. don't evaluate random call beforehand(?) */
	  return(sc->add_i_random);
	}
      if (arg1 == int_one) return(sc->add_1x);
      return(sc->add_2);
    }
  return((args == 3) ? sc->add_3 : ((args == 4) ? sc->add_4 : func));
}

/* ---------------------------------------- subtract ---------------------------------------- */
static s7_pointer negate_p_p(s7_scheme *sc, s7_pointer x)     /* can't use "negate" because it confuses C++! */
{
  switch (type(x))
    {
    case T_INTEGER:
      if (integer(x) == S7_INT64_MIN)
	sole_arg_out_of_range_error_nr(sc, sc->subtract_symbol, x, wrap_string(sc, "most-negative-fixnum can't be negated", 37));
      return(make_integer(sc, -integer(x)));

    case T_RATIO:   return(make_simpler_ratio(sc, -numerator(x), denominator(x)));
    case T_REAL:    return(make_real(sc, -real(x)));
    case T_COMPLEX: return(make_complex_not_0i(sc, -real_part(x), -imag_part(x)));

    default:
      return(method_or_bust_p(sc, x, sc->subtract_symbol, a_number_string));
    }
}

s7_pointer s7i_negate_p_p(s7_scheme *sc, s7_pointer x) {return(negate_p_p(sc, x));}

static inline s7_pointer subtract_if_overflow_to_real_or_big_integer(s7_scheme *sc, s7_int x, s7_int y)
{
#if HAVE_OVERFLOW_CHECKS
  s7_int val;
  if (subtract_overflow(x, y, &val))
    {
      if (WITH_WARNINGS) s7_warn(sc, 128, "integer subtract overflow: (- %" ld64 " %" ld64 ")\n", x, y);
      return(make_real(sc, (long_double)x - (long_double)y));
    }
  return(make_integer(sc, val));
#else
  return(make_integer(sc, x - y));
#endif
}

static s7_pointer subtract_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  switch (type(x))
    {
    case T_INTEGER:
      if (integer(x) == 0)
	return(negate_p_p(sc, y));
      switch (type(y))
	{
	case T_INTEGER:
	  return(subtract_if_overflow_to_real_or_big_integer(sc, integer(x), integer(y)));

	case T_RATIO:
	  {
#if HAVE_OVERFLOW_CHECKS
	    s7_int z;
	    if ((multiply_overflow(integer(x), denominator(y), &z)) ||
		(subtract_overflow(z, numerator(y), &z)))
	      {
		if (WITH_WARNINGS) s7_warn(sc, 128, "integer - ratio overflow: (- %" ld64 " %" ld64 "/%" ld64 ")\n", integer(x), numerator(y), denominator(y));
		return(make_real(sc, (long_double)integer(x) - fraction(y)));
	      }
	      return(make_ratio(sc, z, denominator(y)));
#else
	    return(make_ratio(sc, integer(x) * denominator(y) - numerator(y), denominator(y)));
#endif
	  }
	case T_REAL:
	  return(make_real(sc, (long_double)integer(x) - real(y)));
	case T_COMPLEX:
	  return(make_complex_not_0i(sc, (long_double)integer(x) - real_part(y), -imag_part(y)));
	default:
	  return(method_or_bust_with_type_and_loc_pp(sc, y, sc->subtract_symbol, x, y, a_number_string, 2));
	}

    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  {
#if HAVE_OVERFLOW_CHECKS
	    s7_int z;
	    if ((multiply_overflow(integer(y), denominator(x), &z)) ||
		(subtract_overflow(numerator(x), z, &z)))
	      {
		if (WITH_WARNINGS) s7_warn(sc, 128, "ratio - integer overflow: (- %" ld64 "/%" ld64 " %" ld64 ")\n", numerator(x), denominator(x), integer(y));
		return(make_real(sc, fraction(x) - (long_double)integer(y)));
	      }
	    return(make_ratio(sc, z, denominator(x)));
#else
	    return(make_ratio(sc, numerator(x) - (integer(y) * denominator(x)), denominator(x)));
#endif
	  }
	case T_RATIO:
	  {
	    s7_int d1, d2, n1, n2;
	    parcel_out_fractions(x, y);
	    if (d1 == d2)
	      {
#if HAVE_OVERFLOW_CHECKS
		s7_int q;
		if (subtract_overflow(n1, n2, &q))
		  {
		    if (WITH_WARNINGS) s7_warn(sc, 128, "ratio - ratio overflow: (- %" ld64 "/%" ld64 " %" ld64 "/%" ld64 ")\n", n1, d1, n2, d2);
		    return(make_real(sc, ((long_double)n1 - (long_double)n2) / (long_double)d1));
		  }
	        return(make_ratio_with_div_check(sc, sc->subtract_symbol, q, d1));
#else
		return(make_ratio(sc, numerator(x) - numerator(y), denominator(x)));
#endif
	      }

#if HAVE_OVERFLOW_CHECKS
	    {
	      s7_int n1d2, n2d1, d1d2, q;
	      if ((multiply_overflow(d1, d2, &d1d2)) ||
		  (multiply_overflow(n1, d2, &n1d2)) ||
		  (multiply_overflow(n2, d1, &n2d1)) ||
		  (subtract_overflow(n1d2, n2d1, &q)))
	        {
		  if (WITH_WARNINGS) s7_warn(sc, 128, "ratio - ratio overflow: (- %" ld64 "/%" ld64 " %" ld64 "/%" ld64 ")\n", n1, d1, n2, d2);
		  return(make_real(sc, ((long_double)n1 / (long_double)d1) - ((long_double)n2 / (long_double)d2)));
		}
	      return(make_ratio_with_div_check(sc, sc->subtract_symbol, q, d1d2));
	    }
#else
	    return(make_ratio_with_div_check(sc, sc->subtract_symbol, n1 * d2 - n2 * d1, d1 * d2));
#endif
	  }
	case T_REAL:
	  return(make_real(sc, (s7_double)fraction(x) - real(y)));
	case T_COMPLEX:
	  return(make_complex_not_0i(sc, (s7_double)fraction(x) - real_part(y), -imag_part(y)));
	default:
	  return(method_or_bust_with_type_and_loc_pp(sc, y, sc->subtract_symbol, x, y, a_number_string, 2));
	}

    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER:
	  return(make_real(sc, real(x) - (long_double)integer(y))); /* long_double saves (- 9007199254740996.0 9007199254740995): 1.0 */
	case T_RATIO:
	  return(make_real(sc, real(x) - (s7_double)fraction(y)));
	case T_REAL:
	  return(make_real(sc, real(x) - real(y)));
	case T_COMPLEX:
	  return(make_complex_not_0i(sc, real(x) - real_part(y), -imag_part(y)));
	default:
	  return(method_or_bust_with_type_and_loc_pp(sc, y, sc->subtract_symbol, x, y, a_number_string, 2));
	}

    case T_COMPLEX:
      switch (type(y))
	{
	case T_INTEGER:
	  return(make_complex_not_0i(sc, real_part(x) - integer(y), imag_part(x)));
	case T_RATIO:
	  return(make_complex_not_0i(sc, real_part(x) - (s7_double)fraction(y), imag_part(x)));
	case T_REAL:
	  return(make_complex_not_0i(sc, real_part(x) - real(y), imag_part(x)));
	case T_COMPLEX:
	  return(make_complex(sc, real_part(x) - real_part(y), imag_part(x) - imag_part(y)));
	default:
	  return(method_or_bust_with_type_and_loc_pp(sc, y, sc->subtract_symbol, x, y, a_number_string, 2));
	}

      default:
	return(method_or_bust_pp(sc, x, sc->subtract_symbol, x, y, a_number_string, 1));
    }
}

s7_pointer s7i_subtract_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(subtract_p_pp(sc, x, y));}

static s7_pointer negate_p_p_wrapped(s7_scheme *sc, s7_pointer x)     /* can't use "negate" because it confuses C++! */
{
  switch (type(x))
    {
    case T_INTEGER:
      if (integer(x) == S7_INT64_MIN)
	sole_arg_out_of_range_error_nr(sc, sc->subtract_symbol, x, wrap_string(sc, "most-negative-fixnum can't be negated", 37));
      return(wrap_integer(sc, -integer(x)));
    case T_REAL:
      return(wrap_real(sc, -real(x)));
    case T_COMPLEX:
      return(wrap_complex(sc, -real_part(x), -imag_part(x)));
    }
  return(negate_p_p(sc, x));
}

s7_pointer s7i_negate_p_p_wrapped(s7_scheme *sc, s7_pointer x) {return(negate_p_p_wrapped(sc, x));}

static s7_pointer subtract_if_overflow_to_real_wrapped(s7_scheme *sc, s7_int x, s7_int y)
{
#if HAVE_OVERFLOW_CHECKS
  s7_int val;
  if (subtract_overflow(x, y, &val))
    {
      if (WITH_WARNINGS) s7_warn(sc, 128, "integer subtract overflow: (- %" ld64 " %" ld64 ")\n", x, y);
      return(wrap_real(sc, (long_double)x - (long_double)y));
    }
  return(wrap_integer(sc, val));
#else
  return(wrap_integer(sc, x - y));
#endif
}

static s7_pointer subtract_p_pp_wrapped(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  switch (type(x))
    {
    case T_INTEGER:
      if (integer(x) == 0) return(negate_p_p_wrapped(sc, y));
      switch (type(y))
	{
	case T_INTEGER:	  return(subtract_if_overflow_to_real_wrapped(sc, integer(x), integer(y)));
	case T_REAL:	  return(wrap_real(sc, (long_double)integer(x) - real(y)));
	case T_COMPLEX:	  return(wrap_complex(sc, (long_double)integer(x) - real_part(y), -imag_part(y)));
	}
    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER:	  return(wrap_real(sc, real(x) - (long_double)integer(y))); /* long_double saves (- 9007199254740996.0 9007199254740995): 1.0 */
	case T_REAL:	  return(wrap_real(sc, real(x) - real(y)));
	case T_COMPLEX:	  return(wrap_complex(sc, real(x) - real_part(y), -imag_part(y)));
	}
    case T_COMPLEX:
      switch (type(y))
	{
	case T_INTEGER:	  return(wrap_complex(sc, real_part(x) - integer(y), imag_part(x)));
	case T_REAL:	  return(wrap_complex(sc, real_part(x) - real(y), imag_part(x)));
	case T_COMPLEX:	  return(wrap_real_or_complex(sc, real_part(x) - real_part(y), imag_part(x) - imag_part(y)));
	}}
  return(subtract_p_pp(sc, x, y));
}

s7_pointer s7i_subtract_p_pp_wrapped(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(subtract_p_pp_wrapped(sc, x, y));}

static s7_pointer g_subtract(s7_scheme *sc, s7_pointer args)
{
  #define H_subtract "(- x1 ...) subtracts its trailing arguments from the first, or negates the first if only one it is given"
  #define Q_subtract sc->pcl_n

  s7_pointer x = car(args), p = cdr(args);
  if (is_null(p))
    return(negate_p_p(sc, x));
  for (sc->error_argnum = 0; is_pair(cdr(p)); p = cdr(p), sc->error_argnum++)
    x = subtract_p_pp_wrapped(sc, x, car(p));
  x = subtract_p_pp(sc, x, car(p));
  sc->error_argnum = 0;
  return(x);
}

static s7_pointer minus_c1(s7_scheme *sc, s7_pointer x)
{
  switch (type(x))
    {
    case T_INTEGER: return(subtract_if_overflow_to_real_or_big_integer(sc, integer(x), 1));
    case T_RATIO:   return(subtract_p_pp(sc, x, int_one));
    case T_REAL:    return(make_real(sc, real(x) - 1.0));
    case T_COMPLEX: return(make_complex_not_0i(sc, real_part(x) - 1.0, imag_part(x)));
    default:
      return(method_or_bust_pp(sc, x, sc->subtract_symbol, x, int_one, a_number_string, 1));
    }
  return(x);
}

static s7_pointer g_subtract_x1(s7_scheme *sc, s7_pointer args)
{
  s7_pointer num = car(args);
  /* return((is_t_integer(num)) ? make_integer(sc, integer(num) - 1) : minus_c1(sc, num)); */
  return((is_t_integer(num)) ? subtract_if_overflow_to_real_or_big_integer(sc, integer(num), 1) : minus_c1(sc, num));
}

static s7_pointer g_subtract_2f(s7_scheme *sc, s7_pointer args) /* (- x f) */
{
  const s7_pointer x = car(args);
  const s7_double n = real(cadr(args)); /* checked below is_t_real */
  if (is_t_real(x)) return(make_real(sc, real(x) - n));
  switch (type(x))
    {
    case T_INTEGER: return(make_real(sc, integer(x) - n));
    case T_RATIO:   return(make_real(sc, (s7_double)fraction(x) - n));
    case T_COMPLEX: return(make_complex_not_0i(sc, real_part(x) - n, imag_part(x)));
    default:
      return(method_or_bust(sc, x, sc->subtract_symbol, args, a_number_string, 1));
    }
  return(x);
}

static s7_pointer g_subtract_f2(s7_scheme *sc, s7_pointer args) /* (- f x) */
{
  const s7_pointer x = cadr(args);
  const s7_double n = real(car(args)); /* checked below is_t_real */

  if (is_t_real(x)) return(make_real(sc, n - real(x)));
  switch (type(x))
    {
    case T_INTEGER: return(make_real(sc, n - integer(x)));
    case T_RATIO:   return(make_real(sc, n - (s7_double)fraction(x)));
    case T_COMPLEX: return(make_complex_not_0i(sc, n - real_part(x), -imag_part(x)));
    default:
      return(method_or_bust(sc, x, sc->subtract_symbol, args, a_number_string, 1));
    }
  return(x);
}

static s7_int subtract_i_ii(s7_int i1, s7_int i2) {return(i1 - i2);}
static s7_int subtract_i_i(s7_int x) {return(-x);}
static s7_int subtract_i_iii(s7_int i1, s7_int i2, s7_int i3) {return(i1 - i2 - i3);}
static s7_double subtract_d_d(s7_double x) {return(-x);}
static s7_double subtract_d_dd(s7_double x1, s7_double x2) {return(x1 - x2);}
static s7_double subtract_d_id(s7_int x1, s7_double x2) {return(x1 - x2);}
static s7_double subtract_d_ddd(s7_double x1, s7_double x2, s7_double x3) {return(x1 - x2 - x3);}
static s7_double subtract_d_dddd(s7_double x1, s7_double x2, s7_double x3, s7_double x4) {return(x1 - x2 - x3 - x4);}
static s7_pointer subtract_p_dd(s7_scheme *sc, s7_double x1, s7_double x2) {return(make_real(sc, x1 - x2));}
static s7_pointer subtract_p_ii(s7_scheme *sc, s7_int i1, s7_int i2) {return(make_integer(sc, i1 - i2));}

static s7_pointer g_sub_xi(s7_scheme *sc, s7_pointer x, s7_int y)
{
  if (is_t_integer(x))
    return(subtract_if_overflow_to_real_or_big_integer(sc, integer(x), y));

  switch (type(x))
    {
    case T_RATIO:   return(make_ratio(sc, numerator(x) - (y * denominator(x)), denominator(x)));
    case T_REAL:    return(make_real(sc, real(x) - y));
    case T_COMPLEX: return(make_complex_not_0i(sc, real_part(x) - y, imag_part(x)));
    default: return(method_or_bust_pp(sc, x, sc->subtract_symbol, x, make_integer(sc, y), a_number_string, 1));
    }
  return(x);
}

static s7_pointer subtract_chooser(s7_scheme *sc, s7_pointer func, int32_t args, s7_pointer expr)
{
  s7_pointer arg1, arg2;
  if (args == 1) return(sc->subtract_1);
  if (args != 2) return((args == 3) ? sc->subtract_3 : func);
  arg1 = cadr(expr);
  arg2 = caddr(expr);
  if ((is_pair(arg1)) && (has_fn(arg1)))
    {
      if (fn_proc(arg1) == g_multiply_2) set_fn_direct(arg1, g_multiply_2_wrapped);
      if (fn_proc(arg1) == g_add_2) set_fn_direct(arg1, g_add_2_wrapped);
    }
  if ((is_pair(arg2)) && (has_fn(arg2)) && (fn_proc(arg2) == g_multiply_2)) set_fn_direct(arg2, g_multiply_2_wrapped);
  /* sub_random_i (parallels add_i_random) only occurs in tmap.scm */
  if (arg2 == int_one) return(sc->subtract_x1);
  if (is_t_real(arg1)) return(sc->subtract_f2);
  if (is_t_real(arg2)) return(sc->subtract_2f);
  return(sc->subtract_2);
}


/* ---------------------------------------- multiply ---------------------------------------- */
#define QUOTIENT_FLOAT_LIMIT 1e13
#define QUOTIENT_INT_LIMIT 10000000000000
/* fraction(x) is not accurate enough if it involves numbers over e18 even when done with long_doubles */

static inline s7_pointer multiply_if_overflow_to_real_or_big_integer(s7_scheme *sc, s7_int x, s7_int y)
{
#if HAVE_OVERFLOW_CHECKS
  s7_int val;
  if (multiply_overflow(x, y, &val))
    {
      if (WITH_WARNINGS) s7_warn(sc, 128, "integer multiply overflow: (* %" ld64 " %" ld64 ")\n", x, y);
      return(make_real(sc, (s7_double)x * (s7_double)y));
    }
    return(make_integer(sc, val));
#else
  return(make_integer(sc, x * y));
#endif
}

static s7_pointer integer_ratio_multiply_if_overflow_to_real_or_ratio(s7_scheme *sc, s7_int x, s7_pointer y)
{
#if HAVE_OVERFLOW_CHECKS
  s7_int z;
  if (multiply_overflow(x, numerator(y), &z))
    {
      if (WITH_WARNINGS) s7_warn(sc, 128, "integer * ratio overflow: (* %" ld64 " %" ld64 "/%" ld64 ")\n", x, numerator(y), denominator(y));
      return(make_real(sc, (s7_double)x * (s7_double)fraction(y)));
    }
    return(make_ratio(sc, z, denominator(y)));
#else
  return(make_ratio(sc, x * numerator(y), denominator(y)));
#endif
}

static s7_pointer multiply_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  return(multiply_if_overflow_to_real_or_big_integer(sc, integer(x), integer(y)));
	case T_RATIO:
	  return(integer_ratio_multiply_if_overflow_to_real_or_ratio(sc, integer(x), y));
	case T_REAL:
	  return(make_real(sc, (long_double)integer(x) * real(y)));
	case T_COMPLEX:
	  return(make_complex(sc, (long_double)integer(x) * real_part(y), (long_double)integer(x) * imag_part(y)));
	default:
	  return(method_or_bust_with_type_and_loc_pp(sc, y, sc->multiply_symbol, x, y, a_number_string, 2));
	}
    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  return(integer_ratio_multiply_if_overflow_to_real_or_ratio(sc, integer(y), x));
	case T_RATIO:
	  {
	    s7_int d1, d2, n1, n2;
	    parcel_out_fractions(x, y);
#if HAVE_OVERFLOW_CHECKS
	    {
	      s7_int n1n2, d1d2;
	      if ((multiply_overflow(d1, d2, &d1d2)) ||
		  (multiply_overflow(n1, n2, &n1n2)))
	        {
		  if (WITH_WARNINGS) s7_warn(sc, 128, "ratio * ratio overflow: (* %" ld64 "/%" ld64 " %" ld64 "/%" ld64 ")\n", n1, d1, n2, d2);
		  return(make_real(sc, (s7_double)fraction(x) * (s7_double)fraction(y)));
		}
	      return(make_ratio_with_div_check(sc, sc->multiply_symbol, n1n2, d1d2));
	    }
#else
	    return(make_ratio_with_div_check(sc, sc->multiply_symbol, n1 * n2, d1 * d2));
#endif
	  }
	case T_REAL:
	  return(make_real(sc, (s7_double)fraction(x) * real(y)));
	case T_COMPLEX:
	  return(make_complex(sc, (s7_double)fraction(x) * real_part(y), (s7_double)fraction(x) * imag_part(y)));
	default:
	  return(method_or_bust_with_type_and_loc_pp(sc, y, sc->multiply_symbol, x, y, a_number_string, 2));
	}
    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER:
	  return(make_real(sc, real(x) * (long_double)integer(y)));
	case T_RATIO:
	  return(make_real(sc, (s7_double)fraction(y) * real(x)));
	case T_REAL:
	  return(make_real(sc, real(x) * real(y)));
	case T_COMPLEX:
	  return(make_complex(sc, real(x) * real_part(y), real(x) * imag_part(y)));
	default:
	  return(method_or_bust_with_type_and_loc_pp(sc, y, sc->multiply_symbol, x, y, a_number_string, 2));
	}
    case T_COMPLEX:
      switch (type(y))
	{
	case T_INTEGER:
	  return(make_complex(sc, real_part(x) * integer(y), imag_part(x) * integer(y)));
	case T_RATIO:
	  return(make_complex(sc, real_part(x) * (s7_double)fraction(y), imag_part(x) * (s7_double)fraction(y)));
	case T_REAL:
	  return(make_complex(sc, real_part(x) * real(y), imag_part(x) * real(y)));
	case T_COMPLEX:
	  {
	    s7_double r1 = real_part(x), r2 = real_part(y), i1 = imag_part(x), i2 = imag_part(y);
	    return(make_complex(sc, r1 * r2 - i1 * i2, r1 * i2 + r2 * i1));
	  }
	default:
	  return(method_or_bust_with_type_and_loc_pp(sc, y, sc->multiply_symbol, x, y, a_number_string, 2));
	}

      default:
	return(method_or_bust_pp(sc, x, sc->multiply_symbol, x, y, a_number_string, 1));
    }
}

s7_pointer s7i_multiply_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(multiply_p_pp(sc, x, y));}

static inline s7_pointer multiply_if_overflow_to_real_wrapped(s7_scheme *sc, s7_int x, s7_int y)
{
#if HAVE_OVERFLOW_CHECKS
  s7_int val;
  if (multiply_overflow(x, y, &val))
    {
      if (WITH_WARNINGS) s7_warn(sc, 128, "integer multiply overflow: (* %" ld64 " %" ld64 ")\n", x, y);
      return(wrap_real(sc, (s7_double)x * (s7_double)y));
    }
    return(wrap_integer(sc, val));
#else
  return(wrap_integer(sc, x * y));
#endif
}

static s7_pointer multiply_p_pp_wrapped(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:	  return(multiply_if_overflow_to_real_wrapped(sc, integer(x), integer(y)));
	case T_REAL:	  return(wrap_real(sc, (long_double)integer(x) * real(y)));
	case T_COMPLEX:	  return(wrap_real_or_complex(sc, (long_double)integer(x) * real_part(y), (long_double)integer(x) * imag_part(y)));
	}
    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER:	  return(wrap_real(sc, real(x) * (long_double)integer(y)));
	case T_REAL:	  return(wrap_real(sc, real(x) * real(y)));
	case T_COMPLEX:	  return(wrap_real_or_complex(sc, real(x) * real_part(y), real(x) * imag_part(y)));
	}
    case T_COMPLEX:
      switch (type(y))
	{
	case T_INTEGER:	  return(wrap_real_or_complex(sc, real_part(x) * integer(y), real_part(x) * imag_part(y)));
	case T_REAL:	  return(wrap_real_or_complex(sc, real_part(x) * real(y), imag_part(x) * imag_part(y)));
	case T_COMPLEX:
	  {
	    s7_double r1 = real_part(x), r2 = real_part(y), i1 = imag_part(x), i2 = imag_part(y);
	    return(wrap_real_or_complex(sc, r1 * r2 - i1 * i2, r1 * i2 + r2 * i1));
	  }}}
  return(multiply_p_pp(sc, x, y));
}

s7_pointer s7i_multiply_p_pp_wrapped(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(multiply_p_pp_wrapped(sc, x, y));}

static s7_pointer multiply_p_ppp(s7_scheme *sc, s7_pointer x, s7_pointer y, s7_pointer z)
{
  /* no hits for reals in tnum */
  /* if ((is_t_real(x)) && (is_t_real(y)) && (is_t_real(z))) return(make_real(sc, real(x) * real(y) * real(z))); */
  x = multiply_p_pp_wrapped(sc, x, y);
  sc->error_argnum = 1;
  x = multiply_p_pp(sc, x, z);
  sc->error_argnum = 0;
  return(x);
}

s7_pointer s7i_multiply_p_ppp(s7_scheme *sc, s7_pointer x, s7_pointer y, s7_pointer z) {return(multiply_p_ppp(sc, x, y, z));}

static s7_pointer multiply_p_ppp_wrapped(s7_scheme *sc, s7_pointer x, s7_pointer y, s7_pointer z)
{
  /* no hits for reals in tnum */
  /* if ((is_t_real(x)) && (is_t_real(y)) && (is_t_real(z))) return(make_real(sc, real(x) * real(y) * real(z))); */
  x = multiply_p_pp_wrapped(sc, x, y);
  sc->error_argnum = 1;
  x = multiply_p_pp_wrapped(sc, x, z);
  sc->error_argnum = 0;
  return(x);
}

s7_pointer s7i_multiply_p_ppp_wrapped(s7_scheme *sc, s7_pointer x, s7_pointer y, s7_pointer z) {return(multiply_p_ppp_wrapped(sc, x, y, z));}

static s7_pointer multiply_method_or_bust(s7_scheme *sc, s7_pointer obj, s7_pointer args, s7_pointer typ, int32_t num)
{
  if (has_active_methods(sc, obj))
    return(find_and_apply_method(sc, obj, sc->multiply_symbol, args));
  if (num == 0)
    sole_arg_wrong_type_error_nr(sc, sc->multiply_symbol, obj, typ);
  wrong_type_error_nr(sc, sc->multiply_symbol, num, obj, typ);
  return(NULL);
}

static s7_pointer g_multiply(s7_scheme *sc, s7_pointer args)
{
  #define H_multiply "(* ...) multiplies its arguments"
  #define Q_multiply sc->pcl_n

  s7_pointer x, p;
  if (is_null(args))
    return(int_one);
  x = car(args);
  p = cdr(args);
  if (is_null(p))
    {
      if (!is_number(x))
	return(multiply_method_or_bust(sc, x, args, a_number_string, 0));
      return(x);
    }
  for (sc->error_argnum = 0; is_pair(cdr(p)); p = cdr(p), sc->error_argnum++)
    x = multiply_p_pp_wrapped(sc, x, car(p));
  x = multiply_p_pp(sc, x, car(p));
  sc->error_argnum = 0;
  return(x);
}

static s7_pointer g_mul_xi(s7_scheme *sc, s7_pointer x, s7_int n, int32_t loc)
{
  switch (type(x))
    {
    case T_INTEGER: return(multiply_if_overflow_to_real_or_big_integer(sc, integer(x), n));
    case T_RATIO:   return(integer_ratio_multiply_if_overflow_to_real_or_ratio(sc, n, x));
    case T_REAL:    return(make_real(sc, real(x) * n));
    case T_COMPLEX: return(make_complex(sc, real_part(x) * n, imag_part(x) * n));
    default:
      /* we can get here from mul_2_xi for example so the non-integer argument might not be a symbol */
      return(method_or_bust_pp(sc, x, sc->multiply_symbol, x, make_integer(sc, n), a_number_string, loc));
    }
  return(x);
}

static s7_pointer multiply_p_pi(s7_scheme *sc, s7_pointer p1, s7_int i1) {return(g_mul_xi(sc, p1, i1, 1));}

static s7_pointer g_mul_xf(s7_scheme *sc, s7_pointer x, s7_double y, int32_t num)
{
  /* it's possible to return different argument NaNs depending on the expression or how it is wrapped:
   *   (* (bignum +nan.0) +nan.123) -> nan.123
   *   (let () (define (func) (* (bignum +nan.0) +nan.123)) (func) (func)) -> nan.0
   * latter call is fx_c_aaa->fx_c_ac->g_mul_xf (if +nan.122 instead of +nan.0, we get +nan.122 so we always get one of the NaNs)
   */
  switch (type(x))
    {
    case T_INTEGER: return(make_real(sc, integer(x) * y));
    case T_RATIO:   return(make_real(sc, numerator(x) * y / denominator(x)));
    case T_REAL:    return(make_real(sc, real(x) * y));
    case T_COMPLEX: return(make_complex(sc, real_part(x) * y, imag_part(x) * y));
    default: return(method_or_bust_pp(sc, x, sc->multiply_symbol, x, make_real(sc, y), a_number_string, num));
    }
  return(x);
}

static s7_int multiply_i_ii(s7_int i1, s7_int i2)
{
#if HAVE_OVERFLOW_CHECKS
  s7_int val;
  if (multiply_overflow(i1, i2, &val))
    {
#if WITH_WARNINGS
      fprintf(stderr, "%s[%d]: integer multiply overflow: (* %" ld64 " %" ld64 ")\n", __func__, __LINE__, i1, i2);
#endif
      return(S7_INT64_MAX); /* this is inconsistent with other unopt cases where an overflow -> double result */
    }
  /* (let () (define (func) (do ((i 0 (+ i 1))) ((= i 1)) (do ((j 0 (+ j 1))) ((= j 1)) (even? (* (ash 1 43) (ash 1 43)))))) (define (hi) (func)) (hi)) */
  return(val);
#else
  return(i1 * i2);
#endif
}

static s7_int multiply_i_iii(s7_int i1, s7_int i2, s7_int i3)
{
#if HAVE_OVERFLOW_CHECKS
  s7_int val1, val2;
  if ((multiply_overflow(i1, i2, &val1)) ||
      (multiply_overflow(val1, i3, &val2)))
    {
#if WITH_WARNINGS
      fprintf(stderr, "%s[%d]: integer multiply overflow: (* %" ld64 " %" ld64 " %" ld64 ")\n", __func__, __LINE__, i1, i2, i3);
#endif
      return(S7_INT64_MAX);
    }
  return(val2);
#else
  return(i1 * i2 * i3);
#endif
}

static s7_double multiply_d_d(s7_double x) {return(x);}
static s7_double multiply_d_dd(s7_double x1, s7_double x2) {return(x1 * x2);}
static s7_double multiply_d_id(s7_int x1, s7_double x2) {return(x1 * x2);}
static s7_double multiply_d_ddd(s7_double x1, s7_double x2, s7_double x3) {return(x1 * x2 * x3);}
static s7_double multiply_d_dddd(s7_double x1, s7_double x2, s7_double x3, s7_double x4) {return(x1 * x2 * x3 * x4);}
static s7_pointer mul_p_dd(s7_scheme *sc, s7_double x1, s7_double x2) {return(make_real(sc, x1 * x2));}

static s7_pointer multiply_chooser(s7_scheme *sc, s7_pointer func, int32_t args, s7_pointer expr)
{
  s7_pointer arg1, arg2;
  if (args < 2) return(func);
  arg1 = cadr(expr);
  if ((is_pair(arg1)) && (has_fn(arg1)))
    {
      if (fn_proc(arg1) == g_add_2) set_fn_direct(arg1, g_add_2_wrapped);
      if (fn_proc(arg1) == g_add_3) set_fn_direct(arg1, g_add_3_wrapped);
      if (fn_proc(arg1) == g_subtract_2) set_fn_direct(arg1, g_subtract_2_wrapped);
      if (fn_proc(arg1) == g_subtract_1) set_fn_direct(arg1, g_subtract_1_wrapped);
    }
  arg2 = caddr(expr);
  if ((is_pair(arg2)) && (has_fn(arg2)))
    {
      if (fn_proc(arg2) == g_add_2) set_fn_direct(arg2, g_add_2_wrapped);
      if (fn_proc(arg2) == g_add_3) set_fn_direct(arg2, g_add_3_wrapped);
      if (fn_proc(arg2) == g_subtract_2) set_fn_direct(arg2, g_subtract_2_wrapped);
      if (fn_proc(arg2) == g_subtract_1) set_fn_direct(arg2, g_subtract_1_wrapped);
    }
  if (args == 2) return(sc->multiply_2);
  if (args == 3) return(sc->multiply_3);
  return(func);
}


/* ---------------------------------------- divide ---------------------------------------- */
static s7_pointer complex_invert(s7_scheme *sc, s7_pointer x)
{
  s7_double r2 = real_part(x), i2 = imag_part(x);
  s7_double den = (r2 * r2 + i2 * i2);
  /* here if x is, for example, -inf.0+i, den is +inf.0 so -i2/den is -0.0 (in gcc anyway), so the imag part is 0.0 */
  return(make_complex(sc, r2 / den, -i2 / den));
}

static s7_pointer invert_p_p(s7_scheme *sc, s7_pointer num)
{
  switch (type(num))
    {
    case T_INTEGER:
      if (integer(num) == 0)
	division_by_zero_error_1_nr(sc, sc->divide_symbol, num);
      return(make_simple_ratio(sc, 1, integer(num)));  /* this checks for int */
    case T_RATIO:
      return(make_simple_ratio(sc, denominator(num), numerator(num)));
    case T_REAL:
      if (real(num) == 0.0)
	division_by_zero_error_1_nr(sc, sc->divide_symbol, num);
      return(make_real(sc, 1.0 / real(num)));
    case T_COMPLEX:
      return(complex_invert(sc, num));

    default:
      if_method_exists_return_value(sc, num, sc->divide_symbol, set_plist_1(sc, num));
      wrong_type_error_nr(sc, sc->divide_symbol, 1, num, a_number_string);
    }
  return(NULL);
}

s7_pointer s7i_invert_p_p(s7_scheme *sc, s7_pointer x) {return(invert_p_p(sc, x));}

static s7_pointer divide_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  /* splitting out real/real here saves very little */
  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	  /* -------- integer x -------- */
	case T_INTEGER:
	  if (integer(y) == 0)
	    division_by_zero_error_2_nr(sc, sc->divide_symbol, x, y);
	  if (integer(x) == 1)  /* mainly to handle (/ 1 -9223372036854775808) correctly! */
	    return(invert_p_p(sc, y));
	  return(make_ratio(sc, integer(x), integer(y))); /* make_ratio calls gcd */
	case T_RATIO:
#if HAVE_OVERFLOW_CHECKS
	  {
	    s7_int dn;
	    if (multiply_overflow(integer(x), denominator(y), &dn))
              {
		if (WITH_WARNINGS) s7_warn(sc, 128, "integer / ratio overflow: (/ %" ld64 " %" ld64 "/%" ld64 ")\n", integer(x), numerator(y), denominator(y));
  	        return(make_real(sc, integer(x) * inverted_fraction(y)));
	      }
	    return(make_ratio_with_div_check(sc, sc->divide_symbol, dn, numerator(y)));
	  }
#else
	  return(make_ratio_with_div_check(sc, sc->divide_symbol, integer(x) * denominator(y), numerator(y)));
#endif
	case T_REAL:
	  if (is_NaN(real(y))) return(y);
	  if (is_inf(real(y))) return(real_zero);
	  if (real(y) == 0.0)
	    division_by_zero_error_2_nr(sc, sc->divide_symbol, x, y);
	  return(make_real(sc, (s7_double)(integer(x)) / real(y)));
	case T_COMPLEX:
	  {
	    s7_double r1 = (s7_double)integer(x), r2 = real_part(y), i2 = imag_part(y);
	    s7_double den = 1.0 / (r2 * r2 + i2 * i2);
	    /* we could avoid the squaring (see Knuth II p613 16), not a big deal: (/ 1.0e308+1.0e308i 2.0e308+2.0e308i) => nan, (gmp case is ok here) */
	    return(make_complex(sc, r1 * r2 * den, -(r1 * i2 * den)));
	  }

	default:
	  return(method_or_bust_with_type_and_loc_pp(sc, y, sc->divide_symbol, x, y, a_number_string, 2));
	}
      break;

      /* -------- ratio x -------- */
    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  if (integer(y) == 0)
	    division_by_zero_error_2_nr(sc, sc->divide_symbol, x, y);
#if HAVE_OVERFLOW_CHECKS
	  {
	    s7_int dn;
	    if (multiply_overflow(denominator(x), integer(y), &dn))
              {
		if (WITH_WARNINGS)
		  s7_warn(sc, 128, "ratio / integer overflow: (/ %" ld64 "/%" ld64 " %" ld64 ")\n", numerator(x), denominator(x), integer(y));
	        return(make_real(sc, (long_double)numerator(x) / ((long_double)denominator(x) * (long_double)integer(y))));
	      }
	    return(make_ratio_with_div_check(sc, sc->divide_symbol, numerator(x), dn));
	  }
#else
	  return(make_ratio_with_div_check(sc, sc->divide_symbol, numerator(x), denominator(x) * integer(y)));
#endif
	case T_RATIO:
	  {
	    s7_int d1, d2, n1, n2;
	    parcel_out_fractions(x, y);
	    if (d1 == d2)
	      return(make_ratio_with_div_check(sc, sc->divide_symbol, n1, n2));
#if HAVE_OVERFLOW_CHECKS
	    if ((multiply_overflow(n1, d2, &n1)) ||
		(multiply_overflow(n2, d1, &d1)))
	      {
		s7_double r1, r2;
		if (WITH_WARNINGS)
		  s7_warn(sc, 128, "ratio / ratio overflow: (/ %" ld64 "/%" ld64 " %" ld64 "/%" ld64 ")\n", numerator(x), denominator(x), numerator(y), denominator(y));
		r1 = fraction(x);
		r2 = inverted_fraction(y);
		return(make_real(sc, r1 * r2));
	      }
	    return(make_ratio_with_div_check(sc, sc->divide_symbol, n1, d1));
#else
	    return(make_ratio_with_div_check(sc, sc->divide_symbol, n1 * d2, n2 * d1));
#endif
	  }
	case T_REAL:
	  if (real(y) == 0.0)
	    division_by_zero_error_2_nr(sc, sc->divide_symbol, x, y);
	  return(make_real(sc, (s7_double)fraction(x) / real(y)));
	case T_COMPLEX:
	  {
	    s7_double rx = fraction(x), r2 = real_part(y), i2 = imag_part(y);
	    s7_double den = 1.0 / (r2 * r2 + i2 * i2);
	    return(make_complex(sc, rx * r2 * den, -rx * i2 * den)); /* not unchecked: (/ 3/4 -inf.0+i) */
	  }

	default:
	  return(method_or_bust_with_type_and_loc_pp(sc, y, sc->divide_symbol, x, y, a_number_string, 2));
	}

      /* -------- real x -------- */
    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER:
	  if (integer(y) == 0)
	    division_by_zero_error_2_nr(sc, sc->divide_symbol, x, y);
	  if (is_NaN(real(x))) return(x); /* what is (/ +nan.0 0)? */
	  if (is_inf(real(x)))
	    return((real(x) > 0.0) ? ((integer(y) > 0) ? real_infinity : real_minus_infinity) : ((integer(y) > 0) ? real_minus_infinity : real_infinity));
	  return(make_real(sc, (long_double)real(x) / (long_double)integer(y)));
	case T_RATIO:
	  if (is_NaN(real(x))) return(x);
	  if (is_inf(real(x)))
	    return((real(x) > 0) ? ((numerator(y) > 0) ? real_infinity : real_minus_infinity) : ((numerator(y) > 0) ? real_minus_infinity : real_infinity));
	  return(make_real(sc, real(x) * inverted_fraction(y)));
	case T_REAL:
	  if (is_NaN(real(y))) return(y);
	  if (real(y) == 0.0)
	    division_by_zero_error_2_nr(sc, sc->divide_symbol, x, y);
	  if (is_NaN(real(x))) return(x);
	  if (is_inf(real(y)))
	    return((is_inf(real(x))) ? real_NaN : real_zero);
	  return(make_real(sc, real(x) / real(y)));
	case T_COMPLEX:
	  {
	    s7_double den, r2, i2;
	    if (is_NaN(real(x))) return(complex_NaN);
	    r2 = real_part(y);
	    i2 = imag_part(y);
	    if ((is_NaN(r2)) || (is_inf(r2))) return(complex_NaN);
	    if ((is_NaN(i2)) || (is_inf(i2))) return(complex_NaN);
	    den = 1.0 / (r2 * r2 + i2 * i2);
	    return(make_complex(sc, real(x) * r2 * den, -real(x) * i2 * den));
	  }

	default:
	  return(method_or_bust_with_type_and_loc_pp(sc, y, sc->divide_symbol, x, y, a_number_string, 2));
	}

      /* -------- complex x -------- */
    case T_COMPLEX:
      switch (type(y))
	{
	case T_INTEGER:
	  {
	    s7_double r1;
	    if (integer(y) == 0)
	      division_by_zero_error_2_nr(sc, sc->divide_symbol, x, y);
	    r1 = (long_double)1.0 / (long_double)integer(y);
	    return(make_complex(sc, real_part(x) * r1, imag_part(x) * r1));
	  }
	case T_RATIO:
	  {
	    s7_double frac = inverted_fraction(y);
	    return(make_complex(sc, real_part(x) * frac, imag_part(x) * frac));
	  }
	case T_REAL:
	  {
	    s7_double r1;
	    if (real(y) == 0.0)
	      division_by_zero_error_2_nr(sc, sc->divide_symbol, x, y);
	    r1 = 1.0 / real(y);
	    return(make_complex(sc, real_part(x) * r1, imag_part(x) * r1)); /* (/ 0.0+1.0i +inf.0) */
	  }
	case T_COMPLEX:
	  {
	    s7_double r1 = real_part(x), r2, i1, i2, den;
	    if (is_NaN(r1)) return(x);
	    i1 = imag_part(x);
	    if (is_NaN(i1)) return(x);
	    r2 = real_part(y);
	    if (is_NaN(r2)) return(y);
	    if (is_inf(r2)) return(complex_NaN);
	    i2 = imag_part(y);
	    if (is_NaN(i2)) return(y);
	    den = 1.0 / (r2 * r2 + i2 * i2);
	    return(make_complex(sc, (r1 * r2 + i1 * i2) * den, (r2 * i1 - r1 * i2) * den));
	  }

	default:
	  return(method_or_bust_with_type_and_loc_pp(sc, y, sc->divide_symbol, x, y, a_number_string, 2));
	}


    default: /* x is not a built-in number */
      return(method_or_bust_pp(sc, x, sc->divide_symbol, x, y, a_number_string, 1)); /* not args here! y = apply * to cdr(args) */
    }
  if (S7_DEBUGGING) fprintf(stderr, "%s[%d]: we should not be here\n", __func__, __LINE__);
  return(NULL); /* make the compiler happy */
}

s7_pointer s7i_divide_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(divide_p_pp(sc, x, y));}

static s7_pointer g_divide(s7_scheme *sc, s7_pointer args)
{
  #define H_divide "(/ x1 ...) divides its first argument by the rest, or inverts the first if there is only one argument"
  #define Q_divide sc->pcl_n

  s7_pointer x = car(args), p = cdr(args);
  if (is_null(p))            /* (/ x) */
    {
      if (!is_number(x))
	return(method_or_bust_p(sc, x, sc->divide_symbol, a_number_string));
      return(invert_p_p(sc, x));
    }
  for (sc->error_argnum = 0; is_pair(p); p = cdr(p), sc->error_argnum++)
    x = divide_p_pp(sc, x, car(p));
  sc->error_argnum = 0;
  return(x);
}

static s7_pointer g_divide_by_2(s7_scheme *sc, s7_pointer args)
{
  const s7_pointer num = car(args);
  if (is_t_integer(num))
    {
      s7_int i = integer(num);
      if (i & 1)
	{
	  s7_pointer x;
	  new_cell(sc, x, T_RATIO);
	  set_numerator(x, i);
	  set_denominator(x, 2);
	  return(x);
	}
      return(make_integer(sc, i >> 1));
    }
  switch (type(num))
    {
    case T_RATIO:
#if HAVE_OVERFLOW_CHECKS
      {
	s7_int dn;
	if (multiply_overflow(denominator(num), 2, &dn))
	  {
	    if ((numerator(num) & 1) == 1)
	      {
		if (WITH_WARNINGS) s7_warn(sc, 128, "ratio / 2 overflow: (/ %" ld64 "/%" ld64 " 2)\n", numerator(num), denominator(num));
	        return(make_real(sc, ((long_double)numerator(num) * 0.5) / (long_double)denominator(num)));
	      }
	    return(make_ratio(sc, numerator(num) / 2, denominator(num)));
	  }
	return(make_ratio_with_div_check(sc, sc->divide_symbol, numerator(num), dn));
      }
#else
      return(make_ratio(sc, numerator(num), denominator(num) * 2));
#endif
    case T_REAL:    return(make_real(sc, real(num) * 0.5));
    case T_COMPLEX: return(make_complex_not_0i(sc, real_part(num) * 0.5, imag_part(num) * 0.5));

    default:
      return(method_or_bust_pp(sc, num, sc->divide_symbol, num, int_two, a_number_string, 1));
    }
}

static s7_pointer g_invert_x(s7_scheme *sc, s7_pointer args)
{
  /* (/ 1.0 x) */
  const s7_pointer x = cadr(args);
  if (is_t_real(x))
    {
      s7_double rl = real(x);
      if (rl == 0.0)
	division_by_zero_error_2_nr(sc, sc->divide_symbol, car(args), x);
      return((is_NaN(rl)) ? x : make_real(sc, 1.0 / rl));
    }
  return(divide_p_pp(sc, car(args), x));
}

static s7_double divide_d_7d(s7_scheme *sc, s7_double x)
{
  if (x == 0.0) division_by_zero_error_1_nr(sc, sc->divide_symbol, real_zero);
  return(1.0 / x);
}

static s7_double divide_d_7dd(s7_scheme *sc, s7_double x1, s7_double x2)
{
  if (x2 == 0.0) division_by_zero_error_1_nr(sc, sc->divide_symbol, real_zero);
  return(x1 / x2);
}

static s7_pointer divide_p_ii(s7_scheme *sc, s7_int x, s7_int y) {return(make_ratio_with_div_check(sc, sc->divide_symbol, x, y));}
static s7_pointer divide_p_i(s7_scheme *sc, s7_int x) {return(make_ratio_with_div_check(sc, sc->divide_symbol, 1, x));}

static s7_pointer divide_chooser(s7_scheme *sc, s7_pointer func, int32_t args, s7_pointer expr)
{
  if (args == 1) return(sc->invert_1);
  if (args == 2)
    {
      const s7_pointer arg1 = cadr(expr), arg2 = caddr(expr);
      if ((is_t_real(arg1)) && (real(arg1) == 1.0)) return(sc->invert_x);
      if ((is_pair(arg1)) && (has_fn(arg1)))
	{
	  if (fn_proc(arg1) == g_multiply_2) set_fn_direct(arg1, g_multiply_2_wrapped);
	  else if (fn_proc(arg1) == g_multiply_3) set_fn_direct(arg1, g_multiply_3_wrapped);
	}
      if ((is_pair(arg2)) && (has_fn(arg2)) && (fn_proc(arg2) == g_multiply_2)) set_fn_direct(arg2, g_multiply_2_wrapped);
      return(((is_t_integer(arg2)) && (integer(arg2) == 2)) ? sc->divide_by_2 : sc->divide_2);
    }
  return(func);
}


/* -------------------------------- quotient -------------------------------- */
static inline s7_int quotient_i_7ii(s7_scheme *sc, s7_int x, s7_int y)
{
  if ((y > 0) || (y < -1)) return(x / y);
  if (y == 0)
    division_by_zero_error_2_nr(sc, sc->quotient_symbol, wrap_integer(sc, x), int_zero);
  if (x == S7_INT64_MIN)   /* (quotient most-negative-fixnum -1) */
    sole_arg_out_of_range_error_nr(sc, sc->quotient_symbol, set_elist_2(sc, leastfix, minus_one), it_is_too_large_string);
  return(-x); /* (quotient x -1) */
}

static s7_pointer s7_truncate(s7_scheme *sc, s7_pointer caller, s7_double xf)   /* can't use "truncate" -- it's in unistd.h */
{
  if (fabs(xf) > QUOTIENT_FLOAT_LIMIT)
    sole_arg_out_of_range_error_nr(sc, caller, wrap_real(sc, xf), it_is_too_large_string);
  return(make_integer(sc, (xf > 0.0) ? (s7_int)floor(xf) : (s7_int)ceil(xf)));
}

static s7_int c_quo_dbl(s7_scheme *sc, s7_double x, s7_double y)
{
  s7_double xf;
  if (y == 0.0)
    division_by_zero_error_2_nr(sc, sc->quotient_symbol, wrap_real(sc, x), real_zero);
  if ((is_inf(y)) || (is_NaN(y))) /* here we can't return NAN so I guess we should signal an error */
    wrong_type_error_nr(sc, sc->quotient_symbol, 2, wrap_real(sc, y), a_normal_real_string);
  xf = x / y;
  if (fabs(xf) > QUOTIENT_FLOAT_LIMIT)
    sole_arg_out_of_range_error_nr(sc, sc->quotient_symbol, wrap_real(sc, xf), it_is_too_large_string);
  return((xf > 0.0) ? (s7_int)floor(xf) : (s7_int)ceil(xf));
}

static s7_int quotient_i_ii_unchecked(s7_int i1, s7_int i2) {return(i1 / i2);} /* i2 > 0 */

static s7_pointer quotient_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{

  s7_int d1, d2, n1, n2;
  if ((is_t_integer(x)) && (is_t_integer(y)))
    return(make_integer(sc, quotient_i_7ii(sc, integer(x), integer(y))));

  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  return(make_integer(sc, quotient_i_7ii(sc, integer(x), integer(y))));
	case T_RATIO:
	  n1 = integer(x);
	  d1 = 1;
	  n2 = numerator(y);
	  d2 = denominator(y);
	  /* (quotient -9223372036854775808 -1/9223372036854775807): arithmetic exception in the no-overflow-checks case */
	  goto RATIO_QUO_RATIO;
	case T_REAL:
	  if (real(y) == 0.0)
	    division_by_zero_error_2_nr(sc, sc->quotient_symbol, x, y);
	  if (is_inf(real(y))) return(make_nan_with_payload(sc, __LINE__));
	  if (is_NaN(real(y))) return(y);
	  return(s7_truncate(sc, sc->quotient_symbol, (s7_double)integer(x) / real(y))); /* s7_truncate returns an integer */
	default:
	  return(method_or_bust_pp(sc, y, sc->quotient_symbol, x, y, sc->type_names[T_REAL], 2));
	}

    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  if (integer(y) == 0)
	    division_by_zero_error_2_nr(sc, sc->quotient_symbol, x, y);
	  n1 = numerator(x);
	  d1 = denominator(x);
	  n2 = integer(y);
	  d2 = 1;
	  goto RATIO_QUO_RATIO;
	  /* this can lose:
	   *   (quotient 1 2305843009213693952/4611686018427387903) -> 2, not 1
	   *   (quotient 21053343141/6701487259 3587785776203/1142027682075) -> 1, not 0
	   */
	case T_RATIO:
	  parcel_out_fractions(x, y);
	RATIO_QUO_RATIO:
	  if (d1 == d2)
	    return(make_integer(sc, n1 / n2));              /* (quotient 3/9223372036854775807 1/9223372036854775807) */
	  if (n1 == n2)
	    return(make_integer(sc, d2 / d1));              /* (quotient 9223372036854775807/2 9223372036854775807/8) */
#if HAVE_OVERFLOW_CHECKS
	  {
	    s7_int n1d2, n2d1;
	    if ((multiply_overflow(n1, d2, &n1d2)) ||
		(multiply_overflow(n2, d1, &n2d1)))
	      return(s7_truncate(sc, sc->quotient_symbol, ((long_double)n1 / (long_double)n2) * ((long_double)d2 / (long_double)d1)));
	    return(make_integer(sc, n1d2 / n2d1));
	  }
#else
	  return(make_integer(sc, (n1 * d2) / (n2 * d1)));
#endif
	case T_REAL:
	  if (real(y) == 0.0)
	    division_by_zero_error_2_nr(sc, sc->quotient_symbol, x, y);
	  if (is_inf(real(y))) return(make_nan_with_payload(sc, __LINE__));
	  if (is_NaN(real(y))) return(y);
	  return(s7_truncate(sc, sc->quotient_symbol, (s7_double)fraction(x) / real(y)));
	default:
	  return(method_or_bust_pp(sc, y, sc->quotient_symbol, x, y, sc->type_names[T_REAL], 2));
	}
    case T_REAL:
      if (((is_inf(real(x))) || (is_NaN(real(x)))) && (is_real(y)))
	return(make_nan_with_payload(sc, __LINE__));
      /* if infs allowed we need to return infs/nans, else:
       *    (quotient inf.0 1e-309) -> -9223372036854775808
       *    (quotient inf.0 inf.0) -> -9223372036854775808
       */
      switch (type(y))
	{
	case T_INTEGER:
	  if (integer(y) == 0)
	    division_by_zero_error_2_nr(sc, sc->quotient_symbol, x, y);
	  return(s7_truncate(sc, sc->quotient_symbol, (long_double)real(x) / (long_double)integer(y)));

	case T_RATIO: return(s7_truncate(sc, sc->quotient_symbol, real(x) / (s7_double)fraction(y)));
	case T_REAL:  return(make_integer(sc, c_quo_dbl(sc, real(x), real(y)))); /* c_quo_dbl returns an integer */
	default:      return(method_or_bust_pp(sc, y, sc->quotient_symbol, x, y, sc->type_names[T_REAL], 2));
	}
    default:
      return(method_or_bust_pp(sc, x, sc->quotient_symbol, x, y, sc->type_names[T_REAL], 2));
    }
}

static s7_pointer quotient_p_pi(s7_scheme *sc, s7_pointer x, s7_int y)
{
  if ((is_t_integer(x)) && ((y > 0) || (y < -1))) return(make_integer(sc, integer(x) / y));
  return(quotient_p_pp(sc, x, wrap_integer(sc, y)));
}

s7_pointer s7i_quotient_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(quotient_p_pp(sc, x, y));}

#define H_quotient "(quotient x1 x2) returns the integer quotient of x1 and x2; (quotient 4 3) = 1"
#define Q_quotient sc->pcl_r


/* -------------------------------- remainder -------------------------------- */

#define REMAINDER_FLOAT_LIMIT 1e13

static inline s7_int remainder_i_7ii(s7_scheme *sc, s7_int x, s7_int y)
{
  if ((y > 1) || (y < -1)) return(x % y); /* avoid floating exception if (remainder -9223372036854775808 -1)! */
  if (y == 0)
    division_by_zero_error_2_nr(sc, sc->remainder_symbol, wrap_integer(sc, x), int_zero);
  return(0);
}

static s7_double c_rem_dbl(s7_scheme *sc, s7_double x, s7_double y)
{
  s7_int quo;
  s7_double pre_quo;
  if (is_NaN(y)) return(y);
  if (is_inf(y)) return(NAN);
  pre_quo = x / y;
  if (fabs(pre_quo) > REMAINDER_FLOAT_LIMIT)
    sole_arg_out_of_range_error_nr(sc, sc->remainder_symbol, set_elist_2(sc, wrap_real(sc, x), wrap_real(sc, y)), it_is_too_large_string);
  quo = (pre_quo > 0.0) ? (s7_int)floor(pre_quo) : (s7_int)ceil(pre_quo);
  return(x - (y * quo));
}

static s7_int remainder_i_ii_unchecked(s7_int i1, s7_int i2) {return(i1 % i2);} /* i2 > 1 */
static s7_double remainder_d_7dd(s7_scheme *sc, s7_double x, s7_double y)
{
  if (y == 0.0)
    division_by_zero_error_2_nr(sc, sc->remainder_symbol, wrap_real(sc, x), real_zero);
  if (is_NaN(x)) return(x);
  if (is_inf(x)) return(NAN); /* match remainder_p_pp */
  return(c_rem_dbl(sc, x, y));
}

static s7_pointer remainder_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  s7_int quo, d1, d2, n1, n2;
  s7_double pre_quo;

  if ((is_t_integer(x)) && (is_t_integer(y)))
    return(make_integer(sc, remainder_i_7ii(sc, integer(x), integer(y))));

  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  return(make_integer(sc, remainder_i_7ii(sc, integer(x), integer(y))));
	case T_RATIO:
	  n1 = integer(x);
	  d1 = 1;
	  n2 = numerator(y);
	  d2 = denominator(y);
	  goto RATIO_REM_RATIO;
	case T_REAL:
	  if (real(y) == 0.0)
	    division_by_zero_error_2_nr(sc, sc->remainder_symbol, x, y);
	  if (is_inf(real(y))) return(make_nan_with_payload(sc, __LINE__));
	  if (is_NaN(real(y))) return(y);
	  pre_quo = (long_double)integer(x) / (long_double)real(y);
	  if (fabs(pre_quo) > REMAINDER_FLOAT_LIMIT)
	    sole_arg_out_of_range_error_nr(sc, sc->remainder_symbol, set_elist_2(sc, x, y), it_is_too_large_string);
	  quo = (pre_quo > 0.0) ? (s7_int)floor(pre_quo) : (s7_int)ceil(pre_quo);
	  return(make_real(sc, integer(x) - real(y) * quo));
	default:
	  return(method_or_bust_pp(sc, y, sc->remainder_symbol, x, y, sc->type_names[T_REAL], 2));
	}
    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  n2 = integer(y);
 	  if (n2 == 0)
 	    division_by_zero_error_2_nr(sc, sc->remainder_symbol, x, y);
	  n1 = numerator(x);
	  d1 = denominator(x);
	  d2 = 1;
	  goto RATIO_REM_RATIO;
	case T_RATIO:
	  parcel_out_fractions(x, y);
	RATIO_REM_RATIO:
	  if (d1 == d2)
	    quo = (s7_int)(n1 / n2);
	  else
	    {
	      if (n1 == n2)
		quo = (s7_int)(d2 / d1);
	      else
		{
#if HAVE_OVERFLOW_CHECKS
		  s7_int n1d2, n2d1;
		  if ((multiply_overflow(n1, d2, &n1d2)) ||
		      (multiply_overflow(n2, d1, &n2d1)))
		    {
		      pre_quo = ((long_double)n1 / (long_double)n2) * ((long_double)d2 / (long_double)d1);
		      if (fabs(pre_quo) > REMAINDER_FLOAT_LIMIT)
			sole_arg_out_of_range_error_nr(sc, sc->remainder_symbol, set_elist_2(sc, x, y), it_is_too_large_string);
		      quo = (pre_quo > 0.0) ? (s7_int)floor(pre_quo) : (s7_int)ceil(pre_quo);
		    }
		  else quo = n1d2 / n2d1;
#else
		  quo = (n1 * d2) / (n2 * d1);
#endif
		}}
	  if (quo == 0)
	    return(x);
#if HAVE_OVERFLOW_CHECKS
	  {
	    s7_int dn, nq;
	    if (!multiply_overflow(n2, quo, &nq))
	      {
		if ((d1 == d2) &&
		    (!subtract_overflow(n1, nq, &dn)))
		  return(make_ratio_with_div_check(sc, sc->remainder_symbol, dn, d1));

		if ((!multiply_overflow(n1, d2, &dn)) &&
		    (!multiply_overflow(nq, d1, &nq)) &&
		    (!subtract_overflow(dn, nq, &nq)) &&
		    (!multiply_overflow(d1, d2, &d1)))
		  return(make_ratio_with_div_check(sc, sc->remainder_symbol, nq, d1));
	      }}
#else
	  if (d1 == d2)
	    return(make_ratio_with_div_check(sc, sc->remainder_symbol, n1 - n2 * quo, d1));

	  return(make_ratio_with_div_check(sc, sc->remainder_symbol, n1 * d2 - n2 * d1 * quo, d1 * d2));
#endif
	  sole_arg_out_of_range_error_nr(sc, sc->remainder_symbol, set_elist_2(sc, x, y), intermediate_too_large_string);
	case T_REAL:
	  {
	    s7_double frac;
	    if (real(y) == 0.0)
	      division_by_zero_error_2_nr(sc, sc->remainder_symbol, x, y);
	    if (is_inf(real(y))) return(make_nan_with_payload(sc, __LINE__));
	    if (is_NaN(real(y))) return(y);
	    if (s7_int_abs(numerator(x)) > QUOTIENT_INT_LIMIT)
	      return(subtract_p_pp(sc, x, multiply_p_pp_wrapped(sc, y, quotient_p_pp(sc, x, y))));
	    frac = (s7_double)fraction(x);
	    pre_quo = frac / real(y);
	    if (fabs(pre_quo) > REMAINDER_FLOAT_LIMIT)
	      sole_arg_out_of_range_error_nr(sc, sc->remainder_symbol, set_elist_2(sc, x, y), it_is_too_large_string);
	    quo = (pre_quo > 0.0) ? (s7_int)floor(pre_quo) : (s7_int)ceil(pre_quo);
	    return(make_real(sc, frac - real(y) * quo));
	  }
	default:
	  return(method_or_bust_pp(sc, y, sc->remainder_symbol, x, y, sc->type_names[T_REAL], 2));
	}
    case T_REAL:
      if (((is_inf(real(x))) || (is_NaN(real(x)))) && (is_real(y)))
	{
	  if (is_zero(y))
	    division_by_zero_error_2_nr(sc, sc->remainder_symbol, x, y);
	  return(make_nan_with_payload(sc, __LINE__));
	}
      switch (type(y))
	{
	case T_INTEGER:
	  if (integer(y) == 0)
	    division_by_zero_error_2_nr(sc, sc->remainder_symbol, x, y);
	  /* actually here (and elsewhere) if y > INT64_TO_DOUBLE_LIMIT, the result is probably wrong */
	  pre_quo = (long_double)real(x) / (long_double)integer(y);
	  if (fabs(pre_quo) > REMAINDER_FLOAT_LIMIT)
	    sole_arg_out_of_range_error_nr(sc, sc->remainder_symbol, set_elist_2(sc, x, y), it_is_too_large_string);
	  quo = (pre_quo > 0.0) ? (s7_int)floor(pre_quo) : (s7_int)ceil(pre_quo);
	  return(make_real(sc, real(x) - integer(y) * quo));
	  /* but... (remainder 1e+18 9223372036854775807) -> 1e+18 */
	case T_RATIO:
	  if (s7_int_abs(numerator(y)) > QUOTIENT_INT_LIMIT)
	    return(subtract_p_pp(sc, x, multiply_p_pp_wrapped(sc, y, quotient_p_pp(sc, x, y))));
	  {
	    s7_double frac = (s7_double)fraction(y);
	    pre_quo = real(x) / frac;
	    if (fabs(pre_quo) > REMAINDER_FLOAT_LIMIT)
	      sole_arg_out_of_range_error_nr(sc, sc->remainder_symbol, set_elist_2(sc, x, y), it_is_too_large_string);
	    quo = (pre_quo > 0.0) ? (s7_int)floor(pre_quo) : (s7_int)ceil(pre_quo);
	    return(make_real(sc, real(x) - frac * quo));
	  }
	case T_REAL:
	  if (real(y) == 0.0)
	    division_by_zero_error_2_nr(sc, sc->remainder_symbol, x, y);
	  return(make_real(sc, c_rem_dbl(sc, real(x), real(y))));
	  /* see under sin -- this calculation is completely bogus if "a" is large
	   * (quotient 1e22 (* 2 pi)) -> -9223372036854775808 but it should be 1591549430918953357688,
	   * (remainder 1e22 (* 2 pi)) -> 1.0057952155665e+22 -- the "remainder" is greater than the original argument!
	   * Clisp gives 0.0 here, as does sbcl, currently s7 throws an error (out-of-range).
	   */
	default:
	  return(method_or_bust_pp(sc, y, sc->remainder_symbol, x, y, sc->type_names[T_REAL], 2));
	}
    default:
      return(method_or_bust_pp(sc, x, sc->remainder_symbol, x, y, sc->type_names[T_REAL], 1));
    }
}

static s7_pointer remainder_p_pi(s7_scheme *sc, s7_pointer x, s7_int y)
{
  if ((is_t_integer(x)) && ((y > 1) || (y < -1))) return(make_integer(sc, integer(x) % y));
  return(remainder_p_pp(sc, x, wrap_integer(sc, y)));
}

s7_pointer s7i_remainder_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(remainder_p_pp(sc, x, y));}

#define H_remainder "(remainder x y) returns the remainder of x/y; (remainder 10 3) = 1"
#define Q_remainder sc->pcl_r


/* -------------------------------- modulo -------------------------------- */
static s7_int modulo_i_ii(s7_int x, s7_int y)
{
  s7_int z;
  if (y > 1)
    {
      z = x % y;
      return((z >= 0) ? z : z + y);
    }
  if (y < -1)
    {
      z = x % y;
      return((z > 0) ? z + y : z);
    }
  if (y == 0) return(x);     /* else arithmetic exception */
  return(0);
}

static s7_int modulo_i_ii_unchecked(s7_int i1, s7_int i2) /* here we know i2 > 1 */
{
  s7_int z = i1 % i2;
  return((z < 0) ? (z + i2) : z);
}

static s7_double modulo_d_7dd(s7_scheme *sc, s7_double x, s7_double y)
{
  s7_double z;
  if (is_NaN(x)) return(x);
  if (is_NaN(y)) return(y);
  if ((is_inf(x)) || (is_inf(y))) return(NAN);
  if (y == 0.0) return(x);
  if (fabs(x) > 1e17)
    out_of_range_error_nr(sc, sc->modulo_symbol, int_one, wrap_real(sc, x), it_is_too_large_string);
  z = x / y;
  if ((z > 1e19) || (z < -1e19))
    sole_arg_out_of_range_error_nr(sc, sc->modulo_symbol,
			set_elist_3(sc, sc->divide_symbol, wrap_real(sc, x), wrap_real(sc, y)),
			intermediate_too_large_string);
  return(x - y * (s7_int)floor(z));
}

static s7_pointer modulo_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  s7_double a, b;
  s7_int n1, n2, d1, d2;
  if ((is_t_integer(x)) && (is_t_integer(y))) /* this is nearly always the case */
    return(make_integer(sc, modulo_i_ii(integer(x), integer(y))));

  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_INTEGER:
	  return(make_integer(sc, modulo_i_ii(integer(x), integer(y))));
	case T_RATIO:
	  n1 = integer(x);
	  d1 = 1;
	  n2 = numerator(y);
	  d2 = denominator(y);
	  if ((n1 == n2) && (d1 > d2)) return(x); /* signs match so this should be ok */
	  goto RATIO_MOD_RATIO;
	case T_REAL:
	  if ((integer(x) == S7_INT64_MIN) || (s7_int_abs(integer(x)) > QUOTIENT_INT_LIMIT))
	    out_of_range_error_nr(sc, sc->modulo_symbol, int_one, x, it_is_too_large_string);
	  b = real(y);
	  if (b == 0.0) return(x);
	  if (is_NaN(b)) return(y);
	  if (is_inf(b)) return(make_nan_with_payload(sc, __LINE__));
	  a = (s7_double)integer(x);
	  goto REAL_MOD;
	default:
	  return(method_or_bust_pp(sc, y, sc->modulo_symbol, x, y, sc->type_names[T_REAL], 2));
	}
    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  if (integer(y) == 0) return(x);
	  n1 = numerator(x);
	  d1 = denominator(x);
	  n2 = integer(y);
	  if ((n2 > 0) && (n1 > 0) && (n2 > n1)) return(x);
	  if ((n2 < 0) && (n1 < 0) && (n2 < n1)) return(x);
	  if (n2 == S7_INT64_MIN)
	    sole_arg_out_of_range_error_nr(sc, sc->modulo_symbol, set_elist_3(sc, sc->divide_symbol, x, y), intermediate_too_large_string);
	  /* the problem here is that (modulo 3/2 most-negative-fixnum)
	   * will segfault with signal SIGFPE, Arithmetic exception, so try to trap it.
	   */
	  if ((n1 == n2) && (d1 > 1)) return(x);
	  d2 = 1;
	  goto RATIO_MOD_RATIO;
	case T_RATIO:
	  parcel_out_fractions(x, y);
	  if (d1 == d2)
	    return(make_ratio_with_div_check(sc, sc->modulo_symbol, modulo_i_ii(n1, n2), d1));
	  if ((n1 == n2) && (d1 > d2)) return(x);
	RATIO_MOD_RATIO:
#if HAVE_OVERFLOW_CHECKS
	  {
	    s7_int n2d1, n1d2, d1d2, fl;
	    if (!multiply_overflow(n2, d1, &n2d1))
	      {
		if ((n2d1 == 1) || (n2d1 == -1)) /* (modulo 100 -1/2) */
		  return(int_zero);
		if (!multiply_overflow(n1, d2, &n1d2))
		  {
		    fl = (s7_int)(n1d2 / n2d1);
		    if (((n1 < 0) && (n2 > 0)) ||
			((n1 > 0) && (n2 < 0)))
		      fl -= 1;
		    if (fl == 0)
		      return(x);
		    if ((!multiply_overflow(d1, d2, &d1d2)) &&
			(!multiply_overflow(fl, n2d1, &fl)) &&
			(!subtract_overflow(n1d2, fl, &fl)))
		      return(make_ratio_with_div_check(sc, sc->modulo_symbol, fl, d1d2));
		  }}}
#else
	  {
	    s7_int fl;
	    s7_int n1d2 = n1 * d2;
	    s7_int n2d1 = n2 * d1;
	    if ((n2d1 == 1) || (n2d1 == -1)) /* (modulo 100 -1/2) as above) */
	      return(int_zero);
	    /* can't use "floor" here (float->int ruins everything) */
	    fl = (s7_int)(n1d2 / n2d1);
	    if (((n1 < 0) && (n2 > 0)) ||
		((n1 > 0) && (n2 < 0)))
	      fl -= 1;
	    if (fl == 0)
	      return(x);
	    return(make_ratio_with_div_check(sc, sc->modulo_symbol, n1d2 - (n2d1 * fl), d1 * d2));
	  }
#endif
	  sole_arg_out_of_range_error_nr(sc, sc->modulo_symbol,
				     set_elist_3(sc, sc->divide_symbol, x, y),
				     intermediate_too_large_string);
	case T_REAL:
	  b = real(y);
	  if (is_inf(b)) return(make_nan_with_payload(sc, __LINE__));
	  if (fabs(b) > 1e17)
	    out_of_range_error_nr(sc, sc->modulo_symbol, int_two, y, it_is_too_large_string);
	  if (b == 0.0) return(x);
	  if (is_NaN(b)) return(y);
	  a = fraction(x);
	  return(make_real(sc, a - b * (s7_int)floor(a / b)));
	default:
	  return(method_or_bust_pp(sc, y, sc->modulo_symbol, x, y, sc->type_names[T_REAL], 2));
	}
    case T_REAL:
      a = real(x);
      if (!is_real(y))
	return(method_or_bust_pp(sc, y, sc->modulo_symbol, x, y, sc->type_names[T_REAL], 2));
      if (is_NaN(a)) return(x);
      if (is_inf(a)) return(make_nan_with_payload(sc, __LINE__)); /* not b */
      if (fabs(a) > 1e17)
	out_of_range_error_nr(sc, sc->modulo_symbol, int_one, x, it_is_too_large_string);

      switch (type(y))
	{
	case T_INTEGER:
	  if (integer(y) == 0) return(x);
	  if ((integer(y) == S7_INT64_MIN) || (s7_int_abs(integer(y)) > QUOTIENT_INT_LIMIT))
	    out_of_range_error_nr(sc, sc->modulo_symbol, int_two, y, it_is_too_large_string);
	  b = (s7_double)integer(y);
	  goto REAL_MOD;
	case T_RATIO:
	  b = fraction(y);
	  goto REAL_MOD;
	case T_REAL:
	  b = real(y);
	  if (b == 0.0) return(x);
	  if (is_NaN(b)) return(y);
	  if (is_inf(b)) return(make_nan_with_payload(sc, __LINE__));
	REAL_MOD:
	  {
	    s7_double c = a / b;
	    if (fabs(c) > 1e19)
	      sole_arg_out_of_range_error_nr(sc, sc->modulo_symbol, set_elist_3(sc, sc->divide_symbol, x, y), intermediate_too_large_string);
	    return(make_real(sc, a - b * (s7_int)floor(c)));
	  }
	default:
	  return(method_or_bust_pp(sc, y, sc->modulo_symbol, x, y, sc->type_names[T_REAL], 2));
	}
    default:
      return(method_or_bust_pp(sc, x, sc->modulo_symbol, x, y, sc->type_names[T_REAL], 1));
    }
}

static s7_pointer modulo_p_pi(s7_scheme *sc, s7_pointer x, s7_int y)
{
  if (is_t_integer(x)) return(make_integer(sc, modulo_i_ii(integer(x), y)));
  return(modulo_p_pp(sc, x, wrap_integer(sc, y)));
}

s7_pointer s7i_modulo_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(modulo_p_pp(sc, x, y));}

#define H_modulo "(modulo x y) returns x mod y; (modulo 4 3) = 1.  The arguments can be real numbers."
#define Q_modulo sc->pcl_r


/* ---------------------------------------- max ---------------------------------------- */
static bool is_real_via_method_1(s7_scheme *sc, s7_pointer p)
{
  s7_pointer func = find_method_with_let(sc, p, sc->is_real_symbol);
  if (func != sc->undefined)
    return(is_true(sc, s7_apply_function(sc, func, set_plist_1(sc, p))));
  return(false);
}

#define is_real_via_method(sc, p) ((is_real(p)) || ((has_active_methods(sc, p)) && (is_real_via_method_1(sc, p))))

#define max_out_x(Sc, X, Y) method_or_bust_pp(Sc, X, Sc->max_symbol, X, Y, Sc->type_names[T_REAL], 1)
#define max_out_y(Sc, X, Y) method_or_bust_pp(Sc, Y, Sc->max_symbol, X, Y, Sc->type_names[T_REAL], 2)

s7_pointer max_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  /* same basic code as lt_b_7_pp (or any relop) but max returns NaN if NaN encountered, and methods for < and max return
   *    different results, so it seems simpler to repeat the other code.
   */
  if (type(x) == type(y))
    {
      if (is_t_integer(x))
	return((integer(x) < integer(y)) ? y : x);
      if (is_t_real(x))
	/* return(((is_NaN(real(x))) || (real(x) >= real(y))) ? x : y); */
	return(((real(x) >= real(y)) || (is_NaN(real(x)))) ? x : y);
      if (is_t_ratio(x))
	return((fraction(x) < fraction(y)) ? y : x);
    }
  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_RATIO:
	  return((integer(x) < fraction(y)) ? y : x);
	case T_REAL:
	  return(((integer(x) < real(y)) || (is_NaN(real(y)))) ? y : x);
	default:
	  return(max_out_y(sc, x, y));
	}
      break;
    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  return((fraction(x) < integer(y)) ? y : x);
	case T_REAL:
	  return(((fraction(x) < real(y)) || (is_NaN(real(y)))) ? y : x);
	default:
	  return(max_out_y(sc, x, y));
	}
    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER:
	  return(((real(x) >= integer(y)) || (is_NaN(real(x)))) ? x : y);
	case T_RATIO:
	  return((real(x) < fraction(y)) ? y : x);
	default:
	  return(max_out_y(sc, x, y));
	}
      break;

    default:
      return(max_out_x(sc, x, y));
    }
  return(x);
}

static s7_pointer max_chooser(s7_scheme *sc, s7_pointer func, int32_t args, s7_pointer unused_expr)
{
  return((args == 2) ? sc->max_2 : ((args == 3) ? sc->max_3 : func));
}

static s7_int max_i_ii(s7_int i1, s7_int i2) {return((i1 > i2) ? i1 : i2);}
static s7_int max_i_iii(s7_int i1, s7_int i2, s7_int i3) {return((i1 > i2) ? ((i1 > i3) ? i1 : i3) : ((i2 > i3) ? i2 : i3));}
static s7_double max_d_dd(s7_double x1, s7_double x2) {return(((x1 > x2) || (is_NaN(x1))) ? x1 : x2);}
static s7_double max_d_ddd(s7_double x1, s7_double x2, s7_double x3) {return(max_d_dd(x1, max_d_dd(x2, x3)));}
static s7_double max_d_dddd(s7_double x1, s7_double x2, s7_double x3, s7_double x4) {return(max_d_dd(x1, max_d_ddd(x2, x3, x4)));}


/* ---------------------------------------- min ---------------------------------------- */
#define min_out_x(Sc, X, Y) method_or_bust_pp(Sc, X, Sc->min_symbol, X, Y, Sc->type_names[T_REAL], 1)
#define min_out_y(Sc, X, Y) method_or_bust_pp(Sc, Y, Sc->min_symbol, X, Y, Sc->type_names[T_REAL], 2)

s7_pointer min_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (type(x) == type(y))
    {
      if (is_t_integer(x))
	return((integer(x) > integer(y)) ? y : x);
      if (is_t_real(x))
	return(((real(x) <= real(y)) || (is_NaN(real(x)))) ? x : y);
      if (is_t_ratio(x))
	return((fraction(x) > fraction(y)) ? y : x);
    }
  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_RATIO:       return((integer(x) > fraction(y)) ? y : x);
	case T_REAL:
	  return(((integer(x) > real(y)) || (is_NaN(real(y)))) ? y : x);
	default:
	  return(min_out_y(sc, x, y));
	}
      break;
    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER:
	  return((fraction(x) > integer(y)) ? y : x);
	case T_REAL:
	  return(((fraction(x) > real(y)) || (is_NaN(real(y)))) ? y : x);
	default:
	  return(min_out_y(sc, x, y));
	}
    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER:
	  return(((real(x) <= integer(y)) || (is_NaN(real(x)))) ? x : y);
	case T_RATIO:
	  return((real(x) > fraction(y)) ? y : x);
	default:
	  return(min_out_y(sc, x, y));
	}
      break;

    default:
      return(min_out_x(sc, x, y));
    }
  return(x);
}

static s7_pointer min_chooser(s7_scheme *sc, s7_pointer func, int32_t args, s7_pointer unused_expr)
{
  return((args == 2) ? sc->min_2 : ((args == 3) ? sc->min_3 : func));
}

static s7_int min_i_ii(s7_int i1, s7_int i2) {return((i1 < i2) ? i1 : i2);}
static s7_int min_i_iii(s7_int i1, s7_int i2, s7_int i3) {return((i1 < i2) ? ((i1 < i3) ? i1 : i3) : ((i2 < i3) ? i2 : i3));}
static s7_double min_d_dd(s7_double x1, s7_double x2) {return(((x1 < x2) || (is_NaN(x1))) ? x1 : x2);}
static s7_double min_d_ddd(s7_double x1, s7_double x2, s7_double x3) {return(min_d_dd(x1, min_d_dd(x2, x3)));}
static s7_double min_d_dddd(s7_double x1, s7_double x2, s7_double x3, s7_double x4) {return(min_d_dd(x1, min_d_ddd(x2, x3, x4)));}


/* ---------------------------------------- = ---------------------------------------- */
static bool eq_out_x(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (has_active_methods(sc, x))
    return(find_and_apply_method(sc, x, sc->num_eq_symbol, set_plist_2(sc, x, y)) != sc->F);
  wrong_type_error_nr(sc, sc->num_eq_symbol, 1, x, a_number_string);
  return(false);
}

static bool eq_out_y(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (has_active_methods(sc, y))
    return(find_and_apply_method(sc, y, sc->num_eq_symbol, set_plist_2(sc, x, y)) != sc->F);
  wrong_type_error_nr(sc, sc->num_eq_symbol, 2, y, a_number_string);
  return(false);
}

static bool num_eq_b_7pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  /* (= float int) here can be confusing if the float is the result of (say) (* 4478554083/3166815962 4478554083/3166815962) -- sometimes
   *   the extra low order bits are lost somewhere, so it looks like (= 2.0 2) returning #t.  Maybe the caller should have used eqv?
   */
  if (type(x) == type(y))
    {
      if (is_t_integer(x))
	return(integer(x) == integer(y));
      if (is_t_real(x))
	return(real(x) == real(y));
      if (is_t_complex(x))
	return((real_part(x) == real_part(y)) && (imag_part(x) == imag_part(y)));
      if (is_t_ratio(x))
	return((numerator(x) == numerator(y)) && (denominator(x) == denominator(y)));
    }

  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_RATIO:
	  return(false);
	case T_REAL:
	  return(integer(x) == real(y));
	case T_COMPLEX:
	  return(false);
	default: return(eq_out_y(sc, x, y));
	}
      break;
    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER: return(false);
	case T_REAL:    return(fraction(x) == real(y));
	case T_COMPLEX: return(false);
	default: return(eq_out_y(sc, x, y));
	}
      break;
    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER:
	  return(real(x) == integer(y));
	case T_RATIO:
	  return(real(x) == fraction(y));
	case T_COMPLEX:
	  return(false);
	default: return(eq_out_y(sc, x, y));
	}
      break;
    case T_COMPLEX:
      if (is_real(y)) return(false);
      return(eq_out_y(sc, x, y));

    default: return(eq_out_x(sc, x, y));
    }
  return(false);
}

bool s7i_num_eq_b_7pp(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(num_eq_b_7pp(sc, x, y));}

static bool is_number_via_method(s7_scheme *sc, s7_pointer p)
{
  if (is_number(p))
    return(true);
  if (has_active_methods(sc, p))
    {
      s7_pointer func = find_method_with_let(sc, p, sc->is_number_symbol);
      if (func != sc->undefined)
	return(is_true(sc, s7_apply_function(sc, func, set_plist_1(sc, p))));
    }
  return(false);
}

static s7_pointer g_num_eq(s7_scheme *sc, s7_pointer args)
{
  #define H_num_eq "(= z1 ...) returns #t if all its arguments are equal"
  #define Q_num_eq s7_make_circular_signature(sc, 1, 2, sc->is_boolean_symbol, sc->is_number_symbol)

  const s7_pointer x = car(args);
  s7_pointer nums = cdr(args);
  if (is_null(cdr(nums)))
    return(make_boolean(sc, num_eq_b_7pp(sc, x, car(nums))));

  for (; is_pair(nums); nums = cdr(nums))
    if (!num_eq_b_7pp(sc, x, car(nums)))
      {
	for (nums = cdr(nums); is_pair(nums); nums = cdr(nums))
	  if (!is_number_via_method(sc, car(nums)))
	    wrong_type_error_nr(sc, sc->num_eq_symbol, position_of(nums, args), car(nums), a_number_string);
	return(sc->F);
      }
  return(sc->T);
}

static bool num_eq_b_ii(s7_int i1, s7_int i2) {return(i1 == i2);}
static bool num_eq_b_dd(s7_double i1, s7_double i2) {return(i1 == i2);}
static s7_pointer num_eq_p_dd(s7_scheme *sc, s7_double x1, s7_double x2) {return(make_boolean(sc, x1 == x2));}
static s7_pointer num_eq_p_ii(s7_scheme *sc, s7_int x1, s7_int x2)       {return(make_boolean(sc, x1 == x2));}
static s7_pointer num_eq_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(make_boolean(sc, num_eq_b_7pp(sc, x, y)));}

static s7_pointer num_eq_p_pi(s7_scheme *sc, s7_pointer x, s7_int y)
{
  if (is_t_integer(x))
    return(make_boolean(sc, integer(x) == y));
  if (is_t_real(x))
    return(make_boolean(sc, real(x) == y));
  if (is_number(x))
    return(sc->F); /* complex/ratio can't == int */
  if (has_active_methods(sc, x))
    return(find_and_apply_method(sc, x, sc->num_eq_symbol, set_plist_2(sc, x, make_integer(sc, y))));
  wrong_type_error_nr(sc, sc->num_eq_symbol, 1, x, a_number_string);
#ifdef __TINYC__
  return(sc->F);
#endif
}

static bool num_eq_b_pi(s7_scheme *sc, s7_pointer x, s7_int y)
{
  if (is_t_integer(x))
    return(integer(x) == y);
  if (is_t_real(x))
    return(real(x) == y);
  if (!is_number(x)) /* complex/ratio can't == int */
    wrong_type_error_nr(sc, sc->num_eq_symbol, 1, x, a_number_string);
  return(false);
}

static inline s7_pointer num_eq_xx(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (is_t_integer(x))
    return(make_boolean(sc, integer(x) == integer(y)));
  if (is_t_real(x))
    return(make_boolean(sc, real(x) == integer(y)));
  if (!is_number(x))
    return(make_boolean(sc, eq_out_x(sc, x, y)));
  return(sc->F);
}

s7_pointer s7i_num_eq_xx(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(num_eq_xx(sc, x, y));}

static s7_pointer num_eq_chooser(s7_scheme *sc, s7_pointer func, int32_t args, s7_pointer expr)
{
  s7_pointer arg1, arg2;
  if (args != 2) return(func);
  arg1 = cadr(expr);
  arg2 = caddr(expr);
  if ((is_pair(arg1)) && (has_fn(arg1)) && (fn_proc(arg1) == g_add_3)) set_fn_direct(arg1, g_add_3_wrapped);
  if (is_t_integer(arg2)) return(sc->num_eq_xi);
  return((is_t_integer(arg1)) ? sc->num_eq_ix : sc->num_eq_2);
}


/* ---------------------------------------- < ---------------------------------------- */
static bool lt_out_x(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (has_active_methods(sc, x))
    return(find_and_apply_method(sc, x, sc->lt_symbol, list_2(sc, x, y)) != sc->F); /* not plist */
  wrong_type_error_nr(sc, sc->lt_symbol, 1, x, sc->type_names[T_REAL]);
  return(false);
}

static bool lt_out_y(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (has_active_methods(sc, y))
    return(find_and_apply_method(sc, y, sc->lt_symbol, list_2(sc, x, y)) != sc->F);
  wrong_type_error_nr(sc, sc->lt_symbol, 2, y, sc->type_names[T_REAL]);
  return(false);
}

static bool lt_b_7pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (type(x) == type(y))
    {
      if (is_t_integer(x))
	return(integer(x) < integer(y));
      if (is_t_real(x))
	return(real(x) < real(y));
      if (is_t_ratio(x))
	return(fraction(x) < fraction(y));
    }
  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_RATIO:	return(integer(x) < fraction(y)); /* ?? */
	case T_REAL:	return(integer(x) < real(y));
	default: return(lt_out_y(sc, x, y));
	}
      break;
    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER: return(fraction(x) < integer(y));
	case T_REAL:    return(fraction(x) < real(y));
	default: return(lt_out_y(sc, x, y));
	}
    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER: return(real(x) < integer(y));
	case T_RATIO:	return(real(x) < fraction(y));
	default: return(lt_out_y(sc, x, y));
	}
      break;

    default: return(lt_out_x(sc, x, y));
    }
  return(true);
}

static s7_pointer g_less(s7_scheme *sc, s7_pointer args)
{
  #define H_less "(< x1 ...) returns #t if its arguments are in increasing order"
  #define Q_less s7_make_circular_signature(sc, 1, 2, sc->is_boolean_symbol, sc->is_real_symbol)

  s7_pointer x = car(args), p = cdr(args);
  if (is_null(cdr(p)))
    return(make_boolean(sc, lt_b_7pp(sc, x, car(p))));

  for (; is_pair(p); p = cdr(p))
    {
      if (!lt_b_7pp(sc, x, car(p)))
	{
	  for (p = cdr(p); is_pair(p); p = cdr(p))
	    if (!is_real_via_method(sc, car(p)))
	      wrong_type_error_nr(sc, sc->lt_symbol, position_of(p, args), car(p), sc->type_names[T_REAL]);
	  return(sc->F);
	}
      x = car(p);
    }
  return(sc->T);
}

static bool ratio_lt_pi(s7_pointer x, s7_int y)
{
  if ((y >= 0) && (numerator(x) < 0))
    return(true);
  if ((y <= 0) && (numerator(x) > 0))
    return(false);
  if (denominator(x) < S7_INT32_MAX)
    return(numerator(x) < (y * denominator(x)));
  return(fraction(x) < y);
}

static s7_pointer g_less_x0(s7_scheme *sc, s7_pointer args)
{
  const s7_pointer x = car(args);
  if (is_t_integer(x))
    return(make_boolean(sc, integer(x) < 0));
  if (is_small_real(x))
    return(make_boolean(sc, is_negative(sc, x)));
  return(method_or_bust(sc, x, sc->lt_symbol, args, sc->type_names[T_REAL], 1));
}

static s7_pointer g_less_xi(s7_scheme *sc, s7_pointer args)
{
  const s7_int y = integer(cadr(args));
  const s7_pointer x = car(args);

  if (is_t_integer(x))
    return(make_boolean(sc, integer(x) < y));
  if (is_t_real(x))
    return(make_boolean(sc, real(x) < y));
  if (is_t_ratio(x))
    return(make_boolean(sc, ratio_lt_pi(x, y)));
  return(method_or_bust(sc, x, sc->lt_symbol, args, sc->type_names[T_REAL], 1));
}

static s7_pointer g_less_xf(s7_scheme *sc, s7_pointer args)
{
  const s7_double y = real(cadr(args)); /* chooser below checks is_t_real(y) */
  const s7_pointer x = car(args);

  if (is_t_real(x))
    return(make_boolean(sc, real(x) < y));
  if (is_t_integer(x))
    return(make_boolean(sc, integer(x) < y));
  if (is_t_ratio(x))
    return(make_boolean(sc, fraction(x) < y));
  return(method_or_bust(sc, x, sc->lt_symbol, args, sc->type_names[T_REAL], 1));
}

static inline s7_pointer lt_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(make_boolean(sc, lt_b_7pp(sc, x, y)));}

s7_pointer s7i_lt_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(lt_p_pp(sc, x, y));}

static bool lt_b_ii(s7_int i1, s7_int i2) {return(i1 < i2);}
static bool lt_b_dd(s7_double i1, s7_double i2) {return(i1 < i2);}
static s7_pointer lt_p_dd(s7_scheme *sc, s7_double x1, s7_double x2) {return(make_boolean(sc, x1 < x2));}
static s7_pointer lt_p_ii(s7_scheme *sc, s7_int x1, s7_int x2) {return(make_boolean(sc, x1 < x2));}

static bool lt_b_pi(s7_scheme *sc, s7_pointer x, s7_int y)
{
  if (is_t_integer(x)) return(integer(x) < y);
  if (is_t_real(x))  return(real(x) < y);
  if (is_t_ratio(x)) return(ratio_lt_pi(x, y));
  return(lt_out_x(sc, x, make_integer(sc, y)));
}

static s7_pointer lt_p_pi(s7_scheme *sc, s7_pointer x, s7_int y) {return(make_boolean(sc, lt_b_pi(sc, x, y)));}

static s7_pointer less_chooser(s7_scheme *sc, s7_pointer func, int32_t args, s7_pointer expr)
{
  s7_pointer arg2;
  if (args != 2) return(func);
  arg2 = caddr(expr);
  if (is_t_integer(arg2))
    {
      if (integer(arg2) == 0)
	return(sc->less_x0);
      if ((integer(arg2) < S7_INT32_MAX) && (integer(arg2) > S7_INT32_MIN))
	return(sc->less_xi);
    }
  if (is_t_real(arg2))
    return(sc->less_xf);
  return(sc->less_2);
}


/* ---------------------------------------- <= ---------------------------------------- */
static bool leq_out_x(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (has_active_methods(sc, x))
    return(find_and_apply_method(sc, x, sc->leq_symbol, list_2(sc, x, y)) != sc->F); /* not plist */
  wrong_type_error_nr(sc, sc->leq_symbol, 1, x, sc->type_names[T_REAL]);
  return(false);
}

static bool leq_out_y(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (has_active_methods(sc, y))
    return(find_and_apply_method(sc, y, sc->leq_symbol, list_2(sc, x, y)) != sc->F);
  wrong_type_error_nr(sc, sc->leq_symbol, 2, y, sc->type_names[T_REAL]);
  return(false);
}

static bool leq_b_7pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (type(x) == type(y))
    {
      if (is_t_integer(x))
	return(integer(x) <= integer(y));
      if (is_t_real(x))
	return(real(x) <= real(y));
      if (is_t_ratio(x))
	return(fraction(x) <= fraction(y));
    }
  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_RATIO:	return(integer(x) <= fraction(y)); /* ?? */
	case T_REAL:	return(integer(x) <= real(y));
	default: return(leq_out_y(sc, x, y));
	}
      break;
    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER: return(fraction(x) <= integer(y));
	case T_REAL:    return(fraction(x) <= real(y));
	default: return(leq_out_y(sc, x, y));
	}
    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER: return(real(x) <= integer(y));
	case T_RATIO:	return(real(x) <= fraction(y));
	default: return(leq_out_y(sc, x, y));
	}
      break;

    default: return(leq_out_x(sc, x, y));
    }
  return(true);
}

static s7_pointer g_less_or_equal(s7_scheme *sc, s7_pointer args)
{
  #define H_less_or_equal "(<= x1 ...) returns #t if its arguments are in non-decreasing order"
  #define Q_less_or_equal s7_make_circular_signature(sc, 1, 2, sc->is_boolean_symbol, sc->is_real_symbol)

  s7_pointer x = car(args), p = cdr(args);

  if (is_null(cdr(p)))
    return(make_boolean(sc, leq_b_7pp(sc, x, car(p))));
  for (; is_pair(p); x = car(p), p = cdr(p))
    if (!leq_b_7pp(sc, x, car(p)))
      {
	for (p = cdr(p); is_pair(p); p = cdr(p))
	  if (!is_real_via_method(sc, car(p)))
	    wrong_type_error_nr(sc, sc->leq_symbol, position_of(p, args), car(p), sc->type_names[T_REAL]);
	return(sc->F);
      }
  return(sc->T);
}

static inline s7_pointer leq_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(make_boolean(sc, leq_b_7pp(sc, x, y)));}
static bool leq_b_ii(s7_int i1, s7_int i2) {return(i1 <= i2);}
static bool leq_b_dd(s7_double i1, s7_double i2) {return(i1 <= i2);}
static s7_pointer leq_p_dd(s7_scheme *sc, s7_double x1, s7_double x2) {return(make_boolean(sc, x1 <= x2));}
static s7_pointer leq_p_ii(s7_scheme *sc, s7_int x1, s7_int x2) {return(make_boolean(sc, x1 <= x2));}

static bool ratio_leq_pi(s7_pointer x, s7_int y)
{
  if ((y >= 0) && (numerator(x) <= 0))
    return(true);
  if ((y <= 0) && (numerator(x) > 0))
    return(false);
  if (denominator(x) < S7_INT32_MAX)
    return(numerator(x) <= (y * denominator(x)));
  return(fraction(x) <= y);
}

static s7_pointer g_leq_xi(s7_scheme *sc, s7_pointer args)
{
  const s7_int y = integer(cadr(args));
  const s7_pointer x = car(args);

  if (is_t_integer(x))
    return(make_boolean(sc, integer(x) <= y));
  if (is_t_real(x))
    return(make_boolean(sc, real(x) <= y));
  if (is_t_ratio(x))
    return(make_boolean(sc, ratio_leq_pi(x, y)));
  return(method_or_bust(sc, x, sc->leq_symbol, args, sc->type_names[T_REAL], 1));
}

static bool leq_b_pi(s7_scheme *sc, s7_pointer x, s7_int y)
{
  if (is_t_integer(x)) return(integer(x) <= y);
  if (is_t_real(x))  return(real(x) <= y);
  if (is_t_ratio(x)) return(ratio_leq_pi(x, y));
  if (has_active_methods(sc, x))
    return(find_and_apply_method(sc, x, sc->leq_symbol, list_2(sc, x, make_integer(sc, y)))); /* not plist */
  wrong_type_error_nr(sc, sc->leq_symbol, 1, x, sc->type_names[T_REAL]);
#ifdef __TINYC__
  return(false);
#endif
}

static s7_pointer leq_p_pi(s7_scheme *sc, s7_pointer x, s7_int y) {return(make_boolean(sc, leq_b_pi(sc, x, y)));}

/* bridge for g_leq_2 migration */
bool s7i_leq_b_7pp(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(leq_b_7pp(sc, x, y));}

static s7_pointer g_leq_ixx(s7_scheme *sc, s7_pointer args)
{
  const s7_pointer nums = cdr(args);
  if (is_t_integer(car(nums)))
    {
      if (integer(car(args)) > integer(car(nums)))
	{
	  if (!is_real_via_method(sc, cadr(nums)))
	    wrong_type_error_nr(sc, sc->leq_symbol, 3, cadr(nums), sc->type_names[T_REAL]);
	  return(sc->F);
	}
      if (is_t_integer(cadr(nums)))
	return((integer(car(nums)) > integer(cadr(nums))) ? sc->F : sc->T);
    }
  return(g_less_or_equal(sc, args));
}

static s7_pointer leq_chooser(s7_scheme *sc, s7_pointer func, int32_t args, s7_pointer expr)
{
  if (args == 2)
    {
      const s7_pointer arg2 = caddr(expr);
      if ((is_t_integer(arg2)) && (integer(arg2) < S7_INT32_MAX) && (integer(arg2) > S7_INT32_MIN))
	return(sc->leq_xi);
      return(sc->leq_2);
    }
  if ((args == 3) && (is_t_integer(cadr(expr))))
    return(sc->leq_ixx);
  return(func);
}


/* ---------------------------------------- > ---------------------------------------- */
static bool gt_out_x(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (has_active_methods(sc, x))
    return(find_and_apply_method(sc, x, sc->gt_symbol, list_2(sc, x, y)) != sc->F); /* not plist */
  wrong_type_error_nr(sc, sc->gt_symbol, 1, x, sc->type_names[T_REAL]);
  return(false);
}

static bool gt_out_y(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (has_active_methods(sc, y))
    return(find_and_apply_method(sc, y, sc->gt_symbol, list_2(sc, x, y)) != sc->F);
  wrong_type_error_nr(sc, sc->gt_symbol, 2, y, sc->type_names[T_REAL]);
  return(false);
}

static bool gt_b_7pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (type(x) == type(y))
    {
      if (is_t_integer(x))
	return(integer(x) > integer(y));
      if (is_t_real(x))
	return(real(x) > real(y));
      if (is_t_ratio(x))
	return(fraction(x) > fraction(y));
    }
  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_RATIO:	return(integer(x) > fraction(y)); /* ?? */
	case T_REAL:	return(integer(x) > real(y));
	default: return(gt_out_y(sc, x, y));
	}
      break;
    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER: return(fraction(x) > integer(y));
	case T_REAL:    return(fraction(x) > real(y));
	default: return(gt_out_y(sc, x, y));
	}
    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER: return(real(x) > integer(y));
	case T_RATIO:	return(real(x) > fraction(y));
	default: return(gt_out_y(sc, x, y));
	}
      break;

    default: return(gt_out_x(sc, x, y));
    }
  return(true);
}

static s7_pointer g_greater(s7_scheme *sc, s7_pointer args)
{
  #define H_greater "(> x1 ...) returns #t if its arguments are in decreasing order"
  #define Q_greater s7_make_circular_signature(sc, 1, 2, sc->is_boolean_symbol, sc->is_real_symbol)

  s7_pointer x = car(args), p = cdr(args);

  if (is_null(cdr(p)))
    return(make_boolean(sc, gt_b_7pp(sc, x, car(p))));

  for (; is_pair(p); x = car(p), p = cdr(p))
    if (!gt_b_7pp(sc, x, car(p)))
      {
	for (p = cdr(p); is_pair(p); p = cdr(p))
	  if (!is_real_via_method(sc, car(p)))
	    wrong_type_error_nr(sc, sc->gt_symbol, position_of(p, args), car(p), sc->type_names[T_REAL]);
	return(sc->F);
      }
  return(sc->T);
}

static s7_pointer g_greater_xi(s7_scheme *sc, s7_pointer args)
{
  const s7_int y = integer(cadr(args));
  const s7_pointer x = car(args);

  if (is_t_integer(x))
    return(make_boolean(sc, integer(x) > y));
  if (is_t_real(x))
    return(make_boolean(sc, real(x) > y));
  if (is_t_ratio(x))
    return(make_boolean(sc, !ratio_leq_pi(x, y)));
  return(method_or_bust(sc, x, sc->gt_symbol, args, a_number_string, 1));
}

static s7_pointer g_greater_xf(s7_scheme *sc, s7_pointer args)
{
  const s7_double y = real(cadr(args));
  const s7_pointer x = car(args);

  if (is_t_real(x))
    return(make_boolean(sc, real(x) > y));

  switch (type(x))
    {
    case T_INTEGER:
      return(make_boolean(sc, integer(x) > y));
    case T_RATIO:
      /* (> 9223372036854775807/9223372036854775806 1.0) */
      if (denominator(x) < S7_INT32_MAX) /* y range check was handled in greater_chooser */
	return(make_boolean(sc, (numerator(x) > (y * denominator(x)))));
      return(make_boolean(sc, fraction(x) > y));

    default:
      return(method_or_bust(sc, x, sc->gt_symbol, args, a_number_string, 1));
    }
  return(sc->T);
}

static inline s7_pointer gt_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(make_boolean(sc, gt_b_7pp(sc, x, y)));}
static bool gt_b_ii(s7_int i1, s7_int i2) {return(i1 > i2);}
static bool gt_b_dd(s7_double i1, s7_double i2) {return(i1 > i2);}
static s7_pointer gt_p_dd(s7_scheme *sc, s7_double x1, s7_double x2) {return(make_boolean(sc, x1 > x2));}
static s7_pointer gt_p_ii(s7_scheme *sc, s7_int x1, s7_int x2) {return(make_boolean(sc, x1 > x2));}

static bool gt_b_pi(s7_scheme *sc, s7_pointer x, s7_int y)
{
  if (is_t_integer(x)) return(integer(x) > y);
  if (is_t_real(x))  return(real(x) > y);
  if (is_t_ratio(x)) return(!ratio_leq_pi(x, y));
  return(gt_out_x(sc, x, make_integer(sc, y)));
}

static s7_pointer gt_p_pi(s7_scheme *sc, s7_pointer x, s7_int y) {return(make_boolean(sc, gt_b_pi(sc, x, y)));}

static s7_pointer g_greater_2(s7_scheme *sc, s7_pointer args)
{
  /* ridiculous repetition, but overheads are killing this poor thing */
  const s7_pointer x = car(args), y = cadr(args);
  if (type(x) == type(y))
    {
      if (is_t_integer(x)) return(make_boolean(sc, integer(x) > integer(y)));
      if (is_t_real(x))    return(make_boolean(sc, real(x) > real(y)));
      if (is_t_ratio(x))   return(make_boolean(sc, fraction(x) > fraction(y)));
    }
  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_RATIO:
	  return(gt_p_pp(sc, x, y));
	case T_REAL:
	  return(make_boolean(sc, integer(x) > real(y)));
	default:        return(make_boolean(sc, gt_out_y(sc, x, y)));
	}
      break;
    case T_RATIO:
      return(gt_p_pp(sc, x, y));
    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER: return(make_boolean(sc, real(x) > integer(y)));
	case T_RATIO:   return(make_boolean(sc, real(x) > fraction(y)));
	default:        return(make_boolean(sc, gt_out_y(sc, x, y)));
	}
      break;

    default:            return(make_boolean(sc, gt_out_x(sc, x, y)));
    }
  return(sc->T);
}

static s7_pointer greater_chooser(s7_scheme *sc, s7_pointer func, int32_t args, s7_pointer expr)
{
  s7_pointer arg2;
  if (args != 2) return(func);
  arg2 = caddr(expr);
  if ((is_t_integer(arg2)) && (integer(arg2) < S7_INT32_MAX) && (integer(arg2) > S7_INT32_MIN))
    return(sc->greater_xi);
  if ((is_t_real(arg2)) && (real(arg2) < S7_INT32_MAX) && (real(arg2) > S7_INT32_MIN))
    return(sc->greater_xf);
  return(sc->greater_2);
}


/* ---------------------------------------- >= ---------------------------------------- */
static bool geq_out_x(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (!has_active_methods(sc, x))
    wrong_type_error_nr(sc, sc->geq_symbol, 1, x, sc->type_names[T_REAL]);
  return(find_and_apply_method(sc, x, sc->geq_symbol, list_2(sc, x, y)) != sc->F);   /* not plist */
}

static bool geq_out_y(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (!has_active_methods(sc, y))
    wrong_type_error_nr(sc, sc->geq_symbol, 2, y, sc->type_names[T_REAL]);
  return(find_and_apply_method(sc, y, sc->geq_symbol, list_2(sc, x, y)) != sc->F);   /* not plist */
}

static bool geq_b_7pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (type(x) == type(y))
    {
      if (is_t_integer(x))
	return(integer(x) >= integer(y));
      if (is_t_real(x))
	return(real(x) >= real(y));
      if (is_t_ratio(x))
	return(fraction(x) >= fraction(y));
    }
  switch (type(x))
    {
    case T_INTEGER:
      switch (type(y))
	{
	case T_RATIO:	return(integer(x) >= fraction(y)); /* ?? */
	case T_REAL:	return(integer(x) >= real(y));
	default: return(geq_out_y(sc, x, y));
	}
      break;
    case T_RATIO:
      switch (type(y))
	{
	case T_INTEGER: return(fraction(x) >= integer(y));
	case T_REAL:    return(fraction(x) >= real(y));
	default: return(geq_out_y(sc, x, y));
	}
    case T_REAL:
      switch (type(y))
	{
	case T_INTEGER: return(real(x) >= integer(y));
	case T_RATIO:	return(real(x) >= fraction(y));
	default: return(geq_out_y(sc, x, y));
	}
      break;

    default: return(geq_out_x(sc, x, y));
    }
  return(true);
}

static s7_pointer g_greater_or_equal(s7_scheme *sc, s7_pointer args)
{
  #define H_greater_or_equal "(>= x1 ...) returns #t if its arguments are in non-increasing order"
  #define Q_greater_or_equal s7_make_circular_signature(sc, 1, 2, sc->is_boolean_symbol, sc->is_real_symbol)

  s7_pointer x = car(args), p = cdr(args);
  if (is_null(cdr(p)))
    return(make_boolean(sc, geq_b_7pp(sc, x, car(p))));

  for (; is_pair(p); x = car(p), p = cdr(p))
    if (!geq_b_7pp(sc, x, car(p)))
      {
	for (p = cdr(p); is_pair(p); p = cdr(p))
	  if (!is_real_via_method(sc, car(p)))
	    wrong_type_error_nr(sc, sc->geq_symbol, position_of(p, args), car(p), sc->type_names[T_REAL]);
	return(sc->F);
      }
  return(sc->T);
}

static inline s7_pointer geq_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(make_boolean(sc, geq_b_7pp(sc, x, y)));}
static bool geq_b_ii(s7_int i1, s7_int i2) {return(i1 >= i2);}
static bool geq_b_dd(s7_double i1, s7_double i2) {return(i1 >= i2);}
static s7_pointer geq_p_dd(s7_scheme *sc, s7_double x1, s7_double x2) {return(make_boolean(sc, x1 >= x2));}
static s7_pointer geq_p_ii(s7_scheme *sc, s7_int x1, s7_int x2) {return(make_boolean(sc, x1 >= x2));}

/* bridge for g_geq_2 migration */
bool s7i_geq_b_7pp(s7_scheme *sc, s7_pointer x, s7_pointer y) {return(geq_b_7pp(sc, x, y));}

static s7_pointer g_geq_xf(s7_scheme *sc, s7_pointer args)
{
  s7_double y = real(cadr(args));
  s7_pointer x = car(args);
  return(make_boolean(sc, ((is_t_real(x)) ? (real(x) >= y) : geq_b_7pp(sc, car(args), cadr(args)))));
}

static s7_pointer g_geq_xi(s7_scheme *sc, s7_pointer args)
{
  const s7_int y = integer(cadr(args));
  const s7_pointer x = car(args);

  if (is_t_integer(x))
    return(make_boolean(sc, integer(x) >= y));
  if (is_t_real(x))
    return(make_boolean(sc, real(x) >= y));
  if (is_t_ratio(x))
    return(make_boolean(sc, !ratio_lt_pi(x, y)));
  return(method_or_bust(sc, x, sc->geq_symbol, args, sc->type_names[T_REAL], 1));
}

static bool geq_b_pi(s7_scheme *sc, s7_pointer x, s7_int y)
{
  if (is_t_integer(x)) return(integer(x) >= y);
  if (is_t_real(x))  return(real(x) >= y);
  if (is_t_ratio(x)) return(!ratio_lt_pi(x, y));
  if (!has_active_methods(sc, x))
    wrong_type_error_nr(sc, sc->geq_symbol, 1, x, sc->type_names[T_REAL]);
  return(find_and_apply_method(sc, x, sc->geq_symbol, list_2(sc, x, make_integer(sc, y))));   /* not plist */
}

static s7_pointer geq_p_pi(s7_scheme *sc, s7_pointer x, s7_int y) {return(make_boolean(sc, geq_b_pi(sc, x, y)));}

static s7_pointer geq_chooser(s7_scheme *sc, s7_pointer func, int32_t args, s7_pointer expr)
{
  s7_pointer arg2;
  if (args != 2) return(func);
  arg2 = caddr(expr);
  if ((is_t_integer(arg2)) && (integer(arg2) < S7_INT32_MAX) && (integer(arg2) > S7_INT32_MIN))
    return(sc->geq_xi);
  if ((is_t_real(arg2)) && (real(arg2) < S7_INT32_MAX) && (real(arg2) > S7_INT32_MIN))
    return(sc->geq_xf);
  return(sc->geq_2);
}


/* ---------------------------------------- real-part ---------------------------------------- */
s7_double s7_real_part(s7_pointer x)
{
  switch(type(x))
    {
    case T_INTEGER:     return((s7_double)integer(x));
    case T_RATIO:       return((s7_double)fraction(x));
    case T_REAL:        return(real(x));
    case T_COMPLEX:     return(real_part(x));
    }
  return(0.0);
}

/* ---------------------------------------- imag-part ---------------------------------------- */
s7_double s7_imag_part(s7_pointer x)
{
  if (is_t_complex(x))
    return(imag_part(x));
  return(0.0);
}

/* ---------------------------------------- numerator denominator ---------------------------------------- */
static s7_int numerator_i_7p(s7_scheme *sc, s7_pointer x)
{
  if (is_t_ratio(x)) return(numerator(x));
  if (is_t_integer(x)) return(integer(x));
  return(integer(method_or_bust_p(sc, x, sc->numerator_symbol, a_rational_string)));
}

/* g_numerator is now defined in s7_scheme_predicate.c */
  #define H_numerator "(numerator rat) returns the numerator of the rational number rat"
  #define Q_numerator s7_make_signature(sc, 2, sc->is_integer_symbol, sc->is_rational_symbol)


/* g_denominator is now defined in s7_scheme_predicate.c */
  #define H_denominator "(denominator rat) returns the denominator of the rational number rat"
  #define Q_denominator s7_make_signature(sc, 2, sc->is_integer_symbol, sc->is_rational_symbol)

static s7_int denominator_i_7p(s7_scheme *sc, s7_pointer x)
{
  if (is_t_ratio(x)) return(denominator(x));
  if (is_t_integer(x)) return(1);
  return(integer(method_or_bust_p(sc, x, sc->denominator_symbol, a_rational_string)));
}


/* ---------------------------------------- number? bignum? complex? integer? byte? rational? real?  ---------------------------------------- */
/* g_is_number is now defined in s7_scheme_predicate.c */
#define H_is_number "(number? obj) returns #t if obj is a number"
#define Q_is_number sc->pl_bt

/* g_is_integer is now defined in s7_scheme_predicate.c */
#define H_is_integer "(integer? obj) returns #t if obj is an integer"
#define Q_is_integer sc->pl_bt

static bool is_byte(s7_pointer x) {return((s7_is_integer(x)) && (s7_integer(x) >= 0) && (s7_integer(x) < 256));}
/* g_is_byte is now defined in s7_scheme_predicate.c */
  #define H_is_byte "(byte? obj) returns #t if obj is a byte (an integer between 0 and 255)"
  #define Q_is_byte sc->pl_bt

/* g_is_real is now defined in s7_scheme_predicate.c */
#define H_is_real "(real? obj) returns #t if obj is a real number"
#define Q_is_real sc->pl_bt

/* g_is_complex is now defined in s7_scheme_predicate.c */
#define H_is_complex "(complex? obj) returns #t if obj is a number"
#define Q_is_complex sc->pl_bt

/* g_is_rational is now defined in s7_scheme_predicate.c */
#define H_is_rational "(rational? obj) returns #t if obj is a rational number (either an integer or a ratio)"
#define Q_is_rational sc->pl_bt

/* g_is_float is now defined in s7_scheme_predicate.c */
  #define H_is_float "(float? x) returns #t is x is real and not rational."
  #define Q_is_float sc->pl_bt

static bool is_float_b(s7_pointer x) {return(is_t_real(x));}


/* ---------------------------------------- nan? ---------------------------------------- */
static bool is_nan_b_7p(s7_scheme *sc, s7_pointer x)
{
  return s7_is_nan(sc, x);
}


/* ---------------------------------------- zero? ---------------------------------------- */

static bool is_zero(s7_pointer x)
{
  if (s7_is_integer(x))
    return s7_integer(x) == 0;
  if (s7_is_real(x))
    return s7_real(x) == 0.0;
  return false; /* ratios and complex numbers here are already collapsed into integers and reals */
}

static bool is_positive(s7_scheme *sc, s7_pointer x)
{
  if (s7_is_integer(x))
    return s7_integer(x) > 0;
  if (s7_is_rational(x) && !s7_is_integer(x))
    return s7_numerator(x) > 0;
  if (s7_is_real(x))
    return s7_real(x) > 0.0;
  s7_wrong_type_arg_error(sc, "positive?", 1, x, "a real number");
  return false;
}

static bool is_negative(s7_scheme *sc, s7_pointer x)
{
  if (s7_is_integer(x))
    return s7_integer(x) < 0;
  if (s7_is_rational(x) && !s7_is_integer(x))
    return s7_numerator(x) < 0;
  if (s7_is_real(x))
    return s7_real(x) < 0.0;
  s7_wrong_type_arg_error(sc, "negative?", 1, x, "a real number");
  return false;
}

#if !WITH_PURE_S7
/* ---------------------------------------- exact<->inexact exact? inexact? ---------------------------------------- */










/* ---------------------------------------- integer-length ---------------------------------------- */
static int32_t integer_length(s7_int a)
{
  if (a < 0)
    {
      if (a == S7_INT64_MIN) return(63);
      a = -a;
    }
  if (a < 256LL) return(intlen_bits[a]); /* in gmp, sbcl and clisp (integer-length 0) is 0 */
  if (a < 65536LL) return(8 + intlen_bits[a >> 8]);
  if (a < 16777216LL) return(16 + intlen_bits[a >> 16]);
  if (a < 4294967296LL) return(24 + intlen_bits[a >> 24]);
  if (a < 1099511627776LL) return(32 + intlen_bits[a >> 32]);
  if (a < 281474976710656LL) return(40 + intlen_bits[a >> 40]);
  if (a < 72057594037927936LL) return(48 + intlen_bits[a >> 48]);
  return(56 + intlen_bits[a >> 56]);
}

static s7_pointer g_integer_length(s7_scheme *sc, s7_pointer args)
{
  #define H_integer_length "(integer-length arg) returns the number of bits required to represent the integer 'arg': \
(ceiling (log (if (< arg 0) (- arg) (+ arg 1)) 2))"
  #define Q_integer_length sc->pcl_i

  const s7_pointer num = car(args);
  if (is_t_integer(num))
    {
      s7_int x = integer(num);
      return((x < 0) ? small_int(integer_length(-(x + 1))) : small_int(integer_length(x)));
    }
  return(sole_arg_method_or_bust(sc, num, sc->integer_length_symbol, args, sc->type_names[T_INTEGER]));
}

static s7_int integer_length_i_i(s7_int x) {return((x < 0) ? integer_length(-(x + 1)) : integer_length(x));}
#endif /* !pure s7 */


/* ---------------------------------------- integer-decode-float ---------------------------------------- */
static s7_pointer g_integer_decode_float(s7_scheme *sc, s7_pointer args)
{
  #define H_integer_decode_float "(integer-decode-float x) returns a list containing the significand, exponent, and \
sign of 'x' (1 = positive, -1 = negative).  (integer-decode-float 0.0): (0 0 1)"
  #define Q_integer_decode_float s7_make_signature(sc, 2, sc->is_pair_symbol, sc->is_float_symbol)

  decode_float_t num;
  const s7_pointer x = car(args);
  if (is_t_real(x))
    {
      if (real(x) == 0.0)
	return(list_3(sc, int_zero, int_zero, int_one));
      num.fx = (double)real(x);
      return(list_3(sc,
		    make_integer_unchecked(sc, (s7_int)((num.ix & 0xfffffffffffffLL) | 0x10000000000000LL)),
		    make_integer(sc, (s7_int)(((num.ix & 0x7fffffffffffffffLL) >> 52) - 1023 - 52)),
		    ((num.ix & 0x8000000000000000LL) != 0) ? minus_one : int_one));
    }
  return(method_or_bust_p(sc, x, sc->integer_decode_float_symbol, wrap_string(sc, "a non-rational real", 19)));
}


/* -------------------------------- random-state -------------------------------- */
/* random numbers.  The simple version used in clm.c is probably adequate, but here I'll use Marsaglia's MWC algorithm.
 *     (random num) -> a number (0..num), if num == 0 return 0, use global default state
 *     (random num state) -> same but use this state
 *     (random-state seed) -> make a new state
 *   to save the current seed, use copy, to save it across load, random-state->list and list->random-state.
 *   random-state? returns #t if its arg is one of these guys
 */

static s7_pointer random_state_copy(s7_scheme *sc, s7_pointer args)
{
  s7_pointer new_r, obj = car(args);
  if (!is_random_state(obj)) return(sc->F);
  new_cell(sc, new_r, T_RANDOM_STATE);
  random_seed(new_r) = random_seed(obj);
  random_carry(new_r) = random_carry(obj);
  return(new_r);
}

#ifndef MWC_32
  #define MWC_32 1
#endif

s7_pointer s7_random_state(s7_scheme *sc, s7_pointer args)
{
  #define H_random_state "(random-state seed (carry plausible-default)) returns a new random number state initialized with 'seed'. \
Pass this as the second argument to 'random' to get a repeatable random number sequence:\n\
    (let ((seed (random-state 1234))) (random 1.0 seed))"
  #define Q_random_state s7_make_circular_signature(sc, 1, 2, sc->is_random_state_symbol, sc->is_integer_symbol)

  s7_pointer r1, r2, rs;
  s7_int i1, i2;  /* actually want s7_uint here -- we lose the sign bit? */
  if (is_null(args))
    return(sc->default_random_state);

  r1 = car(args);
  if (!s7_is_integer(r1))
    return(method_or_bust(sc, r1, sc->random_state_symbol, args, sc->type_names[T_INTEGER], 1));
  i1 = integer(r1);
#if MWC_32
  if (i1 < 0)
    out_of_range_error_nr(sc, sc->random_state_symbol, int_one, r1, it_is_negative_string);
#endif
  if (is_null(cdr(args)))
    {
      new_cell(sc, rs, T_RANDOM_STATE);
      random_seed(rs) = (s7_uint)i1;
      random_carry(rs) = 1675393560;                          /* should this be dependent on the seed? */
      return(rs);
    }

  r2 = cadr(args);
  if (!s7_is_integer(r2))
    return(method_or_bust(sc, r2, sc->random_state_symbol, args, sc->type_names[T_INTEGER], 2));
  i2 = integer(r2);
#if MWC_32
  if (i2 < 0)
    out_of_range_error_nr(sc, sc->random_state_symbol, int_two, r2, it_is_negative_string);
#endif
  new_cell(sc, rs, T_RANDOM_STATE);
  random_seed(rs) = (s7_uint)i1;
  random_carry(rs) = (s7_uint)i2;
  return(rs);
}

#define g_random_state s7_random_state

static s7_pointer random_state_getter(s7_scheme *sc, s7_pointer r, s7_int loc)
{
  if (loc == 0) return(make_integer(sc, random_seed(r)));
  if (loc == 1) return(make_integer(sc, random_carry(r)));
  return(sc->F);
}

static s7_pointer random_state_setter(s7_scheme *sc, s7_pointer r, s7_int loc, s7_pointer val)
{
  if (is_t_integer(val))
    {
      s7_int i = s7_integer_clamped_if_gmp(sc, val);
      if (loc == 0) random_seed(r) = i;
      if (loc == 1) random_carry(r) = i;
    }
  return(sc->F);
}


/* -------------------------------- random-state? -------------------------------- */
/* g_is_random_state is now defined in s7_scheme_predicate.c */
  #define H_is_random_state "(random-state? obj) returns #t if obj is a random-state object (from random-state)."
  #define Q_is_random_state sc->pl_bt

bool s7_is_random_state(s7_pointer r) {return(type(r) == T_RANDOM_STATE);}


/* -------------------------------- random-state->list -------------------------------- */
s7_pointer s7_random_state_to_list(s7_scheme *sc, s7_pointer args)
{
  #define H_random_state_to_list "(random-state->list r) returns the random state object as a list.\
You can later apply random-state to this list to continue a random number sequence from any point."
  #define Q_random_state_to_list s7_make_signature(sc, 2, sc->is_pair_symbol, sc->is_random_state_symbol)

  s7_pointer r = (is_null(args)) ? sc->default_random_state : car(args);
  if (!is_random_state(r))
    return(method_or_bust(sc, r, sc->random_state_to_list_symbol, args, a_random_state_object_string, 1));
  return(list_2(sc, make_integer(sc, random_seed(r)), make_integer_unchecked(sc, random_carry(r))));
}

#define g_random_state_to_list s7_random_state_to_list

void s7_set_default_random_state(s7_scheme *sc, s7_int seed, s7_int carry)
{
  s7_pointer rs;
  new_cell(sc, rs, T_RANDOM_STATE);
  random_seed(rs) = (s7_uint)seed;
  random_carry(rs) = (s7_uint)carry;
  sc->default_random_state = rs;
}


/* -------------------------------- random -------------------------------- */
static double next_random(s7_pointer r)
{
#if MWC_32
  /* The multiply-with-carry generator for 32-bit integers:
   *        x(n)=a*x(n-1) + carry mod 2^32
   * Choose multiplier a from this list:
   *   1791398085 1929682203 1683268614 1965537969 1675393560 1967773755 1517746329 1447497129 1655692410 1606218150
   *   2051013963 1075433238 1557985959 1781943330 1893513180 1631296680 2131995753 2083801278 1873196400 1554115554
   * ( or any 'a' for which both a*2^32-1 and a*2^31-1 are prime)
   *
   * see s7_random_state for 64 bit version of this, L26622
   * here's a check that things are not totally broken:
      (define (check-random-integers lo hi)
        (let* ((range (- hi lo))
      	       (num-bins 32)
      	       (bins (make-int-vector num-bins 0)))
          (do ((i 0 (+ i 1))
      	       (x (random range) (random range)))
      	      ((= i 10000) bins)
            (let ((bin (floor (* (/ x range) num-bins))))
      	      (set! (bins bin) (+ (bins bin) 1))))))
      (display (check-random-integers 0 9223372036854775807)) (newline)

      (let ((mx 0) (mn 1000))
        (do ((i 0 (+ i 1)))
            ((= i 10000))
          (let ((val (random 123)))
            (set! mx (max mx val))
            (set! mn (min mn val))))
        (display (list mn mx)) (newline))
   */
  #define RAN_MULT 2131995753UL
  double result;
  s7_uint temp = random_seed(r) * RAN_MULT + random_carry(r);
  random_seed(r) = (temp & 0xffffffffUL);
  random_carry(r) = (temp >> 32);
  result = (double)((uint32_t)(random_seed(r))) / 4294967295.5;
  /* divisor was 2^32-1 = 4294967295.0, but somehow this can round up once in a billion tries?
   *   do we want the double just less than 2^32?
   * can the multiply-add+logand above return 0? I'm getting 0's from (random (expt 2 62))
   */
#else
  /* 64-bit MWC from https://prng.di.unimi.it/#shootout */
  double result;
  #define MWC_A1 0xffebb71d94fcdaf9
  /* The state must be initialized so that 0 < c < MWC_A1 - 1. For simplicity, we suggest to set c = 1 and x to a 64-bit seed. */

  s7_uint x = random_seed(r), c = random_carry(r);
  s7_uint u_result = x;                  /* Or, result = x ^ (x << 32) (see above) */ /* s7_uint == uint64_t */
  const __uint128_t t = MWC_A1 * (__uint128_t)x + c;
  random_seed(r) = t;
  random_carry(r) = t >> 64;
  result = ((long_double)(random_seed(r)) / (long_double)4294967296.0) / (long_double)4294967295.5;
#endif

  return(result);
}

static s7_pointer g_random(s7_scheme *sc, s7_pointer args)
{
  #define H_random "(random num state) returns a random number of the same type as num between zero and num, equalling num only if num is zero"
  #define Q_random s7_make_signature(sc, 3, sc->is_number_symbol, sc->is_number_symbol, sc->is_random_state_symbol)
  s7_pointer r, num;

  /* if we disallow (random 0) the programmer has to protect every call on random with (if (eqv? x 0) 0 (random x)).  If
   *   we claim we're using a half-open interval, then we should also disallow (random 0.0); otherwise the following
   *   must be true: (let* ((x 0.0) (y (random x))) (and (>= y 0.0) (< y x))).  The definition above is consistent
   *   with (random 0) -> 0, simpler to use in practice, and certainly no worse than (/ 0 0) -> 1.
   */
  if (is_null(cdr(args)))
    r = sc->default_random_state;
  else
    {
      r = cadr(args);
      if (!is_random_state(r))
	return(method_or_bust(sc, r, sc->random_symbol, args, a_random_state_object_string, 2));
    }
  num = car(args);
  switch (type(num))
    {
    case T_INTEGER:
      return(make_integer(sc, (s7_int)(integer(num) * next_random(r))));
    case T_RATIO:
      {
	const s7_double x = fraction(num);
	s7_double error;
	s7_int numer = 0, denom = 1;
	/* the error here needs to take the size of the fraction into account.  Otherwise, if
	 *    error is (say) 1e-6 and the fraction is (say) 9000000/9223372036854775807,
	 *    c_rationalize will always return 0.  But even that isn't foolproof:
	 *    (random 1/562949953421312) -> 1/376367230475000
	 */
	if ((x < 1.0e-10) && (x > -1.0e-10))
	  {
	    /* 1e-12 is not tight enough:
	     *    (random 1/2251799813685248) -> 1/2250240579436280
	     *    (random -1/4503599627370496) -> -1/4492889778435526
	     *    (random 1/140737488355328) -> 1/140730223985746
	     *    (random -1/35184372088832) -> -1/35183145492420
	     *    (random -1/70368744177664) -> -1/70366866392738
	     *    (random 1/4398046511104) -> 1/4398033095756
	     *    (random 1/137438953472) -> 1/137438941127
	     */
	    if (numerator(num) < -10)
	      numer = -(s7_int)(floor(-numerator(num) * next_random(r)));
	    else
	      if (numerator(num) > 10)
		numer = (s7_int)floor(numerator(num) * next_random(r));
	      else
		{
		  s7_int diff = S7_INT64_MAX - denominator(num);
		  numer = numerator(num);
		  if (diff < 100)
		    return(make_ratio(sc, numer, denominator(num)));
		  denom = denominator(num) + (s7_int)floor(diff * next_random(r));
		  return(make_ratio_with_div_check(sc, sc->random_symbol, numer, denom));
		}
	    return(make_ratio(sc, numer, denominator(num)));
	  }
	error = ((x < 1e-6) && (x > -1e-6)) ? 1e-18 : 1e-12;
	c_rationalize(x * next_random(r), error, &numer, &denom);
	return(make_simpler_ratio_or_integer(sc, numer, denom));
      }
    case T_REAL:
      return(make_real(sc, real(num) * next_random(r)));
      /* (x >> 11) * 0x1.0p-53, (1LL << 50) * 0x1.0p-53) -> .125, here "x" is 64 bits, but isn't this int64 related? */
    case T_COMPLEX:
      return(make_complex(sc, real_part(num) * next_random(r), imag_part(num) * next_random(r)));
    default:
      return(method_or_bust(sc, num, sc->random_symbol, args, a_number_string, 1));
    }
  return(sc->F);
}

s7_double s7_random(s7_scheme *sc, s7_pointer state)
{
  return(next_random((state) ? state : sc->default_random_state));
}

static s7_double random_d_7d(s7_scheme *sc, s7_double x)
{
  return(x * next_random(sc->default_random_state));
}

static s7_int random_i_7i(s7_scheme *sc, s7_int i)
{
  return((s7_int)(i * next_random(sc->default_random_state)));
}

static s7_pointer g_random_i(s7_scheme *sc, s7_pointer args)
{
  return(make_integer(sc, (s7_int)(integer(car(args)) * next_random(sc->default_random_state))));
}

static s7_pointer g_random_f(s7_scheme *sc, s7_pointer args)
{
  return(make_real(sc, real(car(args)) * next_random(sc->default_random_state)));
}

static s7_pointer g_random_1(s7_scheme *sc, s7_pointer args)
{
  s7_pointer num = car(args), r = sc->default_random_state;
  if (is_t_integer(num))
    return(make_integer(sc, (s7_int)(integer(num) * next_random(r))));
  if (is_t_real(num))
    return(make_real(sc, real(num) * next_random(r)));
  return(g_random(sc, args));
}

static s7_pointer random_p_p(s7_scheme *sc, s7_pointer num)
{
  if (is_t_integer(num))
    return(make_integer(sc, (s7_int)(integer(num) * next_random(sc->default_random_state))));
  if (is_t_real(num))
    return(make_real(sc, real(num) * next_random(sc->default_random_state)));
  return(g_random(sc, set_plist_1(sc, num)));
}

static s7_pointer random_p_p_wrapped(s7_scheme *sc, s7_pointer num)
{
  if (is_t_integer(num))
    return(wrap_integer(sc, (s7_int)(integer(num) * next_random(sc->default_random_state))));
  if (is_t_real(num))
    return(wrap_real(sc, real(num) * next_random(sc->default_random_state)));
  return(g_random(sc, set_plist_1(sc, num)));
}

static s7_pointer random_chooser(s7_scheme *sc, s7_pointer func, int32_t args, s7_pointer expr)
{
  if (args == 1)
    {
      s7_pointer arg1 = cadr(expr);
      if (is_t_integer(arg1))
	return(sc->random_i);
      return((is_t_real(arg1)) ? sc->random_f : sc->random_1);
    }
  return(func);
}

static s7_pointer g_add_i_random(s7_scheme *sc, s7_pointer args)
{
  s7_int x = integer(car(args)), y = opt3_int(args); /* cadadr */
  return(make_integer(sc, x + (s7_int)(y * next_random(sc->default_random_state)))); /* (+ -1 (random 1)) -- placement of the (s7_int) cast matters! */
}

