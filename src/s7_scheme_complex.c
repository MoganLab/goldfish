/* s7_scheme_complex.c - complex number implementations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#include "s7_scheme_complex.h"
#include "s7_internal_helpers.h"
#include <math.h>

#ifndef HAVE_COMPLEX_NUMBERS
  #if __TINYC__ || (__clang__ && __cplusplus)
    #define HAVE_COMPLEX_NUMBERS 0
  #else
    #define HAVE_COMPLEX_NUMBERS 1
  #endif
#endif

#ifndef M_PI
  #define M_PI 3.1415926535897932384626433832795029L
#endif

static s7_pointer make_complex_not_0i(s7_scheme *sc, s7_double r, s7_double i)
{
  if (i == 0.0) return s7_make_real(sc, r);
  return s7_make_complex(sc, r, i);
}

static double my_hypot(double x, double y)
{
  if ((fabs(x) < 1.0e6) && (fabs(y) < 1.0e6))
    return sqrt(x * x + y * y);
  return hypot(x, y);
}

s7_pointer magnitude_p_p(s7_scheme *sc, s7_pointer x)
{
  if (s7_is_integer(x))
    {
      s7_int v = s7_integer(x);
      return (v < 0) ? s7_make_integer(sc, -v) : x;
    }

  if (s7_is_rational(x))
    {
      s7_int n = s7_numerator(x);
      s7_int d = s7_denominator(x);
      return (n < 0) ? s7_make_ratio(sc, -n, d) : x;
    }

  if (s7_is_real(x))
    {
      s7_double r = s7_real(x);
      if (isnan(r)) return x;
      return signbit(r) ? s7_make_real(sc, -r) : x;
    }
  
  if (s7_is_complex(x))
    return s7_make_real(sc, my_hypot(s7_real_part(x), s7_imag_part(x)));

  return s7_wrong_type_arg_error(sc, "magnitude", 1, x, "a number");
}

s7_pointer g_magnitude(s7_scheme *sc, s7_pointer args)
{
  return magnitude_p_p(sc, s7_car(args));
}

s7_int magnitude_i_i(s7_int x) {return((x < 0) ? (-x) : x);} 
s7_double magnitude_d_d(s7_double x) {return((signbit(x)) ? (-x) : x);} 
s7_pointer magnitude_p_z(s7_scheme *sc, s7_pointer z) {return(s7_make_real(sc, my_hypot(s7_real_part(z), s7_imag_part(z))));}

s7_pointer g_angle(s7_scheme *sc, s7_pointer args)
{
  s7_pointer x = s7_car(args);

  if (s7_is_integer(x)) return (s7_integer(x) < 0) ? s7_make_real(sc, M_PI) : s7_make_integer(sc, 0);
  if (s7_is_rational(x)) return (s7_numerator(x) < 0) ? s7_make_real(sc, M_PI) : s7_make_integer(sc, 0);
  if (s7_is_complex(x)) return s7_make_real(sc, atan2(s7_imag_part(x), s7_real_part(x)));
  if (s7_is_real(x))
    {
      s7_double r = s7_real(x);
      if (isnan(r)) return x;
      return (r < 0.0) ? s7_make_real(sc, M_PI) : s7_make_real(sc, 0.0);
    }

  return s7_wrong_type_arg_error(sc, "angle", 1, x, "a number");
}

s7_double angle_d_d(s7_double x) {return((isnan(x)) ? x : ((x < 0.0) ? M_PI : 0.0));}

s7_pointer complex_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y)
{
  if (!s7_is_real(x))
    return s7_wrong_type_arg_error(sc, "complex", 1, x, "a real");
  if (!s7_is_real(y))
    return s7_wrong_type_arg_error(sc, "complex", 2, y, "a real");

  if ((s7_is_integer(y) && (s7_integer(y) == 0)) || (s7_is_real(y) && (s7_real(y) == 0.0)))
    return x;

  return make_complex_not_0i(sc, s7_number_to_real(sc, x), s7_number_to_real(sc, y));
}

s7_pointer complex_p_pp_wrapped(s7_scheme *sc, s7_pointer x, s7_pointer y) {return complex_p_pp(sc, x, y);} 
s7_pointer g_complex(s7_scheme *sc, s7_pointer args) {return complex_p_pp(sc, s7_car(args), s7_cadr(args));}
s7_pointer g_complex_wrapped(s7_scheme *sc, s7_pointer args) {return complex_p_pp_wrapped(sc, s7_car(args), s7_cadr(args));}

s7_pointer complex_p_ii_wrapped(s7_scheme *sc, s7_int x, s7_int y) {return make_complex_not_0i(sc, (s7_double)x, (s7_double)y);} 
s7_pointer complex_p_dd_wrapped(s7_scheme *sc, s7_double x, s7_double y) {return make_complex_not_0i(sc, x, y);} 

s7_pointer complex_p_ii(s7_scheme *sc, s7_int x, s7_int y)
{
  return((y == 0) ? s7_make_integer(sc, x) : make_complex_not_0i(sc, (s7_double)x, (s7_double)y));
}

s7_pointer complex_p_dd(s7_scheme *sc, s7_double x, s7_double y)
{
  return((y == 0.0) ? s7_make_real(sc, x) : make_complex_not_0i(sc, x, y));
}

s7_pointer g_make_polar(s7_scheme *sc, s7_pointer args)
{
  s7_pointer mag = s7_car(args), ang = s7_cadr(args);
  if (!s7_is_real(mag))
    return s7_wrong_type_arg_error(sc, "make-polar", 1, mag, "a real");
  if (!s7_is_real(ang))
    return s7_wrong_type_arg_error(sc, "make-polar", 2, ang, "a real");

  s7_double m = s7_number_to_real(sc, mag);
  s7_double a = s7_number_to_real(sc, ang);
  return make_complex_not_0i(sc, m * cos(a), m * sin(a));
}

s7_double real_part_d_7p(s7_scheme *sc, s7_pointer x)
{
  if (s7_is_number(x)) return s7_real_part(x);
  s7_wrong_type_arg_error(sc, "real-part", 1, x, "a number");
  return 0.0;
}

s7_pointer g_real_part(s7_scheme *sc, s7_pointer args)
{
  s7_pointer x = s7_car(args);
  if (!s7_is_number(x))
    return s7_wrong_type_arg_error(sc, "real-part", 1, x, "a number");
  return real_part_p_p(sc, x);
}

s7_double imag_part_d_7p(s7_scheme *sc, s7_pointer x)
{
  if (s7_is_number(x)) return s7_imag_part(x);
  s7_wrong_type_arg_error(sc, "imag-part", 1, x, "a number");
  return 0.0;
}

s7_pointer real_part_p_p(s7_scheme *sc, s7_pointer x)
{
  if (s7_is_integer(x) || s7_is_rational(x) || s7_is_real(x))
    return x;
  if (s7_is_complex(x))
    return s7_make_real(sc, s7_real_part(x));
  return s7_wrong_type_arg_error(sc, "real-part", 1, x, "a number");
}

s7_pointer imag_part_p_p(s7_scheme *sc, s7_pointer x)
{
  if (s7_is_integer(x) || s7_is_rational(x))
    return s7_make_integer(sc, 0);
  if (s7_is_real(x))
    return s7_make_real(sc, 0.0);
  if (s7_is_complex(x))
    return s7_make_real(sc, s7_imag_part(x));
  return s7_wrong_type_arg_error(sc, "imag-part", 1, x, "a number");
}

s7_pointer g_imag_part(s7_scheme *sc, s7_pointer args)
{
  s7_pointer x = s7_car(args);
  return imag_part_p_p(sc, x);
}

s7_pointer g_is_complex(s7_scheme *sc, s7_pointer args)
{
  s7_pointer p = s7_car(args);
  if (s7_is_number(p)) return(s7_t(sc));
  if (!s7i_has_active_methods(sc, p)) return(s7_f(sc));
  return(s7i_apply_boolean_method(sc, p, s7i_is_complex_symbol(sc)));
}

#if HAVE_COMPLEX_NUMBERS

#ifndef s7_complex_i
  #define s7_complex_i (s7_complex)_Complex_I
#endif

/* OpenBSD/NetBSD fallbacks for complex inverse hyperbolic functions */
s7_complex s7i_catanh_1(s7_complex z) {return(clog((1.0 + z) / (1.0 - z)) / 2.0);}
s7_complex s7i_casinh_1(s7_complex z) {return(clog(z + csqrt(1.0 + z * z)));}
s7_complex s7i_cacosh_1(s7_complex z) {return(clog(z + csqrt(z * z - 1.0)));}

/* Platform-specific fallback implementations for complex trig functions */
#ifndef HAVE_COMPLEX_TRIG
  #if __cplusplus || __TINYC__
    #define HAVE_COMPLEX_TRIG 0
  #else
    #define HAVE_COMPLEX_TRIG 1
  #endif
#endif

#if !HAVE_COMPLEX_TRIG
#if __cplusplus

  static s7_complex ctan(s7_complex z)   {return(csin(z) / ccos(z));}
  static s7_complex ctanh(s7_complex z)  {return(csinh(z) / ccosh(z));}
  static s7_complex casin(s7_complex z)  {return(-s7_complex_i * clog(s7_complex_i * z + csqrt(1.0 - z * z)));}
  static s7_complex cacos(s7_complex z)  {return(-s7_complex_i * clog(z + s7_complex_i * csqrt(1.0 - z * z)));}
  static s7_complex catan(s7_complex z)  {return(s7_complex_i * clog((s7_complex_i + z) / (s7_complex_i - z)) / 2.0);}
  static s7_complex casinh(s7_complex z) {return(clog(z + csqrt(1.0 + z * z)));}
  static s7_complex cacosh(s7_complex z) {return(clog(z + csqrt(z * z - 1.0)));}
  static s7_complex catanh(s7_complex z) {return(clog((1.0 + z) / (1.0 - z)) / 2.0);}
#else

#if (!defined(__FreeBSD__)) || (__FreeBSD__ < 12)
static s7_complex clog(s7_complex z) {return(log(fabs(cabs(z))) + carg(z) * s7_complex_i);}
static s7_complex cpow(s7_complex x, s7_complex y)
{
  s7_double r = cabs(x);
  s7_double theta = carg(x);
  s7_double yre = creal(y);
  s7_double yim = cimag(y);
  s7_double nr = exp(yre * log(r) - yim * theta);
  s7_double ntheta = yre * theta + yim * log(r);
  return(nr * cos(ntheta) + (nr * sin(ntheta)) * s7_complex_i);
}
#endif
#if (!defined(__FreeBSD__)) || (__FreeBSD__ < 9)
  static s7_complex cexp(s7_complex z) {return(exp(creal(z)) * cos(cimag(z)) + (exp(creal(z)) * sin(cimag(z))) * s7_complex_i);}
#endif

#if (!defined(__FreeBSD__)) || (__FreeBSD__ < 10)
  static s7_complex csin(s7_complex z)   {return(sin(creal(z)) * cosh(cimag(z)) + (cos(creal(z)) * sinh(cimag(z))) * s7_complex_i);}
  static s7_complex ccos(s7_complex z)   {return(cos(creal(z)) * cosh(cimag(z)) + (-sin(creal(z)) * sinh(cimag(z))) * s7_complex_i);}
  static s7_complex csinh(s7_complex z)  {return(sinh(creal(z)) * cos(cimag(z)) + (cosh(creal(z)) * sin(cimag(z))) * s7_complex_i);}
  static s7_complex ccosh(s7_complex z)  {return(cosh(creal(z)) * cos(cimag(z)) + (sinh(creal(z)) * sin(cimag(z))) * s7_complex_i);}
  static s7_complex ctan(s7_complex z)   {return(csin(z) / ccos(z));}
  static s7_complex ctanh(s7_complex z)  {return(csinh(z) / ccosh(z));}
  static s7_complex casin(s7_complex z)  {return(-s7_complex_i * clog(s7_complex_i * z + csqrt(1.0 - z * z)));}
  static s7_complex cacos(s7_complex z)  {return(-s7_complex_i * clog(z + s7_complex_i * csqrt(1.0 - z * z)));}
  static s7_complex catan(s7_complex z)  {return(s7_complex_i * clog((s7_complex_i + z) / (s7_complex_i - z)) / 2.0);}
  static s7_complex catanh(s7_complex z) {return(clog((1.0 + z) / (1.0 - z)) / 2.0);}
  static s7_complex casinh(s7_complex z) {return(clog(z + csqrt(1.0 + z * z)));}
  static s7_complex cacosh(s7_complex z) {return(clog(z + csqrt(z * z - 1.0)));}
#endif /* not FreeBSD 10 */
#endif /* not c++ */
#endif /* not HAVE_COMPLEX_TRIG */

#else  /* not HAVE_COMPLEX_NUMBERS */
  #ifndef _Complex_I
    #define _Complex_I 1.0
  #endif
  #define creal(x) 0.0
  #define cimag(x) 0.0
  #define csin(x) sin(x)
  #define casin(x) x
  #define ccos(x) cos(x)
  #define cacos(x) x
  #define ctan(x) x
  #define catan(x) x
  #define csinh(x) x
  #define casinh(x) x
  #define ccosh(x) x
  #define cacosh(x) x
  #define ctanh(x) x
  #define catanh(x) x
  #define cexp(x) exp(x)
  #define cpow(x, y) pow(x, y)
  #define clog(x) log(x)
  #define csqrt(x) sqrt(x)
  #define conj(x) x
#endif /* HAVE_COMPLEX_NUMBERS */
