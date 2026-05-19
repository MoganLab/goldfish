/* s7_liii_string.c - string utility implementations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#include "s7_liii_string.h"
#include "s7.h"
#include "s7_internal_helpers.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* Externally defined in s7.c - character cache */
extern s7_pointer *chars;

/* Helper function for out-of-range errors */
static s7_pointer string_ref_out_of_range(s7_scheme *sc, s7_int index, bool is_negative)
{
  return s7_out_of_range_error(sc, "string-ref", 2, s7_make_integer(sc, index),
                               is_negative ? "it is negative" : "it is too large");
}

static s7_pointer method_or_bust(s7_scheme *sc, s7_pointer obj, const char *name, const char *type_name)
{
  s7_pointer sym = s7_make_symbol(sc, name);
  s7_pointer func = s7_method(sc, obj, sym);
  if (func != s7_undefined(sc))
    return(s7_apply_function(sc, func, s7_cons(sc, obj, s7_nil(sc))));
  return(s7_wrong_type_arg_error(sc, name, 1, obj, type_name));
}

/* -------------------------------- char-position -------------------------------- */

s7_pointer
g_char_position (s7_scheme* sc, s7_pointer args) {
  const s7_pointer arg1= s7_car (args);
  if ((!s7_is_character (arg1)) && (!s7_is_string (arg1)))
    return s7i_method_or_bust (sc, arg1, "char-position", args, "a character", 1);

  const s7_pointer arg2= s7_cadr (args);
  if (!s7_is_string (arg2)) return s7i_method_or_bust (sc, arg2, "char-position", args, "a string", 2);

  s7_int start= 0;
  if (s7_is_pair (s7_cddr (args))) {
    const s7_pointer arg3= s7_caddr (args);
    if (!s7_is_integer (arg3)) return s7i_method_or_bust (sc, arg3, "char-position", args, "an integer", 3);
    start= s7_number_to_integer_with_caller (sc, arg3, "char-position");
    if (start < 0) return s7_wrong_type_arg_error (sc, "char-position", 3, arg3, "a non-negative integer");
  }

  const char*  porig= s7_string (arg2);
  const s7_int len  = s7_string_length (arg2);
  if (start >= len) return s7_f (sc);

  if (s7_is_character (arg1)) {
    const char  c= (char) s7_character (arg1);
    const char* p= strchr ((const char*) (porig + start), (int) c);
    return p ? s7_make_integer (sc, (s7_int) (p - porig)) : s7_f (sc);
  }

  if (s7_string_length (arg1) == 0) return s7_f (sc);

  const char*  pset= s7_string (arg1);
  const s7_int pos = (s7_int) strcspn ((const char*) (porig + start), (const char*) pset);
  if ((pos + start) < len) return s7_make_integer (sc, pos + start);

  return s7_f (sc);
}

s7_pointer
char_position_p_ppi (s7_scheme* sc, s7_pointer chr, s7_pointer str, s7_int start) {
  if (!s7_is_string (str)) return s7_wrong_type_arg_error (sc, "char-position", 2, str, "a string");
  if (start < 0)
    return s7_wrong_type_arg_error (sc, "char-position", 3, s7_make_integer (sc, start), "a non-negative integer");

  const char*  porig= s7_string (str);
  const s7_int len  = s7_string_length (str);
  if (start >= len) return s7_f (sc);

  const char  c= (char) s7_character (chr);
  const char* p= strchr ((const char*) (porig + start), (int) c);
  return p ? s7_make_integer (sc, (s7_int) (p - porig)) : s7_f (sc);
}

s7_pointer
g_char_position_csi (s7_scheme* sc, s7_pointer args) {
  const s7_pointer arg2= s7_cadr (args);
  if (!s7_is_string (arg2)) return g_char_position (sc, args);

  const s7_int len  = s7_string_length (arg2);
  s7_int       start= 0;

  if (s7_is_pair (s7_cddr (args))) {
    const s7_pointer arg3= s7_caddr (args);
    if (!s7_is_integer (arg3)) return g_char_position (sc, args);
    start= s7_number_to_integer_with_caller (sc, arg3, "char-position");
    if (start < 0) return s7_wrong_type_arg_error (sc, "char-position", 3, arg3, "a non-negative integer");
    if (start >= len) return s7_f (sc);
  }

  if (len == 0) return s7_f (sc);

  const char* porig= s7_string (arg2);
  const char  c    = (char) s7_character (s7_car (args));
  const char* p    = strchr ((const char*) (porig + start), (int) c);
  return p ? s7_make_integer (sc, (s7_int) (p - porig)) : s7_f (sc);
}

/* -------------------------------- string-position -------------------------------- */

s7_pointer g_string_position(s7_scheme *sc, s7_pointer args)
{
  const s7_pointer str1 = s7_car(args), str2 = s7_cadr(args);

  if (!s7_is_string(str1))
    return method_or_bust(sc, str1, "string-position", "a string");
  if (!s7_is_string(str2))
    return method_or_bust(sc, str2, "string-position", "a string");

  s7_int start = 0;
  if (s7_is_pair(s7_cddr(args)))
    {
      const s7_pointer arg3 = s7_caddr(args);
      if (!s7_is_integer(arg3))
        return method_or_bust(sc, arg3, "string-position", "an integer");
      start = s7_integer(arg3);
      if (start < 0)
        return s7_wrong_type_arg_error(sc, "string-position", 3, arg3, "a non-negative integer");
    }

  if (s7_string_length(str1) == 0) return s7_f(sc);
  if (start >= s7_string_length(str2)) return s7_f(sc);

  const char *s1 = s7_string(str1);
  const char *s2 = s7_string(str2);
  const char *p2 = strstr((const char *)(s2 + start), s1);
  return p2 ? s7_make_integer(sc, (s7_int)(p2 - s2)) : s7_f(sc);
}

/* -------------------------------- string-ref -------------------------------- */

s7_pointer string_ref_1(s7_scheme *sc, s7_pointer strng, s7_pointer index)
{
  if (!s7_is_integer(index))
    return s7_wrong_type_arg_error(sc, "string-ref", 2, index, "an integer");

  s7_int ind = s7_integer(index);
  if (ind < 0)
    return string_ref_out_of_range(sc, ind, true);
  if (ind >= s7_string_length(strng))
    return string_ref_out_of_range(sc, ind, false);

  const char *str = s7_string(strng);
  return chars[((uint8_t *)str)[ind]];
}

s7_pointer g_string_ref(s7_scheme *sc, s7_pointer args)
{
  s7_pointer str = s7_car(args);
  if (!s7_is_string(str))
    return method_or_bust(sc, str, "string-ref", "a string");
  return string_ref_1(sc, str, s7_cadr(args));
}

/* -------------------------------- string-set! -------------------------------- */

s7_pointer g_string_set(s7_scheme *sc, s7_pointer args)
{
  s7_pointer strng = s7_car(args);
  s7_pointer index = s7_cadr(args);

  if (!s7_is_string(strng))
    return method_or_bust(sc, strng, "string-set!", "a string");
  if (s7_is_immutable(strng))
    return s7_wrong_type_arg_error(sc, "string-set!", 1, strng, "a mutable string");
  if (!s7_is_integer(index))
    return s7_wrong_type_arg_error(sc, "string-set!", 2, index, "an integer");

  s7_int ind = s7_integer(index);
  if (ind < 0)
    return s7_out_of_range_error(sc, "string-set!", 2, index, "it is negative");
  if (ind >= s7_string_length(strng))
    return s7_out_of_range_error(sc, "string-set!", 2, index, "it is too large");

  s7_pointer c = s7_caddr(args);
  if (!s7_is_character(c))
    return s7_wrong_type_arg_error(sc, "string-set!", 3, c, "a character");

  char *str = (char *)s7_string(strng);
  str[ind] = (char)s7_character(c);
  return c;
}

/*---------------------------------string-length---------------------------------*/

s7_pointer g_string_length(s7_scheme *sc, s7_pointer args)
{
  #define H_string_length "(string-length str) returns the length of the string str"
  #define Q_string_length s7_make_signature(sc, 2, sc->is_integer_symbol, sc->is_string_symbol)
  s7_pointer str = s7_car(args);
  if (!s7_is_string(str))
    return(s7i_sole_arg_method_or_bust(sc, str, "string-length", args, "a string"));
  return(s7_make_integer(sc, s7_string_length(str)));
}
