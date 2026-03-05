/* s7_scheme_file.c - file utility implementations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#include "s7_scheme_file.h"
#include "s7.h"
#include "s7_internal_helpers.h"
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

/* -------------------------------- access -------------------------------- */
s7_pointer g_access(s7_scheme *sc, s7_pointer args)
{
  s7_pointer path = s7_car(args), mode = s7_cadr(args);
  if (!s7_is_string(path))
    return(s7_wrong_type_arg_error(sc, "access", 1, path, "a string"));
  if (!s7_is_integer(mode))
    return(s7_wrong_type_arg_error(sc, "access", 2, mode, "an integer"));
  return(s7_make_integer(sc, (s7_int)access((char *)s7_string(path), (int)s7_integer(mode))));
}

/* -------------------------------- file-mtime -------------------------------- */
s7_pointer g_file_mtime(s7_scheme *sc, s7_pointer args)
{
  #define H_file_mtime "(file-mtime file): return the write date of file"

  struct stat statbuf;
  int32_t err;
  const s7_pointer name = s7_car(args);

  if (!s7_is_string(name))
    return(s7i_sole_arg_method_or_bust(sc, name, "file-mtime", args, "a string"));
  {
    err = stat(s7_string(name), &statbuf);
    if (err < 0)
      return(s7_make_integer(sc, -1));
    return(s7_make_integer(sc, (s7_int)(statbuf.st_mtime)));
  }
}

/* -------------------------------- unlink -------------------------------- */
s7_pointer g_unlink(s7_scheme *sc, s7_pointer args)
{
  s7_pointer arg = s7_car(args);
  if (!s7_is_string(arg))
    return(s7_wrong_type_arg_error(sc, "unlink", 1, arg, "a string"));
  return(s7_make_integer(sc, (s7_int)unlink((char*)s7_string(arg))));
}
