/* s7_scheme_file.c - file port implementations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#include "s7_internal.h"
#include "s7_scheme_file.h"
#include "s7_scheme_read.h"
#include "s7_internal_helpers.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !MS_WINDOWS
  #include <sys/stat.h>
#endif

s7_pointer method_or_bust(s7_scheme *sc, s7_pointer obj, s7_pointer method, s7_pointer args, s7_pointer typ, int32_t num);

/* -------- local macros (mirrors of s7.c internals, via s7_internal.h types) -------- */

#ifndef port_file
  #define port_file(p)                    port_port(p)->file
#endif
#ifndef port_data
  #define port_data(p)                    (T_Prt(p))->object.prt.data
#endif
#ifndef port_data_size
  #define port_data_size(p)               (T_Prt(p))->object.prt.size
#endif
#ifndef port_position
  #define port_position(p)                (T_Prt(p))->object.prt.point
#endif
#ifndef port_block
  #define port_block(p)                   (T_Prt(p))->object.prt.block
#endif
#ifndef port_data_block
  #define port_data_block(p)              port_port(p)->block
#endif
#ifndef port_type
  #define port_type(p)                    port_port(p)->ptype
#endif
#ifndef port_line_number
  #define port_line_number(p)             port_port(p)->line_number
#endif
#ifndef port_file_number
  #define port_file_number(p)             port_port(p)->file_number
#endif
#ifndef port_filename
  #define port_filename(p)                port_port(p)->filename
#endif
#ifndef port_filename_length
  #define port_filename_length(p)         port_port(p)->filename_length
#endif
#ifndef port_needs_free
  #define port_needs_free(p)              port_port(p)->needs_free
#endif
#ifndef port_set_closed
  #define port_set_closed(p, Val)         port_port(p)->is_closed = Val
#endif
#ifndef port_set_binary
  #define port_set_binary(p, Val)         port_port(p)->is_binary = Val
#endif
#ifndef add_output_port
  #define add_output_port(sc, p)          add_to_gc_list(sc, sc->output_ports, p)
#endif
#ifndef safe_strlen
  #define safe_strlen(s)                  s7i_safe_strlen(s)
#endif
#ifndef mallocate
  #define mallocate(sc, size)             s7i_mallocate(sc, size)
#endif
#ifndef mallocate_port
  #define mallocate_port(sc)              s7i_mallocate_port(sc)
#endif
#ifndef liberate
  #define liberate(sc, b)                 s7i_liberate(sc, b)
#endif
#ifndef port_set_filename
  #define port_set_filename(sc, port, name, len) s7i_port_set_filename(sc, port, name, (s7_int)(len))
#endif
#ifndef file_error_nr
  #define file_error_nr(sc, caller, descr, name) s7i_file_error_nr(sc, caller, descr, name)
#endif

#ifndef MAX_SIZE_FOR_FILE_TO_STRING_PORT_CONVERSION
  #define MAX_SIZE_FOR_FILE_TO_STRING_PORT_CONVERSION 10000000
#endif

static s7_pointer make_input_file(s7_scheme *sc, const char *name, FILE *fp, bool binary_mode)
{
  return(s7i_read_file(sc, fp, name, MAX_SIZE_FOR_FILE_TO_STRING_PORT_CONVERSION, "open", binary_mode));
}

bool is_directory(const char *filename)
{
#if !MS_WINDOWS
  #ifdef S_ISDIR
    struct stat statbuf;
    return((stat(filename, &statbuf) >= 0) &&
	   (S_ISDIR(statbuf.st_mode)));
  #endif
#endif
  return(false);
}

block_t *expand_filename(s7_scheme *sc, const char *name)
{
#if WITH_GCC
  if ((name[0] == '~') && (name[1] == '/'))    /* catch one special case, "~/..." */
    {
      char *home = getenv("HOME");
      if (home)
	{
	  s7_int len = safe_strlen(name) + safe_strlen(home) + 1;
	  block_t *b = mallocate(sc, len);
	  char *filename = (char *)s7i_block_data(b);
	  filename[0] = '\0';
	  snprintf(filename, (size_t)len, "%s%s", home, name + 1);
	  return(b);
	}}
#endif
  return(NULL);
}

s7_pointer open_input_file_1(s7_scheme *sc, const char *name, const char *mode, const char *caller)
{
  FILE *fp;
#if WITH_GCC
  block_t *b;
#endif
  /* see if we can open this file before allocating a port */
  if (is_directory(name))
    file_error_nr(sc, caller, "file is a directory:", name);
  errno = 0;
  fp = fopen(name, mode);
  if (fp)
    return(make_input_file(sc, name, fp, (strchr(mode, 'b') != NULL)));

#if !MS_WINDOWS
  if (errno == EINVAL)
    file_error_nr(sc, caller, "invalid mode", mode);
#if WITH_GCC
  if ((!name) || (!*name))
    file_error_nr(sc, caller, strerror(errno), name);
  b = expand_filename(sc, name);
  if (b)
    {
      char *new_name = (char *)s7i_block_data(b);
      fp = fopen(new_name, mode);
      liberate(sc, b);
      if (fp)
	return(make_input_file(sc, name, fp, (strchr(mode, 'b') != NULL)));
    }
#endif
#endif
  file_error_nr(sc, caller, strerror(errno), name);
  return(sc->io_error_symbol);
}

s7_pointer s7_open_input_file(s7_scheme *sc, const char *name, const char *mode)
{
  return(open_input_file_1(sc, name, mode, "open-input-file"));
}

s7_pointer g_open_input_file(s7_scheme *sc, s7_pointer args)
{
  const s7_pointer name = s7_car(args);
  /* open-input-file can create a new output file if the file to be opened does not exist, and the "a" mode is given */
  if (!s7_is_string(name))
    return(method_or_bust(sc, name, sc->open_input_file_symbol, args, sc->type_names[T_STRING], 1));
  if (!s7_is_pair(s7_cdr(args)))
    return(open_input_file_1(sc, s7_string(name), "r", "open-input-file"));
  {
    s7_pointer mode = s7_cadr(args);
    if (!s7_is_string(mode))
      return(method_or_bust(sc, mode, sc->open_input_file_symbol, args, wrap_string(sc, "a string (a mode such as \"r\")", 29), 2));
    /* since scheme allows embedded nulls, dumb stuff is accepted here: (open-input-file file "a\x00b") -- should this be an error? */
    return(open_input_file_1(sc, s7_string(name), s7_string(mode), "open-input-file"));
  }
}

s7_pointer s7_open_output_file(s7_scheme *sc, const char *name, const char *mode)
{
  FILE *fp;
  s7_pointer port;
  block_t *block, *b;
  /* see if we can open this file before allocating a port */

  errno = 0;
  fp = fopen(name, mode);
  if (!fp)
    {
#if !MS_WINDOWS
      if (errno == EINVAL)
	file_error_nr(sc, "open-output-file", "invalid mode", mode);
#endif
      file_error_nr(sc, "open-output-file", strerror(errno), name);
    }
  new_cell(sc, port, T_OUTPUT_PORT);
  b = mallocate_port(sc);
  port_block(port) = b;
  port_port(port) = (port_t *)s7i_block_data(b);
  port_type(port) = file_port;
  port_set_binary(port, (strchr(mode, 'b') != NULL));
  port_set_closed(port, false);
  port_filename_length(port) = safe_strlen(name);
  port_set_filename(sc, port, name, port_filename_length(port));
  port_line_number(port) = 1;
  port_file_number(port) = 0;
  port_file(port) = fp;
  port_needs_free(port) = true;  /* hmm -- I think these are freed via s7_close_output_port -> close_output_port */
  port_position(port) = 0;
  port_data_size(port) = sc->output_file_port_length;
  block = mallocate(sc, sc->output_file_port_length);
  port_data_block(port) = block;
  port_data(port) = (uint8_t *)(s7i_block_data(block));
  port_port(port)->pf = s7i_output_file_functions();
  add_output_port(sc, port);
  return(port);
}

s7_pointer g_open_output_file(s7_scheme *sc, s7_pointer args)
{
  s7_pointer name = s7_car(args);
  if (!s7_is_string(name))
    return(method_or_bust(sc, name, sc->open_output_file_symbol, args, sc->type_names[T_STRING], 1));
  if (!s7_is_pair(s7_cdr(args)))
    return(s7_open_output_file(sc, s7_string(name), "w"));
  if (!s7_is_string(s7_cadr(args)))
    return(method_or_bust(sc, s7_cadr(args), sc->open_output_file_symbol, args, wrap_string(sc, "a string (a mode such as \"w\")", 29), 2));
  return(s7_open_output_file(sc, s7_string(name), s7_string(s7_cadr(args))));
}
