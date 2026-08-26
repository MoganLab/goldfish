/* s7_scheme_write.c - write function implementations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#include "s7_internal.h"
#include "s7_scheme_write.h"

#define IF_METHOD_EXISTS_RETURN_VALUE(Sc, Obj, Method_name, Args) \
  do { \
    if (s7i_has_active_methods(Sc, Obj)) { \
      s7_pointer _func_ = s7i_find_method_with_let(Sc, Obj, s7_make_symbol(Sc, Method_name)); \
      if (_func_ != s7_undefined(Sc)) \
        return s7_apply_function(Sc, _func_, Args); \
    } \
  } while (0)

/* -------- cycles -------- */

#define INITIAL_SHARED_INFO_SIZE 8

int32_t shared_ref(shared_info_t *ci, const s7_pointer p)
{
  /* from print after collecting refs, not called by equality check, only called in object_to_port_with_circle_check_1 */
  s7_pointer *objs = ci->objs;
  for (int32_t i = 0; i < ci->top; i++)
    if (objs[i] == p)
      {
	int32_t val = ci->refs[i];
	if (val > 0)
	  ci->refs[i] = -ci->refs[i];
	return(val);
      }
  return(0);
}

void flip_ref(shared_info_t *ci, const s7_pointer p)
{
  s7_pointer *objs = ci->objs;
  for (int32_t i = 0; i < ci->top; i++)
    if (objs[i] == p)
      {
	ci->refs[i] = -ci->refs[i];
	break;
      }
}

int32_t peek_shared_ref_1(shared_info_t *ci, const s7_pointer p)
{
  /* returns 0 if not found, otherwise the ref value for p */
  s7_pointer *objs = ci->objs;
  for (int32_t i = 0; i < ci->top; i++)
    if (objs[i] == p)
      return(ci->refs[i]);
  return(0);
}

int32_t peek_shared_ref(shared_info_t *ci, s7_pointer p)
{
  /* returns 0 if not found, otherwise the ref value for p */
  return((is_collected_unchecked(p)) ? peek_shared_ref_1(ci, p) : 0);
}

void enlarge_shared_info(shared_info_t *ci)
{
  ci->size *= 2;
  ci->size2 = ci->size - 2;
  ci->objs = (s7_pointer *)Realloc(ci->objs, ci->size * sizeof(s7_pointer));
  ci->refs = (int32_t *)Realloc(ci->refs, ci->size * sizeof(int32_t));
  ci->defined = (bool *)Realloc(ci->defined, ci->size * sizeof(bool));
  /* this clearing is needed, memclr is not faster */
  for (int32_t i = ci->top; i < ci->size; i++)
    {
      ci->refs[i] = 0;
      ci->objs[i] = NULL;
    }
}

static bool check_collected(s7_pointer top, shared_info_t *ci)
{
  const s7_pointer *objs_end = (s7_pointer *)(ci->objs + ci->top);
  for (s7_pointer *p = ci->objs; p < objs_end; p++)
    if ((*p) == top)
      {
	int32_t i = (int32_t)(p - ci->objs);
	if (ci->refs[i] == 0)
	  {
	    ci->has_hits = true;
	    ci->refs[i] = ++ci->ref;  /* if found, set the ref number */
	  }
	break;
      }
  set_cyclic(top);
  return(true);
}


static bool collect_vector_info(s7_scheme *sc, shared_info_t *ci, s7_pointer top, bool stop_at_print_length)
{
  s7_int plen;
  bool cyclic = false;

  if (stop_at_print_length)
    {
      plen = sc->print_length;
      if (plen > vector_length(top))
	plen = vector_length(top);
    }
  else plen = vector_length(top);
  for (s7_int i = 0; i < plen; i++)
    {
      const s7_pointer vel = vector_element_unchecked(top, i);   /* "unchecked" because top might be rootlet, I think */
      if ((has_structure(vel)) &&
	  (collect_shared_info(sc, ci, vel, stop_at_print_length)))
	{
	  set_cyclic(vel);
	  cyclic = true;
	  if ((is_c_pointer(vel)) ||
	      (is_iterator(vel)) ||
	      (is_c_object(vel)))
	    check_collected(top, ci);
	}}
  if (cyclic) set_cyclic(top);
  return(cyclic);
}

bool collect_shared_info(s7_scheme *sc, shared_info_t *ci, s7_pointer top, bool stop_at_print_length)
{
  /* look for top in current list.
   * As we collect objects (guaranteed to have structure) we set the collected bit.  If we ever
   *   encounter an object with that bit on, we've seen it before so we have a possible cycle.
   *   Once the collection pass is done, we run through our list, and clear all these bits.
   */
  bool top_cyclic;

  if (is_collected_or_shared(top))
    return((!is_shared(top)) && (check_collected(top, ci)));

  /* top not seen before -- add it to the list */
  set_collected(top);
  if (ci->top == ci->size)
    enlarge_shared_info(ci);
  ci->objs[ci->top++] = top;

  top_cyclic = false;
  /* now search the rest of this structure */
  if (is_pair(top))
    {
      s7_pointer p;
      if ((has_structure(car(top))) &&
	  (collect_shared_info(sc, ci, car(top), stop_at_print_length)))
	top_cyclic = true;

      for (p = cdr(top); is_pair(p); p = cdr(p))
	{
	  if (is_collected_or_shared(p))
	    {
	      set_cyclic(top);
	      set_cyclic(p);
	      if (!is_shared(p))
		return(check_collected(p, ci));
	      if (!top_cyclic)
		for (s7_pointer cp = top; cp != p; cp = cdr(cp)) set_shared(cp);
	      return(top_cyclic);
	    }
 	  set_collected(p);
	  if (ci->top == ci->size)
	    enlarge_shared_info(ci);
	  ci->objs[ci->top++] = p;
	  if ((has_structure(car(p))) &&
	      (collect_shared_info(sc, ci, car(p), stop_at_print_length)))
	    top_cyclic = true;
	}
      if ((has_structure(p)) &&
	  (collect_shared_info(sc, ci, p, stop_at_print_length)))
	{
	  set_cyclic(top);
	  return(true);
	}
      if (!top_cyclic)
	for (s7_pointer cp = top; is_pair(cp); cp = cdr(cp)) set_shared(cp);
      else set_cyclic(top);
      return(top_cyclic);
    }
  switch (type(top))
    {
    case T_VECTOR:
      if (collect_vector_info(sc, ci, top, stop_at_print_length))
	top_cyclic = true;
      break;

    case T_ITERATOR:
      if ((is_sequence(iterator_sequence(top))) && /* might be a function with +iterator+ local */
	  (collect_shared_info(sc, ci, iterator_sequence(top), stop_at_print_length)))
	{
	  if (peek_shared_ref(ci, iterator_sequence(top)) == 0)
	    check_collected(iterator_sequence(top), ci);
	  top_cyclic = true;
	}
      break;

    case T_HASH_TABLE:
      if (hash_table_entries(top) > 0)
	{
	  const s7_int len = (s7_int)hash_table_size(top);
	  hash_entry_t **entries = hash_table_elements(top);
	  const bool keys_safe = hash_keys_not_cyclic(sc, top);
	  for (s7_int i = 0; i < len; i++)
	    for (hash_entry_t *entry = entries[i]; entry; entry = hash_entry_next(entry))
	      {
		if ((!keys_safe) &&
		    (has_structure(hash_entry_key(entry))) &&
		    (collect_shared_info(sc, ci, hash_entry_key(entry), stop_at_print_length)))
		  top_cyclic = true;
		if ((has_structure(hash_entry_value(entry))) &&
		    (collect_shared_info(sc, ci, hash_entry_value(entry), stop_at_print_length)))
		  {
		    if ((is_c_pointer(hash_entry_value(entry))) ||
			(is_iterator(hash_entry_value(entry))) ||
			(is_c_object(hash_entry_value(entry))))
		      check_collected(top, ci);
		    top_cyclic = true;
		  }}}
      break;

    case T_SLOT: /* this can be hit if we somehow collect_shared_info on sc->rootlet via collect_vector_info (see the let case below) */
      if ((has_structure(slot_value(top))) &&
	  (collect_shared_info(sc, ci, slot_value(top), stop_at_print_length)))
	top_cyclic = true;
      break;

    case T_LET:
      if (top == sc->rootlet)
	{
	  if (collect_vector_info(sc, ci, top, stop_at_print_length))
	    top_cyclic = true;
	}
      else
	for (s7_pointer let = top; let; let = let_outlet(let))
	  for (s7_pointer slot = let_slots(let); is_not_slot_end(slot); slot = next_slot(slot))
	    if ((has_structure(slot_value(slot))) &&
		(collect_shared_info(sc, ci, slot_value(slot), stop_at_print_length)))
	      {
		top_cyclic = true;
		if ((is_c_pointer(slot_value(slot))) ||
		    (is_iterator(slot_value(slot))) ||
		    (is_c_object(slot_value(slot))))
		  check_collected(top, ci);
	      }
      break;

    case T_CLOSURE: case T_CLOSURE_STAR:
      if (collect_shared_info(sc, ci, closure_body(top), stop_at_print_length))
	{
	  if (peek_shared_ref(ci, top) == 0)
	    check_collected(top, ci);
	  top_cyclic = true;
	}
      break;

    case T_C_POINTER:
      if ((has_structure(c_pointer_type(top))) &&
	  (collect_shared_info(sc, ci, c_pointer_type(top), stop_at_print_length)))
	{
	  if (peek_shared_ref(ci, c_pointer_type(top)) == 0)
	    check_collected(c_pointer_type(top), ci);
	  top_cyclic = true;
	}
      if ((has_structure(c_pointer_info(top))) &&
	  (collect_shared_info(sc, ci, c_pointer_info(top), stop_at_print_length)))
	{
	  if (peek_shared_ref(ci, c_pointer_info(top)) == 0)
	    check_collected(c_pointer_info(top), ci);
	  top_cyclic = true;
	}
      break;

    case T_C_OBJECT:
      if ((c_object_to_list(sc, top)) &&
	  (c_object_set(sc, top)) &&
	  (collect_shared_info(sc, ci, (*(c_object_to_list(sc, top)))(sc, set_plist_1(sc, top)), stop_at_print_length)))
	{
	  if (peek_shared_ref(ci, top) == 0)
	    check_collected(top, ci);
	  top_cyclic = true;
	}
      break;
    }
  if (!top_cyclic)
    set_shared(top);
  else set_cyclic(top);
  return(top_cyclic);
}

shared_info_t *make_shared_info(s7_scheme *sc)
{
  shared_info_t *ci = (shared_info_t *)Calloc(1, sizeof(shared_info_t));
  ci->size = INITIAL_SHARED_INFO_SIZE;
  ci->size2 = ci->size - 2;
  ci->objs = (s7_pointer *)Malloc(ci->size * sizeof(s7_pointer));
  ci->refs = (int32_t *)Calloc(ci->size, sizeof(int32_t));   /* finder expects 0 = unseen previously */
  ci->defined = (bool *)Calloc(ci->size, sizeof(bool));
  ci->cycle_port = sc->F;
  ci->init_port = sc->F;
  return(ci);
}

void free_shared_info(shared_info_t *ci)
{
  if (ci)
    {
      free(ci->objs);
      free(ci->refs);
      free(ci->defined);
      free(ci);
    }
}

shared_info_t *clear_shared_info(shared_info_t *ci)
{
  if (ci->top > 0)
    {
      memclr((void *)(ci->refs), ci->top * sizeof(int32_t));
      memclr((void *)(ci->defined), ci->top * sizeof(bool));
      for (int32_t i = 0; i < ci->top; i++)
	clear_cyclic_bits(ci->objs[i]); /* LOOP_4 is not faster */
      ci->top = 0;
    }
  ci->ref = 0;
  ci->has_hits = false;
  ci->ctr = 0;
  return(ci);
}

shared_info_t *load_shared_info(s7_scheme *sc, s7_pointer top, bool stop_at_print_length, shared_info_t *ci)
{
  /* for the printer, here only if is_structure(top) and top is not sc->rootlet */
  bool no_problem = true;
  s7_int stop_len;

  /* check for simple cases first */
  if (is_pair(top))
    {
      s7_pointer p = top;
      if (stop_at_print_length)
	{
	  s7_pointer slow = top;
	  stop_len = sc->print_length;
	  for (s7_int k = 0; k < stop_len; k += 2)
	    {
	      if (!is_pair(p)) break;
	      if (has_structure(car(p))) {no_problem = false; break;}
	      p = cdr(p);
	      if (!is_pair(p)) break;
	      if (has_structure(car(p))) {no_problem = false; break;}
	      p = cdr(p);
	      slow = cdr(slow);
	      if (p == slow) {no_problem = false; break;}
	    }}
      else
	if (s7_list_length(sc, top) == 0) /* it is circular at the top level (following cdr) */
	  no_problem = false;
	else
	  for (; is_pair(p); p = cdr(p))
	    if (has_structure(car(p))) {no_problem = false; break;} /* perhaps (and (length > 0 via sequence_is_empty)) or vector typer etc */
      if ((no_problem) &&
	  (!is_null(p)) && (has_structure(p)))
	no_problem = false;
      if (no_problem) return(NULL);
    }
  else
    if (is_t_vector(top)) /* any other vector can't happen */
      {
	stop_len = vector_length(top);
	if ((stop_at_print_length) &&
	    (stop_len > sc->print_length))
	  stop_len = sc->print_length;
	for (s7_int k = 0; k < stop_len; k++)
	  if (has_structure(vector_element(top, k))) {no_problem = false; break;}
	if (no_problem) return(NULL);
      }

    else /* added these 19-Oct-22 -- helps in tgc, but not much elsewhere */
      if ((is_let(top)) && (top != sc->rootlet))
	{
	  for (s7_pointer let = top; (no_problem) && (let); let = let_outlet(let))
	    for (s7_pointer slot = let_slots(let); is_not_slot_end(slot); slot = next_slot(slot))
	      if (has_structure(slot_value(slot))) /* slot_symbol need not be checked? */
		{no_problem = false; break;}
	  if (no_problem) return(NULL);
	}
      else
	if (is_hash_table(top))
	  {
	    hash_entry_t **entries = hash_table_elements(top);
	    bool keys_safe = hash_keys_not_cyclic(sc, top);
	    if (hash_table_entries(top) == 0) return(NULL);
	    for (s7_int len = (s7_int)hash_table_size(top), i = 0; i < len; i++)
	      for (hash_entry_t *entry = entries[i]; entry; entry = hash_entry_next(entry))
		if (((!keys_safe) && (has_structure(hash_entry_key(entry)))) || (has_structure(hash_entry_value(entry))))
		  {no_problem = false; break;}
	    if (no_problem) return(NULL);
	  }

  if ((S7_DEBUGGING) && (is_any_vector(top)) && (!is_t_vector(top))) fprintf(stderr, "%s[%d]: got abnormal vector\n", __func__, __LINE__);
  clear_shared_info(ci);
  {
    /* collect all pointers associated with top */
    const bool cyclic = collect_shared_info(sc, ci, top, stop_at_print_length);
    s7_pointer *ci_objs = ci->objs;
    int32_t *ci_refs = ci->refs;
    int32_t refs = 0;

    for (int32_t i = 0; i < ci->top; i++)
      clear_collected_and_shared(ci_objs[i]);
    if (!cyclic)
      return(NULL);
    if (!(ci->has_hits))
      return(NULL);

    /* find if any were referenced twice (once for just being there, so twice=shared)
     *   we know there's at least one such reference because has_hits is true.
     */
    for (int32_t i = 0; i < ci->top; i++)
      if (ci_refs[i] > 0)
	{
	  set_collected(ci_objs[i]);
	  if (i == refs)
	    refs++;
	  else
	    {
	      ci_objs[refs] = ci_objs[i];
	      ci_refs[refs++] = ci_refs[i];
	      ci_refs[i] = 0;
	      ci_objs[i] = NULL;
	    }}
    ci->top = refs;
    return(ci);
  }
}


/* -------------------------------- cyclic-sequences -------------------------------- */
s7_pointer cyclic_sequences_p_p(s7_scheme *sc, s7_pointer obj)
{
  if (has_structure(obj))
    {
      shared_info_t *ci = (sc->object_out_locked) ? sc->circle_info : load_shared_info(sc, obj, false, sc->circle_info); /* false=don't stop at print length (vectors etc) */
      if (ci)
	{
	  s7i_check_free_heap_size(sc, ci->top);
	  begin_temp(sc->y, sc->nil);
	  for (int32_t i = 0; i < ci->top; i++)
	    sc->y = cons_unchecked(sc, ci->objs[i], sc->y);
	  return_with_end_temp(sc->y);
	}}
  return(sc->nil);
}


/* -------------------------------- newline -------------------------------- */

void s7_newline(s7_scheme *sc, s7_pointer port)
{
  if (port != s7_f(sc))
    s7i_port_write_character(sc, (uint8_t)'\n', port);
}

s7_pointer g_newline(s7_scheme *sc, s7_pointer args)
{
  #define H_newline "(newline (port (current-output-port))) writes a carriage return to the port"
  #define Q_newline s7_make_signature(sc, 2, sc->is_char_symbol, s7_make_signature(sc, 2, sc->is_output_port_symbol, sc->not_symbol))

  const s7_pointer port = (s7_is_pair(args)) ? s7_car(args) : s7_current_output_port(sc);
  if (!s7_is_output_port(sc, port))
    {
      if (port == s7_f(sc)) return s7_make_character(sc, '\n');
      IF_METHOD_EXISTS_RETURN_VALUE(sc, port, "newline", args);
      return s7_wrong_type_arg_error(sc, "newline", 1, port, "an output port or #f");
    }
  if (s7i_port_is_closed(port))
    return s7_wrong_type_arg_error(sc, "newline", 1, port, "an open output port");
  s7_newline(sc, port);
  return s7_make_character(sc, '\n');
}

s7_pointer newline_p(s7_scheme *sc)
{
  s7_newline(sc, s7_current_output_port(sc));
  return s7_make_character(sc, '\n');
}

s7_pointer newline_p_p(s7_scheme *sc, s7_pointer port)
{
  if (!s7_is_output_port(sc, port))
    {
      if (port == s7_f(sc)) return s7_make_character(sc, '\n');
      return s7i_method_or_bust_p(sc, port, "newline", "an output port");
    }
  s7_newline(sc, port);
  return s7_make_character(sc, '\n');
}


/* -------------------------------- write -------------------------------- */

s7_pointer s7_write(s7_scheme *sc, s7_pointer obj, s7_pointer port)
{
  if (port != s7_f(sc))
    {
      if (s7i_port_is_closed(port))
        return s7_wrong_type_arg_error(sc, "write", 2, port, "an open output port");
      s7i_object_out(sc, obj, port, S7I_P_WRITE);
    }
  return obj;
}

s7_pointer write_p_p(s7_scheme *sc, s7_pointer x)
{
  s7_pointer port = s7_current_output_port(sc);
  return (port == s7_f(sc)) ? x : s7i_object_out(sc, x, port, S7I_P_WRITE);
}

s7_pointer write_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer port)
{
  if (!s7_is_output_port(sc, port))
    {
      if (port == s7_f(sc)) return x;
      IF_METHOD_EXISTS_RETURN_VALUE(sc, port, "write", s7_cons(sc, x, s7_cons(sc, port, s7_nil(sc))));
      return s7_wrong_type_arg_error(sc, "write", 2, port, "an output port or #f");
    }
  if (s7i_port_is_closed(port))
    return s7_wrong_type_arg_error(sc, "write", 2, port, "an open output port");
  return s7i_object_out(sc, x, port, S7I_P_WRITE);
}

s7_pointer g_write(s7_scheme *sc, s7_pointer args)
{
  #define H_write "(write obj (port (current-output-port))) writes (object->string obj) to the output port"
  #define Q_write s7_make_signature(sc, 3, sc->T, sc->T, s7_make_signature(sc, 2, sc->is_output_port_symbol, sc->not_symbol))
  IF_METHOD_EXISTS_RETURN_VALUE(sc, s7_car(args), "write", args);
  return write_p_pp(sc, s7_car(args), (s7_is_pair(s7_cdr(args))) ? s7_cadr(args) : s7_current_output_port(sc));
}

s7_pointer g_write_2(s7_scheme *sc, s7_pointer args)
{
  return write_p_pp(sc, s7_car(args), s7_cadr(args));
}


/* -------------------------------- display -------------------------------- */

s7_pointer s7_display(s7_scheme *sc, s7_pointer obj, s7_pointer port)
{
  if (port != s7_f(sc))
    {
      if (s7i_port_is_closed(port))
        return s7_wrong_type_arg_error(sc, "display", 2, port, "an open output port");
      s7i_object_out(sc, obj, port, S7I_P_DISPLAY);
    }
  return obj;
}

s7_pointer display_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer port)
{
  if (!s7_is_output_port(sc, port))
    {
      if (port == s7_f(sc)) return x;
      IF_METHOD_EXISTS_RETURN_VALUE(sc, port, "display", s7_cons(sc, x, s7_cons(sc, port, s7_nil(sc))));
      return s7_wrong_type_arg_error(sc, "display", 2, port, "an output port or #f");
    }
  if (s7i_port_is_closed(port))
    return s7_wrong_type_arg_error(sc, "display", 2, port, "an open output port");
  IF_METHOD_EXISTS_RETURN_VALUE(sc, x, "display", s7_cons(sc, x, s7_cons(sc, port, s7_nil(sc))));
  return s7i_object_out(sc, x, port, S7I_P_DISPLAY);
}

s7_pointer g_display(s7_scheme *sc, s7_pointer args)
{
  #define H_display "(display obj (port (current-output-port))) prints obj"
  #define Q_display s7_make_signature(sc, 3, sc->T, sc->T, s7_make_signature(sc, 2, sc->is_output_port_symbol, sc->not_symbol))
  return display_p_pp(sc, s7_car(args), (s7_is_pair(s7_cdr(args))) ? s7_cadr(args) : s7_current_output_port(sc));
}

s7_pointer g_display_2(s7_scheme *sc, s7_pointer args)
{
  return display_p_pp(sc, s7_car(args), s7_cadr(args));
}

s7_pointer g_display_f(s7_scheme *sc, s7_pointer args)
{
  (void)sc;
  return s7_car(args);
}

s7_pointer display_p_p(s7_scheme *sc, s7_pointer x)
{
  s7_pointer port = s7_current_output_port(sc);
  if (port == s7_f(sc)) return x;
  IF_METHOD_EXISTS_RETURN_VALUE(sc, x, "display", s7_cons(sc, x, s7_nil(sc)));
  return s7i_object_out(sc, x, port, S7I_P_DISPLAY);
}


/* -------------------------------- write-char -------------------------------- */

s7_pointer s7_write_char(s7_scheme *sc, s7_pointer c, s7_pointer port)
{
  if (port != s7_f(sc))
    s7i_port_write_unicode_char(sc, s7_character(c), port);
  return c;
}

s7_pointer write_char_p_pp(s7_scheme *sc, s7_pointer c, s7_pointer port)
{
  if (!s7_is_character(c))
    return s7i_method_or_bust_pp(sc, c, "write-char", c, port, "a character", 1);
  if (!s7_is_output_port(sc, port))
    {
      if (port == s7_f(sc)) return c;
      IF_METHOD_EXISTS_RETURN_VALUE(sc, port, "write-char", s7_cons(sc, c, s7_cons(sc, port, s7_nil(sc))));
      return s7_wrong_type_arg_error(sc, "write-char", 2, port, "an output port or #f");
    }
  s7i_port_write_unicode_char(sc, s7_character(c), port);
  return c;
}

s7_pointer write_char_p_p(s7_scheme *sc, s7_pointer c)
{
  if (!s7_is_character(c))
    return s7i_method_or_bust_p(sc, c, "write-char", "a character");
  s7_pointer port = s7_current_output_port(sc);
  if (port == s7_f(sc)) return c;
  s7i_port_write_unicode_char(sc, s7_character(c), port);
  return c;
}

s7_pointer g_write_char(s7_scheme *sc, s7_pointer args)
{
  #define H_write_char "(write-char char (port (current-output-port))) writes char to the output port"
  #define Q_write_char s7_make_signature(sc, 3, sc->is_char_symbol, sc->is_char_symbol, s7_make_signature(sc, 2, sc->is_output_port_symbol, sc->not_symbol))
  if (s7_is_null(sc, s7_cdr(args)))
    return write_char_p_p(sc, s7_car(args));
  return write_char_p_pp(sc, s7_car(args), (s7_is_pair(s7_cdr(args))) ? s7_cadr(args) : s7_current_output_port(sc));
}


/* -------------------------------- write-string -------------------------------- */

s7_pointer g_write_string(s7_scheme *sc, s7_pointer args)
{
  #define H_write_string "(write-string str port start end) writes str to port."
  #define Q_write_string s7_make_circular_signature(sc, 3, 4, \
                           sc->is_string_symbol, sc->is_string_symbol, \
                           s7_make_signature(sc, 2, sc->is_output_port_symbol, sc->not_symbol),\
                           sc->is_integer_symbol)
  const s7_pointer str = s7_car(args);
  s7_pointer port;
  s7_int start = 0, end;
  if (!s7_is_string(str))
    return s7i_method_or_bust(sc, str, "write-string", args, "a string", 1);
  end = s7_string_length(str);
  if (!s7_is_null(sc, s7_cdr(args)))
    {
      s7_pointer inds = s7_cddr(args);
      port = s7_cadr(args);
      if (!s7_is_null(sc, inds))
        {
          s7_pointer p = s7i_start_and_end(sc, s7_make_symbol(sc, "write-string"), args, 3, inds, &start, &end);
          if (!s7i_is_unused(sc, p)) return p;
        }}
  else port = s7_current_output_port(sc);
  if (!s7_is_output_port(sc, port))
    {
      if (port == s7_f(sc))
        {
          s7_int len;
          if ((start == 0) && (end == s7_string_length(str)))
            return str;
          len = (s7_int)(end - start);
          return s7_make_string_with_length(sc, (const char *)(s7_string(str) + start), len);
        }
      IF_METHOD_EXISTS_RETURN_VALUE(sc, port, "write-string", args);
      return s7_wrong_type_arg_error(sc, "write-string", 2, port, "an output port or #f");
    }
  if (s7i_port_is_closed(port))
    return s7_wrong_type_arg_error(sc, "write-string", 2, port, "an open output port");
  if (start == end) return str;
  s7i_port_write_string(sc, (const char *)(s7_string(str) + start), (end - start), port);
  return str;
}

s7_pointer write_string_p_pp(s7_scheme *sc, s7_pointer str, s7_pointer port)
{
  if (!s7_is_string(str))
    return s7i_method_or_bust_pp(sc, str, "write-string", str, port, "a string", 1);
  if (!s7_is_output_port(sc, port))
    {
      if (port == s7_f(sc)) return str;
      return s7i_method_or_bust_pp(sc, port, "write-string", str, port, "an output port", 2);
    }
  if (s7_string_length(str) > 0)
    s7i_port_write_string(sc, s7_string(str), s7_string_length(str), port);
  return str;
}


/* -------------------------------- write-byte -------------------------------- */

s7_pointer g_write_byte(s7_scheme *sc, s7_pointer args)
{
  #define H_write_byte "(write-byte byte (port (current-output-port))): writes byte to the output port"
  #define Q_write_byte s7_make_signature(sc, 3, sc->is_byte_symbol, sc->is_byte_symbol, s7_make_signature(sc, 2, sc->is_output_port_symbol, sc->not_symbol))

  s7_pointer port;
  const s7_pointer b = s7_car(args);
  s7_int val;
  if (!s7_is_integer(b))
    return s7i_method_or_bust(sc, b, "write-byte", args, "an integer", 1);

  val = s7_integer(b);
  if ((val < 0) || (val > 255))
    return s7_wrong_type_arg_error(sc, "write-byte", 1, b, "an unsigned byte");

  port = (s7_is_pair(s7_cdr(args))) ? s7_cadr(args) : s7_current_output_port(sc);
  if (!s7_is_output_port(sc, port))
    {
      if (port == s7_f(sc)) return b;
      IF_METHOD_EXISTS_RETURN_VALUE(sc, port, "write-byte", args);
      return s7_wrong_type_arg_error(sc, "write-byte", 2, port, "an output port or #f");
    }
  if (s7i_port_is_closed(port))
    return s7_wrong_type_arg_error(sc, "write-byte", 2, port, "an open output port");

  s7i_port_write_character(sc, (uint8_t)val, port);
  return b;
}


/* -------------------------------- current-input-port -------------------------------- */

s7_pointer g_current_input_port(s7_scheme *sc, s7_pointer unused_args)
{
  #define H_current_input_port "(current-input-port) returns the current input port"
  #define Q_current_input_port s7_make_signature(sc, 1, sc->is_input_port_symbol)
  (void)unused_args;
  return s7_current_input_port(sc);
}


/* -------------------------------- current-output-port -------------------------------- */

s7_pointer g_current_output_port(s7_scheme *sc, s7_pointer unused_args)
{
  #define H_current_output_port "(current-output-port) returns the current output port"
  #define Q_current_output_port s7_make_signature(sc, 1, s7_make_signature(sc, 2, sc->is_output_port_symbol, sc->not_symbol))
  (void)unused_args;
  return s7_current_output_port(sc);
}


/* -------------------------------- current-error-port -------------------------------- */

s7_pointer g_current_error_port(s7_scheme *sc, s7_pointer unused_args)
{
  #define H_current_error_port "(current-error-port) returns the current error port"
  #define Q_current_error_port s7_make_signature(sc, 1, s7_make_signature(sc, 2, sc->is_output_port_symbol, sc->not_symbol))
  (void)unused_args;
  return s7_current_error_port(sc);
}


/* -------------------------------- open-output-string -------------------------------- */

s7_pointer g_open_output_string(s7_scheme *sc, s7_pointer unused_args)
{
  #define H_open_output_string "(open-output-string) opens an output string port"
  #define Q_open_output_string s7_make_signature(sc, 1, sc->is_output_port_symbol)
  (void)unused_args;
  return s7_open_output_string(sc);
}
