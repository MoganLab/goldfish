/* s7_internal_helpers.h - internal helper bridge declarations
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 */

#ifndef S7_INTERNAL_HELPERS_H
#define S7_INTERNAL_HELPERS_H

#include "s7.h"

#ifdef __cplusplus
extern "C" {
#endif

s7_pointer s7i_method_or_bust(s7_scheme *sc, s7_pointer obj, const char *method_name,
                              s7_pointer args, const char *type_name, s7_int arg_pos);

bool s7i_method_or_bust_bool(s7_scheme *sc, s7_pointer obj, const char *method_name,
                             s7_pointer args, const char *type_name, s7_int arg_pos);

s7_pointer s7i_sole_arg_method_or_bust(s7_scheme *sc, s7_pointer obj, const char *method_name, s7_pointer args, const char *type_name);

bool s7i_sole_arg_method_or_bust_bool(s7_scheme *sc, s7_pointer obj, const char *method_name, s7_pointer args, const char *type_name);

bool s7i_is_sequence(s7_pointer p);
bool s7i_sequence_is_empty(s7_scheme *sc, s7_pointer seq);
s7_int s7i_sequence_length(s7_scheme *sc, s7_pointer seq);
s7_pointer s7i_find_method_with_let(s7_scheme *sc, s7_pointer obj, s7_pointer method);
bool s7i_has_active_methods(s7_scheme *sc, s7_pointer obj);
/* boolean method dispatch for type predicate migration */
s7_pointer s7i_apply_boolean_method(s7_scheme *sc, s7_pointer obj, s7_pointer method);
void s7i_wrong_type_error_nr(s7_scheme *sc, s7_pointer caller, s7_int arg_num, s7_pointer arg, s7_pointer typ);
s7_pointer s7i_copy_1(s7_scheme *sc, s7_pointer caller, s7_pointer args);
s7_pointer s7i_copy_proper_list(s7_scheme *sc, s7_pointer lst);
s7_int s7i_position_of(const s7_pointer p, s7_pointer args);
s7_pointer s7i_nil_string(void);
s7_pointer s7i_make_empty_string(s7_scheme *sc, s7_int len, char fill);
s7_int s7i_max_string_length(s7_scheme *sc);
s7_int s7i_max_list_length(s7_scheme *sc);

s7_pointer s7i_string_append_1(s7_scheme *sc, s7_pointer args, s7_pointer caller);
s7_pointer s7i_string_1(s7_scheme *sc, s7_pointer args, s7_pointer sym);
s7_pointer s7i_string_c1(s7_scheme *sc, s7_pointer args);
s7_pointer s7i_string_to_number(s7_scheme *sc, char *str, int32_t radix);
s7_pointer make_atom(s7_scheme *sc, char *q, int32_t radix, bool want_symbol, bool with_error);

/* pre-allocated list helpers for string-append migration */
s7_pointer s7i_set_plist_2(s7_scheme *sc, s7_pointer x1, s7_pointer x2);
s7_pointer s7i_set_ulist_1(s7_scheme *sc, s7_pointer x1, s7_pointer x2);
void s7i_string_append_length_error(s7_scheme *sc, s7_pointer caller, s7_int len);
bool s7i_is_string_append_or_symbol_caller(s7_scheme *sc, s7_pointer caller);
void s7i_set_string_value(s7_pointer str, const char *val);
char *s7i_string_value_ptr(s7_pointer str);

/* string comparison helpers for string cmp migration */
int32_t s7i_scheme_strcmp(s7_pointer s1, s7_pointer s2);
bool s7i_scheme_strings_are_equal(s7_pointer x, s7_pointer y);
bool s7i_is_string_via_method(s7_scheme *sc, s7_pointer obj);
s7_pointer s7i_method_or_bust_sym(s7_scheme *sc, s7_pointer obj, s7_pointer method_sym, s7_pointer args, s7_pointer typ, s7_int arg_pos);
s7_pointer s7i_set_plist_1(s7_scheme *sc, s7_pointer x1);
s7_pointer s7i_string_type_name(s7_scheme *sc);
s7_pointer s7i_string_eq_symbol(s7_scheme *sc);
s7_pointer s7i_string_lt_symbol(s7_scheme *sc);
s7_pointer s7i_string_gt_symbol(s7_scheme *sc);
s7_pointer s7i_string_leq_symbol(s7_scheme *sc);
s7_pointer s7i_string_geq_symbol(s7_scheme *sc);
bool s7i_is_true(s7_scheme *sc, s7_pointer p);
s7_pointer s7i_is_string_symbol(s7_scheme *sc);
s7_pointer s7i_is_boolean_symbol(s7_scheme *sc);
s7_pointer s7i_is_unspecified_symbol(s7_scheme *sc);
s7_pointer s7i_is_number_symbol(s7_scheme *sc);
s7_pointer s7i_is_integer_symbol(s7_scheme *sc);
s7_pointer s7i_is_real_symbol(s7_scheme *sc);
s7_pointer s7i_is_complex_symbol(s7_scheme *sc);
s7_pointer s7i_is_rational_symbol(s7_scheme *sc);
s7_pointer s7i_is_keyword_symbol(s7_scheme *sc);
s7_pointer s7i_is_dilambda_symbol(s7_scheme *sc);
s7_pointer s7i_is_sequence_symbol(s7_scheme *sc);
s7_pointer s7i_is_symbol_symbol(s7_scheme *sc);
s7_pointer s7i_is_input_port_symbol(s7_scheme *sc);
s7_pointer s7i_is_output_port_symbol(s7_scheme *sc);
s7_pointer s7i_is_macro_symbol(s7_scheme *sc);
s7_pointer s7i_is_undefined_symbol(s7_scheme *sc);
s7_pointer s7i_is_eof_object_symbol(s7_scheme *sc);
s7_pointer s7i_is_byte_symbol(s7_scheme *sc);
s7_pointer s7i_is_float_symbol(s7_scheme *sc);
s7_pointer s7i_is_random_state_symbol(s7_scheme *sc);
s7_pointer s7i_is_continuation_symbol(s7_scheme *sc);
s7_pointer s7i_is_iterator_symbol(s7_scheme *sc);
s7_pointer s7i_is_gensym_symbol(s7_scheme *sc);
s7_pointer s7i_is_syntax_symbol(s7_scheme *sc);
s7_pointer s7i_is_let_symbol(s7_scheme *sc);
bool s7i_is_goto(s7_pointer p);
bool s7i_is_constant(s7_scheme *sc, s7_pointer p);
s7_pointer s7i_is_c_object_symbol(s7_scheme *sc);
s7_pointer s7i_help_symbol(s7_scheme *sc);
bool s7i_is_undefined(s7_pointer p);
bool s7i_is_eof(s7_pointer p);
bool s7i_is_t_real(s7_pointer p);
bool s7i_is_continuation(s7_pointer p);
const uint8_t *s7i_uppers_ptr(void);

/* bridge functions for s7_scheme_predicate.c migration */
s7_pointer s7i_c_pointer_type(s7_pointer p);
bool s7i_has_methods(s7_pointer p);
bool s7i_is_funclet(s7_pointer p);
bool s7i_is_maclet(s7_pointer p);
s7_pointer s7i_rootlet(s7_scheme *sc);
s7_pointer s7i_is_c_pointer_symbol(s7_scheme *sc);
s7_pointer s7i_is_openlet_symbol(s7_scheme *sc);
s7_pointer s7i_is_funclet_symbol(s7_scheme *sc);

/* bridge functions for g_tree_is_cyclic and g_type_of migration */
bool s7i_tree_is_cyclic(s7_scheme *sc, s7_pointer p);
s7_pointer s7i_type_of(s7_scheme *sc, s7_pointer p);

/* write-related helpers */
typedef enum {S7I_P_DISPLAY, S7I_P_WRITE, S7I_P_READABLE, S7I_P_KEY, S7I_P_CODE} s7i_use_write_t;

bool s7i_port_is_closed(s7_pointer p);
s7_pointer s7i_object_out(s7_scheme *sc, s7_pointer obj, s7_pointer port, s7i_use_write_t choice);
void s7i_port_write_character(s7_scheme *sc, uint8_t c, s7_pointer port);
void s7i_port_write_string(s7_scheme *sc, const char *str, s7_int len, s7_pointer port);
void s7i_port_write_unicode_char(s7_scheme *sc, uint32_t c, s7_pointer port);
s7_pointer s7i_start_and_end(s7_scheme *sc, s7_pointer caller, s7_pointer args, int32_t position, s7_pointer index_args, s7_int *start, s7_int *end);
bool s7i_is_unused(s7_scheme *sc, s7_pointer p);
s7_pointer s7i_method_or_bust_p(s7_scheme *sc, s7_pointer obj, const char *method_name, const char *type_name);
s7_pointer s7i_method_or_bust_pp(s7_scheme *sc, s7_pointer obj, const char *method_name, s7_pointer x1, s7_pointer x2, const char *type_name, s7_int arg_pos);

void s7i_division_by_zero_error(s7_scheme *sc, const char *caller, s7_pointer x, s7_pointer y);

/* max/min core comparison functions (use internal macros, must stay in s7.c) */
s7_pointer max_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y);
s7_pointer min_p_pp(s7_scheme *sc, s7_pointer x, s7_pointer y);

bool s7i_is_subvector(s7_pointer p);
s7_int s7i_subvector_position(s7_pointer p);
s7_pointer s7i_subvector_vector(s7_scheme *sc, s7_pointer p);
bool s7i_is_typed_t_vector(s7_pointer p);
s7_pointer s7i_typed_vector_typer(s7_scheme *sc, s7_pointer p);

s7_pointer s7i_vector_ref_1(s7_scheme *sc, s7_pointer vect, s7_pointer indices);
s7_pointer s7i_vector_ref_p_pp(s7_scheme *sc, s7_pointer vec, s7_pointer ind);

s7_pointer s7i_make_vector_1(s7_scheme *sc, s7_pointer args, s7_pointer caller);
s7_pointer s7i_make_simple_complex_vector(s7_scheme *sc, s7_int len);
s7_complex s7i_to_c_complex(s7_pointer z);
s7_pointer s7i_vector_fill_1(s7_scheme *sc, s7_pointer caller, s7_pointer args);
s7_pointer s7i_vector_append(s7_scheme *sc, s7_pointer args, uint8_t typ, s7_pointer caller);

/* module system helpers */
bool s7i_is_closure(s7_pointer p);
bool s7i_is_closure_star(s7_pointer p);
s7_pointer s7i_missing_key_value(s7_scheme *sc);
const char *s7i_find_autoload_name(s7_scheme *sc, s7_pointer symbol, bool *already_loaded, bool loading);

/* hash-table helpers */
s7_int s7i_hash_table_entries(s7_pointer table);
s7_pointer s7i_hash_table_key_typer(s7_scheme *sc, s7_pointer table);
s7_pointer s7i_hash_table_value_typer(s7_scheme *sc, s7_pointer table);

s7_pointer s7i_ref_index_checked(s7_scheme *sc, s7_pointer caller, s7_pointer in_obj, s7_pointer args);
s7_pointer s7i_hash_table_1(s7_scheme *sc, s7_pointer args, s7_pointer caller);
s7_pointer s7i_make_hash_table_1(s7_scheme *sc, s7_pointer args, s7_pointer caller);
s7_pointer s7i_hash_table_add(s7_scheme *sc, s7_pointer table, s7_pointer key, s7_pointer value);
bool s7i_is_weak_hash_table(s7_pointer p);
void s7i_set_weak_hash_table(s7_pointer p);
void s7i_set_weak_hash_table_iters(s7_pointer p, s7_int val);

s7_double s7i_default_rationalize_error(s7_scheme *sc);

/* symbol helpers */
bool s7i_is_gensym(s7_pointer p);
s7_pointer s7i_symbol_name_cell(s7_pointer sym);
s7_int s7i_symbol_name_length(s7_pointer sym);
s7_pointer s7i_make_symbol_with_length(s7_scheme *sc, const char *name, s7_int len);
s7_pointer s7i_initial_value(s7_pointer symbol);
void s7i_set_initial_value(s7_pointer symbol, s7_pointer value);
bool s7i_initial_value_is_defined(s7_scheme *sc, s7_pointer symbol);

#ifdef __cplusplus
}
#endif

#endif /* S7_INTERNAL_HELPERS_H */
