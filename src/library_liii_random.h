/* library_liii_random.h - random and random-state declarations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#ifndef LIBRARY_LIII_RANDOM_H
#define LIBRARY_LIII_RANDOM_H

#include "s7.h"

#ifdef __cplusplus
extern "C" {
#endif

#define H_random_state                                                                                                 \
  "(random-state seed (carry plausible-default)) returns a new random number state initialized with 'seed'. \
Pass this as the second argument to 'random' to get a repeatable random number sequence:\n\
    (let ((seed (random-state 1234))) (random 1.0 seed))"
#define Q_random_state s7_make_circular_signature (sc, 1, 2, sc->is_random_state_symbol, sc->is_integer_symbol)

#define H_is_random_state "(random-state? obj) returns #t if obj is a random-state object (from random-state)."
#define Q_is_random_state sc->pl_bt

#define H_random_state_to_list                                                                                         \
  "(random-state->list r) returns the random state object as a list.\
You can later apply random-state to this list to continue a random number sequence from any point."
#define Q_random_state_to_list s7_make_signature (sc, 2, sc->is_pair_symbol, sc->is_random_state_symbol)

#define H_random                                                                                                       \
  "(random num state) returns a random number of the same type as num between zero and num, equalling num only if "    \
  "num is zero"
#define Q_random s7_make_signature (sc, 3, sc->is_number_symbol, sc->is_number_symbol, sc->is_random_state_symbol)

#define g_random_state s7_random_state
#define g_random_state_to_list s7_random_state_to_list

double s7_next_random (s7_pointer r);

s7_pointer random_state_copy (s7_scheme* sc, s7_pointer args);
s7_pointer random_state_getter (s7_scheme* sc, s7_pointer r, s7_int loc);
s7_pointer random_state_setter (s7_scheme* sc, s7_pointer r, s7_int loc, s7_pointer val);
s7_pointer random_state_to_let (s7_scheme* sc, s7_pointer rs);

struct shared_info;
bool random_state_equal (s7_scheme* sc, s7_pointer x, s7_pointer y, struct shared_info* ci);

s7_double random_d_7d (s7_scheme* sc, s7_double x);
s7_int    random_i_7i (s7_scheme* sc, s7_int i);

s7_pointer g_random (s7_scheme* sc, s7_pointer args);
s7_pointer g_random_i (s7_scheme* sc, s7_pointer args);
s7_pointer g_random_f (s7_scheme* sc, s7_pointer args);
s7_pointer g_random_1 (s7_scheme* sc, s7_pointer args);

s7_pointer random_p_p (s7_scheme* sc, s7_pointer num);
s7_pointer random_p_p_wrapped (s7_scheme* sc, s7_pointer num);
s7_pointer random_chooser (s7_scheme* sc, s7_pointer func, int32_t args, s7_pointer expr);
s7_pointer g_add_i_random (s7_scheme* sc, s7_pointer args);

#ifdef __cplusplus
}
#endif

#endif /* LIBRARY_LIII_RANDOM_H */
