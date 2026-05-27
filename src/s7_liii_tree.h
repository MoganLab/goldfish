/* s7_liii_tree.h - tree utility declarations for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 *
 * Bill Schottstaedt, bil@ccrma.stanford.edu
 */

#ifndef S7_LIII_TREE_H
#define S7_LIII_TREE_H

#include "s7.h"

#ifdef __cplusplus
extern "C" {
#endif

s7_pointer g_tree_is_cyclic(s7_scheme *sc, s7_pointer args);
s7_pointer g_tree_leaves(s7_scheme *sc, s7_pointer args);
s7_pointer g_tree_memq(s7_scheme *sc, s7_pointer args);
s7_pointer g_tree_set_memq(s7_scheme *sc, s7_pointer args);
s7_pointer g_tree_set_memq_syms(s7_scheme *sc, s7_pointer args);
s7_pointer g_tree_count(s7_scheme *sc, s7_pointer args);

#ifdef __cplusplus
}
#endif

#endif /* S7_LIII_TREE_H */
