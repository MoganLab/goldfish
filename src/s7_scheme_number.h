/* s7_scheme_number.h - number arithmetic interface for s7 Scheme interpreter
 *
 * derived from s7, a Scheme interpreter
 * SPDX-License-Identifier: 0BSD
 */

#ifndef S7_SCHEME_NUMBER_H
#define S7_SCHEME_NUMBER_H

#include "s7.h"

/* -------------------------------- function declarations -------------------------------- */

/* bridge functions and arithmetic helpers are declared in s7_internal_helpers.h */

#ifndef S7_SCHEME_NUMBER_C_BODY
#ifdef S7_SCHEME_NUMBER_IMPLEMENTATION
#include "s7_scheme_number.c"
#endif
#endif

#endif /* S7_SCHEME_NUMBER_H */
