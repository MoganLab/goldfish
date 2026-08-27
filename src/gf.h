//
// gf.h -- Goldfish Foundation interface.
//
// The single C++ API surface between the goldfish runtime (VM, C++
// extension libraries, CLI) and the host Scheme runtime it is built on.
// Today the backend is s7 (src/gf.cpp calls through to s7); replacing s7
// later means swapping src/gf.cpp (and this header's types) without
// touching any caller.  Callers must include gf.h, never s7.h directly.
//
// The interface is intentionally thin and C++17-idiomatic: everything
// lives in goldfish::gf, values are held as gf::pointer (opaque), and a
// backend migration only rewrites the function bodies in gf.cpp.
//

#ifndef GOLDFISH_GF_H
#define GOLDFISH_GF_H

#include <cstdint>
#include <cstddef>

struct s7_scheme;
struct s7_cell;
using s7_pointer = s7_cell*;
using s7_int = int64_t;
using s7_double = double;
using s7_function = s7_pointer (*)(s7_scheme*, s7_pointer);

namespace goldfish {
namespace gf {

// ---------------------------------------------------------------------------
// Types (aliases so callers do not spell s7 type names).
// ---------------------------------------------------------------------------

using scheme = s7_scheme;
using pointer = s7_pointer;
using int_ = s7_int;
using double_ = s7_double;
using function = s7_function;

// Single-source forwarder table (LAYER.md L0 count/version).
#define GF_FWD(ret, name, args, call) ret name args;
#define GF_FWD_VOID(name, args, call) void name args;
#include "gf_forwards.def"
#undef GF_FWD
#undef GF_FWD_VOID

// Build a proper list from its elements (variadic; the element count is
// inferred from the argument pack).
template <typename... Args>
inline pointer list (scheme* sc, Args... args) {
  pointer elems[] = {args...};
  return array_to_list(sc, sizeof...(args), elems);
}

} // namespace gf
} // namespace goldfish

#endif // GOLDFISH_GF_H
