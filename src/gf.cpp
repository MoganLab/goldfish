//
// gf.cpp -- Goldfish Foundation backend (currently s7).
//
// Every goldfish::gf::* function is a thin call through to the host Scheme
// runtime.  Replacing s7 later means rewriting this file's bodies against
// the new runtime; nothing outside goldfish::gf changes.
//

#include "gf.h"
#include "s7.h"

namespace goldfish {
namespace gf {

#define GF_FWD(ret, name, args, call) ret name args { return call; }
#define GF_FWD_VOID(name, args, call) void name args { call; }
#include "gf_forwards.def"
#undef GF_FWD
#undef GF_FWD_VOID

} // namespace gf
} // namespace goldfish
