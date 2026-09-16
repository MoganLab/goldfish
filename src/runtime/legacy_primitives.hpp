#pragma once

#include "runtime/evaluator.hpp"

namespace goldfish::runtime {

// Migration-only support for the existing s7-shaped kernel artifact.  This
// layer is deliberately separate from the normal runtime primitive set.
void install_legacy_primitives(Evaluator& evaluator);

} // namespace goldfish::runtime
