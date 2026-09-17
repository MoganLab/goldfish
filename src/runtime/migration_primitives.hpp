#pragma once

#include "runtime/evaluator.hpp"

namespace goldfish::runtime {

// The one migration boundary for s7-shaped bootstrap artifacts.  This module
// is intentionally separate from the runtime layer so R4 can remove it as a
// unit after the last compatibility artifact is gone.
void install_migration_primitives(Evaluator& evaluator);

} // namespace goldfish::runtime
