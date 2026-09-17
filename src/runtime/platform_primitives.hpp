#pragma once

#include "runtime/evaluator.hpp"

namespace goldfish::runtime {

// OS, filesystem, environment, clock, and digest adapters.  These primitives
// expose capabilities; policy and cache orchestration remain in Scheme.
void install_platform_primitives(Evaluator& evaluator);

} // namespace goldfish::runtime
