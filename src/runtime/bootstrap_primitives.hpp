#pragma once

#include "runtime/evaluator.hpp"

namespace goldfish::runtime {

// Temporary substrate needed while the kernel and library layer bootstrap.
// These names are expected to be replaced by Scheme definitions afterwards.
void install_bootstrap_primitives(Evaluator& evaluator);

} // namespace goldfish::runtime
