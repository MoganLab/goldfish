#pragma once

#include "runtime/evaluator.hpp"

namespace goldfish::runtime {

// Low-level object, reader, environment, and numeric substrate.  Derived
// list/HOF behavior lives in bootstrap_primitives or Scheme libraries.
void install_standard_primitives(Evaluator& evaluator);

} // namespace goldfish::runtime
