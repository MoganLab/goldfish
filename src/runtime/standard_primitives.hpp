#pragma once

#include "runtime/evaluator.hpp"

namespace goldfish::runtime {

// Runtime substrate: object operations, ports/reader, environments, numeric
// atoms, platform handles, and the Unicode runtime boundary. Derived list/HOF
// behavior belongs to Scheme; bootstrap primitives are installed separately.
void install_runtime_primitives(Evaluator& evaluator);
void install_random_primitives(Evaluator& evaluator);

} // namespace goldfish::runtime
