#pragma once

#include "runtime/evaluator.hpp"

namespace goldfish::runtime {

// Unicode properties and case conversion are the runtime boundary for the
// Scheme character library.  The character API itself remains in Scheme.
void install_unicode_primitives(Evaluator& evaluator);

} // namespace goldfish::runtime
