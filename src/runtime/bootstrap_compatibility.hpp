#pragma once

#include "runtime/evaluator.hpp"

namespace goldfish::runtime {

// Compatibility needed by the current lowered bootstrap artifacts.  This is
// intentionally separate from the full s7-shaped migration surface.
void install_native_bootstrap_compatibility(Evaluator& evaluator);

} // namespace goldfish::runtime
