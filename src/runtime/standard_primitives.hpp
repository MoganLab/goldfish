#pragma once

#include "runtime/evaluator.hpp"

namespace goldfish::runtime {

// Runtime substrate: object operations, ports/reader, environments, numeric
// atoms, platform handles, and the Unicode runtime boundary.  Derived
// list/HOF behavior belongs to Scheme; migration-only procedures are exposed
// separately by install_migration_primitives.
void install_runtime_primitives(Evaluator& evaluator);

} // namespace goldfish::runtime
