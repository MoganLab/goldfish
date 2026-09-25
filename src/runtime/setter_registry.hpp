#pragma once

#include "runtime/value.hpp"

#include <unordered_map>

namespace goldfish::runtime {

// Procedures with a two-argument setter, keyed by the procedure object.
// The expander lowers `(set! (proc args...) val)' to
// `((setter proc) args... val)'; the `setter' primitive resolves through
// this registry, so a procedure registered here receives the write form.
// Unregistered procedures keep the historical no-op compatibility setter.
inline std::unordered_map<const Object*, Value>& setter_registry() {
    static std::unordered_map<const Object*, Value> registry;
    return registry;
}

} // namespace goldfish::runtime
