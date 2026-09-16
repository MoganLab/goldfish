#pragma once

#include "runtime/runtime.hpp"
#include "gf.h"

namespace goldfish::runtime {

// Explicit adapter between the current s7 host and the independent runtime.
// The evaluator and its values do not depend on this header.
class S7Bridge final {
public:
    explicit S7Bridge(Runtime& runtime) : runtime_(runtime) {}

    Value from_s7(gf::scheme* scheme, gf::pointer value) const;
    gf::pointer to_s7(gf::scheme* scheme, Value value) const;

    Values eval_s7(gf::scheme* scheme, gf::pointer expression) const;
    gf::pointer values_to_s7(gf::scheme* scheme, const Values& values) const;

private:
    Runtime& runtime_;
};

} // namespace goldfish::runtime
