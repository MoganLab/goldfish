#pragma once

#include "runtime/evaluator.hpp"

#include <map>
#include <stdexcept>
#include <string>

namespace goldfish::runtime {

// The registry is the only boundary between host primitives and the
// evaluator. It stores behavior by name, but does not know about platform
// details or Scheme library policy.
class PrimitiveRegistry final {
public:
    void register_primitive(const std::string& name,
                            PrimitiveObject::Function function) {
        auto inserted = functions_.emplace(name, std::move(function));
        if (!inserted.second)
            throw std::runtime_error("duplicate primitive: " + name);
    }

    void install(Evaluator& evaluator) const {
        for (const auto& entry : functions_)
            evaluator.define_primitive(entry.first, entry.second);
    }

private:
    std::map<std::string, PrimitiveObject::Function> functions_;
};

} // namespace goldfish::runtime
