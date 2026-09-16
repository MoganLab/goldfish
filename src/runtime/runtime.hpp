#pragma once

#include "runtime/primitive.hpp"

namespace goldfish::runtime {

// Public owner of the runtime components. Callers should use this facade
// instead of constructing Heap, Evaluator, and the primitive registry in an
// ad-hoc order.
class Runtime final {
public:
    Runtime() : heap_(), evaluator_(heap_) {}

    Runtime(const Runtime&) = delete;
    Runtime& operator=(const Runtime&) = delete;

    Evaluator& evaluator() noexcept { return evaluator_; }
    const Evaluator& evaluator() const noexcept { return evaluator_; }
    Heap& heap() noexcept { return heap_; }

    void register_primitive(const std::string& name,
                            PrimitiveObject::Function function) {
        primitives_.register_primitive(name, std::move(function));
    }

    void install_primitives() { primitives_.install(evaluator_); }

private:
    Heap heap_;
    Evaluator evaluator_;
    PrimitiveRegistry primitives_;
};

} // namespace goldfish::runtime
