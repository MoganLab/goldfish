#pragma once

#include "runtime/heap.hpp"
#include "runtime/symbol.hpp"

#include <cstdio>
#include <cstdlib>
#include <memory>
#include <stdexcept>
#include <unordered_map>
#include <vector>

namespace goldfish::runtime {

// GOLDFISH_TRACE_THROW=1 logs each thrown runtime_error: the runtime should
// throw rarely (exceptions are not control flow here), so a non-empty
// census means a hot path needs restructuring.
inline void trace_throw(const char* what) {
    if (std::getenv("GOLDFISH_TRACE_THROW"))
        std::fprintf(stderr, "THROW: %s\n", what);
}

// A set! whose target has no binding anywhere in the chain.  Derived
// from std::runtime_error so existing catch sites keep working; the
// set! core form converts it to a keyed 'unbound-variable raise so
// (catch 'unbound-variable ...) sees the host's error type.
class UnboundSetError : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

// A reference to a name with no binding in the chain.  eval converts
// it to a keyed 'unbound-variable raise (host parity); probes that
// catch std::runtime_error (defined?, rootlet fallback) keep working.
class UnboundSymbolError : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

class Environment final {
public:
    explicit Environment(std::shared_ptr<Environment> parent = nullptr)
        : parent_(std::move(parent)) {}

    void define(Value name, Value value) {
        bindings_[symbol_key(name)] = value;
    }

    void set(Value name, Value value) {
        const Object* key = symbol_key(name);
        auto it = bindings_.find(key);
        if (it != bindings_.end()) {
            it->second = value;
            return;
        }
        if (parent_) {
            parent_->set(name, value);
            return;
        }
        trace_throw("set-unbound");
        throw UnboundSetError("set! of unbound symbol: " +
                              symbol_name(name));
    }

    Value lookup(Value name) const {
        const Object* key = symbol_key(name);
        auto it = bindings_.find(key);
        if (it != bindings_.end()) {
            if (it->second.is_object() &&
                it->second.as_object()->type() == ObjectType::Uninitialized)
                throw std::runtime_error("read of uninitialized symbol");
            return it->second;
        }
        if (parent_)
            return parent_->lookup(name);
        trace_throw("lookup-unbound");
        throw UnboundSymbolError("unbound symbol: " + symbol_name(name));
    }

    void trace(Tracer& tracer) const {
        for (const auto& binding : bindings_)
            tracer.mark(binding.second);
        if (parent_)
            parent_->trace(tracer);
    }

    std::vector<std::pair<Value, Value>> entries() const {
        std::vector<std::pair<Value, Value>> result;
        for (const auto& binding : bindings_)
            result.emplace_back(Value::object(const_cast<Object*>(binding.first)),
                                binding.second);
        return result;
    }

private:
    static const Object* symbol_key(Value name) {
        if (!name.is_object() ||
            name.as_object()->type() != ObjectType::Symbol)
            throw std::runtime_error("environment key is not a symbol");
        return name.as_object();
    }

    static std::string symbol_name(Value name) {
        return name.as_object<SymbolObject>()->name;
    }

    std::shared_ptr<Environment> parent_;
    std::unordered_map<const Object*, Value> bindings_;
};

using EnvironmentPtr = std::shared_ptr<Environment>;

} // namespace goldfish::runtime
