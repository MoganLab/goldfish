#pragma once

#include "runtime/debug_flags.hpp"
#include "runtime/heap.hpp"
#include "runtime/ref_ptr.hpp"
#include "runtime/symbol.hpp"

#include <cstdio>
#include <cstdlib>
#include <stdexcept>
#include <unordered_map>
#include <vector>

namespace goldfish::runtime {

// GOLDFISH_TRACE_THROW=1 logs each thrown runtime_error: the runtime should
// throw rarely (exceptions are not control flow here), so a non-empty
// census means a hot path needs restructuring.
inline void trace_throw(const char* what) {
    if (debug_enabled("throw", "GOLDFISH_TRACE_THROW"))
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
// it to a keyed 'unbound-variable raise; probes that catch
// std::runtime_error (such as defined?) keep working.
class UnboundSymbolError : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

class Environment final : public RefCounted<Environment> {
public:
    explicit Environment(RefPtr<Environment> parent = nullptr)
        : parent_(std::move(parent)) {}

    void define(Value name, Value value) {
        bindings_[symbol_key(name)].get() = value;
    }

    // Promote only shared bindings to cells; ordinary lexical bindings stay local.
    void link(Value name, Environment& source, Value source_name) {
        for (Environment* env = &source; env; env = env->parent_.get()) {
            auto found = env->bindings_.find(symbol_key(source_name));
            if (found == env->bindings_.end()) continue;
            Binding& binding = found->second;
            if (!binding.location) {
                binding.location = std::make_shared<Value>(binding.value);
                binding.value = Value::unspecified();
            }
            auto location = binding.location;
            Binding& target = bindings_[symbol_key(name)];
            target.location = std::move(location);
            target.value = Value::unspecified();
            return;
        }
        throw UnboundSymbolError("cannot link unbound symbol: " + symbol_name(source_name));
    }

    void set(Value name, Value value) {
        const Object* key = symbol_key(name);
        auto it = bindings_.find(key);
        if (it != bindings_.end()) {
            it->second.get() = value;
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
        for (const Environment* environment = this; environment != nullptr;
             environment = environment->parent_.get()) {
            auto it = environment->bindings_.find(key);
            if (it != environment->bindings_.end()) {
                Value value = it->second.get();
                if (value.is_object() &&
                    value.as_object()->type() == ObjectType::Uninitialized)
                    throw std::runtime_error(
                        "expected initialized binding: read of uninitialized symbol");
                return value;
            }
        }
        trace_throw("lookup-unbound");
        throw UnboundSymbolError("unbound symbol: " + symbol_name(name));
    }

    void trace(Tracer& tracer) const {
        for (const auto& binding : bindings_)
            tracer.mark(binding.second.get());
        if (parent_)
            parent_->trace(tracer);
    }

    std::vector<std::pair<Value, Value>> entries() const {
        std::vector<std::pair<Value, Value>> result;
        for (const auto& binding : bindings_)
            result.emplace_back(Value::object(const_cast<Object*>(binding.first)),
                                binding.second.get());
        return result;
    }

private:
    struct Binding {
        Value value = Value::unspecified();
        std::shared_ptr<Value> location;
        Value& get() { return location ? *location : value; }
        const Value& get() const { return location ? *location : value; }
    };
    static const Object* symbol_key(Value name) {
        if (!name.is_object() ||
            name.as_object()->type() != ObjectType::Symbol)
            throw std::runtime_error("environment key is not a symbol");
        return name.as_object();
    }

    static std::string symbol_name(Value name) {
        return name.as_object<SymbolObject>()->name;
    }

    RefPtr<Environment> parent_;    std::unordered_map<const Object*, Binding> bindings_;
};

using EnvironmentPtr = RefPtr<Environment>;

} // namespace goldfish::runtime
