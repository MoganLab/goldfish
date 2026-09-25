#pragma once

#include "runtime/environment.hpp"
#include "runtime/error.hpp"
#include "runtime/core_forms.hpp"
#include "runtime/symbol.hpp"

#include <functional>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

namespace goldfish::runtime {

using Values = ValueList;

class UninitializedObject final : public Object {
public:
    UninitializedObject() : Object(ObjectType::Uninitialized) {}
};

class StringObject final : public Object {
public:
    explicit StringObject(std::string value)
        : Object(ObjectType::String), value(std::move(value)) {}

    std::string value;
};

// R7RS bytevector: a raw byte string.  The substrate has no separate
// integer vector, so #u8 literals and the utf8 conversions own this type.
class BytevectorObject final : public Object {
public:
    explicit BytevectorObject(std::string bytes)
        : Object(ObjectType::Bytevector), bytes(std::move(bytes)) {}

    std::string bytes;
};

class CharacterObject final : public Object {
public:
    explicit CharacterObject(char32_t value)
        : Object(ObjectType::Character), value(value) {}

    char32_t value;
};

class EofObject final : public Object {
public:
    EofObject() : Object(ObjectType::Eof) {}
};

class InputStringPortObject final : public Object {
public:
    explicit InputStringPortObject(std::string source)
        : Object(ObjectType::InputPort), source(std::move(source)) {}

    std::string source;
    std::size_t position = 0;
    bool closed = false;
};

class OutputPortObject final : public Object {
public:
    explicit OutputPortObject(std::shared_ptr<std::ostream> stream,
                              std::shared_ptr<std::string> buffer = {})
        : Object(ObjectType::OutputPort), stream(std::move(stream)),
          buffer(std::move(buffer)) {}

    std::shared_ptr<std::ostream> stream;
    std::shared_ptr<std::string> buffer;
    bool closed = false;
};

// Compatibility-only object for the old s7-style inlet/let API.  New runtime
// code must use EnvironmentPtr directly and must not expose this object.
class LegacyLetObject final : public Object {
public:
    explicit LegacyLetObject(EnvironmentPtr environment =
                                 std::make_shared<Environment>())
        : Object(ObjectType::LegacyLet), environment(std::move(environment)) {}

    EnvironmentPtr environment;

protected:
    void trace(Tracer& tracer) override {
        if (environment)
            environment->trace(tracer);
    }
};

// A first-class handle for the evaluator's lexical environment.  Modules may
// own one of these without exposing Environment's C++ representation to
// Scheme code.
class EvalEnvironmentObject final : public Object {
public:
    explicit EvalEnvironmentObject(EnvironmentPtr environment)
        : Object(ObjectType::EvalEnvironment),
          environment(std::move(environment)) {}

    EnvironmentPtr environment;

protected:
    void trace(Tracer& tracer) override {
        if (environment)
            environment->trace(tracer);
    }
};

class VectorObject final : public Object {
public:
    explicit VectorObject(std::vector<Value> values)
        : Object(ObjectType::Vector), values(std::move(values)) {}

    std::vector<Value> values;

protected:
    void trace(Tracer& tracer) override {
        for (Value value : values)
            tracer.mark(value);
    }
};

class PairObject final : public Object {
public:
    PairObject(Value car, Value cdr)
        : Object(ObjectType::Pair), car(car), cdr(cdr) {}

    Value car;
    Value cdr;

protected:
    void trace(Tracer& tracer) override {
        tracer.mark(car);
        tracer.mark(cdr);
    }
};

class ClosureObject final : public Object {
public:
    ClosureObject(Value formals, Value body, EnvironmentPtr environment)
        : Object(ObjectType::Closure),
          formals(formals),
          body(body),
          environment(std::move(environment)) {}

    Value formals;
    Value body;
    EnvironmentPtr environment;

protected:
    void trace(Tracer& tracer) override {
        tracer.mark(formals);
        tracer.mark(body);
        if (environment)
            environment->trace(tracer);
    }
};

class PrimitiveObject final : public Object {
public:
    using Function = std::function<Values(const Values&)>;

    explicit PrimitiveObject(Function function)
        : Object(ObjectType::Primitive), function(std::move(function)) {}

    Function function;
};

class Evaluator final {
public:
    explicit Evaluator(Heap& heap)
        : heap_(heap),
          symbols_(heap),
          core_forms_(symbols_),
          global_(std::make_shared<Environment>()) {}

    EnvironmentPtr global_environment() const noexcept { return global_; }
    Value make_eval_environment(EnvironmentPtr parent = nullptr);
    // Designates the expander's defs frame (set right after the kernel
    // bootstrap creates it) as the ancestor of every parentless frame, so
    // bare gensym references (register thunks, cross-library toplevel
    // aliases) resolve through the chain while module frames stay isolated
    // from one another.
    void set_defs_root(EnvironmentPtr frame);
    Heap& heap() noexcept { return heap_; }

    Value eval(Value expression) { return eval(expression, global_); }
    Value eval(Value expression, EnvironmentPtr environment);
    Values eval_values(Value expression) {
        return eval_values(expression, global_);
    }
    Values eval_values(Value expression, EnvironmentPtr environment);

    void define_primitive(const std::string& name,
                          PrimitiveObject::Function function);

    // Sweep the heap: mark from the global chain, the expander's defs
    // frame, pending tail-call state and the permanent root slots.  Only
    // call at safe points (no unprotected Values live on the C++ stack).
    void collect();

    Value symbol(const std::string& name) {
        return symbols_.intern(name);
    }

    Value string(const std::string& value) {
        return Value::object(heap_.make<StringObject>(value));
    }

    std::string string_value(Value value);
    Value character(char32_t value) {
        return Value::object(heap_.make<CharacterObject>(value));
    }
    char32_t character_value(Value value) const;

    Value pair(Value car, Value cdr) {
        return Value::object(heap_.make<PairObject>(car, cdr));
    }

    Value list(std::initializer_list<Value> values);
    Value list(const std::vector<Value>& values);
    Value vector(const std::vector<Value>& values) {
        return Value::object(heap_.make<VectorObject>(values));
    }
    std::vector<Value> vector_values(Value value) const;
    Values apply_values(Value procedure, const Values& arguments);

private:
    // A tail-position closure call is handed back to the nearest consumer
    // (eval_values / apply) through these instead of a C++ exception: the
    // throw/catch round-trip spent ~45% of boot in unwinding (personality +
    // FDE lookups) under perf.
    bool has_pending_call_ = false;
    Value pending_procedure_ = Value::unspecified();
    Values pending_arguments_;

    Values eval_pair(PairObject& expression, EnvironmentPtr environment);
    Values eval_tail(Value expression, EnvironmentPtr environment);
    Values eval_sequence(Value expressions, EnvironmentPtr environment);
    Values eval_tail_sequence(Value expressions, EnvironmentPtr environment);
    Values apply(Value procedure, const Values& arguments);
    Value list_values(const Values& values);
    std::vector<Value> proper_list(Value value) const;
    std::string symbol_name(Value value) const;

    Heap& heap_;
    SymbolTable symbols_;
    CoreFormRegistry core_forms_;
    EnvironmentPtr global_;
};

} // namespace goldfish::runtime
