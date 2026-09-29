#pragma once

#include "runtime/environment.hpp"
#include "runtime/error.hpp"
#include "runtime/core_forms.hpp"
#include "runtime/symbol.hpp"
#include "runtime/numeric.hpp"

#include <functional>
#include <cstddef>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

namespace goldfish::runtime {

bool equal(Value left, Value right);

using Values = ValueList;

Value current_input_port_value();
Value current_output_port_value();
void set_current_input_port_value(Value port);
void set_current_output_port_value(Value port);

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

class NumberObject final : public Object {
public:
    explicit NumberObject(Number value)
        : Object(ObjectType::Number), value(std::move(value)) {}

    Number value;
};

class RandomSourceObject final : public Object {
public:
    RandomSourceObject() : Object(ObjectType::RandomSource),
        state{0x243f6a8885a308d3ULL, 0x13198a2e03707344ULL} {}
    std::uint64_t state[2];
};

bool is_number(Value value) noexcept;
Number number_value(Value value);
std::string number_to_string(Value value, unsigned radix = 10);

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
    std::string file_path;
    bool file_backed = false;
    bool file_append = false;
    bool closed = false;
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

    enum class Kind : std::uint8_t {
        Ordinary,
        CaptureContinuation,
        DynamicWind,
        Map,
        ForEach,
        Fold,
        Filter,
        Any,
        Every,
        Member,
        Assoc,
        StringForEach,
        VectorFilter,
        CallWithValues,
        WithInputFromString,
        WithOutputToString,
        WithInputFromFile,
        WithOutputToFile,
        CallWithInputString,
        CallWithOutputString,
        CallWithInputFile,
        CallWithOutputFile,
        Catch,
    };

    explicit PrimitiveObject(Function function,
                             Kind kind = Kind::Ordinary)
        : Object(ObjectType::Primitive), function(std::move(function)),
          kind(kind) {}

    Function function;
    Kind kind;
};

struct ContinuationJump final {
    Value continuation;
    Values values;
};

struct DynamicWinder final {
    enum class PortSlot : std::uint8_t { None, Input, Output };

    Value before = Value::unspecified();
    Value after = Value::unspecified();
    Value previous_port = Value::unspecified();
    Value bound_port = Value::unspecified();
    EnvironmentPtr environment;
    std::uint64_t identity = 0;
    PortSlot port_slot = PortSlot::None;
    bool close_bound_port = false;

    void trace(Tracer& tracer) const {
        tracer.mark(before);
        tracer.mark(after);
        tracer.mark(previous_port);
        tracer.mark(bound_port);
        if (environment)
            environment->trace(tracer);
    }
};

// An explicit evaluator continuation frame.  Frames are copied into a
// ContinuationObject when Scheme captures its current continuation; keeping
// the payload here typed lets the precise collector trace every live Value.
struct KontFrame final {
    enum class Kind : std::uint8_t {
        Sequence,
        IfTest,
        WhenTest,
        CallOperator,
        CallArgument,
        DefineValue,
        SetValue,
        LetBinding,
        LetrecBinding,
        ValuesArgument,
        CallWithValuesProducer,
        CallWithValuesConsumer,
        RaiseValue,
        ErrorMessage,
        ErrorIrritant,
        ErrorObjectPredicate,
        ErrorObjectMessage,
        ErrorObjectIrritants,
        ExceptionHandler,
        Catch,
        RethrowRaised,
        RethrowRuntimeError,
        RethrowThrown,
        ApplyProcedure,
        ApplyArgument,
        ModuleReference,
        ModuleAssignment,
        SetterTarget,
        DynamicWind,
        PortCallback,
        MapLoop,
        ForEachLoop,
        FoldLoop,
        FilterLoop,
        AnyLoop,
        EveryLoop,
        MemberLoop,
        AssocLoop,
        VectorFilterLoop,
        ContinuationTransfer,
    };

    Kind kind = Kind::Sequence;
    Value expression = Value::unspecified();
    Value auxiliary = Value::unspecified();
    EnvironmentPtr environment;
    EnvironmentPtr secondary_environment;
    Values values;
    std::size_t index = 0;
    std::size_t stage = 0;
    std::vector<Value> expressions;
    std::vector<DynamicWinder> exiting_winders;
    std::vector<DynamicWinder> entering_winders;
    std::vector<DynamicWinder> winders;

    void trace(Tracer& tracer) const {
        tracer.mark(expression);
        tracer.mark(auxiliary);
        for (Value value : values)
            tracer.mark(value);
        for (Value value : expressions)
            tracer.mark(value);
        for (const DynamicWinder& winder : exiting_winders)
            winder.trace(tracer);
        for (const DynamicWinder& winder : entering_winders)
            winder.trace(tracer);
        for (const DynamicWinder& winder : winders)
            winder.trace(tracer);
        if (environment)
            environment->trace(tracer);
        if (secondary_environment)
            secondary_environment->trace(tracer);
    }
};

struct EvalSnapshot final {
    std::uint64_t machine_id = 0;
    Value expression = Value::unspecified();
    Value initial_procedure = Value::unspecified();
    EnvironmentPtr environment;
    std::vector<KontFrame> frames;
    Values values;
    Values initial_arguments;
    std::vector<DynamicWinder> winders;
    bool returning = false;
    bool applying = false;

    void trace(Tracer& tracer) const {
        tracer.mark(expression);
        tracer.mark(initial_procedure);
        for (Value value : values)
            tracer.mark(value);
        for (Value value : initial_arguments)
            tracer.mark(value);
        for (const DynamicWinder& winder : winders)
            winder.trace(tracer);
        if (environment)
            environment->trace(tracer);
        for (const KontFrame& frame : frames)
            frame.trace(tracer);
    }
};

// Snapshot of evaluator-owned control state.  The saved frames are immutable
// after construction so invoking a multi-shot continuation can copy them into
// the evaluator's working stack without changing later invocations.
class ContinuationObject final : public Object {
public:
    explicit ContinuationObject(EvalSnapshot snapshot)
        : Object(ObjectType::Continuation), snapshot(std::move(snapshot)) {}

    EvalSnapshot snapshot;

protected:
    void trace(Tracer& tracer) override {
        snapshot.trace(tracer);
    }
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
    void define_callcc_primitive(const std::string& name);
    void define_dynamic_wind_primitive(const std::string& name);
    void define_machine_primitive(const std::string& name,
                                  PrimitiveObject::Kind kind);

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

    Value number(Number value) {
        if (value.has_imaginary_part && value.imag.is_zero() &&
            !value.imag.inexact && !value.real.inexact)
            value.has_imaginary_part = false;
        if (!value.has_imaginary_part && !value.real.inexact &&
            value.real.denominator == BigInteger(1) &&
            value.real.numerator.fits_int64())
            return Value::integer(value.real.numerator.to_int64());
        return Value::object(heap_.make<NumberObject>(std::move(value)));
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
    std::vector<EvalSnapshot*> active_evaluations_;
    std::uint64_t next_machine_id_ = 1;
    std::uint64_t next_winder_identity_ = 1;

    Values eval_pair(PairObject& expression, EnvironmentPtr environment);
    Values eval_tail(Value expression, EnvironmentPtr environment);
    Values eval_sequence(Value expressions, EnvironmentPtr environment);
    Values eval_tail_sequence(Value expressions, EnvironmentPtr environment);
    Values apply(Value procedure, const Values& arguments);
    Values run_machine(EvalSnapshot& state);
    Value list_values(const Values& values);
    std::vector<Value> proper_list(Value value) const;
    std::string symbol_name(Value value) const;

    Heap& heap_;
    SymbolTable symbols_;
    CoreFormRegistry core_forms_;
    EnvironmentPtr global_;
};

} // namespace goldfish::runtime
