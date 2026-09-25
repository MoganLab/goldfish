#include "runtime/evaluator.hpp"

#include <cstdio>
#include <cstdlib>
#include <initializer_list>
#include <stdexcept>

namespace goldfish::runtime {

void Evaluator::define_primitive(const std::string& name,
                                 PrimitiveObject::Function function) {
    global_->define(symbol(name),
                    Value::object(heap_.make<PrimitiveObject>(
                        std::move(function))));
}

// The expander's defs frame: created first by the kernel bootstrap and
// then used as the fallback parent of every later parentless frame.
EnvironmentPtr& defs_root_slot() {
    static EnvironmentPtr root;
    return root;
}

Value Evaluator::make_eval_environment(EnvironmentPtr parent) {
    if (!parent) {
        // Parentless frames fall back to the defs root (first frame wins)
        // so bare gensym references stay reachable, instead of aliasing
        // every "new" environment onto one shared set of bindings -- which
        // made module environments clobber each other (the last module to
        // register `remove' decided what every module-ref saw).
        auto frame = std::make_shared<Environment>(
            defs_root_slot() ? defs_root_slot() : global_);
        if (!defs_root_slot()) defs_root_slot() = frame;
        return Value::object(heap_.make<EvalEnvironmentObject>(frame));
    }
    // A fresh frame that FALLS BACK to the explicit parent.
    return Value::object(heap_.make<EvalEnvironmentObject>(
        std::make_shared<Environment>(std::move(parent))));
}

void Evaluator::set_defs_root(EnvironmentPtr frame) {
    defs_root_slot() = std::move(frame);
}

Value Evaluator::list(std::initializer_list<Value> values) {
    Value result = Value::null();
    for (auto it = values.end(); it != values.begin();) {
        --it;
        result = pair(*it, result);
    }
    return result;
}

Value Evaluator::list(const std::vector<Value>& values) {
    Value result = Value::null();
    for (auto it = values.rbegin(); it != values.rend(); ++it)
        result = pair(*it, result);
    return result;
}

Value Evaluator::list_values(const Values& values) {
    Value result = Value::null();
    for (auto it = values.rbegin(); it != values.rend(); ++it)
        result = pair(*it, result);
    return result;
}

std::string Evaluator::symbol_name(Value value) const {
    if (!value.is_object() || value.as_object()->type() != ObjectType::Symbol)
        throw std::runtime_error("expected symbol");
    return value.as_object<SymbolObject>()->name;
}

std::string Evaluator::string_value(Value value) const {
    if (!value.is_object() || value.as_object()->type() != ObjectType::String)
        throw std::runtime_error("expected string");
    return value.as_object<StringObject>()->value;
}

char32_t Evaluator::character_value(Value value) const {
    if (!value.is_object() ||
        value.as_object()->type() != ObjectType::Character)
        throw std::runtime_error("expected character");
    return value.as_object<CharacterObject>()->value;
}

std::vector<Value> Evaluator::vector_values(Value value) const {
    if (!value.is_object() ||
        value.as_object()->type() != ObjectType::Vector)
        throw std::runtime_error("expected vector");
    return value.as_object<VectorObject>()->values;
}

std::vector<Value> Evaluator::proper_list(Value value) const {
    std::vector<Value> result;
    while (!value.is_null()) {
        if (!value.is_object() || value.as_object()->type() != ObjectType::Pair) {
            trace_throw("proper-list");
            throw std::runtime_error("evaluator: expected proper list");
        }
        PairObject* pair_value = value.as_object<PairObject>();
        result.push_back(pair_value->car);
        value = pair_value->cdr;
    }
    return result;
}

Values Evaluator::eval_sequence(Value expressions,
                                EnvironmentPtr environment) {
    Values result{Value::unspecified()};
    for (Value expression : proper_list(expressions))
        result = eval_values(expression, environment);
    return result;
}

Value Evaluator::eval(Value expression, EnvironmentPtr environment) {
    Values result = eval_values(expression, std::move(environment));
    // Single-value contexts collapse a multi-value result to its FIRST
    // value (s7 parity): srfi-8's receive passes its producer unwrapped as
    // an argument, and (define x (values ...)) keeps the first.
    if (result.empty()) return Value::unspecified();
    return result[0];
}

Values Evaluator::eval_values(Value expression, EnvironmentPtr environment) {
    if (!expression.is_object())
        return {expression};

    Object* object = expression.as_object();
    if (object->type() == ObjectType::Symbol) {
        try {
            return {environment->lookup(expression)};
        } catch (const std::exception&) {
            // s7 keywords: the kernel expander already treats :name / name:
            // as self-evaluating (keyword-symbol? in expand.scm); mirror it
            // so an unbound keyword reference yields the symbol itself
            // instead of unbound-symbol.
            const std::string& name =
                expression.as_object<SymbolObject>()->name;
            if (!name.empty() &&
                (name.front() == ':' || name.back() == ':'))
                return {expression};
            throw;
        }
    }
    if (object->type() != ObjectType::Pair)
        return {expression};
    Values result = eval_tail(expression, std::move(environment));
    if (!has_pending_call_)
        return result;
    // Pending tail call: apply it here (the callee's own tail calls are
    // consumed by its nested eval_values / apply, so nothing propagates
    // further).  Arity failures name the callee's first formal; add the
    // call site's operator so the offending library is findable.
    has_pending_call_ = false;
    try {
        return apply(pending_procedure_, pending_arguments_);
    } catch (const std::runtime_error& error) {
        std::string message = error.what();
        if (message.rfind("wrong number of arguments", 0) == 0 &&
            expression.is_object() &&
            expression.as_object()->type() == ObjectType::Pair) {
            Value head = expression.as_object<PairObject>()->car;
            if (head.is_object() &&
                head.as_object()->type() == ObjectType::Symbol) {
                message += " [called as: " +
                           head.as_object<SymbolObject>()->name +
                           ", core-form=" +
                           std::to_string(static_cast<int>(
                               core_forms_.lookup(head))) + "]";
            }
            // Tail calls report here with the application's parent in hand.
        }
        throw std::runtime_error(message);
    }
}

Values Evaluator::apply(Value procedure, const Values& arguments) {
    if (!procedure.is_object()) {
        trace_throw("apply-non-procedure");
        throw std::runtime_error("attempt to apply non-procedure");
    }

    Object* object = procedure.as_object();
    if (object->type() == ObjectType::Primitive)
        return procedure.as_object<PrimitiveObject>()->function(arguments);

    if (object->type() != ObjectType::Closure) {
        trace_throw("apply-non-procedure");
        throw std::runtime_error("attempt to apply non-procedure");
    }

    Value next_procedure = procedure;
    Values next_arguments = arguments;
    for (;;) {
        if (!next_procedure.is_object()) {
            trace_throw("apply-non-procedure");
            throw std::runtime_error("attempt to apply non-procedure");
        }
        Object* next_object = next_procedure.as_object();
        if (next_object->type() == ObjectType::Primitive)
            return next_procedure.as_object<PrimitiveObject>()->function(
                next_arguments);
        if (next_object->type() != ObjectType::Closure) {
            trace_throw("apply-non-procedure");
            throw std::runtime_error("attempt to apply non-procedure");
        }

        ClosureObject* closure = next_procedure.as_object<ClosureObject>();
        EnvironmentPtr call_environment =
            std::make_shared<Environment>(closure->environment);
        std::vector<Value> required;
        Value formals = closure->formals;
        Value rest = Value::null();
        while (!formals.is_null()) {
            if (!formals.is_object() ||
                formals.as_object()->type() != ObjectType::Pair) {
                rest = formals;
                break;
            }
            PairObject* formal_pair = formals.as_object<PairObject>();
            required.push_back(formal_pair->car);
            formals = formal_pair->cdr;
        }
        if (next_arguments.size() < required.size() ||
            (rest.is_null() && next_arguments.size() != required.size())) {
            std::string message =
                "wrong number of arguments: expected " +
                std::to_string(required.size()) +
                (rest.is_null() ? "" : " or more") + ", got " +
                std::to_string(next_arguments.size());
            if (!required.empty() && required[0].is_object() &&
                required[0].as_object()->type() == ObjectType::Symbol)
                message += "; first formal: " +
                           required[0].as_object<SymbolObject>()->name;
            throw std::runtime_error(message);
        }
        for (std::size_t i = 0; i < required.size(); ++i)
            call_environment->define(required[i], next_arguments[i]);
        if (!rest.is_null())
            call_environment->define(
                rest, list_values(Values(next_arguments.begin() + required.size(),
                                         next_arguments.end())));
        Values body_result = eval_tail_sequence(closure->body, call_environment);
        if (has_pending_call_) {
            // The body ended in another closure call: keep iterating instead
            // of recursing (or unwinding) per call.
            has_pending_call_ = false;
            next_procedure = pending_procedure_;
            next_arguments = std::move(pending_arguments_);
            continue;
        }
        return body_result;
    }
}

Values Evaluator::apply_values(Value procedure, const Values& arguments) {
    return apply(procedure, arguments);
}

} // namespace goldfish::runtime
