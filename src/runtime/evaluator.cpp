#include "runtime/evaluator.hpp"

#include <initializer_list>
#include <stdexcept>

namespace goldfish::runtime {

namespace {

Value single_value(const Values& values, const char* context) {
    if (values.size() != 1)
        throw std::runtime_error(std::string(context) +
                                 " expects exactly one value");
    return values[0];
}

} // namespace

void Evaluator::define_primitive(const std::string& name,
                                 PrimitiveObject::Function function) {
    global_->define(symbol(name),
                    Value::object(heap_.make<PrimitiveObject>(
                        std::move(function))));
}

Value Evaluator::make_eval_environment(EnvironmentPtr parent) {
    if (!parent)
        parent = global_;
    return Value::object(
        heap_.make<EvalEnvironmentObject>(std::move(parent)));
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
        if (!value.is_object() || value.as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("expected proper list");
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
    return single_value(eval_values(expression, std::move(environment)),
                        "single-value context");
}

Values Evaluator::eval_values(Value expression, EnvironmentPtr environment) {
    if (!expression.is_object())
        return {expression};

    Object* object = expression.as_object();
    if (object->type() == ObjectType::Symbol)
        return {environment->lookup(expression)};
    if (object->type() != ObjectType::Pair)
        return {expression};
    try {
        return eval_tail(expression, std::move(environment));
    } catch (const TailCall& tail_call) {
        return apply(tail_call.procedure, tail_call.arguments);
    }
}

Values Evaluator::apply(Value procedure, const Values& arguments) {
    if (!procedure.is_object())
        throw std::runtime_error("attempt to apply non-procedure");

    Object* object = procedure.as_object();
    if (object->type() == ObjectType::Primitive)
        return procedure.as_object<PrimitiveObject>()->function(arguments);

    if (object->type() != ObjectType::Closure)
        throw std::runtime_error("attempt to apply non-procedure");

    Value next_procedure = procedure;
    Values next_arguments = arguments;
    for (;;) {
        if (!next_procedure.is_object())
            throw std::runtime_error("attempt to apply non-procedure");
        Object* next_object = next_procedure.as_object();
        if (next_object->type() == ObjectType::Primitive)
            return next_procedure.as_object<PrimitiveObject>()->function(
                next_arguments);
        if (next_object->type() != ObjectType::Closure)
            throw std::runtime_error("attempt to apply non-procedure");

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
            (rest.is_null() && next_arguments.size() != required.size()))
            throw std::runtime_error("wrong number of arguments");
        for (std::size_t i = 0; i < required.size(); ++i)
            call_environment->define(required[i], next_arguments[i]);
        if (!rest.is_null())
            call_environment->define(
                rest, list_values(Values(next_arguments.begin() + required.size(),
                                         next_arguments.end())));
        try {
            return eval_tail_sequence(closure->body, call_environment);
        } catch (const TailCall& tail_call) {
            next_procedure = tail_call.procedure;
            next_arguments = tail_call.arguments;
        }
    }
}

Values Evaluator::apply_values(Value procedure, const Values& arguments) {
    return apply(procedure, arguments);
}

} // namespace goldfish::runtime
