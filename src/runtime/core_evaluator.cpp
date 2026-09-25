#include "runtime/evaluator.hpp"

#include <stdexcept>

namespace goldfish::runtime {

namespace {

bool is_symbol(Value value, const char* name) {
    if (!value.is_object() || value.as_object()->type() != ObjectType::Symbol)
        return false;
    return value.as_object<SymbolObject>()->name == name;
}

std::string error_message(const Evaluator& evaluator, Value value) {
    if (value.is_object() &&
        value.as_object()->type() == ObjectType::String)
        return evaluator.string_value(value);
    if (value.is_object() &&
        value.as_object()->type() == ObjectType::Symbol)
        return value.as_object<SymbolObject>()->name;
    throw std::runtime_error("error message must be a string or symbol");
}

} // namespace

Values Evaluator::eval_tail_sequence(Value expressions,
                                     EnvironmentPtr environment) {
    std::vector<Value> forms = proper_list(expressions);
    if (forms.empty())
        return {Value::unspecified()};
    for (std::size_t i = 0; i + 1 < forms.size(); ++i)
        eval_values(forms[i], environment);
    return eval_tail(forms.back(), std::move(environment));
}

Values Evaluator::eval_tail(Value expression, EnvironmentPtr environment) {
    if (!expression.is_object())
        return {expression};
    Object* object = expression.as_object();
    if (object->type() == ObjectType::Symbol) {
        try {
            return {environment->lookup(expression)};
        } catch (const std::exception&) {
            // Same s7 keyword rule as eval_values (kernel keyword-symbol?):
            // :name / name: are self-evaluating, never variable lookups.
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

    PairObject* pair_expression = expression.as_object<PairObject>();
    CoreForm form = core_forms_.lookup(pair_expression->car);
    if (form == CoreForm::If) {
        std::vector<Value> arguments = proper_list(pair_expression->cdr);
        if (arguments.size() != 2 && arguments.size() != 3)
            throw std::runtime_error("if expects two or three arguments");
        Value test = eval(arguments[0], environment);
        if (!test.is_boolean() || test.as_boolean())
            return eval_tail(arguments[1], std::move(environment));
        return arguments.size() == 3
                   ? eval_tail(arguments[2], std::move(environment))
                   : Values{Value::unspecified()};
    }
    if (form == CoreForm::Let || form == CoreForm::Letrec ||
        form == CoreForm::LetrecStar) {
        std::vector<Value> arguments = proper_list(pair_expression->cdr);
        if (arguments.size() < 2)
            throw std::runtime_error("binding form expects bindings and body");
        EnvironmentPtr child = std::make_shared<Environment>(environment);
        std::vector<Value> bindings = proper_list(arguments[0]);
        if (form == CoreForm::Let) {
            for (Value binding : bindings) {
                std::vector<Value> pair_binding = proper_list(binding);
                if (pair_binding.size() != 2)
                    throw std::runtime_error("let binding expects name and value");
                child->define(pair_binding[0], eval(pair_binding[1], environment));
            }
        } else {
            for (Value binding : bindings) {
                std::vector<Value> pair_binding = proper_list(binding);
                if (pair_binding.size() != 2)
                    throw std::runtime_error("letrec binding expects name and value");
                child->define(pair_binding[0],
                              Value::object(heap_.make<UninitializedObject>()));
                if (form == CoreForm::LetrecStar)
                    child->set(pair_binding[0], eval(pair_binding[1], child));
            }
            if (form == CoreForm::Letrec) {
                for (Value binding : bindings) {
                    std::vector<Value> pair_binding = proper_list(binding);
                    child->set(pair_binding[0], eval(pair_binding[1], child));
                }
            }
        }
        std::vector<Value> body(arguments.begin() + 1, arguments.end());
        return eval_tail_sequence(list_values(body), std::move(child));
    }

    // A sequence in tail position must keep its last expression in tail
    // position.  Going through eval_pair/eval_sequence here used to make
    // every lowered `(begin ...)' add a C++ stack frame, which eventually
    // overflowed while the native evaluator ran the expander itself.
    if (form == CoreForm::Begin)
        return eval_tail_sequence(pair_expression->cdr,
                                  std::move(environment));

    if (form == CoreForm::When || form == CoreForm::Unless) {
        std::vector<Value> arguments = proper_list(pair_expression->cdr);
        if (arguments.size() < 2)
            throw std::runtime_error("when/unless expects a test and body");
        Value test = eval(arguments[0], environment);
        bool selected = !test.is_boolean() || test.as_boolean();
        if (form == CoreForm::Unless)
            selected = !selected;
        if (!selected)
            return {Value::unspecified()};
        return eval_tail_sequence(
            list_values(std::vector<Value>(arguments.begin() + 1,
                                           arguments.end())),
            std::move(environment));
    }

    if (form == CoreForm::Unknown) {
        Value procedure = eval(pair_expression->car, environment);
        Values arguments;
        for (Value argument : proper_list(pair_expression->cdr))
            arguments.push_back(eval(argument, environment));
        if (procedure.is_object() &&
            procedure.as_object()->type() == ObjectType::Closure) {
            // Hand the call to the nearest consumer (eval_values/apply);
            // a throw here made every closure tail call pay full C++ unwind.
            pending_procedure_ = procedure;
            pending_arguments_ = std::move(arguments);
            has_pending_call_ = true;
            return {Value::unspecified()};
        }
        return apply(procedure, arguments);
    }
    return eval_pair(*pair_expression, std::move(environment));
}

Values Evaluator::eval_pair(PairObject& expression,
                            EnvironmentPtr environment) {
    Value head = expression.car;
    Value tail = expression.cdr;
    CoreForm form = core_forms_.lookup(head);

    if (form == CoreForm::Quote) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.size() != 1)
            throw std::runtime_error("quote expects one argument");
        return {arguments[0]};
    }

    if (form == CoreForm::If) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.size() != 2 && arguments.size() != 3)
            throw std::runtime_error("if expects two or three arguments");
        Value test = eval(arguments[0], environment);
        if (!test.is_boolean() || test.as_boolean())
            return eval_values(arguments[1], environment);
        return arguments.size() == 3
                   ? eval_values(arguments[2], environment)
                   : Values{Value::unspecified()};
    }
    if (form == CoreForm::Unless) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.size() < 2)
            throw std::runtime_error("when/unless expects a test and body");
        Value test = eval(arguments[0], environment);
        bool selected = !test.is_boolean() || test.as_boolean();
        selected = !selected;
        if (!selected) return {Value::unspecified()};
        return eval_sequence(
            list_values(std::vector<Value>(arguments.begin() + 1,
                                            arguments.end())),
            environment);
    }

    if (form == CoreForm::Begin)
        return eval_sequence(tail, environment);

    if (form == CoreForm::Values) {
        Values result;
        for (Value argument : proper_list(tail)) {
            // Each argument contributes its values: srfi-8's receive builds
            // its producer as (values expr) where expr may itself be a
            // multi-value call -- those values pass through unchanged.
            Values produced = eval_values(argument, environment);
            for (Value value : produced)
                result.push_back(value);
        }
        return result;
    }

    if (form == CoreForm::CallWithValues) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.size() != 2)
            throw std::runtime_error(
                "call-with-values expects producer and consumer");
        Value producer = eval(arguments[0], environment);
        Values produced = apply(producer, {});
        Value consumer = eval(arguments[1], environment);
        return apply(consumer, produced);
    }

    if (form == CoreForm::Raise) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.size() != 1)
            throw std::runtime_error("raise expects one argument");
        throw RaisedValue(eval(arguments[0], environment));
    }

    if (form == CoreForm::Error) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.empty())
            throw std::runtime_error("error expects a message");
        Value message = eval(arguments[0], environment);
        ValueList irritants;
        for (std::size_t i = 1; i < arguments.size(); ++i)
            irritants.push_back(eval(arguments[i], environment));
        // (error 'key ...) keeps its key for catch handlers (host parity);
        // (error "text" ...) is the R7RS form and carries no key.
        const std::string key =
            message.is_object() &&
                    message.as_object()->type() == ObjectType::Symbol
                ? message.as_object<SymbolObject>()->name
                : std::string();
        throw RaisedValue(Value::object(heap_.make<ErrorObject>(
            error_message(*this, message), std::move(irritants), key)));
    }

    if (form == CoreForm::ErrorObjectPredicate) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.size() != 1)
            throw std::runtime_error("error-object? expects one argument");
        Value value = eval(arguments[0], environment);
        return {Value::boolean(
            value.is_object() &&
            value.as_object()->type() == ObjectType::ErrorObject)};
    }

    if (form == CoreForm::ErrorObjectMessage) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.size() != 1)
            throw std::runtime_error(
                "error-object-message expects one argument");
        Value value = eval(arguments[0], environment);
        if (!value.is_object() ||
            value.as_object()->type() != ObjectType::ErrorObject)
            throw std::runtime_error("not an error object");
        return {string(value.as_object<ErrorObject>()->message)};
    }

    if (form == CoreForm::ErrorObjectIrritants) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.size() != 1)
            throw std::runtime_error(
                "error-object-irritants expects one argument");
        Value value = eval(arguments[0], environment);
        if (!value.is_object() ||
            value.as_object()->type() != ObjectType::ErrorObject)
            throw std::runtime_error("not an error object");
        return {list_values(value.as_object<ErrorObject>()->irritants)};
    }

    if (form == CoreForm::Guard) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.size() < 2)
            throw std::runtime_error("guard expects clauses and body");
        std::vector<Value> specification = proper_list(arguments[0]);
        if (specification.empty())
            throw std::runtime_error("guard expects a binding");
        Value variable = specification[0];
        std::vector<Value> clauses(specification.begin() + 1,
                                   specification.end());
        std::vector<Value> body_forms(arguments.begin() + 1, arguments.end());
        Value body = list_values(body_forms);

        auto handle = [&](Value caught) -> Values {
            EnvironmentPtr handler = std::make_shared<Environment>(environment);
            handler->define(variable, caught);
            for (Value clause : clauses) {
                std::vector<Value> clause_forms = proper_list(clause);
                if (clause_forms.empty())
                    throw std::runtime_error("empty guard clause");
                bool selected = is_symbol(clause_forms[0], "else");
                if (!selected) {
                    Value condition = eval(clause_forms[0], handler);
                    selected = !condition.is_boolean() || condition.as_boolean();
                }
                if (selected) {
                    if (clause_forms.size() == 3 &&
                        is_symbol(clause_forms[1], "=>")) {
                        Value procedure = eval(clause_forms[2], handler);
                        return apply(procedure, {caught});
                    }
                    std::vector<Value> forms(clause_forms.begin() + 1,
                                             clause_forms.end());
                    return eval_sequence(list_values(forms), handler);
                }
            }
            throw RaisedValue(caught);
        };

        try {
            return eval_sequence(body, environment);
        } catch (const RaisedValue& raised) {
            return handle(raised.value());
        } catch (const std::runtime_error& error) {
            Value error_object = Value::object(heap_.make<ErrorObject>(
                error.what(), ValueList{}));
            return handle(error_object);
        }
    }

    if (form == CoreForm::Apply) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.size() < 2)
            throw std::runtime_error("apply expects procedure and arguments");
        Value procedure = eval(arguments[0], environment);
        Values applied;
        for (std::size_t i = 1; i + 1 < arguments.size(); ++i) {
            Values evaluated = eval_values(arguments[i], environment);
            if (evaluated.size() != 1)
                throw std::runtime_error("apply argument expects one value");
            applied.push_back(evaluated[0]);
        }
        Value final_argument = eval(arguments.back(), environment);
        std::vector<Value> tail_arguments = proper_list(final_argument);
        applied.insert(applied.end(), tail_arguments.begin(),
                       tail_arguments.end());
        return apply(procedure, applied);
    }

    if (form == CoreForm::Lambda) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.size() < 2)
            throw std::runtime_error("lambda expects formals and body");
        Value body = Value::null();
        for (auto it = arguments.end(); it != arguments.begin() + 1;) {
            --it;
            body = pair(*it, body);
        }
        return {Value::object(heap_.make<ClosureObject>(
            arguments[0], body, std::move(environment)))};
    }

    if (form == CoreForm::Let) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.size() < 2)
            throw std::runtime_error("let expects bindings and body");
        EnvironmentPtr child = std::make_shared<Environment>(environment);
        for (Value binding : proper_list(arguments[0])) {
            std::vector<Value> pair_binding = proper_list(binding);
            if (pair_binding.size() != 2)
                throw std::runtime_error("let binding expects name and value");
            child->define(pair_binding[0], eval(pair_binding[1], environment));
        }
        Values body{Value::unspecified()};
        for (auto it = arguments.begin() + 1; it != arguments.end(); ++it)
            body = eval_values(*it, child);
        return body;
    }

    if (form == CoreForm::Letrec || form == CoreForm::LetrecStar) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.size() < 2)
            throw std::runtime_error("letrec expects bindings and body");
        EnvironmentPtr child = std::make_shared<Environment>(environment);
        std::vector<Value> bindings = proper_list(arguments[0]);
        if (form == CoreForm::Letrec) {
            for (Value binding : bindings) {
                std::vector<Value> pair_binding = proper_list(binding);
                if (pair_binding.size() != 2)
                    throw std::runtime_error(
                        "letrec binding expects name and value");
                child->define(
                    pair_binding[0],
                    Value::object(heap_.make<UninitializedObject>()));
            }
            for (Value binding : bindings) {
                std::vector<Value> pair_binding = proper_list(binding);
                child->set(pair_binding[0], eval(pair_binding[1], child));
            }
        } else {
            for (Value binding : bindings) {
                std::vector<Value> pair_binding = proper_list(binding);
                if (pair_binding.size() != 2)
                    throw std::runtime_error(
                        "letrec* binding expects name and value");
                child->define(
                    pair_binding[0],
                    Value::object(heap_.make<UninitializedObject>()));
                child->set(pair_binding[0], eval(pair_binding[1], child));
            }
        }
        Values body{Value::unspecified()};
        for (auto it = arguments.begin() + 1; it != arguments.end(); ++it)
            body = eval_values(*it, child);
        return body;
    }

    if (form == CoreForm::Set) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.size() != 2)
            throw std::runtime_error("set! expects name and value");
        Value value = eval(arguments[1], environment);

        if (arguments[0].is_object() &&
            arguments[0].as_object()->type() == ObjectType::Pair) {
            PairObject* target = arguments[0].as_object<PairObject>();
            if (core_forms_.lookup(target->car) == CoreForm::ModuleRef) {
                std::vector<Value> reference = proper_list(target->cdr);
                if (reference.size() != 2)
                    throw std::runtime_error(
                        "module-ref expects module and name");
                Value module_name = eval(reference[0], environment);
                Value name = eval(reference[1], environment);
                Value module_set = environment->lookup(symbol("module-set"));
                return apply(module_set, {module_name, name, value});
            }
            if (core_forms_.lookup(target->car) == CoreForm::Setter) {
                std::vector<Value> setter_target = proper_list(target->cdr);
                if (setter_target.size() != 1)
                    throw std::runtime_error("setter expects one procedure");
                // Setter registration is a compatibility hook for the
                // lowered module substrate. module-ref has native write
                // handling above; other setters are not part of R2 yet.
                (void)setter_target;
                return {Value::unspecified()};
            }
            std::vector<Value> target_form = proper_list(arguments[0]);
            if (target_form.empty())
                throw std::runtime_error("set! target is empty");
            Value procedure = eval(target_form[0], environment);
            Values setter_arguments;
            for (std::size_t i = 1; i < target_form.size(); ++i)
                setter_arguments.push_back(eval(target_form[i], environment));
            setter_arguments.push_back(value);
            apply(procedure, setter_arguments);
            return {Value::unspecified()};
        }
        environment->set(arguments[0], value);
        return {Value::unspecified()};
    }

    if (form == CoreForm::Define) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.size() != 2)
            throw std::runtime_error("define expects name and value");
        Value value = eval(arguments[1], environment);
        environment->define(arguments[0], value);
        return {Value::unspecified()};
    }

    if (form == CoreForm::ModuleRef) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.size() != 2)
            throw std::runtime_error("module-ref expects module and name");
        Value procedure = environment->lookup(head);
        Values evaluated;
        for (Value argument : arguments)
            evaluated.push_back(eval(argument, environment));
        return apply(procedure, evaluated);
    }

    if (form == CoreForm::ModuleSet) {
        std::vector<Value> arguments = proper_list(tail);
        if (arguments.size() != 3)
            throw std::runtime_error("module-set expects module, name, value");
        Value procedure = environment->lookup(head);
        Values evaluated;
        for (Value argument : arguments)
            evaluated.push_back(eval(argument, environment));
        return apply(procedure, evaluated);
    }

    Value procedure = eval(head, environment);
    Values arguments;
    for (Value argument : proper_list(tail))
        arguments.push_back(eval(argument, environment));
    return apply(procedure, arguments);
}

} // namespace goldfish::runtime
