#include "runtime/standard_primitives.hpp"

#include "runtime/reader.hpp"

#include <algorithm>
#include <cerrno>
#include <cmath>
#include <cstring>
#include <cstdlib>
#include <limits>
#include <numeric>
#include <stdexcept>

namespace goldfish::runtime {

namespace {

void require_arity(const Values& args, std::size_t count,
                   const char* name) {
    if (args.size() != count)
        throw std::runtime_error(std::string(name) + " expects " +
                                 std::to_string(count) + " arguments");
}

bool same(Value left, Value right) { return left == right; }

bool equal(Value left, Value right) {
    if (left == right)
        return true;
    if (!left.is_object() || !right.is_object() ||
        left.as_object()->type() != right.as_object()->type())
        return false;
    switch (left.as_object()->type()) {
    case ObjectType::Pair: {
        PairObject* a = left.as_object<PairObject>();
        PairObject* b = right.as_object<PairObject>();
        return equal(a->car, b->car) && equal(a->cdr, b->cdr);
    }
    case ObjectType::String:
        return left.as_object<StringObject>()->value ==
               right.as_object<StringObject>()->value;
    case ObjectType::Vector: {
        const auto& a = left.as_object<VectorObject>()->values;
        const auto& b = right.as_object<VectorObject>()->values;
        return a.size() == b.size() &&
               std::equal(a.begin(), a.end(), b.begin(), equal);
    }
    default:
        return false;
    }
}

void install(Evaluator& evaluator, const char* name,
             PrimitiveObject::Function function) {
    evaluator.define_primitive(name, std::move(function));
}

} // namespace

void install_standard_primitives(Evaluator& evaluator) {
    install(evaluator, "abs", [](const Values& args) {
        require_arity(args, 1, "abs");
        if (!args[0].is_integer()) throw std::runtime_error("abs expects an integer");
        if (args[0].as_integer() == std::numeric_limits<std::int64_t>::min())
            throw std::runtime_error("abs integer overflow");
        return Values{Value::integer(std::llabs(args[0].as_integer()))};
    });
    for (const char* name : {"min", "max"}) {
        install(evaluator, name, [name](const Values& args) {
            if (args.empty())
                throw std::runtime_error(std::string(name) + " expects an argument");
            std::int64_t result = args[0].as_integer();
            for (std::size_t i = 1; i < args.size(); ++i) {
                std::int64_t value = args[i].as_integer();
                result = std::string(name) == "min" ? std::min(result, value)
                                                     : std::max(result, value);
            }
            return Values{Value::integer(result)};
        });
    }
    install(evaluator, "expt", [](const Values& args) {
        require_arity(args, 2, "expt");
        const double result = std::pow(static_cast<double>(args[0].as_integer()),
                                       static_cast<double>(args[1].as_integer()));
        if (!std::isfinite(result) ||
            result < static_cast<double>(std::numeric_limits<std::int64_t>::min()) ||
            result > static_cast<double>(std::numeric_limits<std::int64_t>::max()))
            throw std::runtime_error("expt result is outside integer range");
        return Values{Value::integer(static_cast<std::int64_t>(result))};
    });
    install(evaluator, "defined?", [&evaluator](const Values& args) {
        require_arity(args, 1, "defined?");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::Symbol)
            return Values{Value::boolean(false)};
        try {
            (void)evaluator.global_environment()->lookup(args[0]);
            return Values{Value::boolean(true)};
        } catch (const std::runtime_error&) {
            return Values{Value::boolean(false)};
        }
    });
    install(evaluator, "make-eval-environment",
            [&evaluator](const Values& args) {
                if (!args.empty())
                    throw std::runtime_error(
                        "make-eval-environment expects no arguments");
                return Values{evaluator.make_eval_environment()};
            });
    // The old host exposed `rootlet' as an eval target.  Keep the name only
    // as a capability-neutral handle; native code never receives a host
    // inlet and the returned object is the explicit global environment.
    install(evaluator, "rootlet", [&evaluator](const Values& args) {
        require_arity(args, 0, "rootlet");
        return Values{evaluator.make_eval_environment()};
    });
    install(evaluator, "eval-environment?", [](const Values& args) {
        require_arity(args, 1, "eval-environment?");
        return Values{Value::boolean(
            args[0].is_object() &&
            args[0].as_object()->type() == ObjectType::EvalEnvironment)};
    });
    install(evaluator, "eval-environment-define!", [](const Values& args) {
        require_arity(args, 3, "eval-environment-define!");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::EvalEnvironment)
            throw std::runtime_error(
                "eval-environment-define! expects an eval environment");
        args[0].as_object<EvalEnvironmentObject>()->environment->define(
            args[1], args[2]);
        return Values{Value::unspecified()};
    });
    install(evaluator, "eval-environment-set!", [](const Values& args) {
        require_arity(args, 3, "eval-environment-set!");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::EvalEnvironment)
            throw std::runtime_error(
                "eval-environment-set! expects an eval environment");
        args[0].as_object<EvalEnvironmentObject>()->environment->set(args[1],
                                                                       args[2]);
        return Values{Value::unspecified()};
    });
    install(evaluator, "eval-environment-ref", [](const Values& args) {
        require_arity(args, 2, "eval-environment-ref");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::EvalEnvironment)
            throw std::runtime_error(
                "eval-environment-ref expects an eval environment");
        return Values{args[0].as_object<EvalEnvironmentObject>()
                          ->environment->lookup(args[1])};
    });
    install(evaluator, "eval", [&evaluator](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error("eval expects one or two arguments");
        EnvironmentPtr environment = evaluator.global_environment();
        if (args.size() == 2) {
            if (!args[1].is_object() ||
                args[1].as_object()->type() != ObjectType::EvalEnvironment)
                throw std::runtime_error(
                    "eval expects an eval environment as its second argument");
            environment =
                args[1].as_object<EvalEnvironmentObject>()->environment;
        }
        return evaluator.eval_values(args[0], std::move(environment));
    });

    install(evaluator, "setter", [&evaluator](const Values& args) {
        require_arity(args, 1, "setter");
        return Values{Value::object(evaluator.heap().make<PrimitiveObject>(
            [](const Values& setter_args) {
                if (setter_args.size() != 2)
                    throw std::runtime_error("setter procedure expects two arguments");
                return Values{Value::unspecified()};
            }))};
    });
    Value eof = Value::object(evaluator.heap().make<EofObject>());
    install(evaluator, "eof-object", [eof](const Values& args) {
        require_arity(args, 0, "eof-object");
        return Values{eof};
    });
    install(evaluator, "eof-object?", [](const Values& args) {
        require_arity(args, 1, "eof-object?");
        return Values{Value::boolean(args[0].is_object() &&
                                     args[0].as_object()->type() == ObjectType::Eof)};
    });
    install(evaluator, "open-input-string", [&evaluator](const Values& args) {
        require_arity(args, 1, "open-input-string");
        return Values{Value::object(evaluator.heap().make<InputStringPortObject>(
            evaluator.string_value(args[0])))};
    });
    install(evaluator, "read", [eof](const Values& args) {
        if (args.size() > 1)
            throw std::runtime_error("read expects zero or one arguments");
        if (args.empty())
            return Values{eof};
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::InputPort)
            throw std::runtime_error("read expects an input port");
        auto* port = args[0].as_object<InputStringPortObject>();
        if (port->position == port->source.size())
            return Values{eof};
        throw std::runtime_error("read from non-empty input port is not bootstrapped");
    });
    install(evaluator, "boolean?", [](const Values& args) {
        require_arity(args, 1, "boolean?");
        return Values{Value::boolean(args[0].is_boolean())};
    });
    install(evaluator, "integer?", [](const Values& args) {
        require_arity(args, 1, "integer?");
        return Values{Value::boolean(args[0].is_integer())};
    });
    install(evaluator, "pair?", [](const Values& args) {
        require_arity(args, 1, "pair?");
        return Values{Value::boolean(
            args[0].is_object() && args[0].as_object()->type() == ObjectType::Pair)};
    });
    install(evaluator, "null?", [](const Values& args) {
        require_arity(args, 1, "null?");
        return Values{Value::boolean(args[0].is_null())};
    });
    install(evaluator, "symbol?", [](const Values& args) {
        require_arity(args, 1, "symbol?");
        return Values{Value::boolean(args[0].is_object() &&
                                     args[0].as_object()->type() == ObjectType::Symbol)};
    });
    install(evaluator, "string?", [](const Values& args) {
        require_arity(args, 1, "string?");
        return Values{Value::boolean(args[0].is_object() &&
                                     args[0].as_object()->type() == ObjectType::String)};
    });
    install(evaluator, "vector?", [](const Values& args) {
        require_arity(args, 1, "vector?");
        return Values{Value::boolean(args[0].is_object() &&
                                     args[0].as_object()->type() == ObjectType::Vector)};
    });
    install(evaluator, "procedure?", [](const Values& args) {
        require_arity(args, 1, "procedure?");
        bool result = args[0].is_object() &&
                      (args[0].as_object()->type() == ObjectType::Closure ||
                       args[0].as_object()->type() == ObjectType::Primitive);
        return Values{Value::boolean(result)};
    });
    install(evaluator, "eq?", [](const Values& args) {
        require_arity(args, 2, "eq?");
        return Values{Value::boolean(same(args[0], args[1]))};
    });
    install(evaluator, "eqv?", [](const Values& args) {
        require_arity(args, 2, "eqv?");
        return Values{Value::boolean(same(args[0], args[1]))};
    });
    install(evaluator, "equal?", [](const Values& args) {
        require_arity(args, 2, "equal?");
        return Values{Value::boolean(equal(args[0], args[1]))};
    });
    install(evaluator, "catch", [&evaluator](const Values& args) {
        require_arity(args, 3, "catch");
        auto matches = [&evaluator](Value wanted, Value actual) {
            return (wanted.is_boolean() && wanted.as_boolean()) ||
                   evaluator.apply_values(
                       evaluator.global_environment()->lookup(
                           evaluator.symbol("equal?")),
                       {wanted, actual})[0].as_boolean();
        };
        try {
            return evaluator.apply_values(args[1], {});
        } catch (const ThrownValue& thrown) {
            if (!matches(args[0], thrown.tag())) throw;
            Values handler_args{thrown.tag()};
            handler_args.insert(handler_args.end(), thrown.arguments().begin(),
                                thrown.arguments().end());
            return evaluator.apply_values(args[2], handler_args);
        } catch (const RaisedValue& raised) {
            if (!matches(args[0], Value::boolean(true))) throw;
            return evaluator.apply_values(
                args[2], {evaluator.symbol("raised"), raised.value()});
        } catch (const std::runtime_error& error) {
            if (!matches(args[0], Value::boolean(true))) throw;
            Value error_object = Value::object(
                evaluator.heap().make<ErrorObject>(error.what(), ValueList{}));
            return evaluator.apply_values(
                args[2], {evaluator.symbol("error"), error_object});
        }
    });
    install(evaluator, "throw", [](const Values& args) -> Values {
        if (args.empty()) throw std::runtime_error("throw expects a tag");
        ValueList arguments(args.begin() + 1, args.end());
        throw ThrownValue(args[0], std::move(arguments));
    });

    install(evaluator, "cons", [&evaluator](const Values& args) {
        require_arity(args, 2, "cons");
        return Values{evaluator.pair(args[0], args[1])};
    });
    install(evaluator, "car", [](const Values& args) {
        require_arity(args, 1, "car");
        if (!args[0].is_object() || args[0].as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("car expects a pair");
        return Values{args[0].as_object<PairObject>()->car};
    });
    install(evaluator, "cdr", [](const Values& args) {
        require_arity(args, 1, "cdr");
        if (!args[0].is_object() || args[0].as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("cdr expects a pair");
        return Values{args[0].as_object<PairObject>()->cdr};
    });
    for (const char* name : {"caar", "cadr", "cdar", "cddr",
                             "caaar", "caadr", "cadar", "caddr",
                             "cdaar", "cdadr", "cddar", "cdddr",
                             "caaaar", "caaadr", "caadar", "caaddr",
                             "cadaar", "cadadr", "caddar", "cadddr",
                             "cdaaar", "cdaadr", "cdadar", "cdaddr",
                             "cddaar", "cddadr", "cdddar", "cddddr"}) {
        install(evaluator, name, [name](const Values& args) {
            require_arity(args, 1, name);
            Value value = args[0];
            const char* last = name + std::strlen(name) - 1;
            for (const char* it = last - 1; it >= name + 1; --it) {
                if (!value.is_object() || value.as_object()->type() != ObjectType::Pair)
                    throw std::runtime_error(std::string(name) +
                                             " expects nested pairs");
                auto* pair = value.as_object<PairObject>();
                value = *it == 'a' ? pair->car : pair->cdr;
            }
            return Values{value};
        });
    }
    install(evaluator, "set-car!", [](const Values& args) {
        require_arity(args, 2, "set-car!");
        if (!args[0].is_object() || args[0].as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("set-car! expects a pair");
        args[0].as_object<PairObject>()->car = args[1];
        return Values{Value::unspecified()};
    });
    install(evaluator, "set-cdr!", [](const Values& args) {
        require_arity(args, 2, "set-cdr!");
        if (!args[0].is_object() || args[0].as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("set-cdr! expects a pair");
        args[0].as_object<PairObject>()->cdr = args[1];
        return Values{Value::unspecified()};
    });
    install(evaluator, "list", [&evaluator](const Values& args) {
        return Values{evaluator.list(args)};
    });
    install(evaluator, "make-vector", [&evaluator](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error("make-vector expects one or two arguments");
        std::int64_t size = args[0].as_integer();
        if (size < 0)
            throw std::runtime_error("make-vector size is negative");
        Value fill = args.size() == 2 ? args[1] : Value::unspecified();
        return Values{evaluator.vector(std::vector<Value>(
            static_cast<std::size_t>(size), fill))};
    });
    install(evaluator, "vector", [&evaluator](const Values& args) {
        return Values{evaluator.vector(args)};
    });
    install(evaluator, "vector-length", [&evaluator](const Values& args) {
        require_arity(args, 1, "vector-length");
        return Values{Value::integer(static_cast<std::int64_t>(
            evaluator.vector_values(args[0]).size()))};
    });
    install(evaluator, "vector-ref", [&evaluator](const Values& args) {
        require_arity(args, 2, "vector-ref");
        auto values = evaluator.vector_values(args[0]);
        std::int64_t index = args[1].as_integer();
        if (index < 0 || static_cast<std::size_t>(index) >= values.size())
            throw std::runtime_error("vector-ref index out of bounds: " +
                                     std::to_string(index) + " / " +
                                     std::to_string(values.size()));
        return Values{values[static_cast<std::size_t>(index)]};
    });
    install(evaluator, "vector-set!", [&evaluator](const Values& args) {
        require_arity(args, 3, "vector-set!");
        auto& values = args[0].as_object<VectorObject>()->values;
        std::int64_t index = args[1].as_integer();
        if (index < 0 || static_cast<std::size_t>(index) >= values.size())
            throw std::runtime_error("vector-set! index out of bounds");
        values[static_cast<std::size_t>(index)] = args[2];
        return Values{Value::unspecified()};
    });

    install(evaluator, "symbol->string", [&evaluator](const Values& args) {
        require_arity(args, 1, "symbol->string");
        return Values{evaluator.string(args[0].as_object<SymbolObject>()->name)};
    });
    install(evaluator, "string->symbol", [&evaluator](const Values& args) {
        require_arity(args, 1, "string->symbol");
        return Values{evaluator.symbol(evaluator.string_value(args[0]))};
    });
    install(evaluator, "number->string", [&evaluator](const Values& args) {
        require_arity(args, 1, "number->string");
        return Values{evaluator.string(std::to_string(args[0].as_integer()))};
    });
    install(evaluator, "string->number", [&evaluator](const Values& args) {
        require_arity(args, 1, "string->number");
        const std::string text = evaluator.string_value(args[0]);
        char* end = nullptr;
        errno = 0;
        const long long result = std::strtoll(text.c_str(), &end, 10);
        if (errno == ERANGE || end != text.c_str() + text.size())
            return Values{Value::boolean(false)};
        return Values{Value::integer(static_cast<std::int64_t>(result))};
    });
    install(evaluator, "string-length", [&evaluator](const Values& args) {
        require_arity(args, 1, "string-length");
        return Values{Value::integer(static_cast<std::int64_t>(
            evaluator.string_value(args[0]).size()))};
    });
    install(evaluator, "string-append", [&evaluator](const Values& args) {
        std::string result;
        for (Value arg : args)
            result += evaluator.string_value(arg);
        return Values{evaluator.string(result)};
    });
    install(evaluator, "string-ref", [&evaluator](const Values& args) {
        require_arity(args, 2, "string-ref");
        const std::string& value = evaluator.string_value(args[0]);
        std::int64_t index = args[1].as_integer();
        if (index < 0 || static_cast<std::size_t>(index) >= value.size())
            throw std::runtime_error("string-ref index out of bounds");
        return Values{evaluator.character(
            static_cast<unsigned char>(value[static_cast<std::size_t>(index)]))};
    });
    install(evaluator, "char->integer", [&evaluator](const Values& args) {
        require_arity(args, 1, "char->integer");
        return Values{Value::integer(static_cast<std::int64_t>(
            evaluator.character_value(args[0])))};
    });
    install(evaluator, "integer->char", [&evaluator](const Values& args) {
        require_arity(args, 1, "integer->char");
        return Values{evaluator.character(static_cast<char32_t>(
            args[0].as_integer()))};
    });
    install(evaluator, "char?", [](const Values& args) {
        require_arity(args, 1, "char?");
        return Values{Value::boolean(args[0].is_object() &&
                                     args[0].as_object()->type() == ObjectType::Character)};
    });

    install(evaluator, "+", [](const Values& args) {
        std::int64_t result = 0;
        for (Value arg : args) result += arg.as_integer();
        return Values{Value::integer(result)};
    });
    install(evaluator, "-", [](const Values& args) {
        if (args.empty()) throw std::runtime_error("- expects arguments");
        std::int64_t result = args[0].as_integer();
        if (args.size() == 1) result = -result;
        for (std::size_t i = 1; i < args.size(); ++i) result -= args[i].as_integer();
        return Values{Value::integer(result)};
    });
    install(evaluator, "*", [](const Values& args) {
        std::int64_t result = 1;
        for (Value arg : args) result *= arg.as_integer();
        return Values{Value::integer(result)};
    });
    install(evaluator, "=", [](const Values& args) {
        if (args.size() < 2) throw std::runtime_error("= expects two arguments");
        for (std::size_t i = 1; i < args.size(); ++i)
            if (args[i].as_integer() != args[0].as_integer()) return Values{Value::boolean(false)};
        return Values{Value::boolean(true)};
    });
    install(evaluator, "modulo", [](const Values& args) {
        require_arity(args, 2, "modulo");
        std::int64_t divisor = args[1].as_integer();
        if (divisor == 0)
            throw std::runtime_error("modulo divisor is zero");
        std::int64_t result = args[0].as_integer() % divisor;
        if (result < 0)
            result += std::llabs(divisor);
        return Values{Value::integer(result)};
    });
    install(evaluator, "positive?", [](const Values& args) {
        require_arity(args, 1, "positive?");
        return Values{Value::boolean(args[0].as_integer() > 0)};
    });
    install(evaluator, "zero?", [](const Values& args) {
        require_arity(args, 1, "zero?");
        return Values{Value::boolean(args[0].as_integer() == 0)};
    });
    install(evaluator, "quotient", [](const Values& args) {
        require_arity(args, 2, "quotient");
        if (args[1].as_integer() == 0) throw std::runtime_error("quotient divisor is zero");
        return Values{Value::integer(args[0].as_integer() / args[1].as_integer())};
    });
    install(evaluator, "remainder", [](const Values& args) {
        require_arity(args, 2, "remainder");
        if (args[1].as_integer() == 0) throw std::runtime_error("remainder divisor is zero");
        return Values{Value::integer(args[0].as_integer() % args[1].as_integer())};
    });
    install(evaluator, "/", [](const Values& args) {
        if (args.empty()) throw std::runtime_error("/ expects arguments");
        std::int64_t result = args[0].as_integer();
        if (args.size() == 1) {
            if (result == 0) throw std::runtime_error("division by zero");
            return Values{Value::integer(1 / result)};
        }
        for (std::size_t i = 1; i < args.size(); ++i) {
            auto divisor = args[i].as_integer();
            if (divisor == 0) throw std::runtime_error("division by zero");
            result /= divisor;
        }
        return Values{Value::integer(result)};
    });
    auto install_comparison = [&evaluator](const char* name,
                                           bool (*compare)(std::int64_t, std::int64_t)) {
        install(evaluator, name, [name, compare](const Values& args) {
            if (args.size() < 2) throw std::runtime_error(std::string(name) + " expects two arguments");
            for (std::size_t i = 1; i < args.size(); ++i)
                if (!compare(args[i - 1].as_integer(), args[i].as_integer()))
                    return Values{Value::boolean(false)};
            return Values{Value::boolean(true)};
        });
    };
    install_comparison("<", [](auto a, auto b) { return a < b; });
    install_comparison(">", [](auto a, auto b) { return a > b; });
    install_comparison("<=", [](auto a, auto b) { return a <= b; });
    install_comparison(">=", [](auto a, auto b) { return a >= b; });
}

} // namespace goldfish::runtime
