#include "runtime/standard_primitives.hpp"

#include "runtime/reader.hpp"
#include "runtime/platform_primitives.hpp"
#include "runtime/unicode_primitives.hpp"

#include <algorithm>
#include <cerrno>
#include <cmath>
#include <cstring>
#include <cstdlib>
#include <fstream>
#include <filesystem>
#include <iostream>
#include <iterator>
#include <limits>
#include <numeric>
#include <sstream>
#include <stdexcept>

namespace goldfish::runtime {

namespace fs = std::filesystem;

namespace {

void require_arity(const Values& args, std::size_t count,
                   const char* name) {
    if (args.size() != count)
        throw std::runtime_error(std::string(name) + " expects " +
                                 std::to_string(count) + " arguments");
}

bool same(Value left, Value right) { return left == right; }

bool symbol_named(Value value, const char* name) {
    return value.is_object() &&
           value.as_object()->type() == ObjectType::Symbol &&
           value.as_object<SymbolObject>()->name == name;
}

std::string raised_message(const RaisedValue& raised) {
    Value value = raised.value();
    if (value.is_object() &&
        value.as_object()->type() == ObjectType::ErrorObject) {
        const auto* error = value.as_object<ErrorObject>();
        std::string message = error->message;
        for (Value irritant : error->irritants) {
            message += " ";
            if (irritant.is_integer())
                message += std::to_string(irritant.as_integer());
            else if (irritant.is_object() &&
                     irritant.as_object()->type() == ObjectType::String)
                message += irritant.as_object<StringObject>()->value;
            else if (irritant.is_object() &&
                     irritant.as_object()->type() == ObjectType::Symbol)
                message += irritant.as_object<SymbolObject>()->name;
            else
                message += "<value>";
        }
        return message;
    }
    return "raised Scheme value";
}

std::vector<Value> proper_list(Value value) {
    std::vector<Value> result;
    while (!value.is_null()) {
        if (!value.is_object() ||
            value.as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("expected proper list");
        auto* pair = value.as_object<PairObject>();
        result.push_back(pair->car);
        value = pair->cdr;
    }
    return result;
}

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

InputStringPortObject& input_port(Value value, const char* name) {
    if (!value.is_object() || value.as_object()->type() != ObjectType::InputPort)
        throw std::runtime_error(std::string(name) + " expects an input port");
    auto& port = *value.as_object<InputStringPortObject>();
    if (port.closed)
        throw std::runtime_error(std::string(name) + " from closed input port");
    return port;
}

bool source_delimiter(unsigned char c) {
    return std::isspace(c) || c == '(' || c == ')' || c == '[' || c == ']' ||
           c == '"' || c == ';' || c == '.';
}

void append_utf8(std::string& output, unsigned value) {
    if (value <= 0x7f) output.push_back(static_cast<char>(value));
    else if (value <= 0x7ff) {
        output.push_back(static_cast<char>(0xc0 | (value >> 6)));
        output.push_back(static_cast<char>(0x80 | (value & 0x3f)));
    } else if (value <= 0xffff) {
        output.push_back(static_cast<char>(0xe0 | (value >> 12)));
        output.push_back(static_cast<char>(0x80 | ((value >> 6) & 0x3f)));
        output.push_back(static_cast<char>(0x80 | (value & 0x3f)));
    } else {
        output.push_back(static_cast<char>(0xf0 | (value >> 18)));
        output.push_back(static_cast<char>(0x80 | ((value >> 12) & 0x3f)));
        output.push_back(static_cast<char>(0x80 | ((value >> 6) & 0x3f)));
        output.push_back(static_cast<char>(0x80 | (value & 0x3f)));
    }
}

std::string format_value(const Evaluator& evaluator, Value value) {
    if (value.is_integer()) return std::to_string(value.as_integer());
    if (value.is_boolean()) return value.as_boolean() ? "#t" : "#f";
    if (value.is_null()) return "()";
    if (value.is_object() &&
        value.as_object()->type() == ObjectType::String)
        return evaluator.string_value(value);
    if (value.is_object() &&
        value.as_object()->type() == ObjectType::Symbol)
        return value.as_object<SymbolObject>()->name;
    if (value.is_object() && value.as_object()->type() == ObjectType::Pair) {
        const auto* pair = value.as_object<PairObject>();
        return "(" + format_value(evaluator, pair->car) + " . " +
               format_value(evaluator, pair->cdr) + ")";
    }
    return "#<object>";
}

void install(Evaluator& evaluator, const char* name,
             PrimitiveObject::Function function) {
    evaluator.define_primitive(name, std::move(function));
}

OutputPortObject& output_port(Value value, const char* name) {
    if (!value.is_object() || value.as_object()->type() != ObjectType::OutputPort)
        throw std::runtime_error(std::string(name) + " expects an output port");
    auto& port = *value.as_object<OutputPortObject>();
    if (port.closed)
        throw std::runtime_error(std::string(name) + " on closed output port");
    return port;
}

} // namespace

void install_runtime_primitives(Evaluator& evaluator) {
    // Platform capability adapters are installed as a separate layer.
    install_platform_primitives(evaluator);

    // Ports and textual output are runtime objects; formatting policy stays
    // in Scheme libraries.
    auto stdout_stream = std::shared_ptr<std::ostream>(&std::cout,
                                                       [](std::ostream*) {});
    Value current_output = Value::object(
        evaluator.heap().make<OutputPortObject>(stdout_stream));
    auto write_text = [&evaluator, current_output](const Values& args,
                                                   const char* name) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error(std::string(name) +
                                     " expects one or two arguments");
        Value port_value = args.size() == 2 ? args[1] : current_output;
        *output_port(port_value, name).stream
            << format_value(evaluator, args[0]);
        return Values{Value::unspecified()};
    };
    install(evaluator, "display",
            [write_text](const Values& args) {
                return write_text(args, "display");
            });
    install(evaluator, "write",
            [write_text](const Values& args) {
                return write_text(args, "write");
            });
    install(evaluator, "write-shared",
            [write_text](const Values& args) {
                return write_text(args, "write-shared");
            });
    install(evaluator, "write-simple",
            [write_text](const Values& args) {
                return write_text(args, "write-simple");
            });
    install(evaluator, "newline", [](const Values& args) {
        if (args.size() > 1)
            throw std::runtime_error("newline expects zero or one arguments");
        if (args.empty()) std::cout << '\n';
        else *output_port(args[0], "newline").stream << '\n';
        return Values{Value::unspecified()};
    });
    install(evaluator, "write-char", [&evaluator, current_output](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error("write-char expects one or two arguments");
        Value port = args.size() == 2 ? args[1] : current_output;
        *output_port(port, "write-char").stream
            << static_cast<char>(evaluator.character_value(args[0]));
        return Values{Value::unspecified()};
    });
    install(evaluator, "write-string", [&evaluator, current_output](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error("write-string expects one or two arguments");
        Value port = args.size() == 2 ? args[1] : current_output;
        *output_port(port, "write-string").stream
            << evaluator.string_value(args[0]);
        return Values{Value::unspecified()};
    });
    install(evaluator, "native-error-object?", [](const Values& args) {
        require_arity(args, 1, "native-error-object?");
        return Values{Value::boolean(
            args[0].is_object() &&
            args[0].as_object()->type() == ObjectType::ErrorObject)};
    });
    install(evaluator, "native-error-object-message", [&evaluator](const Values& args) {
        require_arity(args, 1, "native-error-object-message");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::ErrorObject)
            throw std::runtime_error(
                "native-error-object-message expects an error object");
        return Values{evaluator.string(
            args[0].as_object<ErrorObject>()->message)};
    });
    // Integer atoms used by the kernel and by the bootstrap libraries.
    install(evaluator, "lognot", [](const Values& args) {
        require_arity(args, 1, "lognot");
        if (!args[0].is_integer())
            throw std::runtime_error("lognot expects an integer");
        return Values{Value::integer(~args[0].as_integer())};
    });
    for (const char* name : {"logand", "logior", "logxor"}) {
        install(evaluator, name, [name](const Values& args) {
            if (args.empty())
                throw std::runtime_error(std::string(name) +
                                         " expects at least one argument");
            for (Value value : args)
                if (!value.is_integer())
                    throw std::runtime_error(std::string(name) +
                                             " expects integers");
            std::int64_t result = args[0].as_integer();
            for (std::size_t i = 1; i < args.size(); ++i) {
                const std::int64_t value = args[i].as_integer();
                if (std::string(name) == "logand") result &= value;
                else if (std::string(name) == "logior") result |= value;
                else result ^= value;
            }
            return Values{Value::integer(result)};
        });
    }
    install(evaluator, "ash", [](const Values& args) {
        require_arity(args, 2, "ash");
        if (!args[0].is_integer() || !args[1].is_integer())
            throw std::runtime_error("ash expects integers");
        const std::int64_t value = args[0].as_integer();
        const std::int64_t shift = args[1].as_integer();
        if (shift >= 0) {
            if (shift >= 63) return Values{Value::integer(0)};
            return Values{Value::integer(static_cast<std::int64_t>(
                static_cast<std::uint64_t>(value) << shift))};
        }
        const std::int64_t amount = shift == std::numeric_limits<std::int64_t>::min()
                                        ? 63 : -shift;
        if (amount >= 63)
            return Values{Value::integer(value < 0 ? -1 : 0)};
        if (value >= 0)
            return Values{Value::integer(value >> amount)};
        const std::uint64_t magnitude =
            static_cast<std::uint64_t>(-(value + 1)) + 1;
        const std::uint64_t rounded =
            (magnitude >> amount) +
            ((magnitude & ((std::uint64_t{1} << amount) - 1)) != 0);
        return Values{Value::integer(-static_cast<std::int64_t>(rounded))};
    });
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
    // Explicit evaluation environments.  Legacy inlet support is installed
    // later by migration_primitives.cpp, not by this runtime layer.
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
    install(evaluator, "symbol->value", [&evaluator](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error("symbol->value expects one or two arguments");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::Symbol)
            throw std::runtime_error("symbol->value expects a symbol");
        if (args.size() == 2 &&
            (!args[1].is_object() ||
             args[1].as_object()->type() != ObjectType::EvalEnvironment))
            throw std::runtime_error("symbol->value expects an environment");
        try {
            if (args.size() == 2)
                return Values{args[1].as_object<EvalEnvironmentObject>()
                                  ->environment->lookup(args[0])};
            return Values{evaluator.global_environment()->lookup(args[0])};
        } catch (const std::runtime_error&) {
            throw std::runtime_error("unbound symbol");
        }
    });
    install(evaluator, "make-eval-environment",
            [&evaluator](const Values& args) {
                if (args.size() > 1)
                    throw std::runtime_error(
                        "make-eval-environment expects zero or one argument");
                if (args.empty())
                    return Values{evaluator.make_eval_environment()};
                if (!args[0].is_object() ||
                    args[0].as_object()->type() != ObjectType::EvalEnvironment)
                    throw std::runtime_error(
                        "make-eval-environment expects an eval environment parent");
                return Values{evaluator.make_eval_environment(
                    args[0].as_object<EvalEnvironmentObject>()->environment)};
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
            if (args[1].is_object() &&
                args[1].as_object()->type() == ObjectType::EvalEnvironment)
                environment =
                    args[1].as_object<EvalEnvironmentObject>()->environment;
            else
                throw std::runtime_error(
                    "eval expects an eval environment as its second argument");
        }
        return evaluator.eval_values(args[0], std::move(environment));
    });
    // Private alias for the runtime eval primitive.  (scheme eval) resolves
    // this name instead of `eval', so importing a user-level eval cannot
    // rebind the host evaluator into a recursive loop.
    evaluator.define_primitive("%host-eval", [&evaluator](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error("eval expects one or two arguments");
        EnvironmentPtr environment = evaluator.global_environment();
        if (args.size() == 2) {
            if (args[1].is_object() &&
                args[1].as_object()->type() == ObjectType::EvalEnvironment)
                environment =
                    args[1].as_object<EvalEnvironmentObject>()->environment;
            else
                throw std::runtime_error(
                    "eval expects an eval environment as its second argument");
        }
        return evaluator.eval_values(args[0], std::move(environment));
    });
    install(evaluator, "dynamic-wind", [&evaluator](const Values& args) {
        require_arity(args, 3, "dynamic-wind");
        evaluator.apply_values(args[0], {});
        try {
            Values result = evaluator.apply_values(args[1], {});
            evaluator.apply_values(args[2], {});
            return result;
        } catch (...) {
            evaluator.apply_values(args[2], {});
            throw;
        }
    });
    // Input/output ports and the tiny reader boundary.
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
    install(evaluator, "open-input-file", [&evaluator](const Values& args) {
        require_arity(args, 1, "open-input-file");
        const std::string path = evaluator.string_value(args[0]);
        std::ifstream input(path, std::ios::binary);
        if (!input)
            throw std::runtime_error("cannot open input file: " + path);
        std::string source((std::istreambuf_iterator<char>(input)),
                           std::istreambuf_iterator<char>());
        return Values{Value::object(
            evaluator.heap().make<InputStringPortObject>(std::move(source)))};
    });
    install(evaluator, "open-output-file", [&evaluator](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error("open-output-file expects one or two arguments");
        const std::string path = evaluator.string_value(args[0]);
        const bool append = args.size() == 2 && evaluator.string_value(args[1]) == "a";
        auto file = std::make_shared<std::ofstream>(
            path, std::ios::binary | (append ? std::ios::app : std::ios::trunc));
        if (!*file) throw std::runtime_error("cannot open output file: " + path);
        std::shared_ptr<std::ostream> stream = file;
        return Values{Value::object(
            evaluator.heap().make<OutputPortObject>(std::move(stream)))};
    });
    install(evaluator, "open-output-string", [&evaluator](const Values& args) {
        require_arity(args, 0, "open-output-string");
        auto buffer = std::make_shared<std::string>();
        auto stream = std::make_shared<std::ostringstream>();
        return Values{Value::object(evaluator.heap().make<OutputPortObject>(
            std::move(stream), std::move(buffer)))};
    });
    install(evaluator, "output-port?", [](const Values& args) {
        require_arity(args, 1, "output-port?");
        return Values{Value::boolean(args[0].is_object() &&
            args[0].as_object()->type() == ObjectType::OutputPort)};
    });
    install(evaluator, "current-output-port", [current_output](const Values& args) {
        require_arity(args, 0, "current-output-port");
        return Values{current_output};
    });
    install(evaluator, "close-output-port", [](const Values& args) {
        require_arity(args, 1, "close-output-port");
        auto& port = output_port(args[0], "close-output-port");
        port.stream->flush();
        port.closed = true;
        return Values{Value::unspecified()};
    });
    install(evaluator, "flush-output-port", [](const Values& args) {
        require_arity(args, 1, "flush-output-port");
        output_port(args[0], "flush-output-port").stream->flush();
        return Values{Value::unspecified()};
    });
    install(evaluator, "input-port?", [](const Values& args) {
        require_arity(args, 1, "input-port?");
        return Values{Value::boolean(
            args[0].is_object() &&
            args[0].as_object()->type() == ObjectType::InputPort)};
    });
    install(evaluator, "close-input-port", [](const Values& args) {
        require_arity(args, 1, "close-input-port");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::InputPort)
            throw std::runtime_error("close-input-port expects an input port");
        args[0].as_object<InputStringPortObject>()->closed = true;
        return Values{Value::unspecified()};
    });
    install(evaluator, "file-exists?", [&evaluator](const Values& args) {
        require_arity(args, 1, "file-exists?");
        std::ifstream input(evaluator.string_value(args[0]), std::ios::binary);
        return Values{Value::boolean(static_cast<bool>(input))};
    });
    install(evaluator, "load-find-module-file", [&evaluator](const Values& args) {
        require_arity(args, 1, "load-find-module-file");
        const std::string requested = evaluator.string_value(args[0]);
        std::vector<std::string> candidates = {requested, "goldfish/" + requested};
        if (const char* search_path = std::getenv("GOLDFISH_NATIVE_LOAD_PATH")) {
            std::stringstream paths(search_path);
            std::string directory;
            while (std::getline(paths, directory, ':'))
                if (!directory.empty())
                    candidates.push_back((fs::path(directory) / requested).string());
        }
        for (const std::string& path : candidates) {
            std::ifstream input(path, std::ios::binary);
            if (input) return Values{evaluator.string(path)};
        }
        return Values{Value::boolean(false)};
    });
    install(evaluator, "read-forms", [&evaluator](const Values& args) {
        require_arity(args, 1, "read-forms");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::InputPort)
            throw std::runtime_error("read-forms expects an input port");
        auto* port = args[0].as_object<InputStringPortObject>();
        if (port->closed)
            throw std::runtime_error("read-forms from closed input port");
        std::vector<Value> forms;
        while (port->position < port->source.size()) {
            TinyReader reader(evaluator,
                              port->source.substr(port->position));
            std::optional<Value> form = reader.read();
            port->position += reader.position();
            if (!form) break;
            forms.push_back(*form);
        }
        return Values{evaluator.list(forms)};
    });
    install(evaluator, "g-tiny-read", [&evaluator, eof](const Values& args) {
        require_arity(args, 1, "g-tiny-read");
        auto& port = input_port(args[0], "g-tiny-read");
        if (port.position == port.source.size()) return Values{eof};
        TinyReader reader(evaluator, port.source.substr(port.position));
        std::optional<Value> value = reader.read();
        port.position += reader.position();
        return Values{value ? *value : eof};
    });
    // Bootstrap-only source entry.  Once the expander is installed, source
    // forms must pass through expand-eval; evaluating raw forms here would
    // make ordinary derived forms (and, in particular, `and') look like
    // missing primitives.  Before expand-eval exists this remains the seed
    // evaluator used to bring the loader itself up.  Do not route this
    // through compile-file: compile-file itself is part of the expander
    // bootstrap and doing so creates a recursive loader dependency.
    install(evaluator, "load-source-file", [&evaluator](const Values& args) {
        require_arity(args, 1, "load-source-file");
        const std::string requested = evaluator.string_value(args[0]);
        std::string path = requested;
        std::ifstream input(path, std::ios::binary);
        if (!input) {
            path = "goldfish/" + requested;
            input.clear();
            input.open(path, std::ios::binary);
        }
        if (!input) {
            if (const char* search_path = std::getenv("GOLDFISH_NATIVE_LOAD_PATH")) {
                std::stringstream paths(search_path);
                std::string directory;
                while (!input && std::getline(paths, directory, ':')) {
                    if (directory.empty()) continue;
                    path = (fs::path(directory) / requested).string();
                    input.clear();
                    input.open(path, std::ios::binary);
                }
            }
        }
        if (!input)
            throw std::runtime_error("load-source-file: cannot open " + requested);
        std::string source((std::istreambuf_iterator<char>(input)),
                           std::istreambuf_iterator<char>());
        const bool seed_file = requested == "core/gfo.scm" ||
                               path == "goldfish/core/gfo.scm";
        const bool prelude_file = requested == "liii/prelude.scm" ||
                                  path == "goldfish/liii/prelude.scm";
        auto lookup_bootstrap_binding = [&evaluator](const char* name) {
            try {
                return evaluator.global_environment()->lookup(
                    evaluator.symbol(name));
            } catch (const std::runtime_error&) {
                Value expander = evaluator.global_environment()->lookup(
                    evaluator.symbol("the-expander-library"));
                Value module_environment = evaluator.apply_values(
                    evaluator.global_environment()->lookup(
                        evaluator.symbol("module-eval-environment")),
                    {expander})[0];
                return module_environment.as_object<EvalEnvironmentObject>()
                    ->environment->lookup(evaluator.symbol(name));
            }
        };
        Value expand_eval;
        try {
            expand_eval = lookup_bootstrap_binding("expand-eval");
        } catch (const std::runtime_error&) {
            expand_eval = Value::unspecified();
        }
        try {
            std::vector<Value> datums;
            TinyReader reader(evaluator, std::move(source));
            while (std::optional<Value> form = reader.read())
                datums.push_back(*form);
            if (prelude_file && !expand_eval.is_unspecified()) {
                // Prelude transformers are intentionally installed into the
                // base library.  Process them one at a time so each macro is
                // visible to the next transformer body; the normal source
                // unit path uses a temporary library instead.
                Value compile_toplevel = lookup_bootstrap_binding(
                    "compile-toplevel");
                Value result = Value::unspecified();
                for (std::size_t index = 0; index < datums.size(); ++index) {
                    try {
                        Value lowered = evaluator.apply_values(
                            compile_toplevel, {datums[index]})[0];
                        result = evaluator.eval(lowered);
                    } catch (const std::exception& error) {
                        throw std::runtime_error(
                            "prelude form " + std::to_string(index) + ": " +
                            error.what());
                    }
                }
                return Values{result};
            }
            if (seed_file && !expand_eval.is_unspecified()) {
                // core/gfo.scm is the one source file that must establish the
                // cache layer itself.  Install it as a real exp-library,
                // linked explicitly to the base library, then publish its
                // value definitions into the root evaluator.  In particular,
                // do not evaluate one raw `define' at a time: that loses the
                // library's expansion context and makes recursive helpers
                // resolve through the wrong environment.
                Value make_exp_library =
                    lookup_bootstrap_binding("make-exp-library");
                Value seed_library = evaluator.apply_values(
                    make_exp_library,
                    {evaluator.list({evaluator.symbol("gfo-seed")})})[0];
                Value base_library = evaluator.global_environment()->lookup(
                    evaluator.symbol("the-base-library"));
                Value add_use = lookup_bootstrap_binding("exp-library-add-use!");
                evaluator.apply_values(add_use, {seed_library, base_library});

                Value wrap_expression =
                    lookup_bootstrap_binding("wrap-expression");
                Value set_library = lookup_bootstrap_binding("stx-set-library");
                Value initial_context =
                    lookup_bootstrap_binding("initial-context");
                Value expand_library_body =
                    lookup_bootstrap_binding("expand-library-body");
                Value lower = lookup_bootstrap_binding("lower");
                std::vector<Value> syntax_forms;
                for (Value datum : datums) {
                    Value syntax = evaluator.apply_values(
                        wrap_expression, {datum})[0];
                    syntax_forms.push_back(evaluator.apply_values(
                        set_library, {syntax, seed_library})[0]);
                }
                Value context = evaluator.apply_values(initial_context, {})[0];
                Values expanded = evaluator.apply_values(
                    expand_library_body,
                    {evaluator.list(syntax_forms), seed_library, context});
                if (expanded.empty())
                    throw std::runtime_error("seed expansion returned no definitions");
                Value definitions = expanded[0];
                Value module_environment = evaluator.apply_values(
                    lookup_bootstrap_binding("module-eval-environment"),
                    {evaluator.global_environment()->lookup(
                        evaluator.symbol("the-expander-library"))})[0];
                Value binding_kind = lookup_bootstrap_binding("binding-kind");
                Value binding_value = lookup_bootstrap_binding("binding-value");
                Value toplevel_ref =
                    lookup_bootstrap_binding("toplevel-ref-gensym");
                Value bindings = evaluator.apply_values(
                    lookup_bootstrap_binding("exp-library-bindings"),
                    {seed_library})[0];
                EnvironmentPtr eval_environment =
                    module_environment.as_object<EvalEnvironmentObject>()
                        ->environment;
                Value result = Value::unspecified();
                std::size_t seed_definition_index = 0;
                for (Value definition : proper_list(definitions)) {
                    try {
                        result = evaluator.apply_values(lower, {definition})[0];
                        result = evaluator.eval(result, eval_environment);
                    } catch (const std::exception& error) {
                        throw std::runtime_error(
                            "seed definition " +
                            std::to_string(seed_definition_index) + ": " +
                            error.what());
                    }
                    ++seed_definition_index;
                }
                for (Value entry : proper_list(bindings)) {
                    if (!entry.is_object() ||
                        entry.as_object()->type() != ObjectType::Pair)
                        continue;
                    auto* pair = entry.as_object<PairObject>();
                    Value binding = pair->cdr;
                    if (!symbol_named(
                            evaluator.apply_values(binding_kind, {binding})[0],
                            "toplevel"))
                        continue;
                    Value reference = evaluator.apply_values(
                        binding_value, {binding})[0];
                    Value gensym = evaluator.apply_values(
                        toplevel_ref, {reference})[0];
                    evaluator.global_environment()->define(
                        pair->car, evaluator.eval(gensym, eval_environment));
                }
                return Values{result};
            }
            const bool program_source =
                requested == "goldfish/expander/build-combined.scm" ||
                path == "goldfish/expander/build-combined.scm";
            if (program_source && !expand_eval.is_unspecified()) {
                Value import_form = evaluator.list({
                    evaluator.symbol("import"),
                    evaluator.list({evaluator.symbol("except"),
                                    evaluator.list({evaluator.symbol("goldfish")}),
                                    evaluator.symbol("bytevector?"),
                                    evaluator.symbol("make-bytevector"),
                                    evaluator.symbol("bytevector"),
                                    evaluator.symbol("bytevector-length"),
                                    evaluator.symbol("bytevector-u8-ref"),
                                    evaluator.symbol("bytevector-u8-set!"),
                                    evaluator.symbol("bytevector-copy"),
                                    evaluator.symbol("bytevector-copy!"),
                                    evaluator.symbol("bytevector-append"),
                                    evaluator.symbol("bytevector->u8-list"),
                                    evaluator.symbol("u8-list->bytevector"),
                                    evaluator.symbol("utf8->string"),
                                    evaluator.symbol("string->utf8"),
                                    evaluator.symbol("bytevector-advance-utf8"),
                                    evaluator.symbol("utf8-string-length")})});
                evaluator.apply_values(expand_eval, {import_form});
                Value result = Value::unspecified();
                for (Value datum : datums)
                    result = evaluator.apply_values(expand_eval, {datum})[0];
                return Values{result};
            }
            if (!expand_eval.is_unspecified()) {
                // Bootstrap source files are programs rather than declared
                // libraries, but their definitions still need one shared
                // expansion unit so forward references resolve together.
                Value make_exp_library = lookup_bootstrap_binding(
                    "make-exp-library");
                Value base_library = evaluator.global_environment()->lookup(
                    evaluator.symbol("the-base-library"));
                const bool seed_source = requested == "liii/prelude.scm" ||
                    path == "goldfish/liii/prelude.scm" ||
                    requested == "expander/bootstrap-prelude.scm" ||
                    path == "goldfish/expander/bootstrap-prelude.scm";
                const bool internal_source = seed_source ||
                    requested == "expander/lib/install.scm" ||
                    path.find("goldfish/expander/lib/") == 0;
                Value source_library = seed_source
                    ? base_library
                    : internal_source
                        ? evaluator.apply_values(
                              make_exp_library,
                              {evaluator.list({evaluator.symbol("native-source")})})[0]
                        : evaluator.apply_values(
                              lookup_bootstrap_binding("make-program-library"), {})[0];
                if (!internal_source && !seed_source) {
                    // A loaded source file is a program body.  Its leading
                    // import declarations belong to the program library,
                    // not to the expression pass; remove them after
                    // applying the same Scheme import-set machinery used by
                    // the expander.
                    Value import_symbol = evaluator.symbol("import");
                    Value import_into = evaluator.apply_values(
                        lookup_bootstrap_binding("module-ref"),
                        {evaluator.global_environment()->lookup(
                             evaluator.symbol("the-expander-library")),
                         evaluator.symbol("import-into-library!")})[0];
                    std::vector<Value> body_datums;
                    for (Value datum : datums) {
                        std::vector<Value> form;
                        if (datum.is_object() &&
                            datum.as_object()->type() == ObjectType::Pair)
                            form = proper_list(datum);
                        if (!form.empty() && form[0] == import_symbol) {
                            try {
                                evaluator.apply_values(import_into,
                                                       {source_library,
                                                        evaluator.list({
                                                            evaluator.list(
                                                                std::vector<Value>(
                                                                    form.begin() + 1,
                                                                    form.end()))})});
                            } catch (const RaisedValue& raised) {
                                throw std::runtime_error(
                                    std::string("source import: ") +
                                    raised_message(raised));
                            }
                        } else {
                            body_datums.push_back(datum);
                        }
                    }
                    datums = std::move(body_datums);
                }
                if (!seed_source)
                    evaluator.apply_values(
                        lookup_bootstrap_binding("exp-library-add-use!"),
                        {source_library, base_library});
                // Source bootstrap must not inherit a reader binding from an
                // older cached reader artifact.  Pin this one dependency at
                // the source unit boundary; ordinary programs still use the
                // Scheme reader through their normal imports.
                Value native_read_forms = evaluator.apply_values(
                    lookup_bootstrap_binding("make-primitive-binding"),
                    {evaluator.symbol("read-forms")})[0];
                evaluator.apply_values(
                    lookup_bootstrap_binding("exp-library-define!"),
                    {source_library, evaluator.symbol("read-forms"),
                     native_read_forms});
                Value wrap_expression = lookup_bootstrap_binding("wrap-expression");
                Value set_library = lookup_bootstrap_binding("stx-set-library");
                std::vector<Value> syntax_forms;
                for (Value datum : datums) {
                    Value syntax = evaluator.apply_values(
                        wrap_expression, {datum})[0];
                    syntax_forms.push_back(evaluator.apply_values(
                        set_library, {syntax, source_library})[0]);
                }
                Values expanded;
                try {
                    expanded = evaluator.apply_values(
                        lookup_bootstrap_binding("expand-library-body"),
                        {evaluator.list(syntax_forms), source_library,
                         evaluator.apply_values(
                             lookup_bootstrap_binding("initial-context"), {})[0]});
                } catch (const RaisedValue& raised) {
                    throw std::runtime_error(std::string("source expansion: ") +
                                             raised_message(raised));
                } catch (const std::exception& error) {
                    throw std::runtime_error(std::string("source expansion: ") +
                                             error.what());
                }
                Value definitions = expanded[0];
                Value module_environment = evaluator.apply_values(
                    lookup_bootstrap_binding("module-eval-environment"),
                    {evaluator.global_environment()->lookup(
                        evaluator.symbol("the-expander-library"))})[0];
                EnvironmentPtr eval_environment =
                    module_environment.as_object<EvalEnvironmentObject>()
                        ->environment;
                Value result = Value::unspecified();
                Value lower = lookup_bootstrap_binding("lower");
                std::size_t definition_index = 0;
                for (Value definition : proper_list(definitions)) {
                    try {
                        Value lowered_definition =
                            evaluator.apply_values(lower, {definition})[0];
                        result = evaluator.eval(lowered_definition, eval_environment);
                    } catch (const RaisedValue& raised) {
                        std::string detail = "raised a Scheme error";
                        if (raised.value().is_object() &&
                            raised.value().as_object()->type() == ObjectType::ErrorObject) {
                            const auto* error =
                                raised.value().as_object<ErrorObject>();
                            detail = error->message;
                            for (Value irritant : error->irritants) {
                                detail += " ";
                                detail += format_value(evaluator, irritant);
                            }
                        }
                        throw std::runtime_error(
                            "source definition " +
                            std::to_string(definition_index) + ": " + detail);
                    } catch (const std::exception& error) {
                        throw std::runtime_error(
                            "source definition " +
                            std::to_string(definition_index) + ": " +
                            error.what());
                    }
                    ++definition_index;
                }
                Value source_bindings = evaluator.apply_values(
                     lookup_bootstrap_binding("exp-library-bindings"),
                         {source_library})[0];
                for (Value entry : proper_list(source_bindings)) {
                    if (!entry.is_object() ||
                        entry.as_object()->type() != ObjectType::Pair)
                        continue;
                    Value name = entry.as_object<PairObject>()->car;
                    Value binding = entry.as_object<PairObject>()->cdr;
                    if (evaluator.apply_values(
                            lookup_bootstrap_binding("binding-kind"),
                            {binding})[0].as_object<SymbolObject>()->name !=
                        "toplevel")
                        continue;
                    Value reference = evaluator.apply_values(
                        lookup_bootstrap_binding("binding-value"), {binding})[0];
                    Value gensym = evaluator.apply_values(
                        lookup_bootstrap_binding("toplevel-ref-gensym"),
                        {reference})[0];
                    try {
                        evaluator.global_environment()->define(
                            name, evaluator.eval(gensym, eval_environment));
                    } catch (const std::runtime_error& error) {
                        throw std::runtime_error(
                            "source binding alias " +
                            name.as_object<SymbolObject>()->name + ": " +
                            error.what());
                    }
                }
                return Values{result};
            }
            Value result = Value::unspecified();
            for (std::size_t index = 0; index < datums.size(); ++index) {
                try {
                    result = evaluator.eval(datums[index]);
                } catch (const RaisedValue& raised) {
                    std::string detail = "raised a Scheme error";
                    if (raised.value().is_object() &&
                        raised.value().as_object()->type() == ObjectType::ErrorObject)
                        {
                            const auto* error =
                                raised.value().as_object<ErrorObject>();
                            detail = error->message;
                            for (Value irritant : error->irritants) {
                                detail += " ";
                                if (irritant.is_object() &&
                                    irritant.as_object()->type() == ObjectType::Symbol)
                                    detail += irritant.as_object<SymbolObject>()->name;
                                else if (irritant.is_object() &&
                                         irritant.as_object()->type() == ObjectType::String)
                                    detail += evaluator.string_value(irritant);
                                else if (irritant.is_integer())
                                    detail += std::to_string(irritant.as_integer());
                                else
                                    detail += "<value>";
                            }
                        }
                    throw std::runtime_error("source form " +
                                             std::to_string(index) + ": " +
                                             detail);
                }
            }
            return Values{result};
        } catch (const RaisedValue& raised) {
            throw std::runtime_error("load-source-file: evaluated " + path +
                                     ": " + raised_message(raised));
        } catch (const std::exception& error) {
            throw std::runtime_error("load-source-file: evaluated " + path +
                                     ": " + error.what());
        }
    });
    // Source bootstrap helpers.  They are deliberately primitive operations;
    // source policy and compilation remain in the Scheme expander.
    install(evaluator, "g-read-token", [&evaluator](const Values& args) {
        require_arity(args, 2, "g-read-token");
        auto& port = input_port(args[0], "g-read-token");
        char32_t first = evaluator.character_value(args[1]);
        std::string token;
        if (first <= 0x7f) token.push_back(static_cast<char>(first));
        else append_utf8(token, static_cast<unsigned>(first));
        while (port.position < port.source.size() &&
               !source_delimiter(static_cast<unsigned char>(port.source[port.position])))
            token.push_back(port.source[port.position++]);
        return Values{evaluator.string(token)};
    });
    install(evaluator, "g-read-string", [&evaluator](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error("g-read-string expects one or two arguments");
        auto& port = input_port(args[0], "g-read-string");
        char32_t delimiter = args.size() == 2 ? evaluator.character_value(args[1]) : U'"';
        std::string result;
        while (port.position < port.source.size()) {
            unsigned char c = static_cast<unsigned char>(port.source[port.position++]);
            if (c == delimiter) return Values{evaluator.string(result)};
            if (c != '\\') { result.push_back(static_cast<char>(c)); continue; }
            if (port.position == port.source.size())
                throw std::runtime_error("g-read-string: unterminated escape");
            unsigned char escaped = static_cast<unsigned char>(port.source[port.position++]);
            switch (escaped) {
            case 'a': result.push_back('\a'); break;
            case 'b': result.push_back('\b'); break;
            case 't': result.push_back('\t'); break;
            case 'n': result.push_back('\n'); break;
            case 'r': result.push_back('\r'); break;
            case 'f': result.push_back('\f'); break;
            case 'v': result.push_back('\v'); break;
            case '0': result.push_back('\0'); break;
            case 'e': result.push_back('\x1b'); break;
            case '\\': result.push_back('\\'); break;
            case '"': result.push_back('"'); break;
            case '|': result.push_back('|'); break;
            case 'x': {
                unsigned value = 0;
                std::size_t digits = 0;
                while (port.position < port.source.size()) {
                    unsigned char h = static_cast<unsigned char>(port.source[port.position]);
                    unsigned digit = h >= '0' && h <= '9' ? h - '0' :
                                     h >= 'a' && h <= 'f' ? h - 'a' + 10 :
                                     h >= 'A' && h <= 'F' ? h - 'A' + 10 : 16;
                    if (digit >= 16) break;
                    value = value * 16 + digit;
                    ++digits;
                    ++port.position;
                }
                if (digits == 0 || port.position == port.source.size() ||
                    port.source[port.position++] != ';')
                    throw std::runtime_error("g-read-string: invalid hex escape");
                append_utf8(result, value);
                break;
            }
            default:
                throw std::runtime_error("g-read-string: invalid escape");
            }
        }
        throw std::runtime_error("g-read-string: unterminated string");
    });
    install(evaluator, "g-undefined", [](const Values& args) {
        if (args.size() > 1)
            throw std::runtime_error("g-undefined expects zero or one arguments");
        return Values{Value::unspecified()};
    });
    install(evaluator, "peek-char", [&evaluator, eof](const Values& args) {
        if (args.size() > 1)
            throw std::runtime_error("peek-char expects zero or one arguments");
        if (args.empty()) return Values{eof};
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::InputPort)
            throw std::runtime_error("peek-char expects an input port");
        auto* port = args[0].as_object<InputStringPortObject>();
        if (port->closed)
            throw std::runtime_error("peek-char from closed input port");
        if (port->position == port->source.size()) return Values{eof};
        return Values{evaluator.character(static_cast<unsigned char>(
            port->source[port->position]))};
    });
    install(evaluator, "read-char", [&evaluator, eof](const Values& args) {
        if (args.size() > 1)
            throw std::runtime_error("read-char expects zero or one arguments");
        if (args.empty()) return Values{eof};
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::InputPort)
            throw std::runtime_error("read-char expects an input port");
        auto* port = args[0].as_object<InputStringPortObject>();
        if (port->closed)
            throw std::runtime_error("read-char from closed input port");
        if (port->position == port->source.size()) return Values{eof};
        return Values{evaluator.character(static_cast<unsigned char>(
            port->source[port->position++]))};
    });
    install(evaluator, "g-delimiter?", [&evaluator](const Values& args) {
        require_arity(args, 1, "g-delimiter?");
        char32_t character = evaluator.character_value(args[0]);
        return Values{Value::boolean(character == U'\0' ||
                                     std::isspace(static_cast<unsigned char>(character)) ||
                                     character == U'(' || character == U')' ||
                                     character == U'[' || character == U']' ||
                                     character == U'"' || character == U';' ||
                                     character == U'.')};
    });
    install(evaluator, "g-valid-identifier?", [&evaluator](const Values& args) {
        require_arity(args, 1, "g-valid-identifier?");
        return Values{Value::boolean(!evaluator.string_value(args[0]).empty())};
    });
    install(evaluator, "g-enabled?", [&evaluator](const Values& args) {
        require_arity(args, 1, "g-enabled?");
        return Values{Value::boolean(false)};
    });
    install(evaluator, "call-with-input-file",
            [&evaluator](const Values& args) {
                require_arity(args, 2, "call-with-input-file");
                Value port = evaluator.apply_values(
                    evaluator.global_environment()->lookup(
                        evaluator.symbol("open-input-file")),
                    {args[0]})[0];
                try {
                    Values result = evaluator.apply_values(args[1], {port});
                    port.as_object<InputStringPortObject>()->closed = true;
                    return result;
                } catch (...) {
                    port.as_object<InputStringPortObject>()->closed = true;
                    throw;
                }
            });
    install(evaluator, "call-with-output-file",
            [&evaluator](const Values& args) {
                require_arity(args, 2, "call-with-output-file");
                Value port = evaluator.apply_values(
                    evaluator.global_environment()->lookup(
                        evaluator.symbol("open-output-file")),
                    {args[0]})[0];
                try {
                    Values result = evaluator.apply_values(args[1], {port});
                    port.as_object<OutputPortObject>()->stream->flush();
                    port.as_object<OutputPortObject>()->closed = true;
                    return result;
                } catch (...) {
                    port.as_object<OutputPortObject>()->closed = true;
                    throw;
                }
            });
    install(evaluator, "get-output-string", [&evaluator](const Values& args) {
        require_arity(args, 1, "get-output-string");
        auto& port = output_port(args[0], "get-output-string");
        auto stream = std::dynamic_pointer_cast<std::ostringstream>(port.stream);
        if (!stream || !port.buffer)
            throw std::runtime_error("get-output-string expects a string port");
        return Values{evaluator.string(stream->str())};
    });
    install(evaluator, "delete-file", [&evaluator](const Values& args) {
        require_arity(args, 1, "delete-file");
        std::error_code error;
        fs::remove(evaluator.string_value(args[0]), error);
        if (error) throw std::runtime_error("cannot delete file");
        return Values{Value::unspecified()};
    });
    install(evaluator, "read", [&evaluator, eof](const Values& args) {
        if (args.size() > 1)
            throw std::runtime_error("read expects zero or one arguments");
        if (args.empty())
            return Values{eof};
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::InputPort)
            throw std::runtime_error("read expects an input port");
        auto* port = args[0].as_object<InputStringPortObject>();
        if (port->closed)
            throw std::runtime_error("read from closed input port");
        if (port->position == port->source.size())
            return Values{eof};
        TinyReader reader(evaluator,
                          port->source.substr(port->position));
        std::optional<Value> value = reader.read();
        port->position += reader.position();
        return Values{value ? *value : eof};
    });
    // Core object predicates and pair/vector operations.
    install(evaluator, "boolean?", [](const Values& args) {
        require_arity(args, 1, "boolean?");
        return Values{Value::boolean(args[0].is_boolean())};
    });
    install(evaluator, "integer?", [](const Values& args) {
        require_arity(args, 1, "integer?");
        return Values{Value::boolean(args[0].is_integer())};
    });
    install(evaluator, "number?", [](const Values& args) {
        require_arity(args, 1, "number?");
        return Values{Value::boolean(args[0].is_integer())};
    });
    install(evaluator, "real?", [](const Values& args) {
        require_arity(args, 1, "real?");
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
    install(evaluator, "list?", [](const Values& args) {
        require_arity(args, 1, "list?");
        Value rest = args[0];
        while (!rest.is_null()) {
            if (!rest.is_object() ||
                rest.as_object()->type() != ObjectType::Pair)
                return Values{Value::boolean(false)};
            rest = rest.as_object<PairObject>()->cdr;
        }
        return Values{Value::boolean(true)};
    });
    install(evaluator, "proper-list?", [](const Values& args) {
        require_arity(args, 1, "proper-list?");
        Value rest = args[0];
        while (!rest.is_null()) {
            if (!rest.is_object() ||
                rest.as_object()->type() != ObjectType::Pair)
                return Values{Value::boolean(false)};
            rest = rest.as_object<PairObject>()->cdr;
        }
        return Values{Value::boolean(true)};
    });
    install(evaluator, "list-ref", [](const Values& args) {
        require_arity(args, 2, "list-ref");
        Value rest = args[0];
        std::int64_t index = args[1].as_integer();
        if (index < 0)
            throw std::runtime_error("list-ref index is negative");
        while (index-- > 0) {
            if (!rest.is_object() || rest.as_object()->type() != ObjectType::Pair)
                throw std::runtime_error("list-ref index out of bounds");
            rest = rest.as_object<PairObject>()->cdr;
        }
        if (!rest.is_object() || rest.as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("list-ref index out of bounds");
        return Values{rest.as_object<PairObject>()->car};
    });
    install(evaluator, "sort", [&evaluator](const Values& args) {
        require_arity(args, 2, "sort");
        std::vector<Value> values;
        Value rest = args[1];
        while (!rest.is_null()) {
            if (!rest.is_object() || rest.as_object()->type() != ObjectType::Pair)
                throw std::runtime_error("sort expects a proper list");
            values.push_back(rest.as_object<PairObject>()->car);
            rest = rest.as_object<PairObject>()->cdr;
        }
        for (std::size_t i = 1; i < values.size(); ++i) {
            Value item = values[i];
            std::size_t j = i;
            while (j > 0) {
                Value before = evaluator.apply_values(
                    args[0], {item, values[j - 1]})[0];
                if (!before.is_boolean() || !before.as_boolean()) break;
                values[j] = values[j - 1];
                --j;
            }
            values[j] = item;
        }
        return Values{evaluator.list(values)};
    });
    install(evaluator, "symbol?", [](const Values& args) {
        require_arity(args, 1, "symbol?");
        return Values{Value::boolean(args[0].is_object() &&
                                     args[0].as_object()->type() == ObjectType::Symbol)};
    });
    install(evaluator, "keyword?", [](const Values& args) {
        require_arity(args, 1, "keyword?");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::Symbol)
            return Values{Value::boolean(false)};
        const auto& name = args[0].as_object<SymbolObject>()->name;
        return Values{Value::boolean(!name.empty() && name.front() == ':')};
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
    // The binary bytevector substrate is intentionally deferred to its own
    // runtime layer; the expander still needs a total predicate here.
    install(evaluator, "bytevector?", [](const Values& args) {
        require_arity(args, 1, "bytevector?");
        return Values{Value::boolean(false)};
    });
    install(evaluator, "procedure?", [](const Values& args) {
        require_arity(args, 1, "procedure?");
        bool result = args[0].is_object() &&
                      (args[0].as_object()->type() == ObjectType::Closure ||
                       args[0].as_object()->type() == ObjectType::Primitive);
        return Values{Value::boolean(result)};
    });
    install(evaluator, "type-of", [&evaluator](const Values& args) {
        require_arity(args, 1, "type-of");
        if (args[0].is_null()) return Values{evaluator.symbol("null")};
        if (args[0].is_unspecified())
            return Values{evaluator.symbol("unspecified")};
        if (args[0].is_boolean()) return Values{evaluator.symbol("boolean")};
        if (args[0].is_integer()) return Values{evaluator.symbol("integer")};
        switch (args[0].as_object()->type()) {
        case ObjectType::Pair: return Values{evaluator.symbol("pair")};
        case ObjectType::Symbol: return Values{evaluator.symbol("symbol")};
        case ObjectType::String: return Values{evaluator.symbol("string")};
        case ObjectType::Vector: return Values{evaluator.symbol("vector")};
        case ObjectType::Character: return Values{evaluator.symbol("character")};
        case ObjectType::Primitive:
        case ObjectType::Closure: return Values{evaluator.symbol("procedure")};
        default: return Values{evaluator.symbol("object")};
        }
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
    install(evaluator, "string->keyword", [&evaluator](const Values& args) {
        require_arity(args, 1, "string->keyword");
        std::string name = evaluator.string_value(args[0]);
        if (name.empty() || name.front() != ':') name.insert(name.begin(), ':');
        return Values{evaluator.symbol(name)};
    });
    install(evaluator, "keyword->symbol", [&evaluator](const Values& args) {
        require_arity(args, 1, "keyword->symbol");
        const std::string& name = args[0].as_object<SymbolObject>()->name;
        return Values{evaluator.symbol(name.size() > 0 && name.front() == ':'
                                             ? name.substr(1) : name)};
    });
    install(evaluator, "symbol->keyword", [&evaluator](const Values& args) {
        require_arity(args, 1, "symbol->keyword");
        std::string name = args[0].as_object<SymbolObject>()->name;
        if (name.empty() || name.front() != ':') name.insert(name.begin(), ':');
        return Values{evaluator.symbol(name)};
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
    install(evaluator, "string-copy", [&evaluator](const Values& args) {
        if (args.size() != 1 && args.size() != 3)
            throw std::runtime_error("string-copy expects one or three arguments");
        const std::string& value = evaluator.string_value(args[0]);
        const std::size_t start = args.size() == 3
            ? static_cast<std::size_t>(args[1].as_integer()) : 0;
        const std::size_t end = args.size() == 3
            ? static_cast<std::size_t>(args[2].as_integer()) : value.size();
        if (start > end || end > value.size())
            throw std::runtime_error("string-copy index out of bounds");
        return Values{evaluator.string(value.substr(start, end - start))};
    });
    install(evaluator, "string->list", [&evaluator](const Values& args) {
        require_arity(args, 1, "string->list");
        std::vector<Value> chars;
        for (unsigned char c : evaluator.string_value(args[0]))
            chars.push_back(evaluator.character(c));
        return Values{evaluator.list(chars)};
    });
    install(evaluator, "list->string", [&evaluator](const Values& args) {
        require_arity(args, 1, "list->string");
        std::string result;
        for (Value character : proper_list(args[0]))
            result.push_back(static_cast<char>(evaluator.character_value(character)));
        return Values{evaluator.string(result)};
    });
    install(evaluator, "string-set!", [&evaluator](const Values& args) {
        require_arity(args, 3, "string-set!");
        auto& value = args[0].as_object<StringObject>()->value;
        const auto index = args[1].as_integer();
        if (index < 0 || static_cast<std::size_t>(index) >= value.size())
            throw std::runtime_error("string-set! index out of bounds");
        value[static_cast<std::size_t>(index)] = static_cast<char>(
            evaluator.character_value(args[2]));
        return Values{Value::unspecified()};
    });
    install(evaluator, "string-copy!", [&evaluator](const Values& args) {
        if (args.size() != 3 && args.size() != 5)
            throw std::runtime_error("string-copy! expects three or five arguments");
        auto& target = args[0].as_object<StringObject>()->value;
        const auto target_start = args[1].as_integer();
        const std::string& source = evaluator.string_value(args[2]);
        const auto source_start = args.size() == 5 ? args[3].as_integer() : 0;
        const auto source_end = args.size() == 5
            ? args[4].as_integer() : static_cast<std::int64_t>(source.size());
        if (target_start < 0 || source_start < 0 || source_end < source_start ||
            source_end > static_cast<std::int64_t>(source.size()) ||
            target_start + source_end - source_start >
                static_cast<std::int64_t>(target.size()))
            throw std::runtime_error("string-copy! index out of bounds");
        target.replace(static_cast<std::size_t>(target_start),
                       static_cast<std::size_t>(source_end - source_start),
                       source, static_cast<std::size_t>(source_start),
                       static_cast<std::size_t>(source_end - source_start));
        return Values{Value::unspecified()};
    });
    install(evaluator, "string-fill!", [&evaluator](const Values& args) {
        require_arity(args, 2, "string-fill!");
        auto& value = args[0].as_object<StringObject>()->value;
        const char character = static_cast<char>(evaluator.character_value(args[1]));
        std::fill(value.begin(), value.end(), character);
        return Values{Value::unspecified()};
    });
    install(evaluator, "string-for-each", [&evaluator](const Values& args) {
        if (args.size() < 2)
            throw std::runtime_error("string-for-each expects a procedure and strings");
        std::vector<std::string> strings;
        for (std::size_t i = 1; i < args.size(); ++i)
            strings.push_back(evaluator.string_value(args[i]));
        std::size_t length = strings[0].size();
        for (const auto& string : strings) length = std::min(length, string.size());
        for (std::size_t i = 0; i < length; ++i) {
            Values call;
            for (const auto& string : strings) call.push_back(
                evaluator.character(static_cast<unsigned char>(string[i])));
            evaluator.apply_values(args[0], call);
        }
        return Values{Value::unspecified()};
    });
    for (const auto& entry : {std::pair<const char*, bool(*)(const std::string&, const std::string&)>{
                                  "string=?", [](const auto& a, const auto& b) { return a == b; }},
                              {"string<?", [](const auto& a, const auto& b) { return a < b; }},
                              {"string>?", [](const auto& a, const auto& b) { return a > b; }},
                              {"string<=?", [](const auto& a, const auto& b) { return a <= b; }},
                              {"string>=?", [](const auto& a, const auto& b) { return a >= b; }}}) {
        install(evaluator, entry.first, [name = entry.first, compare = entry.second,
                                         &evaluator](const Values& args) {
            if (args.size() < 2)
                throw std::runtime_error(std::string(name) + " expects two arguments");
            for (std::size_t i = 1; i < args.size(); ++i)
                if (!compare(evaluator.string_value(args[i - 1]),
                             evaluator.string_value(args[i])))
                    return Values{Value::boolean(false)};
            return Values{Value::boolean(true)};
        });
    }
    install(evaluator, "format", [&evaluator](const Values& args) {
        if (args.size() < 2 ||
            (!args[0].is_boolean() &&
             !(args[0].is_object() &&
               args[0].as_object()->type() == ObjectType::String)) ||
            !args[1].is_object() ||
            args[1].as_object()->type() != ObjectType::String)
            throw std::runtime_error("format expects a destination and string");
        const std::string pattern = evaluator.string_value(args[1]);
        std::string result;
        std::size_t argument = 2;
        for (std::size_t i = 0; i < pattern.size(); ++i) {
            if (pattern[i] != '~' || i + 1 >= pattern.size()) {
                result += pattern[i];
                continue;
            }
            char directive = pattern[++i];
            if (directive == '~') {
                result += '~';
            } else if (directive == '%') {
                result += '\n';
            } else if (directive == 'a' || directive == 'A' ||
                       directive == 's' || directive == 'S') {
                if (argument >= args.size())
                    throw std::runtime_error("format missing argument");
                result += format_value(evaluator, args[argument++]);
            } else {
                result += '~';
                result += directive;
            }
        }
        // String destinations are not runtime ports yet.  Returning the
        // formatted string also covers the #f destination used by bootstrap.
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
    install(evaluator, "substring", [&evaluator](const Values& args) {
        require_arity(args, 3, "substring");
        const std::string& value = evaluator.string_value(args[0]);
        auto start = args[1].as_integer();
        auto end = args[2].as_integer();
        if (start < 0 || end < start || static_cast<std::size_t>(end) > value.size())
            throw std::runtime_error("substring index out of bounds");
        return Values{evaluator.string(value.substr(static_cast<std::size_t>(start),
                                                     static_cast<std::size_t>(end - start)))};
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
    install_unicode_primitives(evaluator);
    for (const auto& entry : {std::pair<const char*, bool(*)(char32_t, char32_t)>{
                                  "char=?", [](char32_t a, char32_t b) { return a == b; }},
                              {"char<?", [](char32_t a, char32_t b) { return a < b; }},
                              {"char>?", [](char32_t a, char32_t b) { return a > b; }},
                              {"char<=?", [](char32_t a, char32_t b) { return a <= b; }},
                              {"char>=?", [](char32_t a, char32_t b) { return a >= b; }}}) {
        install(evaluator, entry.first, [name = entry.first, compare = entry.second,
                                         &evaluator](const Values& args) {
            if (args.size() < 2)
                throw std::runtime_error(std::string(name) + " expects two arguments");
            for (std::size_t i = 1; i < args.size(); ++i)
                if (!compare(evaluator.character_value(args[i - 1]),
                             evaluator.character_value(args[i])))
                    return Values{Value::boolean(false)};
            return Values{Value::boolean(true)};
        });
    }

    // Arithmetic atoms.
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
