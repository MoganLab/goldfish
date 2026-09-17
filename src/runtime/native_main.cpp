#include "runtime/bootstrap.hpp"

#include <iostream>
#include <cstdlib>
#include <stdexcept>
#include <string>

using namespace goldfish::runtime;

namespace {

void print_value(const Value& value, std::ostream& output) {
    if (value.is_null()) {
        output << "()";
    } else if (value.is_unspecified()) {
        output << "#<unspecified>";
    } else if (value.is_boolean()) {
        output << (value.as_boolean() ? "#t" : "#f");
    } else if (value.is_integer()) {
        output << value.as_integer();
    } else {
        Object* object = value.as_object();
        switch (object->type()) {
        case ObjectType::Symbol:
            output << value.as_object<SymbolObject>()->name;
            break;
        case ObjectType::String:
            output << '"' << value.as_object<StringObject>()->value << '"';
            break;
        case ObjectType::Character:
            output << "#\\";
            output << static_cast<char>(value.as_object<CharacterObject>()->value);
            break;
        case ObjectType::Eof:
            output << "#<eof>";
            break;
        case ObjectType::ErrorObject:
            output << "#<error "
                   << value.as_object<ErrorObject>()->message << ">";
            break;
        case ObjectType::Pair: {
            output << '(';
            Value rest = value;
            bool first = true;
            while (rest.is_object() &&
                   rest.as_object()->type() == ObjectType::Pair) {
                if (!first) output << ' ';
                first = false;
                auto* pair = rest.as_object<PairObject>();
                print_value(pair->car, output);
                rest = pair->cdr;
            }
            if (!rest.is_null()) {
                output << " . ";
                print_value(rest, output);
            }
            output << ')';
            break;
        }
        case ObjectType::Vector: {
            output << "#(";
            const auto& values = value.as_object<VectorObject>()->values;
            for (std::size_t i = 0; i < values.size(); ++i) {
                if (i != 0) output << ' ';
                print_value(values[i], output);
            }
            output << ')';
            break;
        }
        default:
            output << "#<object>";
            break;
        }
    }
}

Value lookup(Evaluator& evaluator, const char* name) {
    return evaluator.eval(evaluator.symbol(name));
}

void eval_source(Evaluator& evaluator, const std::string& source) {
    Value input = evaluator.apply_values(
        lookup(evaluator, "open-input-string"),
        {evaluator.string(source)})[0];
    Value forms = evaluator.apply_values(lookup(evaluator, "read-forms"),
                                        {input})[0];
    Value lowered = evaluator.apply_values(
        lookup(evaluator, "compile-program"), {forms})[0];
    print_value(evaluator.eval(lowered), std::cout);
    std::cout << '\n';
}

void eval_file(Evaluator& evaluator, const std::string& path) {
    Value lowered = evaluator.apply_values(
        lookup(evaluator, "compile-file"), {evaluator.string(path)})[0];
    print_value(evaluator.eval(lowered), std::cout);
    std::cout << '\n';
}

} // namespace

int main(int argc, char** argv) {
    Runtime runtime;
    try {
        setenv("GOLDFISH_NATIVE_ARTIFACTS", "1", 1);
        NativeBootstrap bootstrap(runtime);
        bootstrap.install_primitives();
        bootstrap.load_kernel("goldfish/expander/kernel-combined.scm");
        bootstrap.load_cached_runtime();
        runtime.evaluator().apply_values(
            lookup(runtime.evaluator(), "load-source-file"),
            {runtime.evaluator().string("expander/lib/install.scm")});
        bootstrap.install_expansion_helpers();

        if (argc == 1) {
            std::string line;
            while (std::cout << "> " && std::getline(std::cin, line))
                eval_source(runtime.evaluator(), line);
            return 0;
        }
        if (argc == 3 && std::string(argv[1]) == "-e") {
            eval_source(runtime.evaluator(), argv[2]);
            return 0;
        }
        if (argc == 2) {
            eval_file(runtime.evaluator(), argv[1]);
            return 0;
        }
        std::cerr << "usage: gf-native [-e expression] [file]\n";
        return 2;
    } catch (const RaisedValue& raised) {
        if (raised.value().is_object() &&
            raised.value().as_object()->type() == ObjectType::ErrorObject) {
            const auto* error = raised.value().as_object<ErrorObject>();
            std::cerr << error->message;
            for (Value irritant : error->irritants) {
                std::cerr << ' ';
                print_value(irritant, std::cerr);
            }
            std::cerr << '\n';
        } else {
            std::cerr << "native Scheme error: ";
            print_value(raised.value(), std::cerr);
            std::cerr << '\n';
        }
        return 1;
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
