#include "runtime/bootstrap.hpp"
#include "runtime/reader.hpp"

#include <iostream>
#include <cstdlib>
#include <filesystem>
#include <exception>
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

void eval_source(Evaluator& evaluator, const std::string& source,
                 bool print_result = true) {
    Value input = evaluator.apply_values(
        lookup(evaluator, "open-input-string"),
        {evaluator.string(source)})[0];
    Value forms = evaluator.apply_values(lookup(evaluator, "read-forms"),
                                        {input})[0];
    Value lowered = evaluator.apply_values(
        lookup(evaluator, "compile-program"), {forms})[0];
    Value result = evaluator.eval(lowered);
    if (print_result) {
        print_value(result, std::cout);
        std::cout << '\n';
    }
}

void eval_file(Evaluator& evaluator, const std::string& path) {
    Value lowered = evaluator.apply_values(
        lookup(evaluator, "compile-file"), {evaluator.string(path)})[0];
    print_value(evaluator.eval(lowered), std::cout);
    std::cout << '\n';
}

void load_source(Evaluator& evaluator, const std::string& path) {
    evaluator.apply_values(lookup(evaluator, "load-source-file"),
                           {evaluator.string(path)});
}

void install_source_expander(Evaluator& evaluator) {
    // The full expand-eval procedure is part of the library installer.  A
    // cache-free bootstrap needs the kernel's per-form entry point first so
    // that the Scheme prelude can install its derived forms.
    evaluator.define_primitive(
        "expand-eval", [&evaluator](const Values& args) {
            if (args.size() != 1)
                throw std::runtime_error("expand-eval expects one argument");
            Value compile = evaluator.eval(evaluator.symbol("compile-toplevel"));
            Value lowered = evaluator.apply_values(compile, args)[0];
            return evaluator.eval_values(lowered);
    });
}

void eval_test_path(Evaluator& evaluator, const std::string& path) {
    namespace fs = std::filesystem;
    if (fs::is_directory(path)) {
        for (const fs::directory_entry& entry : fs::directory_iterator(path)) {
            if (entry.path().extension() == ".scm")
                eval_test_path(evaluator, entry.path().string());
        }
        return;
    }
    load_source(evaluator, path);
}

void configure_load_path(int argc, char** argv) {
    std::string paths;
    for (int i = 1; i + 1 < argc; ++i) {
        if (std::string(argv[i]) != "-I" &&
            std::string(argv[i]) != "--load-path")
            continue;
        if (!paths.empty()) paths += ':';
        paths += argv[++i];
    }
    if (!paths.empty()) setenv("GOLDFISH_NATIVE_LOAD_PATH", paths.c_str(), 1);
}

std::string startup_mode(int argc, char** argv) {
    for (int i = 1; i + 1 < argc; ++i)
        if (std::string(argv[i]) == "-m" ||
            std::string(argv[i]) == "--mode")
            return argv[i + 1];
    return "default";
}

void install_mode_imports(Evaluator& evaluator, const std::string& mode) {
    std::string imports;
    if (mode == "r7rs" || mode == "scheme")
        imports = "(import (scheme base))";
    else if (mode == "default" || mode == "liii")
        imports = "(import (scheme base) (liii base) (liii error))";
    else if (mode == "sicp")
        imports = "(import (scheme base) (srfi sicp))";
    else if (mode == "s7")
        return;
    else
        throw std::runtime_error("unknown mode: " + mode);
    eval_source(evaluator, imports, false);
}

} // namespace

int main(int argc, char** argv) {
    Runtime runtime;
    try {
        configure_load_path(argc, argv);
        setenv("GOLDFISH_NATIVE_ARTIFACTS", "1", 1);
        NativeBootstrap bootstrap(runtime);
        bootstrap.install_primitives();
        bootstrap.load_kernel("goldfish/expander/kernel-combined.scm");
        bool cached = true;
        try {
            bootstrap.load_cached_runtime();
        } catch (const std::exception& error) {
            std::cerr << "native cache unavailable: " << error.what() << '\n';
            cached = false;
        }
        if (!cached) {
            // install.scm uses this marker to avoid repeating the artifact
            // boot sequence.  A cache-free run has no artifacts, so let its
            // Scheme installer build the same layer from source.
            unsetenv("GOLDFISH_NATIVE_ARTIFACTS");
            install_source_expander(runtime.evaluator());
            load_source(runtime.evaluator(), "expander/bootstrap-prelude.scm");
            load_source(runtime.evaluator(), "liii/prelude.scm");
        }
        load_source(runtime.evaluator(), "expander/lib/install.scm");
        bootstrap.install_expansion_helpers();
        Value standard_library = runtime.evaluator().apply_values(
            lookup(runtime.evaluator(), "module-ref"),
            {lookup(runtime.evaluator(), "the-expander-library"),
             runtime.evaluator().symbol("install-standard-library!")})[0];
        runtime.evaluator().apply_values(standard_library, {});
        install_mode_imports(runtime.evaluator(), startup_mode(argc, argv));

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
        int command = 1;
        if (std::string(argv[command]) == "--help" ||
            std::string(argv[command]) == "-h") {
            std::cout << "usage: gf [-m MODE] [-I DIR] [-e CODE] [load|test] PATH ...\n";
            return 0;
        }
        if (std::string(argv[command]) == "-m" ||
            std::string(argv[command]) == "--mode") {
            if (command + 1 >= argc)
                throw std::runtime_error("--mode requires a mode");
            command += 2;
        }
        while (command < argc && (std::string(argv[command]) == "-I" ||
                                  std::string(argv[command]) == "--load-path")) {
            if (command + 1 >= argc)
                throw std::runtime_error("-I requires a directory");
            command += 2;
        }
        if (command >= argc) {
            std::string line;
            while (std::cout << "> " && std::getline(std::cin, line))
                eval_source(runtime.evaluator(), line);
            return 0;
        }
        if (std::string(argv[command]) == "eval" ||
            std::string(argv[command]) == "-e") {
            if (++command >= argc) throw std::runtime_error("eval requires code");
            eval_source(runtime.evaluator(), argv[command]);
            return 0;
        }
        if (std::string(argv[command]) == "load") {
            if (++command >= argc) throw std::runtime_error("load requires a file");
            load_source(runtime.evaluator(), argv[command]);
            return 0;
        }
        if (std::string(argv[command]) == "test") {
            if (++command >= argc) throw std::runtime_error("test requires a path");
            for (; command < argc; ++command)
                eval_test_path(runtime.evaluator(), argv[command]);
            return 0;
        }
        if (command < argc) {
            eval_file(runtime.evaluator(), argv[command]);
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
