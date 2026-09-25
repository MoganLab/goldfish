#include "runtime/bootstrap.hpp"
#include "runtime/platform_primitives.hpp"
#include "runtime/reader.hpp"

#include <iostream>
#include <chrono>
#include <cstdlib>
#include <filesystem>
#include <exception>
#include <sstream>
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
        case ObjectType::Bytevector: {
            output << "#u8(";
            const std::string& bytes = value.as_object<BytevectorObject>()->bytes;
            for (std::size_t i = 0; i < bytes.size(); ++i) {
                if (i) output << ' ';
                output << static_cast<int>(
                    static_cast<unsigned char>(bytes[i]));
            }
            output << ')';
            break;
        }
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

Value eval_value(Evaluator& evaluator, const std::string& source) {
    Value input = evaluator.apply_values(
        lookup(evaluator, "open-input-string"),
        {evaluator.string(source)})[0];
    Value forms = evaluator.apply_values(lookup(evaluator, "read-forms"),
                                        {input})[0];
    // Top-level forms compile into the session PROGRAM library, the same
    // target the host's expand-eval path uses: a program starts empty and
    // accumulates its imports (mode seeds, `(import ...)` in -e strings and
    // tool drivers all land here).  compile-program's implicit base-library
    // target would hide them from (program-library), which is what the test
    // worker reads back to replay the seed into a fresh program.
    Value module_ref = lookup(evaluator, "module-ref");
    Value expander = lookup(evaluator, "the-expander-library");
    // Fetch through module-ref: a cold source bootstrap never re-binds the
    // lib layer's names into the global environment (warm artifacts do),
    // so a bare lookup only works in the warm case.  The module slot holds
    // the accessor *procedure*; call it for the live library object.
    Value program_library = evaluator.apply_values(
        evaluator.apply_values(
            module_ref, {expander, evaluator.symbol("program-library")})[0],
        Values{})[0];
    Value lowered = evaluator.apply_values(
        lookup(evaluator, "compile-program-into"), {forms, program_library})[0];
    return evaluator.eval(lowered);
}

void eval_source(Evaluator& evaluator, const std::string& source,
                 bool print_result = true) {
    Value result = eval_value(evaluator, source);
    if (print_result) {
        print_value(result, std::cout);
        std::cout << '\n';
    }
}

void eval_file(Evaluator& evaluator, const std::string& path) {
    // Host parity: script files go through the Scheme loader.  Its compiled
    // artifact evaluates in the expander's environment, where the lowered
    // program's bare gensym references (library definitions, register
    // thunks) actually live -- evaluating in the global environment loses
    // them.  The loader also shares the library cache with `import'.
    Value result = evaluator.apply_values(lookup(evaluator, "load"),
                                          {evaluator.string(path)})[0];
    print_value(result, std::cout);
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
            // Top-level form boundary: the previous form's expansion
            // garbage is unreachable and the stack here is shallow (the
            // loader's own frames only), so a conservative collection is
            // both safe and cheap.  This is what bounds per-file memory.
            evaluator.collect();
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
            std::string(argv[i]) != "-A" &&
            std::string(argv[i]) != "--load-path")
            continue;
        if (!paths.empty()) paths += ':';
        paths += argv[++i];
    }
    if (!paths.empty()) setenv("GOLDFISH_NATIVE_LOAD_PATH", paths.c_str(), 1);
}

// --- project tool dispatch (the `gf <tool>` path), mirroring the host's
// --- src/gf_tool_dispatch.hpp: candidates come from (liii project)'s
// --- gfproject-tool-imports, each is imported after its tools/<cmd> root
// --- (plus the sibling tools/common) joins the load path, then main runs
// --- and its integer result becomes the exit code.

bool has_local_tool(const std::string& command) {
    std::error_code error;
    return std::filesystem::exists(std::filesystem::path("tools") / command,
                                   error) ||
           std::filesystem::exists(
               std::filesystem::path("tools") / (command + ".scm"), error);
}

void append_to_load_path(const std::string& directory) {
    const char* current = std::getenv("GOLDFISH_NATIVE_LOAD_PATH");
    std::string updated = current && *current ? std::string(current) + ":" : "";
    updated += directory;
    setenv("GOLDFISH_NATIVE_LOAD_PATH", updated.c_str(), 1);
}

std::vector<std::filesystem::path> tool_root_candidates(
    const std::string& command) {
    namespace fs = std::filesystem;
    std::vector<fs::path> roots;
    std::error_code error;
    fs::path cwd = fs::current_path(error);
    if (!error) roots.push_back(cwd / "tools" / command);
#if defined(__linux__)
    fs::path self = fs::read_symlink("/proc/self/exe", error);
    if (!error) {
        fs::path library_root = self.parent_path().parent_path();
        roots.push_back(library_root / "tools" / command);
        roots.push_back(library_root.parent_path() / "tools" / command);
    }
#endif
    return roots;
}

// Format a raised Scheme value the way main()'s handler does, so tool
// dispatch failures show the real error message instead of
// "user-raised value".
std::string describe_raised(const RaisedValue& raised) {
    std::ostringstream out;
    if (raised.value().is_object() &&
        raised.value().as_object()->type() == ObjectType::ErrorObject) {
        const auto* error = raised.value().as_object<ErrorObject>();
        out << error->message;
        for (Value irritant : error->irritants) {
            out << ' ';
            print_value(irritant, out);
        }
    } else {
        out << "native Scheme error: ";
        print_value(raised.value(), out);
    }
    return out.str();
}

std::string describe_thrown(const ThrownValue& thrown) {
    std::ostringstream out;
    print_value(thrown.tag(), out);
    for (Value argument : thrown.arguments()) {
        out << ' ';
        print_value(argument, out);
    }
    return out.str();
}

// Returns the tool's exit code, or -1 when `command` is not a project tool.
int try_project_tool(Evaluator& evaluator, int argc, char** argv,
                     int command) {
    namespace fs = std::filesystem;
    if (command >= argc) return -1;
    const std::string cmd = argv[command];
    if (cmd.empty() || cmd[0] == '-' || cmd.find('/') != std::string::npos)
        return -1;
    // Built-ins skip dispatch unless a local tools/<cmd> overrides them.
    static const char* builtins[] = {"help", "version", "eval", "-e", "load",
                                     "repl", "run",   "--help", "-h"};
    for (const char* builtin : builtins) {
        if (cmd == builtin) {
            if (has_local_tool(cmd)) break;
            return -1;
        }
    }

    std::string quoted = "\"";
    for (char character : cmd) {
        if (character == '\\' || character == '"') quoted += '\\';
        quoted += character;
    }
    quoted += '"';

    Value candidates;
    try {
        eval_value(evaluator, "(import (liii project))");
        candidates = eval_value(
            evaluator,
            "(catch #t (lambda () (gfproject-tool-imports " + quoted +
                ")) (lambda args '()))");
    } catch (const std::exception&) {
        return -1;
    }
    if (!candidates.is_object() ||
        candidates.as_object()->type() != ObjectType::Pair)
        return -1; // not a project tool

    fs::path tool_root;
    std::error_code error;
    for (const fs::path& candidate : tool_root_candidates(cmd)) {
        if (fs::is_directory(candidate, error)) {
            tool_root = candidate;
            break;
        }
        error.clear();
    }
    if (tool_root.empty()) {
        std::cerr << "Error: tools/" << cmd << "/ directory not found.\n";
        return 1;
    }
    append_to_load_path(tool_root.string());
    fs::path common = tool_root.parent_path() / "common";
    if (fs::is_directory(common, error))
        append_to_load_path(common.string());

    bool saw_candidate = false;
    std::string last_error;
    for (Value rest = candidates;
         rest.is_object() && rest.as_object()->type() == ObjectType::Pair;
         rest = rest.as_object<PairObject>()->cdr) {
        Value expression = rest.as_object<PairObject>()->car;
        if (!expression.is_object() ||
            expression.as_object()->type() != ObjectType::String)
            continue;
        saw_candidate = true;
        const std::string import_expression =
            evaluator.string_value(expression);
        try {
            eval_value(evaluator, import_expression);
        } catch (const RaisedValue& raised) {
            last_error = std::string("Error ") + import_expression + ": " +
                         describe_raised(raised);
            continue;
        } catch (const ThrownValue& thrown) {
            last_error = std::string("Error ") + import_expression + ": " +
                         describe_thrown(thrown);
            continue;
        } catch (const std::exception& caught) {
            last_error = std::string("Error ") + import_expression + ": " +
                         caught.what();
            continue;
        }
        Value main_proc = Value::unspecified();
        bool is_tool_main = false;
        try {
            main_proc = eval_value(evaluator, "main");
            is_tool_main =
                main_proc.is_object() &&
                (main_proc.as_object()->type() == ObjectType::Closure ||
                 main_proc.as_object()->type() == ObjectType::Primitive);
        } catch (const RaisedValue& raised) {
            last_error = std::string("Error: Failed to find main function via ") +
                         import_expression + ": " + describe_raised(raised);
            continue;
        } catch (const ThrownValue& thrown) {
            last_error = std::string("Error: Failed to find main function via ") +
                         import_expression + ": " + describe_thrown(thrown);
            continue;
        } catch (const std::exception& caught) {
            last_error = std::string("Error: Failed to find main function via ") +
                         import_expression + ": " + caught.what();
            continue;
        }
        if (!is_tool_main) {
            last_error = "Error: Failed to find main function via " +
                         import_expression + ".";
            continue;
        }
        // Running main: failures propagate to main()'s handler like the
        // host, where an erroring tool exits nonzero instead of falling
        // through to the next candidate.
        Values result = evaluator.apply_values(main_proc, {});
        if (!result.empty() && result[0].is_integer())
            return static_cast<int>(result[0].as_integer());
        return 0;
    }
    if (!last_error.empty()) std::cerr << last_error << '\n';
    else if (saw_candidate)
        std::cerr << "Error: tool \"" << cmd << "\" provided no candidates.\n";
    return saw_candidate ? 1 : -1;
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
    if (mode == "r7rs")
        imports = "(import (scheme base))";
    else if (mode == "scheme")
        // Host splits these: r7rs is (scheme base) alone, `scheme' adds the
        // liii extension layer.
        imports = "(import (scheme base) (liii base) (liii error))";
    else if (mode == "default" || mode == "liii")
        // Mirrors the host's liii seed: the test worker replays
        // (program-library) uses as the per-file seed, so a trimmed list
        // here gives test files fewer names than they get on the host.
        // (scheme inexact) stays out until inexact numbers land natively --
        // it needs sqrt/exp at load time -- and comes back with the float
        // workstream.
        imports =
            "(import (goldfish) (scheme base) (scheme write) (scheme read)"
            " (scheme file) (scheme process-context) (scheme time)"
            " (scheme char) (scheme complex) (scheme cxr)"
            " (scheme eval) (scheme case-lambda) (liii base) (liii error))";
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
        set_native_command_line(argc, argv);
        setenv("GOLDFISH_NATIVE_ARTIFACTS", "1", 1);
        // Boot stage timing: GOLDFISH_NATIVE_TIMING=1 reports each stage to
        // stderr (ms since the previous stage).
        const bool timing = std::getenv("GOLDFISH_NATIVE_TIMING") != nullptr;
        const auto stage_start = std::chrono::steady_clock::now();
        auto last = stage_start;
        auto stage = [&timing, &last](const char* name) {
            auto now = std::chrono::steady_clock::now();
            if (timing)
                std::cerr << "[timing] " << name << " "
                          << std::chrono::duration_cast<std::chrono::milliseconds>(
                                 now - last)
                                 .count()
                          << " ms\n";
            last = now;
        };
        NativeBootstrap bootstrap(runtime);
        bootstrap.install_primitives();
        stage("install-primitives");
        bootstrap.load_kernel("goldfish/expander/kernel-combined.scm");
        stage("load-kernel");
        runtime.evaluator().collect();
        bool cached = true;
        try {
            bootstrap.load_cached_runtime();
        } catch (const std::exception& error) {
            std::cerr << "native cache unavailable: " << error.what() << '\n';
            cached = false;
        }
        stage("load-cached-runtime");
        if (!cached) {
            // install.scm uses this marker to avoid repeating the artifact
            // boot sequence.  A cache-free run has no artifacts, so let its
            // Scheme installer build the same layer from source.
            unsetenv("GOLDFISH_NATIVE_ARTIFACTS");
            install_source_expander(runtime.evaluator());
            load_source(runtime.evaluator(), "expander/bootstrap-prelude.scm");
            load_source(runtime.evaluator(), "liii/prelude.scm");
            stage("cold-source-bootstrap");
        }
        load_source(runtime.evaluator(), "expander/lib/install.scm");
        stage("load-install-scm");
        runtime.evaluator().collect();
        bootstrap.install_expansion_helpers();
        stage("expansion-helpers");
        // The Scheme-side composite surface (map/list->vector/copy-ish
        // helpers, the numeric predicates) lives in base-functions.scm; the
        // host loads it during its seed and native never did, so names like
        // list->vector stayed unbound for tool code.  RUNTIME_CONTRACT lists
        // this file as the migrated substrate for the runtime layer.
        load_source(runtime.evaluator(), "expander/lib/base-functions.scm");
        stage("base-functions");
        // s7's hashtable surface comes from s7 itself on the host; native
        // gets the Scheme adapter (vector of bucket alists, same contract).
        load_source(runtime.evaluator(), "expander/lib/native-hash-adapter.scm");
        stage("hash-adapter");
        runtime.evaluator().collect();
        Value standard_library = runtime.evaluator().apply_values(
            lookup(runtime.evaluator(), "module-ref"),
            {lookup(runtime.evaluator(), "the-expander-library"),
             runtime.evaluator().symbol("install-standard-library!")})[0];
        runtime.evaluator().apply_values(standard_library, {});
        stage("standard-library");
        runtime.evaluator().collect();
        install_mode_imports(runtime.evaluator(), startup_mode(argc, argv));
        if (timing)
            std::cerr << "[timing] boot total "
                      << std::chrono::duration_cast<std::chrono::milliseconds>(
                             std::chrono::steady_clock::now() - stage_start)
                             .count()
                      << " ms\n";

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
                                  std::string(argv[command]) == "-A" ||
                                  std::string(argv[command]) == "--load-path")) {
            if (command + 1 >= argc)
                throw std::runtime_error("-I requires a directory");
            command += 2;
        }
        // Project tool dispatch (`gf test ...` and friends); -1 means the
        // word is not a tool and the built-in handlers below take over.
        {
            int tool_exit =
                try_project_tool(runtime.evaluator(), argc, argv, command);
            if (tool_exit != -1) return tool_exit;
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
            // Same loader as file arguments (host parity): the Scheme
            // loader shares the library cache with `import' and falls back
            // to per-form expansion; the C++ source loader diverges on
            // larger programs.  A cold source bootstrap has not defined
            // the Scheme `load' yet, so keep the C++ loader as fallback.
            // Misses surface as keyed raises (or plain runtime errors
            // from older paths), so the probe catches both.
            Value scheme_loader;
            try {
                scheme_loader = lookup(runtime.evaluator(), "load");
            } catch (const std::exception&) {
                scheme_loader = Value::unspecified();
            }
            for (; command < argc; ++command) {
                if (scheme_loader.is_object())
                    runtime.evaluator().apply_values(
                        scheme_loader,
                        {runtime.evaluator().string(argv[command])});
                else
                    load_source(runtime.evaluator(), argv[command]);
            }
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
    } catch (const ThrownValue& thrown) {
        // throw's payload: report (tag irritants ...) like the host.
        std::cerr << "thrown: ";
        print_value(thrown.tag(), std::cerr);
        for (Value argument : thrown.arguments()) {
            std::cerr << ' ';
            print_value(argument, std::cerr);
        }
        std::cerr << '\n';
        return 1;
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
