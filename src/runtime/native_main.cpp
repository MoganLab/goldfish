#include "runtime/bootstrap.hpp"
#include "runtime/debug_flags.hpp"
#include "runtime/platform_primitives.hpp"
#include "runtime/reader.hpp"

#include <iostream>
#include <chrono>
#include <cstdlib>
#include <sys/wait.h>
#include <unistd.h>
#include <filesystem>
#include <exception>
#include <sstream>
#include <stdexcept>
#include <string>

using namespace goldfish::runtime;

namespace {

void configure_native_cache_dir() {
    const char* configured = std::getenv("GOLDFISH_CACHE_DIR");
    if (configured && *configured) return;

    std::filesystem::path root;
    if (const char* xdg = std::getenv("XDG_CACHE_HOME"); xdg && *xdg) {
        root = xdg;
    } else if (const char* home = std::getenv("HOME"); home && *home) {
        root = std::filesystem::path(home) / ".cache";
    } else {
        root = ".";
    }
    root /= "goldfish";
    root /= "native-ccache";
    const std::string path = root.string();
    if (setenv("GOLDFISH_CACHE_DIR", path.c_str(), 0) != 0)
        throw std::runtime_error("could not configure native cache directory");
}

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

std::string with_fix_hint(const std::string& message,
                          const std::string& source_path = {},
                          const std::string& context = {}) {
    const bool candidate =
        message.find("unexpected close paren") != std::string::npos ||
        message.find("missing close paren") != std::string::npos ||
        context.find("unexpected close paren") != std::string::npos ||
        context.find("missing close paren") != std::string::npos;
    if (!candidate ||
        message.find("Hint: try `") != std::string::npos)
        return message;
    std::string path = source_path;
    if (path.empty()) {
        std::size_t marker = message.rfind(" in ");
        const std::string& path_source = marker == std::string::npos
            ? context : message;
        if (marker == std::string::npos) marker = path_source.rfind(" in ");
        if (marker == std::string::npos) return message;
        path = path_source.substr(marker + 4);
    }
    if (!path.empty() && path.back() == '"') path.pop_back();
    if (path.empty()) return message;
    return message + "\nHint: try `gf fix " + path +
           "` to repair common parenthesis issues.";
}

// Run one file in a forked child of the already-booted process; the
// parent waits and returns the child's exit code (the child reports
// errors like the file-argument dispatch).  Shared by the goldtest
// primitive and the --each-file sweeper.
static int fork_and_run_file(Evaluator& evaluator,
                             const std::string& path) {
    // Pending parent output must not flush twice (once in the child's
    // copied buffer, once in the parent).
    std::fflush(nullptr);
    const pid_t pid = fork();
    if (pid < 0)
        throw std::runtime_error("fork-test-file: fork failed");
    if (pid == 0) {
        int code = 1;
        try {
            eval_file(evaluator, path);
            code = 0;
        } catch (const ThrownValue& thrown) {
            std::cerr << "thrown: ";
            print_value(thrown.tag(), std::cerr);
            for (Value argument : thrown.arguments()) {
                std::cerr << ' ';
                print_value(argument, std::cerr);
            }
            std::cerr << '\n';
        } catch (const RaisedValue& raised) {
            if (raised.value().is_object() &&
                raised.value().as_object()->type() ==
                    ObjectType::ErrorObject) {
                const auto* error =
                    raised.value().as_object<ErrorObject>();
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
        } catch (const std::exception& error) {
            std::cerr << error.what() << '\n';
        } catch (...) {
        }
        // _exit: skip atexit/static destructors -- the child's address
        // space is a copy, not the owner.
        std::fflush(nullptr);
        _exit(code);
    }
    int wstatus = 0;
    if (waitpid(pid, &wstatus, 0) < 0)
        throw std::runtime_error("fork-test-file: waitpid failed");
    if (WIFEXITED(wstatus))
        return WEXITSTATUS(wstatus);
    if (WIFSIGNALED(wstatus))
        return 128 + WTERMSIG(wstatus);
    return 125;
}

void install_fork_runner(Evaluator& evaluator) {
    // C2 fork-runner: goldtest's isolated path would fork+exec a fresh
    // boot per file; this forks the already-booted process instead --
    // no exec, COW pages, and every child starts from the same pristine
    // parent snapshot (boot amortized to zero, isolation perfect).
    // Safe without exec because the runtime is single-threaded (tbox
    // never starts threads here).
    evaluator.define_primitive(
        "fork-test-file", [&evaluator](const Values& args) {
            if (args.size() < 1 || args.size() > 2)
                throw std::runtime_error(
                    "fork-test-file expects a file and an optional load dir");
            const std::string path = evaluator.string_value(args[0]);
            if (args.size() == 2 && args[1].is_object() &&
                args[1].as_object()->type() == ObjectType::String) {
                const std::string dir = evaluator.string_value(args[1]);
                const char* current =
                    std::getenv("GOLDFISH_NATIVE_LOAD_PATH");
                const std::string paths =
                    current && *current ? current : "";
                bool present = false;
                for (std::size_t start = 0; start <= paths.size();) {
                    std::size_t end = paths.find(':', start);
                    if (end == std::string::npos)
                        end = paths.size();
                    if (paths.compare(start, end - start, dir) == 0)
                        present = true;
                    start = end + 1;
                }
                if (!present) {
                    const std::string updated =
                        paths.empty() ? dir : paths + ":" + dir;
                    setenv("GOLDFISH_NATIVE_LOAD_PATH", updated.c_str(), 1);
                }
            }
            return Values{
                Value::integer(fork_and_run_file(evaluator, path))};
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
    static const char* builtins[] = {"help", "version", "eval", "-e",
                                     "load", "--help", "-h"};
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
    try {
        configure_native_cache_dir();
        Runtime runtime;
        configure_load_path(argc, argv);
        set_native_command_line(argc, argv);
        setenv("GOLDFISH_NATIVE_ARTIFACTS", "1", 1);
        // Boot stage timing: GOLDFISH_DEBUG=timing (or legacy
        // GOLDFISH_NATIVE_TIMING=1) reports each stage to stderr (ms since
        // the previous stage).
        const bool timing =
            debug_enabled("timing", "GOLDFISH_NATIVE_TIMING");
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
        bootstrap.install_source_expander();
        if (!cached) {
            // install.scm uses this marker to avoid repeating the artifact
            // boot sequence.  A cache-free run has no artifacts, so let its
            // Scheme installer build the same layer from source.
            unsetenv("GOLDFISH_NATIVE_ARTIFACTS");
            load_source(runtime.evaluator(), "expander/bootstrap-prelude.scm");
            load_source(runtime.evaluator(), "liii/prelude.scm");
            stage("cold-source-bootstrap");
        }
        load_source(runtime.evaluator(), "expander/lib/install.scm");
        stage("load-install-scm");
        runtime.evaluator().collect();
        bootstrap.install_expansion_helpers();
        stage("expansion-helpers");
        load_source(runtime.evaluator(), "expander/lib/base-functions.scm");
        load_source(runtime.evaluator(), "expander/lib/native-hash-adapter.scm");
        load_source(runtime.evaluator(), "expander/lib/native-abi.scm");
        stage("native-scheme-surface");
        if (cached) bootstrap.load_cached_base_runtime();
        // The cached bootstrap artifact list already evaluates standard.scm.
        // Reinstalling it here creates fresh transformer bindings after
        // scheme/base.scm has captured interfaces to the originals, so
        // re-exports such as (scheme lazy)'s delay-force look like conflicts.
        if (!cached) {
            Value standard_library = runtime.evaluator().apply_values(
                lookup(runtime.evaluator(), "module-ref"),
                {lookup(runtime.evaluator(), "the-expander-library"),
                 runtime.evaluator().symbol("install-standard-library!")})[0];
            runtime.evaluator().apply_values(standard_library, {});
        }
        stage("standard-library");
        runtime.evaluator().collect();
        install_mode_imports(runtime.evaluator(), startup_mode(argc, argv));
        // The Scheme source reader is intentionally loaded from source; it
        // must not depend on a host-generated gfo cache to start native mode.
        load_source(runtime.evaluator(), "liii/reader.scm");
        stage("load-source-reader");
        if (timing)
            std::cerr << "[timing] boot total "
                      << std::chrono::duration_cast<std::chrono::milliseconds>(
                             std::chrono::steady_clock::now() - stage_start)
                             .count()
                      << " ms\n";

        // Every dispatch path gets the fork runner; goldtest probes it
        // with defined? and falls back to the shell path when absent
        // (host).
        install_fork_runner(runtime.evaluator());

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
        if (std::string(argv[command]) == "--each-file") {
            // C2 sweep mode: one boot, one fork per file, goldtest-style
            // verdict rows -- avoids reloading the goldtest tool per file.
            if (++command >= argc)
                throw std::runtime_error("--each-file requires paths");
            int failures = 0;
            std::size_t ran = 0;
            for (; command < argc; ++command) {
                const std::string file = argv[command];
                const int rc = fork_and_run_file(runtime.evaluator(), file);
                std::printf("  %s ... %s\n", file.c_str(),
                            rc == 0 ? "PASS" : "FAIL");
                std::fflush(stdout);
                ++ran;
                if (rc != 0)
                    ++failures;
            }
            std::printf("\n  Total:  %zu\n  Passed: %d\n  Failed: %d\n", ran,
                        static_cast<int>(ran) - failures, failures);
            return failures == 0 ? 0 : 1;
        }
        if (std::string(argv[command]) == "test") {
            if (++command >= argc) throw std::runtime_error("test requires a path");
            for (; command < argc; ++command)
                eval_test_path(runtime.evaluator(), argv[command]);
            return 0;
        }
        if (std::string(argv[command]) == "run") {
            // Former host parity path: load the target silently, then
            // invoke its `main' procedure; a non-procedure `main' is an
            // error naming the target.
            if (++command >= argc)
                throw std::runtime_error("run requires a target");
            for (; command < argc; ++command) {
                const std::string target = argv[command];
                runtime.evaluator().apply_values(
                    lookup(runtime.evaluator(), "load"),
                    {runtime.evaluator().string(target)});
                Value main_proc;
                auto is_procedure = [](const Value& value) {
                    return value.is_object() &&
                           (value.as_object()->type() == ObjectType::Closure ||
                            value.as_object()->type() ==
                                ObjectType::Primitive);
                };
                try {
                    main_proc = lookup(runtime.evaluator(), "main");
                } catch (const std::exception&) {
                    main_proc = Value::unspecified();
                }
                // Loaded-file defines live in the session program library;
                // expand-eval is the entry that resolves them there (host
                // falls back from name_to_value to eval-through-reader).
                if (!is_procedure(main_proc)) {
                    try {
                        main_proc = runtime.evaluator().apply_values(
                            lookup(runtime.evaluator(), "expand-eval"),
                            {runtime.evaluator().symbol("main")})[0];
                    } catch (const std::exception&) {
                        main_proc = Value::unspecified();
                    }
                }
                if (!is_procedure(main_proc))
                    throw std::runtime_error(
                        "No main function found in target: " + target);
                runtime.evaluator().apply_values(main_proc, {});
            }
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
        std::cerr << "usage: gf [-e expression] [file]\n";
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
            std::string source_path;
            std::string context;
            for (Value irritant : error->irritants)
                if (irritant.is_object() &&
                    irritant.as_object()->type() == ObjectType::String) {
                    const std::string& candidate =
                        irritant.as_object<StringObject>()->value;
                    context += " " + candidate;
                    const std::size_t marker = candidate.rfind(" in ");
                    if (marker != std::string::npos)
                        source_path = candidate.substr(marker + 4);
                    else if (candidate.find('/') != std::string::npos &&
                             candidate.find(".scm") != std::string::npos)
                        source_path = candidate;
                }
            std::cerr << with_fix_hint(error->message, source_path, context);
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
        std::cerr << with_fix_hint(error.what()) << '\n';
        return 1;
    }
}
