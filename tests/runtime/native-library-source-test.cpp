#include "runtime/bootstrap.hpp"
#include "runtime/reader.hpp"

#include <cstdlib>
#include <exception>
#include <iostream>
#include <string>
#include <stdexcept>

using namespace goldfish::runtime;

int main() {
    try {
        Runtime runtime;
        NativeBootstrap bootstrap(runtime);
        setenv("GOLDFISH_NATIVE_ARTIFACTS", "1", 1);
        bootstrap.install_primitives();
        // This fixture deliberately replays artifacts compiled by bin/gf.
        runtime.evaluator().define_primitive("g_executable", [&runtime](const Values&) {
            return Values{runtime.evaluator().string("bin/gf")};
        });
        bootstrap.load_kernel("goldfish/expander/kernel-combined.scm");
        bootstrap.load_cached_runtime();

        Evaluator& evaluator = runtime.evaluator();
        bootstrap.install_source_expander();
        Value load_source = evaluator.eval(evaluator.symbol("load-source-file"));
        evaluator.apply_values(
            load_source, {evaluator.string("expander/lib/install.scm")});
        bootstrap.install_expansion_helpers();
        for (const char* source : {"expander/lib/base-functions.scm",
                                   "expander/lib/native-hash-adapter.scm",
                                   "expander/lib/native-abi.scm"})
            evaluator.apply_values(load_source, {evaluator.string(source)});
        bootstrap.load_cached_base_runtime();

        Value compile_file = evaluator.eval(evaluator.symbol("compile-file"));
        Value lowered =
            evaluator
                .apply_values(
                    compile_file,
                    {evaluator.string(
                        "tests/runtime/fixtures/native-srfi13-program.scm")})[0];
        if (evaluator.string_value(evaluator.eval(lowered)) != "a,b")
            throw std::runtime_error("compiled SRFI 13 program returned an incorrect result");
        bootstrap.load_artifact("tests/runtime/fixtures/numbered-library.gfo");
        if (evaluator.eval(evaluator.symbol("numbered-library-probe")).as_integer() != 42)
            throw std::runtime_error("numbered library artifact returned an incorrect result");
        evaluator.collect();
        const std::size_t before = evaluator.heap().allocated();
        Value payload = Value::null();
        for (std::size_t i = 0; i < 4096; ++i)
            payload = evaluator.pair(Value::integer(0), payload);
        evaluator.global_environment()->define(evaluator.symbol("scale-payload"), payload);
        TinyReader retention_reader(evaluator,
            "(begin (define scale-module (make-module '(scale-module))) "
            "(module-define! scale-module 'payload scale-payload) "
            "(%eval-environment-link! (module-eval-environment scale-module) 'payload "
            "(%current-eval-environment) 'scale-payload) "
            "(set! scale-payload #f))");
        evaluator.eval(*retention_reader.read());
        evaluator.collect();
        if (evaluator.heap().allocated() > before + 128)
            throw std::runtime_error("module metadata retains replaced export values: " +
                                     std::to_string(evaluator.heap().allocated() - before));
    } catch (const std::exception& error) {
        // Fail with a message instead of an uncaught exception aborting the
        // gate with a core dump.
        std::cerr << "error: " << error.what() << '\n';
        if (std::string(error.what()).find("bootstrap cache") !=
            std::string::npos)
            std::cerr << "warm it with: sh tools/warm-bootstrap-cache.sh\n";
        return 1;
    }
}
