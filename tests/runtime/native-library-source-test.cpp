#include "runtime/bootstrap.hpp"

#include <cassert>
#include <cstdlib>
#include <string>

using namespace goldfish::runtime;

int main() {
    Runtime runtime;
    NativeBootstrap bootstrap(runtime);
    setenv("GOLDFISH_NATIVE_ARTIFACTS", "1", 1);
    bootstrap.install_primitives();
    bootstrap.load_kernel("goldfish/expander/kernel-combined.scm");
    bootstrap.load_cached_runtime();

    Evaluator& evaluator = runtime.evaluator();
    Value load_source = evaluator.eval(evaluator.symbol("load-source-file"));
    evaluator.apply_values(
        load_source, {evaluator.string("expander/lib/install.scm")});
    bootstrap.install_expansion_helpers();

    Value compile_file = evaluator.eval(evaluator.symbol("compile-file"));
    Value lowered = evaluator.apply_values(
        compile_file,
        {evaluator.string("tests/runtime/fixtures/native-srfi13-program.scm")})[0];
    assert(evaluator.string_value(evaluator.eval(lowered)) == "a,b");
}
