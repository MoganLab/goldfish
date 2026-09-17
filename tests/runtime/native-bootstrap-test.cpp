#include "runtime/bootstrap.hpp"

#include <cassert>

using namespace goldfish::runtime;

int main() {
    Runtime runtime;
    NativeBootstrap bootstrap(runtime);
    bootstrap.install_primitives();
    bootstrap.install_primitives();
    bootstrap.load_kernel("goldfish/expander/kernel-combined.scm");
    bootstrap.load_kernel("goldfish/expander/kernel-combined.scm");

    Evaluator& evaluator = runtime.evaluator();
    Value library = evaluator.eval(evaluator.symbol("the-expander-library"));
    Value module_predicate = evaluator.eval(evaluator.symbol("module?"));
    assert(evaluator.apply_values(module_predicate, {library})[0].as_boolean());

    Value expander_predicate = evaluator.eval(evaluator.symbol("exp-library?"));
    Value base_library = evaluator.eval(evaluator.symbol("the-base-library"));
    assert(evaluator.apply_values(expander_predicate, {base_library})[0]
               .as_boolean());

    Value module_define = evaluator.eval(evaluator.symbol("module-define!"));
    Value name = evaluator.symbol("native-bootstrap-probe");
    evaluator.apply_values(module_define,
                           {library, name, Value::integer(123)});
    Value module_ref = evaluator.eval(evaluator.symbol("module-ref"));
    assert(evaluator.apply_values(module_ref, {library, name})[0].as_integer() ==
           123);

    bootstrap.load_library_artifact("tests/runtime/fixtures/lowered-library.gfo");
    assert(evaluator.eval(evaluator.symbol("native-library-probe"))
               .as_integer() == 321);
}
