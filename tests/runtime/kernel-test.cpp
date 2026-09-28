#include "runtime/artifact.hpp"
#include "runtime/bootstrap_primitives.hpp"
#include "runtime/runtime.hpp"
#include "runtime/standard_primitives.hpp"

#include <cassert>

using namespace goldfish::runtime;

int main() {
    Runtime runtime;
    Evaluator& evaluator = runtime.evaluator();
    install_runtime_primitives(evaluator);
    install_bootstrap_primitives(evaluator);

    ArtifactLoader loader(evaluator);
    loader.load_file("goldfish/expander/kernel-combined.scm");

    Value library = evaluator.eval(evaluator.symbol("the-expander-library"));
    Value module_predicate = evaluator.eval(evaluator.symbol("module?"));
    Values is_module = evaluator.apply_values(module_predicate, {library});
    assert(is_module.size() == 1 && is_module[0].as_boolean());

    Value expander_library = evaluator.eval(evaluator.symbol("exp-library?"));
    Value base_library = evaluator.eval(evaluator.symbol("the-base-library"));
    Values is_expander_library =
        evaluator.apply_values(expander_library, {base_library});
    assert(is_expander_library.size() == 1 &&
           is_expander_library[0].as_boolean());

    Value eval_environment = evaluator.eval(
        evaluator.symbol("module-eval-environment"));
    Value native_environment =
        evaluator.apply_values(eval_environment, {library})[0];
    Value environment_predicate =
        evaluator.eval(evaluator.symbol("eval-environment?"));
    assert(evaluator.apply_values(environment_predicate, {native_environment})[0]
               .as_boolean());

    Value module_define = evaluator.eval(evaluator.symbol("module-define!"));
    Value probe_name = evaluator.symbol("native-kernel-probe");
    evaluator.apply_values(module_define,
                           {library, probe_name, Value::integer(99)});
    Value module_ref = evaluator.eval(evaluator.symbol("module-ref"));
    assert(evaluator.apply_values(module_ref, {library, probe_name})[0]
               .as_integer() == 99);

    Value module_set = evaluator.eval(evaluator.symbol("module-set"));
    evaluator.apply_values(module_set,
                           {library, probe_name, Value::integer(100)});
    evaluator.collect();
    assert(evaluator.apply_values(module_ref, {library, probe_name})[0]
               .as_integer() == 100);
}
