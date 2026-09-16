#include "runtime/bootstrap.hpp"

#include <cassert>

using namespace goldfish::runtime;

int main(int argc, char** argv) {
    assert(argc >= 2);
    Runtime runtime;
    NativeBootstrap bootstrap(runtime);
    Evaluator& evaluator = runtime.evaluator();
    bootstrap.install_primitives();
    bootstrap.load_kernel("goldfish/expander/kernel-combined.scm");
    Value constructor = evaluator.eval(evaluator.symbol("make-toplevel-ref"));
    evaluator.apply_values(constructor,
                           {evaluator.symbol("probe"), Value::boolean(false),
                            evaluator.symbol("probe"), Value::boolean(false)});
    for (int i = 1; i < argc; ++i)
        bootstrap.load_artifact(argv[i]);

    Value base = evaluator.eval(evaluator.symbol("the-base-library"));
    Value ref_own = evaluator.eval(evaluator.symbol("exp-library-ref-own"));
    Value binding = evaluator.apply_values(
        ref_own, {base, evaluator.symbol("pair-or-null?")})[0];
    assert(!binding.is_boolean() || binding.as_boolean());
    Value binding_value = evaluator.eval(evaluator.symbol("binding-value"));
    Value reference = evaluator.apply_values(binding_value, {binding})[0];
    Value gensym = evaluator.eval(evaluator.symbol("toplevel-ref-gensym"));
    Value global_name = evaluator.apply_values(gensym, {reference})[0];
    assert(evaluator.eval(global_name).is_object());

    try {
        Value lookup = evaluator.eval(evaluator.symbol("lookup-module"));
        Value compiler = evaluator.apply_values(
            lookup, {evaluator.list({evaluator.symbol("goldfish"),
                                     evaluator.symbol("compiler")})})[0];
        assert(compiler.is_object());
    } catch (const RaisedValue& raised) {
        if (raised.value().is_object() &&
            raised.value().as_object()->type() == ObjectType::ErrorObject)
            throw std::runtime_error(
                raised.value().as_object<ErrorObject>()->message);
        throw;
    }
}
