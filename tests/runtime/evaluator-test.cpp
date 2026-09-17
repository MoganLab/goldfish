#include "runtime/runtime.hpp"
#include "runtime/migration_primitives.hpp"
#include "runtime/standard_primitives.hpp"

#include <cassert>
#include <stdexcept>

using namespace goldfish::runtime;

int main() {
    Runtime runtime;
    Evaluator& evaluator = runtime.evaluator();
    install_runtime_primitives(evaluator);

    bool migration_alias_is_absent = false;
    try {
        (void)evaluator.eval(evaluator.symbol("rootlet"));
    } catch (const std::runtime_error&) {
        migration_alias_is_absent = true;
    }
    assert(migration_alias_is_absent);

    install_migration_primitives(evaluator);

    Value make_environment = evaluator.eval(
        evaluator.symbol("make-eval-environment"));
    Value eval_environment =
        evaluator.apply_values(make_environment, {})[0];
    Value environment_predicate =
        evaluator.eval(evaluator.symbol("eval-environment?"));
    assert(evaluator.apply_values(environment_predicate, {eval_environment})[0]
               .as_boolean());
    auto environment =
        eval_environment.as_object<EvalEnvironmentObject>()->environment;
    environment->define(evaluator.symbol("isolated"), Value::integer(41));
    assert(evaluator.eval(evaluator.symbol("isolated"), environment)
               .as_integer() == 41);
    Value eval_primitive = evaluator.eval(evaluator.symbol("eval"));
    Value eval_form = evaluator.list({
        evaluator.symbol("begin"),
        evaluator.list({evaluator.symbol("define"),
                        evaluator.symbol("evaluated"), Value::integer(42)}),
        evaluator.symbol("evaluated")});
    assert(evaluator.apply_values(eval_primitive,
                                  {eval_form, eval_environment})[0]
               .as_integer() == 42);

    assert(evaluator.symbol("same") == evaluator.symbol("same"));

    runtime.register_primitive("+",
        [](const std::vector<Value>& args) {
            if (args.size() < 2)
                throw std::runtime_error("+ expects at least two integers");
            std::int64_t result = 0;
            for (Value argument : args) {
                if (!argument.is_integer())
                    throw std::runtime_error("+ expects integers");
                result += argument.as_integer();
            }
            return Values{Value::integer(result)};
        });
    runtime.install_primitives();

    Value x = evaluator.symbol("x");
    Value y = evaluator.symbol("y");
    Value expression = evaluator.list({
        evaluator.symbol("let"),
        evaluator.list({evaluator.list({x, Value::integer(40)})}),
        evaluator.list({
            evaluator.list({evaluator.symbol("lambda"),
                            evaluator.list({y}),
                            evaluator.list({evaluator.symbol("+"), x, y})}),
            Value::integer(2)}),
    });

    assert(evaluator.eval(expression).as_integer() == 42);

    Value definition = evaluator.list({evaluator.symbol("define"), x,
                                       Value::integer(7)});
    evaluator.eval(definition);
    assert(evaluator.eval(x).as_integer() == 7);

    Value mutation = evaluator.list({evaluator.symbol("begin"),
                                     evaluator.list({evaluator.symbol("set!"),
                                                     x, Value::integer(9)}),
                                     x});
    assert(evaluator.eval(mutation).as_integer() == 9);

    Value recursive = evaluator.list({
        evaluator.symbol("letrec"),
        evaluator.list({evaluator.list({x, Value::integer(11)})}), x});
    assert(evaluator.eval(recursive).as_integer() == 11);

    Value loop = evaluator.symbol("loop");
    Value n = evaluator.symbol("n");
    Value tail_recursive = evaluator.list({
        evaluator.symbol("letrec"),
        evaluator.list({evaluator.list({
            loop, evaluator.list({evaluator.symbol("lambda"),
                                   evaluator.list({n}),
                                   evaluator.list({evaluator.symbol("begin"),
                                                   evaluator.list({evaluator.symbol("if"),
                                                                   evaluator.list({evaluator.symbol("="), n,
                                                                                   Value::integer(0)}),
                                                                   Value::integer(0),
                                                                   evaluator.list({loop,
                                                                                   evaluator.list({evaluator.symbol("-"), n,
                                                                                                   Value::integer(1)})})})})})})}),
        evaluator.list({loop, Value::integer(10000)})});
    assert(evaluator.eval(tail_recursive).as_integer() == 0);

    Value sequential = evaluator.list({
        evaluator.symbol("letrec*"),
        evaluator.list({
            evaluator.list({x, Value::integer(3)}),
            evaluator.list({y, evaluator.list({evaluator.symbol("+"), x,
                                                Value::integer(4)})})}),
        y});
    assert(evaluator.eval(sequential).as_integer() == 7);

    Value empty_begin = evaluator.list({evaluator.symbol("begin")});
    assert(evaluator.eval(empty_begin).is_unspecified());

    bool rejected_forward_reference = false;
    Value invalid_recursive = evaluator.list({
        evaluator.symbol("letrec"),
        evaluator.list({evaluator.list({x, y}),
                        evaluator.list({y, Value::integer(1)})}),
        x});
    try {
        evaluator.eval(invalid_recursive);
    } catch (const std::runtime_error&) {
        rejected_forward_reference = true;
    }
    assert(rejected_forward_reference);

    Value producer = evaluator.list({evaluator.symbol("lambda"),
                                     evaluator.list({}),
                                     evaluator.list({evaluator.symbol("values"),
                                                     Value::integer(5),
                                                     Value::integer(6)})});
    Value consumer = evaluator.list({
        evaluator.symbol("lambda"), evaluator.list({x, y}),
        evaluator.list({evaluator.symbol("+"), x, y})});
    Value cwv = evaluator.list({evaluator.symbol("call-with-values"),
                                producer, consumer});
    assert(evaluator.eval(cwv).as_integer() == 11);

    Value values_expression = evaluator.list({evaluator.symbol("values"),
                                              Value::integer(1),
                                              Value::integer(2)});
    Values values = evaluator.eval_values(values_expression);
    assert(values.size() == 2);
    assert(values[0].as_integer() == 1);
    assert(values[1].as_integer() == 2);

    Value multi_value_argument = evaluator.list({
        evaluator.symbol("+"),
        evaluator.list({evaluator.symbol("values"), Value::integer(1),
                        Value::integer(2)}),
        Value::integer(3)});
    bool rejected_multi_value_argument = false;
    try {
        evaluator.eval(multi_value_argument);
    } catch (const std::runtime_error&) {
        rejected_multi_value_argument = true;
    }
    assert(rejected_multi_value_argument);

    Value multi_value_apply_argument = evaluator.list({
        evaluator.symbol("apply"), evaluator.symbol("+"),
        evaluator.list({evaluator.symbol("values"), Value::integer(1),
                        Value::integer(2)}),
        evaluator.list({evaluator.symbol("quote"), evaluator.list({})})});
    bool rejected_multi_value_apply_argument = false;
    try {
        evaluator.eval(multi_value_apply_argument);
    } catch (const std::runtime_error&) {
        rejected_multi_value_apply_argument = true;
    }
    assert(rejected_multi_value_apply_argument);

    Value rest_lambda = evaluator.list({
        evaluator.symbol("lambda"), evaluator.pair(x, y), y});
    Value rest_call = evaluator.list({rest_lambda, Value::integer(1),
                                      Value::integer(2), Value::integer(3)});
    Value rest_values = evaluator.eval(rest_call);
    assert(rest_values.as_object<PairObject>()->car.as_integer() == 2);
    assert(rest_values.as_object<PairObject>()->cdr.as_object<PairObject>()
               ->car.as_integer() == 3);

    Value rest_only_lambda = evaluator.list({evaluator.symbol("lambda"), y,
                                             y});
    Value rest_only_call = evaluator.list({rest_only_lambda, Value::integer(8),
                                           Value::integer(9)});
    Value rest_only_values = evaluator.eval(rest_only_call);
    assert(rest_only_values.as_object<PairObject>()->car.as_integer() == 8);

    Value quoted_tail = evaluator.list({
        evaluator.symbol("quote"),
        evaluator.list({Value::integer(2), Value::integer(3)})});
    Value applied = evaluator.list({evaluator.symbol("apply"),
                                    evaluator.symbol("+"), Value::integer(1),
                                    quoted_tail});
    assert(evaluator.eval(applied).as_integer() == 6);

    Value guarded = evaluator.list({
        evaluator.symbol("guard"),
        evaluator.list({evaluator.symbol("caught"),
                        evaluator.list({evaluator.symbol("else"),
                                        evaluator.symbol("caught")})}),
        evaluator.list({evaluator.symbol("raise"), Value::integer(42)})});
    assert(evaluator.eval(guarded).as_integer() == 42);

    Value error_guarded = evaluator.list({
        evaluator.symbol("guard"),
        evaluator.list({evaluator.symbol("caught"),
                        evaluator.list({evaluator.symbol("else"),
                                        evaluator.symbol("caught")})}),
        evaluator.list({evaluator.symbol("error"), evaluator.string("boom"),
                        Value::integer(1), Value::integer(2)})});
    Value error_value = evaluator.eval(error_guarded);
    assert(evaluator.string_value(
               evaluator.eval(evaluator.list({
                   evaluator.symbol("error-object-message"), error_value}))) ==
           "boom");
    Value irritants = evaluator.eval(evaluator.list({
        evaluator.symbol("error-object-irritants"), error_value}));
    assert(irritants.as_object<PairObject>()->car.as_integer() == 1);
    assert(irritants.as_object<PairObject>()->cdr.as_object<PairObject>()
               ->car.as_integer() == 2);

    Value conditional_guard = evaluator.list({
        evaluator.symbol("guard"),
        evaluator.list({evaluator.symbol("caught"),
                        evaluator.list({Value::boolean(false),
                                        Value::integer(0)}),
                        evaluator.list({evaluator.symbol("else"),
                                        evaluator.symbol("caught")})}),
        evaluator.list({evaluator.symbol("raise"), Value::integer(17)})});
    assert(evaluator.eval(conditional_guard).as_integer() == 17);

    Value runtime_error_guard = evaluator.list({
        evaluator.symbol("guard"),
        evaluator.list({evaluator.symbol("caught"),
                        evaluator.list({evaluator.symbol("else"),
                                        evaluator.symbol("caught")})}),
        evaluator.symbol("missing")});
    Value runtime_error = evaluator.eval(runtime_error_guard);
    Value is_error = evaluator.eval(evaluator.list({
        evaluator.symbol("error-object?"), runtime_error}));
    assert(is_error.as_boolean());

    evaluator.eval(evaluator.list({
        evaluator.symbol("define"), evaluator.symbol("module-ref"),
        evaluator.list({evaluator.symbol("lambda"),
                        evaluator.list({evaluator.symbol("m"),
                                        evaluator.symbol("name")}),
                        evaluator.list({evaluator.symbol("let-ref"),
                                        evaluator.symbol("m"),
                                        evaluator.symbol("name")})})}));
    evaluator.eval(evaluator.list({
        evaluator.symbol("define"), evaluator.symbol("module-set"),
        evaluator.list({evaluator.symbol("lambda"),
                        evaluator.list({evaluator.symbol("m"),
                                        evaluator.symbol("name"),
                                        evaluator.symbol("value")}),
                        evaluator.list({evaluator.symbol("let-set!"),
                                        evaluator.symbol("m"),
                                        evaluator.symbol("name"),
                                        evaluator.symbol("value")})})}));
    Value module_name = evaluator.symbol("m");
    evaluator.eval(evaluator.list({
        evaluator.symbol("define"), module_name,
        evaluator.list({evaluator.symbol("inlet"),
                        evaluator.list({evaluator.symbol("quote"), x}),
                        Value::integer(21)})}));
    Value module_reference = evaluator.list({
        evaluator.symbol("module-ref"),
        module_name, evaluator.list({evaluator.symbol("quote"), x})});
    assert(evaluator.eval(module_reference).as_integer() == 21);

    Value module_assignment = evaluator.list({
        evaluator.symbol("set!"), module_reference, Value::integer(34)});
    evaluator.eval(module_assignment);
    assert(evaluator.eval(module_reference).as_integer() == 34);

    Value handler = evaluator.list({evaluator.symbol("lambda"),
                                    evaluator.list({x}), Value::integer(99)});
    Value arrow_guard = evaluator.list({
        evaluator.symbol("guard"),
        evaluator.list({evaluator.symbol("caught"),
                        evaluator.list({Value::boolean(true),
                                        evaluator.symbol("=>"), handler})}),
        evaluator.list({evaluator.symbol("raise"), Value::integer(1)})});
    assert(evaluator.eval(arrow_guard).as_integer() == 99);

    Value catch_handler = evaluator.list({
        evaluator.symbol("lambda"), evaluator.list({x, evaluator.symbol("value")}),
        evaluator.symbol("value")});
    Value caught_throw = evaluator.list({
        evaluator.symbol("catch"), evaluator.list({evaluator.symbol("quote"),
                                                     evaluator.symbol("tag")}),
        evaluator.list({evaluator.symbol("lambda"), evaluator.list({}),
                        evaluator.list({evaluator.symbol("throw"),
                                        evaluator.list({evaluator.symbol("quote"),
                                                        evaluator.symbol("tag")}),
                                        Value::integer(7)})}),
        catch_handler});
    assert(evaluator.eval(caught_throw).as_integer() == 7);

    evaluator.collect();
    assert(evaluator.eval(module_reference).as_integer() == 34);
}
