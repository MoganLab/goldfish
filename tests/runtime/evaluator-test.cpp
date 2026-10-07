#include "runtime/runtime.hpp"
#include "runtime/standard_primitives.hpp"
#include "runtime/reader.hpp"
#include "runtime/artifact.hpp"
#include "runtime/bootstrap_primitives.hpp"

#include <cassert>
#include <stdexcept>

using namespace goldfish::runtime;

int main() {
    Runtime runtime;
    Evaluator& evaluator = runtime.evaluator();
    install_runtime_primitives(evaluator);

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

    // 3.5: bounded continuation frames, including primitive-mandated tail calls.
    for (const std::string& step : {
             std::string("(loop (- n 1))"),
             std::string("(apply loop (list (- n 1)))"),
             std::string("(call-with-values (lambda () (- n 1)) loop)"),
             std::string("(call/cc (lambda (ignored) (loop (- n 1))))")}) {
        auto frame_count = [&](int iterations) {
            TinyReader tail_reader(evaluator,
                "(letrec ((loop (lambda (n) (if (= n 0) "
                "(call/cc (lambda (k) k)) " + step + ")))) (loop " +
                std::to_string(iterations) + "))");
            Value result = evaluator.eval(*tail_reader.read());
            if (!result.is_object() ||
                result.as_object()->type() != ObjectType::Continuation)
                throw std::runtime_error("tail audit did not return a continuation");
            return result.as_object<ContinuationObject>()->snapshot.frames.size();
        };
        if (frame_count(10) != frame_count(4000))
            throw std::runtime_error("tail calls retained continuation frames: " + step);
    }

    // Eval's evaluated expression must share its caller's evaluator machine.
    std::uint64_t tail_eval_machine = 0;
    evaluator.define_primitive("r7rs-audit-machine", [&](const Values& args) {
        const auto machine = args.at(0).as_object<ContinuationObject>()->snapshot.machine_id;
        if (tail_eval_machine && machine != tail_eval_machine)
            throw std::runtime_error("tail eval entered a nested evaluator machine");
        tail_eval_machine = machine;
        return Values{Value::unspecified()};
    });
    TinyReader eval_tail_reader(evaluator,
        "(begin (define r7rs-audit-env (interaction-environment)) "
        "(define r7rs-audit-loop (lambda (n) "
        "(begin (call/cc r7rs-audit-machine) "
        "(if (= n 0) 42 (eval (list 'r7rs-audit-loop (- n 1)) r7rs-audit-env))))) "
        "(r7rs-audit-loop 4000))");
    if (evaluator.eval(*eval_tail_reader.read()).as_integer() != 42)
        throw std::runtime_error("tail eval returned an incorrect result");

    evaluator.define_primitive("exception-gc", [&](const Values& args) {
        evaluator.collect();
        return args;
    });
    TinyReader exception_reader(evaluator,
        "(%native-with-exception-handler (lambda (obj) (exception-gc obj)) "
        "(lambda () (+ 1 (car (%native-raise-continuable (cons 41 '()))))))");
    if (evaluator.eval(*exception_reader.read()).as_integer() != 42)
        throw std::runtime_error("continuable handler lost its value or continuation");
    TinyReader guard_forward_reader(evaluator,
        "(%native-with-exception-handler (lambda (obj) obj) "
        "(lambda () (guard (obj (#f 'unreachable)) "
        "(+ 1 (%native-raise-continuable 3)))))");
    if (evaluator.eval(*guard_forward_reader.read()).as_integer() != 4)
        throw std::runtime_error("unmatched core guard lost the raising continuation");

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

    // Export aliases retain one location even after their source frame is gone.
    auto source_frame = make_ref<Environment>();
    auto alias_frame = make_ref<Environment>();
    Value source_name = evaluator.symbol("shared-source");
    Value alias_name = evaluator.symbol("shared-alias");
    source_frame->define(source_name, Value::integer(1));
    alias_frame->link(alias_name, *source_frame, source_name);
    source_frame->set(source_name, Value::integer(2));
    if (alias_frame->lookup(alias_name).as_integer() != 2)
        throw std::runtime_error("alias did not observe a source assignment");
    alias_frame->set(alias_name, Value::integer(3));
    if (source_frame->lookup(source_name).as_integer() != 3)
        throw std::runtime_error("source did not observe an alias assignment");
    source_frame->define(source_name, evaluator.list({Value::integer(4)}));
    source_frame.reset();
    evaluator.global_environment()->define(evaluator.symbol("shared-root"),
        Value::object(evaluator.heap().make<EvalEnvironmentObject>(alias_frame)));
    evaluator.collect();
    if (alias_frame->lookup(alias_name).as_object<PairObject>()->car.as_integer() != 4)
        throw std::runtime_error("shared export location lost its GC root");

    install_runtime_primitives(evaluator);
    install_bootstrap_primitives(evaluator);
    ArtifactLoader loader(evaluator);
    loader.load_file("goldfish/expander/kernel-combined.scm");
    auto promise_frame_count = [&](int iterations) {
        TinyReader promise_reader(evaluator,
            "(letrec ((chain (lambda (n) "
            "(make-lazy-promise (lambda () (if (= n 0) "
            "(make-lazy-promise (lambda () (call/cc (lambda (k) k)))) "
            "(chain (- n 1)))) #t)))) (force (chain " +
            std::to_string(iterations) + ")))" );
        Value result = evaluator.eval(*promise_reader.read());
        if (!result.is_object() ||
            result.as_object()->type() != ObjectType::Continuation)
            throw std::runtime_error("promise tail audit did not return a continuation");
        return result.as_object<ContinuationObject>()->snapshot.frames.size();
    };
    if (promise_frame_count(10) != promise_frame_count(4000))
        throw std::runtime_error("tail promises retained pending memoization frames");
}
