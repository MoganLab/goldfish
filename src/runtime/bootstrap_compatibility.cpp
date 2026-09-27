#include "runtime/bootstrap_compatibility.hpp"

#include "runtime/bootstrap_primitives.hpp"
#include "runtime/setter_registry.hpp"
#include "runtime/symbol.hpp"

#include <memory>
#include <stdexcept>
#include <string>
#include <utility>

namespace goldfish::runtime {

namespace {

void require_arity(const Values& args, std::size_t count, const char* name) {
    if (args.size() != count)
        throw std::runtime_error(std::string(name) + " expects " +
                                 std::to_string(count) + " arguments");
}

void install(Evaluator& evaluator, const char* name,
             PrimitiveObject::Function function) {
    evaluator.define_primitive(name, std::move(function));
}

} // namespace

void install_native_bootstrap_compatibility(Evaluator& evaluator) {
    // These names are consumed by existing lowered bootstrap artifacts.  They
    // are not part of the runtime substrate and do not create LegacyLet data.
    auto rootlet = std::make_shared<Value>(Value::unspecified());
    install(evaluator, "rootlet", [&evaluator, rootlet](const Values& args) {
        require_arity(args, 0, "rootlet");
        if (rootlet->is_unspecified())
            *rootlet = evaluator.make_eval_environment();
        return Values{*rootlet};
    });
    for (const char* name : {"when", "unless"}) {
        install(evaluator, name, [name](const Values& args) {
            if (args.size() < 2)
                throw std::runtime_error(std::string(name) +
                                         " expects a test and a body");
            const bool test = !args[0].is_boolean() || args[0].as_boolean();
            const bool selected = std::string(name) == "when" ? test : !test;
            return Values{selected ? args.back() : Value::unspecified()};
        });
    }
    install(evaluator, "setter", [&evaluator](const Values& args) {
        require_arity(args, 1, "setter");
        if (args[0].is_object()) {
            auto registered = setter_registry().find(args[0].as_object());
            if (registered != setter_registry().end())
                return Values{registered->second};
        }
        return Values{Value::object(evaluator.heap().make<PrimitiveObject>(
            [](const Values& setter_args) {
                if (setter_args.size() != 2)
                    throw std::runtime_error(
                        "setter procedure expects two arguments");
                return Values{Value::unspecified()};
            }))};
    });

    // s7 surface the audited %internal-names list (expander/lib/install.scm)
    // and the liii/srfi layers expect to resolve from (import (goldfish)).
    // Host these names come from s7's rootlet; natively they are provided
    // here as runtime values behind the bare-name fallback.

    install(evaluator, "stacktrace", [&evaluator](const Values& args) {
        // No frame walk yet; an empty string lets srfi-78's safe wrapper
        // substitute "[no stacktrace available]".
        (void)args;
        return Values{evaluator.string("")};
    });

    // Hooks: a tagged pair ('hook . functions).  Native code only reads
    // and replaces the function list today (nothing invokes hooks at
    // runtime); calling a hook is a plain "apply non-procedure" error.
    const auto make_hook = [&evaluator](const Values&) {
        return Values{evaluator.pair(evaluator.symbol("hook"), Value::null())};
    };
    install(evaluator, "make-hook", make_hook);
    install(evaluator, "hook-functions", [&evaluator](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error(
                "hook-functions expects (hook) or (hook functions)");
        if (!args[0].is_object() ||
            args[0].as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("hook-functions: not a hook");
        auto* hook = args[0].as_object<PairObject>();
        const bool tagged =
            hook->car.is_object() &&
            hook->car.as_object()->type() == ObjectType::Symbol &&
            hook->car.as_object<SymbolObject>()->name == "hook";
        if (!tagged)
            throw std::runtime_error("hook-functions: not a hook");
        if (args.size() == 1)
            return Values{hook->cdr};
        hook->cdr = args[1];
        return Values{Value::unspecified()};
    });
    // `hook-functions' doubles as its own setter: (set! (hook-functions h)
    // v) lowers to ((setter hook-functions) h v), which resolves here.
    {
        Value hook_functions =
            evaluator.global_environment()->lookup(evaluator.symbol(
                "hook-functions"));
        setter_registry()[hook_functions.as_object()] = hook_functions;
    }

    // Resolution-only stand-ins: s7 forms outside the native contract.
    // The audit requires them to resolve; invoking one must fail loudly.
    for (const char* name :
         {"with-let", "sublet", "unlet", "let-set!", "load-expanded",
          "le-rootlet-copy"}) {
        install(evaluator, name, [name](const Values&) -> Values {
            throw std::runtime_error(
                std::string(name) + ": s7 compatibility form, not supported "
                                    "by the native runtime");
        });
    }

    // Value (not procedure) bindings.
    evaluator.global_environment()->define(evaluator.symbol("*s7*"),
                                            Value::null());
    evaluator.global_environment()->define(
        evaluator.symbol("*exit-hook*"),
        evaluator.pair(evaluator.symbol("hook"), Value::null()));

    install_bootstrap_primitives(evaluator);
}

} // namespace goldfish::runtime
