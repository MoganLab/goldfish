#include "runtime/migration_primitives.hpp"

#include "runtime/bootstrap_primitives.hpp"
#include "runtime/legacy_primitives.hpp"

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

void install_legacy_aliases(Evaluator& evaluator) {
    // These names are consumed by old lowered bootstrap artifacts.  They are
    // deliberately not part of the runtime substrate.
    install(evaluator, "rootlet", [&evaluator](const Values& args) {
        require_arity(args, 0, "rootlet");
        return Values{evaluator.make_eval_environment()};
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
        return Values{Value::object(evaluator.heap().make<PrimitiveObject>(
            [](const Values& setter_args) {
                if (setter_args.size() != 2)
                    throw std::runtime_error("setter procedure expects two arguments");
                return Values{Value::unspecified()};
            }))};
    });

    // Keep the old inlet argument accepted only while the migration layer is
    // installed.  The runtime's eval primitive itself has no LegacyLet
    // dependency.
    install(evaluator, "eval", [&evaluator](const Values& args) {
        if (args.size() != 1 && args.size() != 2)
            throw std::runtime_error("eval expects one or two arguments");
        EnvironmentPtr environment = evaluator.global_environment();
        if (args.size() == 2) {
            if (args[1].is_object() &&
                args[1].as_object()->type() == ObjectType::EvalEnvironment)
                environment =
                    args[1].as_object<EvalEnvironmentObject>()->environment;
            else if (args[1].is_object() &&
                     args[1].as_object()->type() == ObjectType::LegacyLet)
                environment =
                    args[1].as_object<LegacyLetObject>()->environment;
            else
                throw std::runtime_error(
                    "eval expects an eval environment as its second argument");
        }
        return evaluator.eval_values(args[0], std::move(environment));
    });
}

} // namespace

void install_migration_primitives(Evaluator& evaluator) {
    install_legacy_aliases(evaluator);
    install_bootstrap_primitives(evaluator);
    install_legacy_primitives(evaluator);
}

} // namespace goldfish::runtime
