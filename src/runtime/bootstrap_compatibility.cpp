#include "runtime/bootstrap_compatibility.hpp"

#include "runtime/bootstrap_primitives.hpp"

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
                    throw std::runtime_error(
                        "setter procedure expects two arguments");
                return Values{Value::unspecified()};
            }))};
    });
    install_bootstrap_primitives(evaluator);
}

} // namespace goldfish::runtime
