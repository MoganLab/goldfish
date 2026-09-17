#include "runtime/legacy_primitives.hpp"

#include <stdexcept>

namespace goldfish::runtime {

namespace {

void require_arity(const Values& args, std::size_t count,
                   const char* name) {
    if (args.size() != count)
        throw std::runtime_error(std::string(name) + " expects " +
                                 std::to_string(count) + " arguments");
}

EnvironmentPtr legacy_environment(Value value, const char* name) {
    if (!value.is_object())
        throw std::runtime_error(std::string(name) + " expects an inlet");
    if (value.as_object()->type() == ObjectType::LegacyLet)
        return value.as_object<LegacyLetObject>()->environment;
    if (value.as_object()->type() == ObjectType::EvalEnvironment)
        return value.as_object<EvalEnvironmentObject>()->environment;
    throw std::runtime_error(std::string(name) + " expects an inlet");
}

void require_legacy_let(Value value, const char* name) {
    if (!value.is_object() ||
        value.as_object()->type() != ObjectType::LegacyLet)
        throw std::runtime_error(std::string(name) + " expects an inlet");
}

} // namespace

void install_legacy_primitives(Evaluator& evaluator) {
    evaluator.define_primitive("inlet", [&evaluator](const Values& args) {
        if (args.size() % 2 != 0)
            throw std::runtime_error("inlet expects key/value pairs");
        auto object = evaluator.heap().make<LegacyLetObject>();
        for (std::size_t i = 0; i < args.size(); i += 2)
            object->environment->define(args[i], args[i + 1]);
        return Values{Value::object(object)};
    });
    evaluator.define_primitive("let?", [](const Values& args) {
        require_arity(args, 1, "let?");
        return Values{Value::boolean(args[0].is_object() &&
                                     args[0].as_object()->type() ==
                                         ObjectType::LegacyLet)};
    });
    // Transitional compatibility for bootstrap module records.  Lexical
    // Environment remains the real implementation; this wrapper is only the
    // old inlet-shaped API consumed by the bootstrap layer.
    evaluator.define_primitive("sublet", [&evaluator](const Values& args) {
        require_arity(args, 1, "sublet");
        EnvironmentPtr parent;
        if (args[0].is_object() &&
            args[0].as_object()->type() == ObjectType::LegacyLet)
            parent = args[0].as_object<LegacyLetObject>()->environment;
        else if (args[0].is_object() &&
                 args[0].as_object()->type() == ObjectType::EvalEnvironment)
            parent = args[0].as_object<EvalEnvironmentObject>()->environment;
        else
            throw std::runtime_error("sublet expects an inlet or eval environment");
        return Values{Value::object(evaluator.heap().make<LegacyLetObject>(
            std::make_shared<Environment>(std::move(parent))))};
    });
    evaluator.define_primitive("unlet", [](const Values& args) {
        require_arity(args, 1, "unlet");
        require_legacy_let(args[0], "unlet");
        return Values{Value::unspecified()};
    });
    evaluator.define_primitive("with-let", [](const Values& args) {
        if (args.empty()) throw std::runtime_error("with-let expects an inlet");
        require_legacy_let(args[0], "with-let");
        return Values{Value::unspecified()};
    });
    evaluator.define_primitive("let-ref", [](const Values& args) {
        require_arity(args, 2, "let-ref");
        return Values{legacy_environment(args[0], "let-ref")->lookup(args[1])};
    });
    evaluator.define_primitive("let-set!", [](const Values& args) {
        require_arity(args, 3, "let-set!");
        legacy_environment(args[0], "let-set!")->set(args[1], args[2]);
        return Values{Value::unspecified()};
    });
    evaluator.define_primitive("varlet", [](const Values& args) {
        require_arity(args, 3, "varlet");
        require_legacy_let(args[0], "varlet");
        args[0].as_object<LegacyLetObject>()->environment->define(args[1],
                                                                    args[2]);
        return Values{Value::unspecified()};
    });
    evaluator.define_primitive("let->list", [&evaluator](const Values& args) {
        require_arity(args, 1, "let->list");
        require_legacy_let(args[0], "let->list");
        std::vector<Value> result;
        for (const auto& entry :
             args[0].as_object<LegacyLetObject>()->environment->entries())
            result.push_back(evaluator.pair(entry.first, entry.second));
        return Values{evaluator.list(result)};
    });
}

} // namespace goldfish::runtime
