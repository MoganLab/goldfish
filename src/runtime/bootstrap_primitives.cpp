#include "runtime/bootstrap_primitives.hpp"

#include <algorithm>
#include <stdexcept>

namespace goldfish::runtime {

namespace {

void require_arity(const Values& args, std::size_t count, const char* name) {
    if (args.size() != count)
        throw std::runtime_error(std::string(name) + " expects " +
                                 std::to_string(count) + " arguments");
}

std::vector<Value> proper_list(Value value) {
    std::vector<Value> result;
    while (!value.is_null()) {
        if (!value.is_object() ||
            value.as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("expected proper list");
        auto* pair = value.as_object<PairObject>();
        result.push_back(pair->car);
        value = pair->cdr;
    }
    return result;
}

// eq?/eqv?-style comparison for the membership primitives: characters and
// eof are immediate on the host and compare by value, everything else by
// identity.  The Scheme reader's `case ch' dispatch runs through memv, so
// identity comparison on characters broke every warm read.
bool identical(Value left, Value right) {
    if (left == right) return true;
    if (left.is_object() && right.is_object()) {
        if (left.as_object()->type() == ObjectType::Character &&
            right.as_object()->type() == ObjectType::Character)
            return left.as_object<CharacterObject>()->value ==
                   right.as_object<CharacterObject>()->value;
        if (left.as_object()->type() == ObjectType::Eof &&
            right.as_object()->type() == ObjectType::Eof)
            return true;
    }
    return false;
}

void install(Evaluator& evaluator, const char* name,
             PrimitiveObject::Function function) {
    evaluator.define_primitive(name, std::move(function));
}

} // namespace

void install_bootstrap_primitives(Evaluator& evaluator) {
    install(evaluator, "not", [](const Values& args) {
        require_arity(args, 1, "not");
        return Values{Value::boolean(args[0].is_boolean() && !args[0].as_boolean())};
    });
    install(evaluator, "length", [](const Values& args) {
        require_arity(args, 1, "length");
        return Values{Value::integer(static_cast<std::int64_t>(
            proper_list(args[0]).size()))};
    });
    install(evaluator, "reverse", [&evaluator](const Values& args) {
        require_arity(args, 1, "reverse");
        auto values = proper_list(args[0]);
        std::reverse(values.begin(), values.end());
        return Values{evaluator.list(values)};
    });
    install(evaluator, "append", [&evaluator](const Values& args) {
        std::vector<Value> result;
        for (std::size_t i = 0; i + 1 < args.size(); ++i) {
            auto part = proper_list(args[i]);
            result.insert(result.end(), part.begin(), part.end());
        }
        Value tail = args.empty() ? Value::null() : args.back();
        for (auto it = result.rbegin(); it != result.rend(); ++it)
            tail = evaluator.pair(*it, tail);
        return Values{tail};
    });
    install(evaluator, "memq", [](const Values& args) {
        require_arity(args, 2, "memq");
        Value rest = args[1];
        while (!rest.is_null()) {
            if (!rest.is_object() || rest.as_object()->type() != ObjectType::Pair)
                throw std::runtime_error("memq expects a proper list");
            if (identical(args[0], rest.as_object<PairObject>()->car))
                return Values{rest};
            rest = rest.as_object<PairObject>()->cdr;
        }
        return Values{Value::boolean(false)};
    });
    install(evaluator, "memv", [](const Values& args) {
        require_arity(args, 2, "memv");
        Value rest = args[1];
        while (!rest.is_null()) {
            if (!rest.is_object() || rest.as_object()->type() != ObjectType::Pair)
                throw std::runtime_error("memv expects a proper list");
            Value item = rest.as_object<PairObject>()->car;
            if (identical(item, args[0])) return Values{rest};
            rest = rest.as_object<PairObject>()->cdr;
        }
        return Values{Value::boolean(false)};
    });
    install(evaluator, "assq", [](const Values& args) {
        require_arity(args, 2, "assq");
        Value rest = args[1];
        while (!rest.is_null()) {
            if (!rest.is_object() || rest.as_object()->type() != ObjectType::Pair)
                throw std::runtime_error("assq expects an association list");
            Value entry = rest.as_object<PairObject>()->car;
            if (entry.is_object() && entry.as_object()->type() == ObjectType::Pair &&
                identical(args[0], entry.as_object<PairObject>()->car))
                return Values{entry};
            rest = rest.as_object<PairObject>()->cdr;
        }
        return Values{Value::boolean(false)};
    });
    install(evaluator, "assv", [](const Values& args) {
        require_arity(args, 2, "assv");
        Value rest = args[1];
        while (!rest.is_null()) {
            if (!rest.is_object() || rest.as_object()->type() != ObjectType::Pair)
                throw std::runtime_error("assv expects an association list");
            Value entry = rest.as_object<PairObject>()->car;
            if (entry.is_object() && entry.as_object()->type() == ObjectType::Pair &&
                identical(entry.as_object<PairObject>()->car, args[0]))
                return Values{entry};
            rest = rest.as_object<PairObject>()->cdr;
        }
        return Values{Value::boolean(false)};
    });
    // Higher-order iteration state lives on the evaluator control stack so
    // multi-shot continuations can resume callbacks and their loops.
    evaluator.define_machine_primitive("map", PrimitiveObject::Kind::Map);
    evaluator.define_machine_primitive("for-each",
                                       PrimitiveObject::Kind::ForEach);
    evaluator.define_machine_primitive("fold", PrimitiveObject::Kind::Fold);
    evaluator.define_machine_primitive("filter",
                                       PrimitiveObject::Kind::Filter);
    evaluator.define_machine_primitive("any", PrimitiveObject::Kind::Any);
    evaluator.define_machine_primitive("every", PrimitiveObject::Kind::Every);
    evaluator.define_machine_primitive("member",
                                       PrimitiveObject::Kind::Member);
    evaluator.define_machine_primitive("assoc", PrimitiveObject::Kind::Assoc);
}

} // namespace goldfish::runtime
