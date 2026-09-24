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

bool truth(Value value) { return !value.is_boolean() || value.as_boolean(); }

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
            if (args[0] == rest.as_object<PairObject>()->car) return Values{rest};
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
            if (item == args[0]) return Values{rest};
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
                args[0] == entry.as_object<PairObject>()->car) return Values{entry};
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
                entry.as_object<PairObject>()->car == args[0])
                return Values{entry};
            rest = rest.as_object<PairObject>()->cdr;
        }
        return Values{Value::boolean(false)};
    });
    install(evaluator, "member", [&evaluator](const Values& args) {
        if (args.size() != 2 && args.size() != 3)
            throw std::runtime_error("member expects two or three arguments");
        Value comparator = args.size() == 3
                              ? args[2]
                              : evaluator.global_environment()->lookup(
                                    evaluator.symbol("equal?"));
        Value rest = args[1];
        while (!rest.is_null()) {
            if (!rest.is_object() || rest.as_object()->type() != ObjectType::Pair)
                throw std::runtime_error("member expects a proper list");
            Values matched = evaluator.apply_values(
                comparator, {args[0], rest.as_object<PairObject>()->car});
            if (matched.size() != 1)
                throw std::runtime_error("member comparator returned multiple values");
            if (truth(matched[0])) return Values{rest};
            rest = rest.as_object<PairObject>()->cdr;
        }
        return Values{Value::boolean(false)};
    });
    install(evaluator, "assoc", [&evaluator](const Values& args) {
        if (args.size() != 2 && args.size() != 3)
            throw std::runtime_error("assoc expects two or three arguments");
        Value comparator = args.size() == 3
                              ? args[2]
                              : evaluator.global_environment()->lookup(
                                    evaluator.symbol("equal?"));
        Value rest = args[1];
        while (!rest.is_null()) {
            if (!rest.is_object() || rest.as_object()->type() != ObjectType::Pair)
                throw std::runtime_error("assoc expects an association list");
            Value entry = rest.as_object<PairObject>()->car;
            if (entry.is_object() && entry.as_object()->type() == ObjectType::Pair) {
                Values matched = evaluator.apply_values(
                    comparator, {args[0], entry.as_object<PairObject>()->car});
                if (matched.size() != 1)
                    throw std::runtime_error("assoc comparator returned multiple values");
                if (truth(matched[0])) return Values{entry};
            }
            rest = rest.as_object<PairObject>()->cdr;
        }
        return Values{Value::boolean(false)};
    });
    install(evaluator, "filter", [&evaluator](const Values& args) {
        require_arity(args, 2, "filter");
        std::vector<Value> result;
        for (Value value : proper_list(args[1])) {
            Values selected = evaluator.apply_values(args[0], {value});
            if (selected.size() != 1)
                throw std::runtime_error("filter predicate returned multiple values");
            if (truth(selected[0])) result.push_back(value);
        }
        return Values{evaluator.list(result)};
    });
    install(evaluator, "any", [&evaluator](const Values& args) {
        require_arity(args, 2, "any");
        for (Value value : proper_list(args[1])) {
            Values result = evaluator.apply_values(args[0], {value});
            if (result.size() != 1)
                throw std::runtime_error("any predicate returned multiple values");
            if (truth(result[0])) return Values{result[0]};
        }
        return Values{Value::boolean(false)};
    });
    install(evaluator, "every", [&evaluator](const Values& args) {
        require_arity(args, 2, "every");
        Value last = Value::boolean(true);
        for (Value value : proper_list(args[1])) {
            Values result = evaluator.apply_values(args[0], {value});
            if (result.size() != 1)
                throw std::runtime_error("every predicate returned multiple values");
            last = result[0];
            if (!truth(last)) return Values{Value::boolean(false)};
        }
        return Values{last};
    });
    install(evaluator, "fold", [&evaluator](const Values& args) {
        require_arity(args, 3, "fold");
        Value result = args[1];
        for (Value value : proper_list(args[2])) {
            Values next = evaluator.apply_values(args[0], {result, value});
            if (next.size() != 1)
                throw std::runtime_error("fold procedure returned multiple values");
            result = next[0];
        }
        return Values{result};
    });
    install(evaluator, "map", [&evaluator](const Values& args) {
        if (args.size() < 2) throw std::runtime_error("map expects procedure and list");
        std::vector<std::vector<Value>> lists;
        for (std::size_t i = 1; i < args.size(); ++i)
            lists.push_back(proper_list(args[i]));
        std::vector<Value> result;
        for (std::size_t i = 0; i < lists[0].size(); ++i) {
            Values call_args;
            for (const auto& list : lists) {
                if (i >= list.size()) throw std::runtime_error("map list lengths differ");
                call_args.push_back(list[i]);
            }
            Values values = evaluator.apply_values(args[0], call_args);
            if (values.size() != 1) throw std::runtime_error("map procedure returned multiple values");
            result.push_back(values[0]);
        }
        return Values{evaluator.list(result)};
    });
    install(evaluator, "for-each", [&evaluator](const Values& args) {
        if (args.size() < 2) throw std::runtime_error("for-each expects procedure and list");
        for (Value value : proper_list(args[1]))
            evaluator.apply_values(args[0], {value});
        return Values{Value::unspecified()};
    });
}

} // namespace goldfish::runtime
