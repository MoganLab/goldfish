#include "runtime/bootstrap_primitives.hpp"

#include <algorithm>
#include <unordered_map>
#include <unordered_set>
#include <stdexcept>

namespace goldfish::runtime {

namespace {

void require_arity(const Values& args, std::size_t count, const char* name) {
    if (args.size() != count)
        throw std::runtime_error(std::string(name) + " expects " +
                                 std::to_string(count) + " arguments");
}

std::vector<Value> proper_list(Value value) {
    if (!is_proper_list(value))
        throw std::runtime_error("expected proper list");
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
            if (equivalent(item, args[0])) return Values{rest};
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
                equivalent(entry.as_object<PairObject>()->car, args[0]))
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

    // %interface-table : src-library (list (visible . original)) strict?
    //                    lib-name -> exp-library
    // Native bulk build of an import interface table -- the exp-library the
    // Scheme import-view constructs per distinct import set.  Semantics are
    // the interpreted loop's, byte for byte: bindings resolve through the
    // source's own table then its uses (exp-library-ref), a visible name
    // bound twice with different bindings raises under key 'import, a
    // missing original raises under strict only.  The interpreted loop cost
    // ~35 us per entry (scheme/base's (goldfish) import: 260 ms); building
    // the same buckets natively removes the per-entry interpreter dispatch.
    // %interface-table : src-library (list (visible . original)) strict?
    //                    lib-name -> exp-library
    // Native bulk build of an import interface table -- the exp-library the
    // Scheme import-view constructs per distinct import set.  Semantics are
    // the interpreted loop's: bindings resolve through the source's own
    // table then its uses (exp-library-ref), a visible name bound twice with
    // different bindings raises under key 'import, a missing original
    // raises under strict only.  The interpreted loop cost ~35 us per entry
    // (scheme/base's (goldfish) import: ~260 ms of warm boot); this version
    // materializes the source table once and issues one kernel call per
    // insertion, keeping every record layout detail inside the kernel.
    install(evaluator, "%interface-table",
            [&evaluator](const Values& args) -> Values {
        require_arity(args, 4, "%interface-table");
        const Value src = args[0];
        Value pairs = args[1];
        const bool strict = args[2].is_boolean() && args[2].as_boolean();
        const Value lib_name = args[3];

        auto kcall = [&evaluator](const char* name, const Values& arguments) {
            return evaluator.apply_values(evaluator.eval(evaluator.symbol(name)),
                                          arguments)[0];
        };

        // exp-library-ref semantics: the source's own table, then each use's
        // own table, newest first, one level deep.  exp-library-bindings
        // materializes OWN definitions only -- re-exported imports (most of
        // (scheme base)'s surface) live in the uses -- so the uses' tables
        // fold in here too, first-writer-wins to match the newest-first
        // shadowing.
        std::unordered_map<std::string, Value> table;
        auto absorb = [&](Value owner) {
            Value rest = kcall("exp-library-bindings", {owner});
            while (rest.is_object() &&
                   rest.as_object()->type() == ObjectType::Pair) {
                auto* cell = rest.as_object<PairObject>();
                if (cell->car.is_object() &&
                    cell->car.as_object()->type() == ObjectType::Pair) {
                    auto* entry = cell->car.as_object<PairObject>();
                    if (entry->car.is_object() &&
                        entry->car.as_object()->type() ==
                            ObjectType::Symbol)
                        table.emplace(
                            entry->car.as_object<SymbolObject>()->name,
                            entry->cdr);
                }
                rest = cell->cdr;
            }
        };
        absorb(src);
        {
            Value uses = kcall("exp-library-uses", {src});
            while (uses.is_object() &&
                   uses.as_object()->type() == ObjectType::Pair) {
                Value view = uses.as_object<PairObject>()->car;
                if (view.is_object() &&
                    view.as_object()->type() == ObjectType::Pair)
                    view = view.as_object<PairObject>()->car;
                absorb(view);
                uses = uses.as_object<PairObject>()->cdr;
            }
        }

        const Value iface = kcall("make-exp-library",
                                  {kcall("exp-library-name", {src})});

        // Raises mirror the interpreted (error 'import "~a..." arg ...) shape
        // exactly: message = the format string, irritants = its arguments --
        // load-library-guard rebuilds the detail by formatting them.
        std::unordered_set<std::string> inserted;
        while (pairs.is_object() &&
               pairs.as_object()->type() == ObjectType::Pair) {
            // Each cell of `pairs' holds one (visible . original) mapping.
            auto* mapped =
                pairs.as_object<PairObject>()->car.as_object<PairObject>();
            const Value visible = mapped->car;
            const Value original = mapped->cdr;
            auto found = table.find(original.as_object<SymbolObject>()->name);
            if (found == table.end()) {
                if (strict)
                    // Identical to (error 'import "~a..." original lib-name):
                    // a ThrownValue, so load-library-guard's catch sees the
                    // same (format-string arg ...) payload the interpreted
                    // import-view produced.
                    throw ThrownValue(evaluator.symbol("import"),
                                      {evaluator.string("~a has no binding in ~a"),
                                       original, lib_name});
            } else if (inserted.insert(
                               visible.as_object<SymbolObject>()->name)
                               .second) {
                kcall("exp-library-define!", {iface, visible, found->second});
            } else {
                Value prior = kcall("exp-library-ref-own", {iface, visible});
                if (!(prior == found->second))
                    throw ThrownValue(
                        evaluator.symbol("import"),
                        {evaluator.string(
                             "~a bound more than once with different "
                             "bindings (~a)"),
                         visible, lib_name});
            }
            pairs = pairs.as_object<PairObject>()->cdr;
        }
        return Values{iface};
    });
}

} // namespace goldfish::runtime
