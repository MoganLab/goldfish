#include "runtime/artifact.hpp"

#include "runtime/reader.hpp"

#include <fstream>
#include <iterator>
#include <stdexcept>

namespace goldfish::runtime {

namespace {

bool symbol_named(Value value, const char* name) {
    return value.is_object() &&
           value.as_object()->type() == ObjectType::Symbol &&
           value.as_object<SymbolObject>()->name == name;
}

std::vector<Value> proper_list(Value value) {
    std::vector<Value> result;
    while (!value.is_null()) {
        if (!value.is_object() ||
            value.as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("artifact: expected proper list");
        PairObject* pair = value.as_object<PairObject>();
        result.push_back(pair->car);
        value = pair->cdr;
    }
    return result;
}

std::string library_name(Value value) {
    if (value.is_object() &&
        value.as_object()->type() == ObjectType::Symbol) {
        return "(" + value.as_object<SymbolObject>()->name + ")";
    }
    std::vector<Value> parts = proper_list(value);
    if (parts.empty())
        throw std::runtime_error("artifact: empty library name");
    std::string result = "(";
    for (std::size_t i = 0; i < parts.size(); ++i) {
        if (i != 0) result += ' ';
        if (!symbol_named(parts[i], "rename") &&
            parts[i].is_object() &&
            parts[i].as_object()->type() == ObjectType::Symbol)
            result += parts[i].as_object<SymbolObject>()->name;
        else
            throw std::runtime_error("artifact: invalid library name");
    }
    return result + ")";
}

void collect_import_names(Value value,
                          const ArtifactLoader::DependencyLoader& load) {
    if (!value.is_object() || value.as_object()->type() != ObjectType::Pair)
        return;
    std::vector<Value> items = proper_list(value);
    if (items.empty()) return;
    if (symbol_named(items[0], "rename")) {
        if (items.size() >= 2) collect_import_names(items.back(), load);
        return;
    }
    if (symbol_named(items[0], "only") || symbol_named(items[0], "except") ||
        symbol_named(items[0], "prefix")) {
        if (items.size() >= 2) collect_import_names(items[1], load);
        return;
    }
    bool all_symbols = true;
    for (Value item : items)
        all_symbols = all_symbols && item.is_object() &&
                      item.as_object()->type() == ObjectType::Symbol;
    if (all_symbols) {
        load(library_name(value));
        return;
    }
    for (Value item : items) collect_import_names(item, load);
}

} // namespace

Value ArtifactLoader::call(const char* name, const Values& arguments) {
    auto kernel_procedure = kernel_api_.find(name);
    Value procedure = kernel_procedure == kernel_api_.end()
                          ? evaluator_.eval(evaluator_.symbol(name))
                          : kernel_procedure->second;
    Values result;
    try {
        result = evaluator_.apply_values(procedure, arguments);
    } catch (const std::runtime_error& error) {
        throw std::runtime_error(std::string("artifact: call ") + name +
                                 ": " + error.what());
    }
    if (result.size() != 1)
        throw std::runtime_error(std::string("artifact: ") + name +
                                 " returned multiple values");
    return result[0];
}

void ArtifactLoader::capture_kernel_api() {
    for (const char* name : {"make-exp-library", "exp-library-name",
                             "exp-library-define!", "make-primitive-binding",
                             "make-toplevel-binding", "make-toplevel-ref",
                             "make-syntax", "make-transformer-binding",
                             "module-eval-environment", "make-module",
                             "module-define!", "register-module",
                             "lookup-module",
                             "exp-library-ref-own", "binding-kind",
                             "binding-value", "toplevel-ref-gensym"})
        kernel_api_[name] = evaluator_.eval(evaluator_.symbol(name));
}

Value ArtifactLoader::deserialize_cache_value(Value value) {
    if (!value.is_object()) return value;
    auto found = deserialize_memo_.find(value.as_object());
    if (found != deserialize_memo_.end()) return found->second;
    if (value.as_object()->type() == ObjectType::Pair) {
        auto* pair = value.as_object<PairObject>();
        if (symbol_named(pair->car, "lib*")) {
            std::vector<Value> fields = proper_list(value);
            if (fields.size() != 2)
                throw std::runtime_error("artifact: malformed lib* value");
            std::string name = library_name(fields[1]);
            auto library = exp_libraries_.find(name);
            if (library == exp_libraries_.end())
                throw std::runtime_error("artifact: cache library not loaded: " +
                                         name);
            return library->second;
        }
        if (symbol_named(pair->car, "stx*")) {
            std::vector<Value> fields = proper_list(value);
            if (fields.size() != 4)
                throw std::runtime_error("artifact: malformed stx* value");
            Value form = deserialize_cache_value(fields[1]);
            Value context = deserialize_cache_value(fields[2]);
            Value library = deserialize_cache_value(fields[3]);
            return call("make-syntax", {form, context, library});
        }
        Value result = evaluator_.pair(deserialize_cache_value(pair->car),
                                       deserialize_cache_value(pair->cdr));
        deserialize_memo_[value.as_object()] = result;
        return result;
    }
    if (value.as_object()->type() == ObjectType::Vector) {
        std::vector<Value> values = evaluator_.vector_values(value);
        for (Value& item : values) item = deserialize_cache_value(item);
        Value result = evaluator_.vector(values);
        deserialize_memo_[value.as_object()] = result;
        return result;
    }
    return value;
}

void ArtifactLoader::restore_library_metadata(
    const std::vector<Value>& library, const Value& exp_library) {
    // A library record is (name exports imports bindings macros lowered-defs).
    for (Value entry : proper_list(library[3])) {
        if (!entry.is_object() || entry.as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("artifact: malformed binding entry");
        // Library caches retain a marker for transformer bindings in the
        // binding table; the executable transformer is restored from the
        // separate macros section below.
        auto* entry_pair = entry.as_object<PairObject>();
        if (symbol_named(entry_pair->cdr, "transformer"))
            continue;
        auto fields = proper_list(entry);
        if (fields.size() < 2)
            throw std::runtime_error("artifact: malformed binding entry");
        Value name = fields[0];
        std::vector<Value> descriptor_values(fields.begin() + 1, fields.end());
        Value descriptor_value = evaluator_.list(descriptor_values);
        std::vector<Value> descriptor = proper_list(descriptor_value);
        if (descriptor.empty())
            continue;
        if (symbol_named(descriptor[0], "primitive")) {
            if (descriptor.size() != 2)
                throw std::runtime_error("artifact: malformed primitive binding");
            call("exp-library-define!",
                 {exp_library, name,
                  call("make-primitive-binding", {descriptor[1]})});
            continue;
        }
        if (!symbol_named(descriptor[0], "toplevel") || descriptor.size() < 5)
            throw std::runtime_error("artifact: unsupported binding descriptor");
        Value home = Value::boolean(false);
        std::vector<Value> home_desc;
        if (!descriptor[2].is_boolean() || descriptor[2].as_boolean())
            home_desc = proper_list(descriptor[2]);
        if (!home_desc.empty()) {
            if (home_desc.size() != 2 ||
                !symbol_named(home_desc[0], "libref"))
                throw std::runtime_error("artifact: malformed binding home");
            std::string home_name = library_name(home_desc[1]);
            if (home_name == library_name(library[0]))
                home = exp_library;
            else {
                auto found = exp_libraries_.find(home_name);
                if (found == exp_libraries_.end())
                    throw std::runtime_error("artifact: binding home not loaded: " +
                                             home_name);
                home = found->second;
            }
        }
        Value reference = call("make-toplevel-ref",
                               {descriptor[1], home, descriptor[3],
                                descriptor[4]});
        call("exp-library-define!",
             {exp_library, name, call("make-toplevel-binding", {reference})});
    }

    Value expander = evaluator_.eval(evaluator_.symbol("the-expander-library"));
    Value expand_env = call("module-eval-environment", {expander});
    for (Value macro : proper_list(library[4])) {
        if (!macro.is_object() || macro.as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("artifact: malformed transformer entry");
        auto* pair = macro.as_object<PairObject>();
        deserialize_memo_.clear();
        Value serialized = deserialize_cache_value(pair->cdr);
        if (!expand_env.is_object() ||
            expand_env.as_object()->type() != ObjectType::EvalEnvironment)
            throw std::runtime_error("artifact: invalid expander environment");
        Value transformer = evaluator_.eval(
            serialized,
            expand_env.as_object<EvalEnvironmentObject>()->environment);
        call("exp-library-define!",
             {exp_library, pair->car,
              call("make-transformer-binding", {transformer})});
    }
}

Value ArtifactLoader::load_file(const std::string& path) {
    std::ifstream input(path);
    if (!input)
        throw std::runtime_error("cannot open artifact: " + path);
    std::string source((std::istreambuf_iterator<char>(input)),
                       std::istreambuf_iterator<char>());
    TinyReader reader(evaluator_, std::move(source));
    Value result = Value::unspecified();
    while (std::optional<Value> form = reader.read())
        result = evaluator_.eval(*form);
    return result;
}

Value ArtifactLoader::load_gfo_file(const std::string& path) {
    std::ifstream input(path);
    if (!input)
        throw std::runtime_error("cannot open artifact: " + path);
    std::string source((std::istreambuf_iterator<char>(input)),
                       std::istreambuf_iterator<char>());
    TinyReader reader(evaluator_, std::move(source));
    std::optional<Value> record = reader.read();
    if (!record || reader.read())
        throw std::runtime_error("artifact: gfo must contain one record");

    std::vector<Value> fields = proper_list(*record);
    if (fields.size() < 4 || !fields[0].is_object() ||
        fields[0].as_object()->type() != ObjectType::Symbol ||
        fields[0].as_object<SymbolObject>()->name != "gfo" ||
        !fields[1].is_integer() || fields[1].as_integer() != 0)
        throw std::runtime_error("artifact: unsupported gfo envelope");
    if (fields[3].is_object() &&
        fields[3].as_object()->type() == ObjectType::Pair &&
        symbol_named(fields[3].as_object<PairObject>()->car, "bundle"))
        return load_bundle_gfo_file(path);
    return evaluator_.eval(fields[3]);
}

Value ArtifactLoader::load_library_gfo_file(const std::string& path) {
    std::ifstream input(path);
    if (!input)
        throw std::runtime_error("cannot open artifact: " + path);
    std::string source((std::istreambuf_iterator<char>(input)),
                       std::istreambuf_iterator<char>());
    TinyReader reader(evaluator_, std::move(source));
    std::optional<Value> record = reader.read();
    if (!record || reader.read())
        throw std::runtime_error("artifact: gfo must contain one record");
    std::vector<Value> fields = proper_list(*record);
    if (fields.size() < 4 || !fields[0].is_object() ||
        fields[0].as_object()->type() != ObjectType::Symbol ||
        fields[0].as_object<SymbolObject>()->name != "gfo" ||
        !fields[1].is_integer() || fields[1].as_integer() != 0)
        throw std::runtime_error("artifact: unsupported gfo envelope");

    std::vector<Value> payload = proper_list(fields[3]);
    if (payload.size() < 4 || !symbol_named(payload[0], "bundle") ||
        !payload[1].is_integer() || payload[1].as_integer() != 1)
        throw std::runtime_error("artifact: malformed library payload");
    if (!symbol_named(payload[2], "libraries"))
        throw std::runtime_error("artifact: expected libraries bundle");

    for (std::size_t i = 3; i < payload.size(); ++i) {
        std::vector<Value> section = proper_list(payload[i]);
        if (section.size() >= 2 && symbol_named(section[0], "libs")) {
            for (std::size_t j = 1; j < section.size(); ++j) {
                std::vector<Value> library = proper_list(section[j]);
                if (library.size() < 6)
                    throw std::runtime_error(
                        "artifact: malformed library record (expected six fields)");
                Value exp_library = call("make-exp-library", {library[0]});
                exp_libraries_[library_name(library[0])] = exp_library;
                const std::string name = library_name(library[0]);
                try {
                    restore_library_metadata(library, exp_library);
                } catch (const std::runtime_error& error) {
                    throw std::runtime_error("artifact: metadata for " + name +
                                             ": " + error.what());
                }
                std::size_t index = 0;
                for (Value definition : proper_list(library[5])) {
                    try {
                        evaluator_.eval(definition);
                    } catch (const std::runtime_error& error) {
                        throw std::runtime_error(
                            "artifact: definition " + std::to_string(index) +
                            " for " + name + ": " + error.what());
                    }
                    ++index;
                }
                // A cache is also a runtime module artifact.  Rebuild its
                // value module explicitly so native loading does not depend
                // on a registration expression finding the right global
                // helper after another library has introduced a same-named
                // binding.
                Value module = call("make-module", {library[0]});
                for (Value export_name : proper_list(library[1])) {
                    Value binding = call("exp-library-ref-own",
                                         {exp_library, export_name});
                    if (binding.is_boolean() && !binding.as_boolean()) continue;
                    Value kind = call("binding-kind", {binding});
                    Value value = Value::boolean(false);
                    if (symbol_named(kind, "toplevel")) {
                        Value ref = call("binding-value", {binding});
                        Value gensym = call("toplevel-ref-gensym", {ref});
                        value = evaluator_.eval(gensym);
                    } else if (symbol_named(kind, "primitive")) {
                        value = evaluator_.eval(call("binding-value", {binding}));
                    } else if (symbol_named(kind, "transformer")) {
                        value = call("binding-value", {binding});
                    } else {
                        continue;
                    }
                    call("module-define!", {module, export_name, value});
                }
                call("register-module", {module});
                (void)call("lookup-module", {library[0]});
            }
            return Value::unspecified();
        }
    }
    throw std::runtime_error("artifact: libraries bundle has no libs section");
}

Value ArtifactLoader::load_bundle_gfo_file(const std::string& path) {
    return load_bundle_gfo_file(path, {});
}

Value ArtifactLoader::load_bundle_gfo_file(
    const std::string& path, const DependencyLoader& load_dependency) {
    std::ifstream input(path);
    if (!input)
        throw std::runtime_error("cannot open artifact: " + path);
    std::string source((std::istreambuf_iterator<char>(input)),
                       std::istreambuf_iterator<char>());
    TinyReader reader(evaluator_, std::move(source));
    std::optional<Value> record = reader.read();
    if (!record || reader.read())
        throw std::runtime_error("artifact: gfo must contain one record");
    std::vector<Value> fields = proper_list(*record);
    if (fields.size() < 4 || !symbol_named(fields[0], "gfo") ||
        !fields[1].is_integer() || fields[1].as_integer() != 0)
        throw std::runtime_error("artifact: unsupported gfo envelope");
    std::vector<Value> bundle = proper_list(fields[3]);
    if (bundle.size() < 4 || !symbol_named(bundle[0], "bundle") ||
        !bundle[1].is_integer() || bundle[1].as_integer() != 1)
        throw std::runtime_error("artifact: malformed bundle");

    if (symbol_named(bundle[2], "module")) {
        Value defs = Value::null();
        Value macros = Value::null();
        Value bindings = Value::null();
        for (std::size_t i = 3; i < bundle.size(); ++i) {
            std::vector<Value> section = proper_list(bundle[i]);
            if (section.empty()) continue;
            if (symbol_named(section[0], "defs"))
                defs = bundle[i];
            else if (symbol_named(section[0], "macros"))
                macros = bundle[i];
            else if (symbol_named(section[0], "bindings"))
                bindings = bundle[i];
        }
        if (defs.is_null())
            throw std::runtime_error("artifact: module bundle has no defs section");
        std::vector<Value> definition_section = proper_list(defs);
        for (std::size_t i = 1; i < definition_section.size(); ++i)
            evaluator_.eval(definition_section[i]);

        // Module cache records are produced for an existing library (the
        // bootstrap library in the current pipeline), so their metadata is
        // installed into that library after the lowered definitions run.
        Value base = evaluator_.eval(evaluator_.symbol("the-base-library"));
        exp_libraries_[library_name(call("exp-library-name", {base}))] = base;
        Value binding_entries = bindings.is_null()
                                    ? Value::null()
                                    : bindings.as_object<PairObject>()->cdr;
        Value macro_entries = macros.is_null()
                                  ? Value::null()
                                  : macros.as_object<PairObject>()->cdr;
        Value record = evaluator_.list(
            {call("exp-library-name", {base}), Value::null(), Value::null(),
             binding_entries, macro_entries, Value::null()});
        restore_library_metadata(proper_list(record), base);
        return Value::unspecified();
    }
    if (symbol_named(bundle[2], "program")) {
        for (std::size_t i = 3; i < bundle.size(); ++i) {
            std::vector<Value> section = proper_list(bundle[i]);
            if (section.size() >= 1 && symbol_named(section[0], "exprs")) {
                Value result = Value::unspecified();
                for (std::size_t j = 1; j < section.size(); ++j)
                    result = evaluator_.eval(deserialize_cache_value(section[j]));
                return result;
            }
        }
        throw std::runtime_error("artifact: program bundle has no exprs section");
    }
    if (symbol_named(bundle[2], "libraries")) {
        if (load_dependency) {
            for (std::size_t i = 3; i < bundle.size(); ++i) {
                std::vector<Value> section = proper_list(bundle[i]);
                if (section.size() >= 2 && symbol_named(section[0], "libs")) {
                    for (std::size_t j = 1; j < section.size(); ++j) {
                        std::vector<Value> library = proper_list(section[j]);
                        if (library.size() < 6)
                            throw std::runtime_error(
                                "artifact: malformed library record");
                        collect_import_names(library[2], load_dependency);
                    }
                }
            }
        }
        return load_library_gfo_file(path);
    }
    throw std::runtime_error("artifact: unsupported bundle kind");
}

} // namespace goldfish::runtime
