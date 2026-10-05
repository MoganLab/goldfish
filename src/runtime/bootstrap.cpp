#include "runtime/bootstrap.hpp"

#include "runtime/bootstrap_primitives.hpp"
#include "runtime/debug_flags.hpp"
#include "runtime/standard_primitives.hpp"
#include "runtime/reader.hpp"

#include <cstdlib>
#include <array>
#include <cstdio>
#include <filesystem>
#include <stdexcept>
#include <vector>
#include <algorithm>
#include <fstream>
#include <iterator>

#if defined(__unix__) || defined(__APPLE__)
#include <sys/resource.h>
#endif

namespace goldfish::runtime {

namespace {

constexpr std::array<const char*, 12> native_bootstrap_artifacts = {
    "expander/lib/syntax-runtime.scm-o2.gfo",
    "expander/lib/syntax-case.scm-o2.gfo",
    "expander/lib/define-record-type.scm-o2.gfo",
    "expander/lib/core-macros.scm-o2.gfo",
    "expander/lib/cond-expand.scm-o2.gfo",
    "expander/lib/defmacro.scm-o2.gfo",
    "expander/lib/define-star.scm-o2.gfo",
    "expander/lib/module-registry.scm-o2.gfo",
    "expander/lib/module.scm-o2.gfo",
    "expander/lib/standard.scm-o2.gfo",
    "scheme/case-lambda.scm-o2.gfo",
    "scheme/base.scm-o2.gfo"
};

namespace fs = std::filesystem;

void prepare_native_stack() {
#if defined(__unix__) || defined(__APPLE__)
    struct rlimit limits {};
    if (getrlimit(RLIMIT_STACK, &limits) == 0 &&
        limits.rlim_cur < limits.rlim_max) {
        // Expanding the kernel and compiler is ordinary Scheme evaluation,
        // not a separate VM stack.  Use the process' permitted stack budget
        // so a valid deeply nested syntax tree cannot fail as a random C++
        // segmentation fault during bootstrap.
        limits.rlim_cur = limits.rlim_max;
        (void)setrlimit(RLIMIT_STACK, &limits);
    }
#endif
}

fs::path cache_root_from_environment() {
    if (const char* override_root = std::getenv("GOLDFISH_CACHE_DIR"))
        if (*override_root) return fs::path(override_root);
    if (const char* xdg = std::getenv("XDG_CACHE_HOME"))
        if (*xdg) return fs::path(xdg) / "goldfish" / "native-ccache";
    if (const char* home = std::getenv("HOME"))
        if (*home)
            return fs::path(home) / ".cache" / "goldfish" / "native-ccache";
    return fs::path(".goldfish-native-cache");
}

Value call(Evaluator& evaluator, const char* name, const Values& args = {}) {
    return evaluator.apply_values(evaluator.eval(evaluator.symbol(name)), args)[0];
}

std::vector<Value> list_values(Value value) {
    std::vector<Value> result;
    while (!value.is_null()) {
        if (!value.is_object() || value.as_object()->type() != ObjectType::Pair)
            throw std::runtime_error("native bootstrap cache: expected proper list");
        auto* pair = value.as_object<PairObject>();
        result.push_back(pair->car);
        value = pair->cdr;
    }
    return result;
}

bool named(Value value, const char* name) {
    return value.is_object() && value.as_object()->type() == ObjectType::Symbol &&
           value.as_object<SymbolObject>()->name == name;
}

fs::path locate(Evaluator& evaluator, const std::string& relative) {
    for (Value directory : list_values(call(evaluator, "g_load-path"))) {
        fs::path path = fs::path(evaluator.string_value(directory)) / relative;
        if (fs::exists(path)) return path;
    }
    return relative;
}

std::string file_hash(Evaluator& evaluator, const char* primitive, const fs::path& path) {
    Value hash = call(evaluator, primitive, {evaluator.string(path.string())});
    return hash.is_boolean() ? "-" : evaluator.string_value(hash);
}

std::string cache_version(Evaluator& evaluator) {
    // Keep the pipeline inputs identical to core/gfo.scm.
    std::vector<std::string> files = {
        "core/gfo.scm", "core/ir.scm", "liii/prelude.scm", "liii/reader.scm",
        "expander/bootstrap-prelude.scm", "scheme/base.scm", "scheme/case-lambda.scm",
        "expander/kernel-combined.scm", "compiler.scm", "expander/tree-il.scm"
    };
    for (const char* directory : {"expander/lib", "compiler"}) {
        std::vector<std::string> entries;
        std::error_code error;
        fs::path path = locate(evaluator, directory);
        for (const auto& entry : fs::directory_iterator(path, error)) {
            if (entry.path().extension() == ".scm")
                entries.push_back(std::string(directory) + "/" + entry.path().filename().string());
        }
        if (error) throw std::runtime_error("native bootstrap cache: unreadable pipeline directory");
        std::sort(entries.begin(), entries.end());
        files.insert(files.end(), entries.begin(), entries.end());
    }
    std::string input = "runtime:" + file_hash(evaluator, "g_sha256-by-file",
        evaluator.string_value(call(evaluator, "g_executable"))) + ";";
    for (const auto& file : files)
        input += file + ":" + file_hash(evaluator, "g_sha256-by-file", locate(evaluator, file)) + ";";
    return "v" + evaluator.string_value(call(evaluator, "g_sha256", {evaluator.string(input)})).substr(0, 12);
}

std::vector<Value> source_stamp(Evaluator& evaluator, const fs::path& source) {
    if (!fs::is_regular_file(source))
        throw std::runtime_error("native bootstrap cache: missing source " + source.string());
    Value path = evaluator.string(source.string());
    return {call(evaluator, "g_path-getmtime", {path}),
            call(evaluator, "g_path-getsize", {path}),
            call(evaluator, "g_md5-by-file", {path})};
}

void validate_artifact(Evaluator& evaluator, const fs::path& artifact,
                       const std::string& relative) {
    std::ifstream stream(artifact);
    if (!stream) throw std::runtime_error("missing artifact " + artifact.string());
    std::string contents((std::istreambuf_iterator<char>(stream)), {});
    TinyReader reader(evaluator, std::move(contents));
    auto record = reader.read();
    if (!record || reader.read()) throw std::runtime_error("expected one gfo record");
    auto fields = list_values(*record);
    if (fields.size() < 4 || fields.size() > 5 || !named(fields[0], "gfo") ||
        !equal(fields[1], Value::integer(0)))
        throw std::runtime_error("unsupported gfo envelope");
    auto stamp = source_stamp(evaluator, locate(evaluator, relative));
    auto kernel = source_stamp(evaluator, locate(evaluator, "expander/kernel-combined.scm"));
    stamp.insert(stamp.end(), kernel.begin(), kernel.end());
    stamp.push_back(evaluator.symbol("engine-abi"));
    stamp.push_back(Value::integer(1));
    if (!equal(fields[2], evaluator.list(stamp))) throw std::runtime_error("stale source stamp");
    auto bundle = list_values(fields[3]);
    if (bundle.size() < 4 || !named(bundle[0], "bundle") ||
        !equal(bundle[1], Value::integer(1)) ||
        !(named(bundle[2], "module") || named(bundle[2], "libraries")))
        throw std::runtime_error("malformed bootstrap bundle");
    bool defs = false, macros = false, bindings = false, libraries = false;
    for (std::size_t i = 3; i < bundle.size(); ++i) {
        auto section = list_values(bundle[i]);
        if (section.empty()) throw std::runtime_error("empty bundle section");
        defs |= named(section[0], "defs");
        macros |= named(section[0], "macros");
        bindings |= named(section[0], "bindings");
        libraries |= named(section[0], "libs");
    }
    if ((named(bundle[2], "module") && !(defs && macros && bindings)) ||
        (named(bundle[2], "libraries") && !libraries))
        throw std::runtime_error("incomplete bootstrap bundle");
    if (fields.size() == 5 && !(fields[4].is_boolean() && !fields[4].as_boolean())) {
        for (Value dependency : list_values(fields[4])) {
            if (!dependency.is_object() || dependency.as_object()->type() != ObjectType::Pair)
                throw std::runtime_error("invalid dependency stamp");
            auto* pair = dependency.as_object<PairObject>();
            const bool external = named(pair->cdr, "external");
            auto stored = external ? std::vector<Value>{pair->car, pair->cdr} : list_values(dependency);
            if (stored.size() < 2) throw std::runtime_error("invalid dependency stamp");
            std::string source;
            for (Value part : list_values(stored[0])) {
                if (!source.empty()) source += '/';
                if (part.is_object() && part.as_object()->type() == ObjectType::Symbol)
                    source += part.as_object<SymbolObject>()->name;
                else if (is_number(part) && number_value(part).is_exact() &&
                         number_value(part).is_integer() && !number_value(part).real.numerator.negative())
                    source += number_to_string(part);
                else throw std::runtime_error("invalid dependency name");
            }
            source += ".scm";
            auto path = locate(evaluator, source);
            if (external) {
                if (fs::exists(path)) throw std::runtime_error("external dependency gained a source");
            } else {
                auto current = source_stamp(evaluator, path);
                current.insert(current.begin(), stored[0]);
                if (!equal(dependency, evaluator.list(current)))
                    throw std::runtime_error("stale dependency " + source);
            }
        }
    }
}

} // namespace

NativeBootstrap::NativeBootstrap(Runtime& runtime)
    : runtime_(runtime), loader_(runtime.evaluator()) {
    prepare_native_stack();
}

void NativeBootstrap::install_primitives() {
    if (primitives_installed_) return;
    install_runtime_primitives(runtime_.evaluator());
    install_bootstrap_primitives(runtime_.evaluator());
    native_read_forms_ = runtime_.evaluator().global_environment()->lookup(
        runtime_.evaluator().symbol("read-forms"));
    primitives_installed_ = true;
}

Value NativeBootstrap::load_kernel(const std::string& path) {
    if (kernel_loaded_) return Value::unspecified();
    Value result = loader_.load_file(path);
    loader_.capture_kernel_api();
    native_module_eval_environment_ = runtime_.evaluator().eval(
        runtime_.evaluator().symbol("module-eval-environment"));
    Value register_module = runtime_.evaluator().eval(
        runtime_.evaluator().symbol("register-module"));
    Value expander_library = runtime_.evaluator().eval(
        runtime_.evaluator().symbol("the-expander-library"));
    // the-expander-library is constructed while the kernel is evaluated;
    // install its native environment explicitly before loading libraries.
    Value eval_environment = runtime_.evaluator().apply_values(
        native_module_eval_environment_, {expander_library})[0];
    // Designate this frame as the defs root: every later parentless frame
    // (module environments, program environments) descends from it, so bare
    // gensym refs resolve while module frames stay isolated.
    runtime_.evaluator().set_defs_root(
        eval_environment.as_object<EvalEnvironmentObject>()->environment);
    runtime_.evaluator().apply_values(register_module, {expander_library});
    // kernel-combined is the implementation library itself and is already
    // installed in the evaluator; imports of (goldfish) must not ask the
    // artifact registry for a second copy.
    loaded_.insert("(goldfish)");
    kernel_loaded_ = true;
    return result;
}

std::string NativeBootstrap::cache_directory(const std::string& cache_root) {
    const fs::path root = cache_root.empty() ? cache_root_from_environment()
                                             : fs::path(cache_root);
    const std::string tag = cache_version(runtime_.evaluator());
    return (root.filename() == tag ? root : root / tag).string();
}

std::string NativeBootstrap::validate_cached_runtime(const std::string& cache_root) {
    const fs::path version = cache_directory(cache_root);
    if (!fs::is_directory(version))
        throw std::runtime_error("native bootstrap cache not found under " +
                                 version.string());
    for (const char* artifact : native_bootstrap_artifacts) {
        std::string source = artifact;
        source.resize(source.size() - std::string("-o2.gfo").size());
        try {
            validate_artifact(runtime_.evaluator(), version / artifact, source);
        } catch (const std::exception& error) {
            throw std::runtime_error("native bootstrap cache " + (version / artifact).string() +
                                     ": " + error.what());
        }
    }
    return version.string();
}

void NativeBootstrap::load_cached_runtime(const std::string& cache_root) {
    const fs::path version = validate_cached_runtime(cache_root);

    // This order is the dependency order of the current bootstrap chain.
    // It is deliberately kept here, next to the native bootstrap boundary;
    // ordinary libraries use the Scheme module loader after this point.
    const std::vector<fs::path> artifacts = {
        "expander/lib/syntax-runtime.scm-o2.gfo",
        "expander/lib/syntax-case.scm-o2.gfo",
        "expander/lib/define-record-type.scm-o2.gfo",
        "expander/lib/core-macros.scm-o2.gfo",
        "expander/lib/cond-expand.scm-o2.gfo",
        "expander/lib/defmacro.scm-o2.gfo",
        "expander/lib/define-star.scm-o2.gfo",
        "expander/lib/module-registry.scm-o2.gfo",
        "expander/lib/module.scm-o2.gfo",
        "expander/lib/standard.scm-o2.gfo",
        "scheme/case-lambda.scm-o2.gfo",
    };
    for (const fs::path& artifact : artifacts) {
        const fs::path path = version / artifact;
        std::error_code error;
        if (!fs::is_regular_file(path, error))
            throw std::runtime_error("native bootstrap artifact missing: " +
                                     path.string());
        try {
            load_artifact(path.string());
        } catch (const std::exception& error) {
            throw std::runtime_error("native bootstrap artifact " +
                                     path.string() + ": " + error.what());
        }
    }
    deferred_base_artifact_ = (version / "scheme/base.scm-o2.gfo").string();
    // The Scheme source reader is loaded by the driver after the installer;
    // keep its bootstrap dependency on the native reader primitive intact.
    runtime_.evaluator().global_environment()->define(
        runtime_.evaluator().symbol("read-forms"), native_read_forms_);
    runtime_.evaluator().global_environment()->define(
        runtime_.evaluator().symbol("module-eval-environment"),
        native_module_eval_environment_);
    Value base_library = runtime_.evaluator().eval(
        runtime_.evaluator().symbol("the-base-library"));
    Value read_forms_binding = runtime_.evaluator().apply_values(
        runtime_.evaluator().eval(
            runtime_.evaluator().symbol("make-primitive-binding")),
        {runtime_.evaluator().symbol("read-forms")})[0];
    runtime_.evaluator().apply_values(
        runtime_.evaluator().eval(
            runtime_.evaluator().symbol("exp-library-define!")),
        {base_library, runtime_.evaluator().symbol("read-forms"),
         read_forms_binding});
}

void NativeBootstrap::load_cached_base_runtime() {
    if (deferred_base_artifact_.empty()) return;
    try {
        load_artifact(deferred_base_artifact_);
    } catch (const std::exception& error) {
        throw std::runtime_error("native bootstrap artifact " +
                                 deferred_base_artifact_ + ": " + error.what());
    }
    deferred_base_artifact_.clear();
}

void NativeBootstrap::install_expansion_helpers() {
    Evaluator& evaluator = runtime_.evaluator();
    Value base = evaluator.eval(evaluator.symbol("the-base-library"));
    Value expander = evaluator.eval(evaluator.symbol("the-expander-library"));
    Value expander_environment = evaluator.apply_values(
        evaluator.eval(evaluator.symbol("module-eval-environment")),
        {expander})[0];
    Value ref_own = evaluator.eval(evaluator.symbol("exp-library-ref-own"));
    Value binding_value = evaluator.eval(evaluator.symbol("binding-value"));
    Value binding_kind = evaluator.eval(evaluator.symbol("binding-kind"));
    Value toplevel_ref_gensym =
        evaluator.eval(evaluator.symbol("toplevel-ref-gensym"));
    Value module_define = evaluator.eval(evaluator.symbol("module-define!"));
    Value define = evaluator.eval(evaluator.symbol("exp-library-define!"));
    Value make_primitive =
        evaluator.eval(evaluator.symbol("make-primitive-binding"));
    for (const char* name : {"parse-template", "syntax-case-dispatch",
                             "fast-instantiate", "sr-build-transformer",
                             "subst-ellipsis", "dr-field-datum",
                             "dr-record-defs", "dr-register-def",
                             "dr-interleave-register",
                             "cond-expand-feature-satisfied?",
                             "cond-expand-requirement-valid?",
                             "cond-expand-select",
                             "*cond-expand-features*"}) {
        Value symbol = evaluator.symbol(name);
        Value binding = evaluator.apply_values(ref_own, {base, symbol})[0];
        if (!binding.is_boolean()) {
            Value kind = evaluator.apply_values(binding_kind, {binding})[0];
            if (kind.is_object() &&
                kind.as_object()->type() == ObjectType::Symbol &&
                kind.as_object<SymbolObject>()->name == "toplevel") {
                Value reference = evaluator.apply_values(binding_value, {binding})[0];
                Value gensym = evaluator.apply_values(
                    toplevel_ref_gensym, {reference})[0];
                Value value = evaluator.eval(
                    gensym,
                    expander_environment.as_object<EvalEnvironmentObject>()
                        ->environment);
                evaluator.apply_values(module_define, {expander, symbol, value});
            }
        }
        evaluator.apply_values(
            define,
            {base, symbol,
             evaluator.apply_values(make_primitive, {symbol})[0]});
    }
}

void NativeBootstrap::install_source_expander() {
    Evaluator& evaluator = runtime_.evaluator();
    evaluator.define_primitive(
        "expand-eval", [&evaluator](const Values& args) {
            if (args.size() != 1)
                throw std::runtime_error("expand-eval expects one argument");
            evaluator.collect();
            if (debug_enabled("progress")) {
                static std::size_t form_count = 0;
                std::fprintf(stderr, "[progress] form %zu\n", ++form_count);
            }
            Value compile = evaluator.eval(evaluator.symbol("compile-toplevel"));
            Value lowered = evaluator.apply_values(compile, args)[0];
            return evaluator.eval_values(lowered);
        });
}

void NativeBootstrap::load_cached_source(const std::string& path) {
    Evaluator& evaluator = runtime_.evaluator();
    // Reuse the Scheme install cache: it restores both lowered definitions
    // and their bindings, including the source unit's local transformers.
    Value library = call(evaluator, "make-exp-library",
        {evaluator.list({evaluator.symbol("native-source"),
                         evaluator.string(path)})});
    call(evaluator, "exp-library-add-use!",
         {library, evaluator.eval(evaluator.symbol("the-base-library"))});
    call(evaluator, "exp-library-define!",
         {library, evaluator.symbol("read-forms"),
          call(evaluator, "make-primitive-binding",
               {evaluator.symbol("read-forms")})});
    call(evaluator, "install-library-file!", {library, evaluator.string(path)});
    Value environment = call(evaluator, "module-eval-environment",
        {evaluator.eval(evaluator.symbol("the-expander-library"))});
    for (Value entry : list_values(call(evaluator, "exp-library-bindings", {library}))) {
        auto* pair = entry.as_object<PairObject>();
        if (!named(call(evaluator, "binding-kind", {pair->cdr}), "toplevel"))
            continue;
        Value reference = call(evaluator, "binding-value", {pair->cdr});
        Value gensym = call(evaluator, "toplevel-ref-gensym", {reference});
        evaluator.global_environment()->define(pair->car,
            evaluator.eval(gensym,
                environment.as_object<EvalEnvironmentObject>()->environment));
    }
}

Value NativeBootstrap::load_library_artifact(const std::string& path) {
    return loader_.load_library_gfo_file(path);
}

Value NativeBootstrap::load_artifact(const std::string& path) {
    return loader_.load_gfo_file(path);
}

void NativeBootstrap::register_library(const std::string& name,
                                       const std::string& path) {
    auto [it, inserted] = libraries_.emplace(name, path);
    if (!inserted && it->second != path)
        throw std::runtime_error("native library registered twice: " + name);
}

void NativeBootstrap::load_library(const std::string& name) {
    if (loaded_.count(name)) return;
    if (!loading_.insert(name).second)
        throw std::runtime_error("native library dependency cycle: " + name);
    auto found = libraries_.find(name);
    if (found == libraries_.end())
        throw std::runtime_error("native library not registered: " + name);
    try {
        loader_.load_bundle_gfo_file(found->second,
                                     [this](const std::string& dependency) {
                                         load_library(dependency);
                                     });
    } catch (...) {
        loading_.erase(name);
        throw;
    }
    loading_.erase(name);
    loaded_.insert(name);
}

} // namespace goldfish::runtime
