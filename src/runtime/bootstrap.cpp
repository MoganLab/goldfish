#include "runtime/bootstrap.hpp"

#include "runtime/bootstrap_compatibility.hpp"
#include "runtime/debug_flags.hpp"
#include "runtime/standard_primitives.hpp"

#include <cstdlib>
#include <array>
#include <cstdio>
#include <filesystem>
#include <stdexcept>
#include <vector>

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

bool has_native_bootstrap_artifacts(const fs::path& root) {
    std::error_code error;
    for (const char* relative : native_bootstrap_artifacts) {
        if (!fs::is_regular_file(root / relative, error)) {
            error.clear();
            return false;
        }
    }
    return true;
}

fs::path find_cache_version(fs::path root) {
    std::error_code error;
    if (has_native_bootstrap_artifacts(root)) return root;
    fs::path selected;
    fs::file_time_type selected_time{};
    if (!fs::is_directory(root, error)) return {};
    for (const fs::directory_entry& entry : fs::directory_iterator(root, error)) {
        if (error) break;
        if (!has_native_bootstrap_artifacts(entry.path())) {
            error.clear();
            continue;
        }
        fs::file_time_type time{};
        bool readable = true;
        for (const char* relative : native_bootstrap_artifacts) {
            const auto artifact_time =
                fs::last_write_time(entry.path() / relative, error);
            if (error) {
                error.clear();
                readable = false;
                break;
            }
            if (artifact_time > time) time = artifact_time;
        }
        if (!readable) continue;
        if (selected.empty() || time > selected_time) {
            selected = entry.path();
            selected_time = time;
        }
    }
    return selected;
}

} // namespace

NativeBootstrap::NativeBootstrap(Runtime& runtime)
    : runtime_(runtime), loader_(runtime.evaluator()) {
    prepare_native_stack();
}

void NativeBootstrap::install_primitives() {
    if (primitives_installed_) return;
    install_runtime_primitives(runtime_.evaluator());
    install_native_bootstrap_compatibility(runtime_.evaluator());
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
    // install the native environment explicitly so module-eval-environment
    // never falls back to the legacy inlet representation.
    Value eval_environment = runtime_.evaluator().apply_values(
        runtime_.evaluator().eval(
            runtime_.evaluator().symbol("make-eval-environment")), {})[0];
    // Designate this frame as the defs root: every later parentless frame
    // (module environments, program environments) descends from it, so bare
    // gensym refs resolve while module frames stay isolated.
    runtime_.evaluator().set_defs_root(
        eval_environment.as_object<EvalEnvironmentObject>()->environment);
    runtime_.evaluator().apply_values(
        runtime_.evaluator().eval(
            runtime_.evaluator().symbol("module-define!")),
        {expander_library,
         runtime_.evaluator().symbol("__eval-environment"), eval_environment});
    runtime_.evaluator().apply_values(register_module, {expander_library});
    // kernel-combined is the implementation library itself and is already
    // installed in the evaluator; imports of (goldfish) must not ask the
    // artifact registry for a second copy.
    loaded_.insert("(goldfish)");
    kernel_loaded_ = true;
    return result;
}

void NativeBootstrap::load_cached_runtime(const std::string& cache_root) {
    const fs::path root = cache_root.empty() ? cache_root_from_environment()
                                             : fs::path(cache_root);
    const fs::path version = find_cache_version(root);
    if (version.empty())
        throw std::runtime_error("native bootstrap cache not found under " +
                                 root.string());

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
        "scheme/base.scm-o2.gfo",
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
