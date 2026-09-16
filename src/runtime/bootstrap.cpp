#include "runtime/bootstrap.hpp"

#include "runtime/bootstrap_primitives.hpp"
#include "runtime/legacy_primitives.hpp"
#include "runtime/standard_primitives.hpp"

namespace goldfish::runtime {

void NativeBootstrap::install_primitives() {
    install_standard_primitives(runtime_.evaluator());
    install_bootstrap_primitives(runtime_.evaluator());
    install_legacy_primitives(runtime_.evaluator());
}

Value NativeBootstrap::load_kernel(const std::string& path) {
    Value result = loader_.load_file(path);
    loader_.capture_kernel_api();
    Value register_module = runtime_.evaluator().eval(
        runtime_.evaluator().symbol("register-module"));
    Value expander_library = runtime_.evaluator().eval(
        runtime_.evaluator().symbol("the-expander-library"));
    runtime_.evaluator().apply_values(register_module, {expander_library});
    // kernel-combined is the implementation library itself and is already
    // installed in the evaluator; imports of (goldfish) must not ask the
    // artifact registry for a second copy.
    loaded_.insert("(goldfish)");
    return result;
}

Value NativeBootstrap::load_library_artifact(const std::string& path) {
    return loader_.load_library_gfo_file(path);
}

Value NativeBootstrap::load_artifact(const std::string& path) {
    return loader_.load_gfo_file(path);
}

void NativeBootstrap::register_library(const std::string& name,
                                       const std::string& path) {
    libraries_[name] = path;
}

void NativeBootstrap::load_library(const std::string& name) {
    if (loaded_.count(name)) return;
    if (!loading_.insert(name).second)
        throw std::runtime_error("native library dependency cycle: " + name);
    auto found = libraries_.find(name);
    if (found == libraries_.end())
        throw std::runtime_error("native library not registered: " + name);
    loader_.load_bundle_gfo_file(found->second,
                                 [this](const std::string& dependency) {
                                     load_library(dependency);
                                 });
    loading_.erase(name);
    loaded_.insert(name);
}

} // namespace goldfish::runtime
