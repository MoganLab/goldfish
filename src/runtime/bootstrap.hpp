#pragma once

#include "runtime/artifact.hpp"
#include "runtime/runtime.hpp"

#include <map>
#include <set>

namespace goldfish::runtime {

// The native bootstrap owns runtime initialization order and artifact loading.
class NativeBootstrap final {
public:
    explicit NativeBootstrap(Runtime& runtime);

    void install_primitives();
    Value load_kernel(const std::string& path);
    // Restore the bootstrap artifacts from the content-addressed
    // cache. The default is the native-ccache under the usual XDG/home cache
    // root; GOLDFISH_CACHE_DIR relocates it, matching the Scheme cache layer.
    void load_cached_runtime(const std::string& cache_root = {});
    // Preflight the entire cache before any artifact mutates the runtime.
    std::string validate_cached_runtime(const std::string& cache_root = {});
    std::string cache_directory(const std::string& cache_root = {});
    void load_cached_base_runtime();
    void install_source_expander();
    void install_expansion_helpers();
    // Install a definition-only bootstrap unit and publish its value aliases.
    // Cache misses retain the native Scheme source expansion path.
    void load_cached_source(const std::string& path);
    // Warm-start the bootstrap installer from its captured module bundle
    // through the native-source-unit replay.  False when the bundle is
    // missing or stale; the caller falls back to a source load.
    bool load_cached_installer(const std::string& cache_root = {});
    // Capture expander/lib/install.scm into the install cache after a
    // source load (install-library-file! replays a valid bundle or
    // re-expands and saves one).
    void capture_installer();
    Value load_library_artifact(const std::string& path);
    Value load_artifact(const std::string& path);
    void register_library(const std::string& name, const std::string& path);
    void load_library(const std::string& name);

private:
    Runtime& runtime_;
    ArtifactLoader loader_;
    std::map<std::string, std::string> libraries_;
    std::set<std::string> loading_;
    std::set<std::string> loaded_;
    bool primitives_installed_ = false;
    bool kernel_loaded_ = false;
    Value native_read_forms_ = Value::unspecified();
    Value native_module_eval_environment_ = Value::unspecified();
    std::string deferred_base_artifact_;
};

} // namespace goldfish::runtime
