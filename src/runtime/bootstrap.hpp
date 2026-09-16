#pragma once

#include "runtime/artifact.hpp"
#include "runtime/runtime.hpp"

#include <map>
#include <set>

namespace goldfish::runtime {

// The native bootstrap owns the order in which the small runtime substrate,
// migration-only bootstrap procedures, and the legacy kernel compatibility
// procedures are installed.  It accepts lowered artifacts only; source
// Scheme is expanded before it reaches this boundary.
class NativeBootstrap final {
public:
    explicit NativeBootstrap(Runtime& runtime)
        : runtime_(runtime), loader_(runtime.evaluator()) {}

    void install_primitives();
    Value load_kernel(const std::string& path);
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
};

} // namespace goldfish::runtime
