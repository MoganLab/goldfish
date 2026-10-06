#pragma once

#include "runtime/evaluator.hpp"

#include <string>
#include <functional>
#include <map>

namespace goldfish::runtime {

class ArtifactLoader final {
public:
    explicit ArtifactLoader(Evaluator& evaluator) : evaluator_(evaluator) {}

    Value load_file(const std::string& path);
    Value load_gfo_file(const std::string& path);
    Value load_library_gfo_file(const std::string& path);
    Value load_bundle_gfo_file(const std::string& path);
    // Replay a module bundle with the semantics load_cached_source gives a
    // cached source unit: a dedicated (native-source "<key>") library
    // linked to the base library, the binding table restored into that
    // library, lowered definitions evaluated in the expander module
    // environment, and toplevel values aliased to the root evaluator.  The
    // bootstrap installer replays through this because it defines the
    // Scheme cache layer a normal replay depends on.
    Value load_source_unit_gfo_file(const std::string& path,
                                    const std::string& unit_key);
    void capture_kernel_api();
    using DependencyLoader = std::function<void(const std::string&)>;
    Value load_bundle_gfo_file(const std::string& path,
                               const DependencyLoader& load_dependency);

private:
    Value call(const char* name, const Values& arguments);
    Value deserialize_cache_value(Value value);
    void restore_library_metadata(const std::vector<Value>& library,
                                  const Value& exp_library);
    Evaluator& evaluator_;
    std::map<std::string, Value> exp_libraries_;
    std::map<const Object*, Value> deserialize_memo_;
    std::map<std::string, Value> kernel_api_;
};

} // namespace goldfish::runtime
