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
