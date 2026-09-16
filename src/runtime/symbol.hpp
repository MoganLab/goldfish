#pragma once

#include "runtime/heap.hpp"

#include <map>
#include <string>

namespace goldfish::runtime {

class SymbolObject final : public Object {
public:
    explicit SymbolObject(std::string name)
        : Object(ObjectType::Symbol), name(std::move(name)) {}

    std::string name;
};

class SymbolTable final {
public:
    explicit SymbolTable(Heap& heap) : heap_(heap), roots_(heap) {}

    Value intern(const std::string& name) {
        auto found = symbols_.find(name);
        if (found != symbols_.end())
            return found->second;

        auto inserted = symbols_.emplace(
            name, Value::object(heap_.make<SymbolObject>(name)));
        roots_.protect(inserted.first->second);
        return inserted.first->second;
    }

private:
    Heap& heap_;
    std::map<std::string, Value> symbols_;
    RootScope roots_;
};

} // namespace goldfish::runtime
