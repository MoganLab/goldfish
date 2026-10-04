#pragma once

#include "runtime/heap.hpp"

#include <exception>
#include <string>
#include <utility>
#include <vector>

namespace goldfish::runtime {

using ValueList = std::vector<Value>;

class SchemeException : public std::exception {
public:
    SchemeException(std::string key, std::string message,
                    ValueList irritants = {})
        : key_(std::move(key)),
          message_(std::move(message)),
          irritants_(std::move(irritants)) {}

    const char* what() const noexcept override { return message_.c_str(); }
    const std::string& key() const noexcept { return key_; }
    const ValueList& irritants() const noexcept { return irritants_; }

private:
    std::string key_;
    std::string message_;
    ValueList irritants_;
};

class ErrorObject final : public Object {
public:
    ErrorObject(std::string message, ValueList irritants,
                std::string key = {})
        : Object(ObjectType::ErrorObject),
          message(std::move(message)),
          irritants(std::move(irritants)), key(std::move(key)) {}

    std::string message;
    ValueList irritants;
    // Non-empty when raised as (error 'key ...): the host's catch hands
    // (key irritants...) to handlers, while (error "text" ...) and C++
    // runtime errors hand the object itself (R7RS guard semantics).
    std::string key;

protected:
    void trace(Tracer& tracer) override {
        for (Value irritant : irritants)
            tracer.mark(irritant);
    }
};

class RaisedValue final : public SchemeException {
public:
    explicit RaisedValue(Value value, bool continuable = false)
        : SchemeException("raised", "user-raised value"), value_(value),
          continuable_(continuable) {}

    Value value() const noexcept { return value_; }
    bool continuable() const noexcept { return continuable_; }

private:
    Value value_;
    bool continuable_;
};

class ThrownValue final : public SchemeException {
public:
    ThrownValue(Value tag, ValueList arguments)
        : SchemeException("throw", "user throw", arguments),
          tag_(tag),
          arguments_(std::move(arguments)) {}

    Value tag() const noexcept { return tag_; }
    const ValueList& arguments() const noexcept { return arguments_; }

private:
    Value tag_;
    ValueList arguments_;
};

} // namespace goldfish::runtime
