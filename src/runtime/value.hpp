#pragma once

#include <cstdint>
#include <stdexcept>
#include <type_traits>

namespace goldfish::runtime {

class Object;

enum class ValueKind : std::uint8_t {
    Null,
    Unspecified,
    Boolean,
    Integer,
    Object,
};

// A language value. Object identity is represented by the runtime object
// handle, never by a host-specific pointer exposed to Scheme code.
class Value final {
public:
    static Value null() noexcept { return Value(); }
    static Value unspecified() noexcept {
        Value result;
        result.kind_ = ValueKind::Unspecified;
        return result;
    }
    static Value boolean(bool value) noexcept {
        Value result;
        result.kind_ = ValueKind::Boolean;
        result.boolean_ = value;
        return result;
    }
    static Value integer(std::int64_t value) noexcept {
        Value result;
        result.kind_ = ValueKind::Integer;
        result.integer_ = value;
        return result;
    }
    static Value object(Object* value) noexcept {
        Value result;
        result.kind_ = ValueKind::Object;
        result.object_ = value;
        return result;
    }

    ValueKind kind() const noexcept { return kind_; }
    bool is_null() const noexcept { return kind_ == ValueKind::Null; }
    bool is_unspecified() const noexcept {
        return kind_ == ValueKind::Unspecified;
    }
    bool is_boolean() const noexcept { return kind_ == ValueKind::Boolean; }
    bool is_integer() const noexcept { return kind_ == ValueKind::Integer; }
    bool is_object() const noexcept { return kind_ == ValueKind::Object; }

    bool as_boolean() const {
        require(ValueKind::Boolean);
        return boolean_;
    }
    std::int64_t as_integer() const {
        require(ValueKind::Integer);
        return integer_;
    }
    Object* as_object() const {
        require(ValueKind::Object);
        return object_;
    }

    template <typename T>
    T* as_object() const {
        static_assert(std::is_base_of<Object, T>::value,
                      "T must derive from runtime::Object");
        return static_cast<T*>(as_object());
    }

    friend bool operator==(const Value& left, const Value& right) noexcept {
        if (left.kind_ != right.kind_)
            return false;
        switch (left.kind_) {
        case ValueKind::Null:
        case ValueKind::Unspecified:
            return true;
        case ValueKind::Boolean:
            return left.boolean_ == right.boolean_;
        case ValueKind::Integer:
            return left.integer_ == right.integer_;
        case ValueKind::Object:
            return left.object_ == right.object_;
        }
        return false;
    }

    friend bool operator!=(const Value& left, const Value& right) noexcept {
        return !(left == right);
    }

private:
    void require(ValueKind expected) const {
        if (kind_ != expected)
            throw std::logic_error("runtime value has the wrong kind");
    }

    ValueKind kind_ = ValueKind::Null;
    union {
        bool boolean_;
        std::int64_t integer_ = 0;
        Object* object_;
    };
};

} // namespace goldfish::runtime
