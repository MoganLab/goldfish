#pragma once

#include "runtime/value.hpp"

#include <cstddef>
#include <functional>
#include <iterator>
#include <memory>
#include <type_traits>
#include <utility>
#include <vector>

namespace goldfish::runtime {

class Heap;
class Tracer;

enum class ObjectType : std::uint8_t {
    User,
    Uninitialized,
    Pair,
    Symbol,
    String,
    ErrorObject,
    Closure,
    Primitive,
    Vector,
    Character,
    Bytevector,
    Eof,
    InputPort,
    OutputPort,
    LegacyLet,
    EvalEnvironment,
    Module,
};

class Object {
public:
    explicit Object(ObjectType type = ObjectType::User) noexcept
        : type_(type) {}
    virtual ~Object() = default;

    ObjectType type() const noexcept { return type_; }

protected:
    // Subclasses describe their outgoing Value references here. The default
    // is appropriate for leaf objects and keeps the first runtime types small.
    virtual void trace(Tracer&) {}

private:
    friend class Heap;
    Object* next_ = nullptr;
    bool marked_ = false;
    ObjectType type_;
};

class Tracer final {
public:
    explicit Tracer(Heap& heap) noexcept : heap_(heap) {}

    void mark(Value value) noexcept;
    void mark(Object* object) noexcept;

private:
    Heap& heap_;
};

class RootScope final {
public:
    explicit RootScope(class Heap& heap) noexcept : heap_(&heap) {}
    RootScope(const RootScope&) = delete;
    RootScope& operator=(const RootScope&) = delete;
    ~RootScope();

    void protect(Value& value);
    void unprotect(Value& value) noexcept;

private:
    friend class Heap;
    class Heap* heap_;
    std::vector<Value*> roots_;
};

class Heap final {
public:
    // Temporary exact-tracing reference backend. Keep the public surface small;
    // the runtime must remain replaceable by a mature collector backend.
    Heap() = default;
    Heap(const Heap&) = delete;
    Heap& operator=(const Heap&) = delete;
    ~Heap();

    template <typename T, typename... Args>
    T* make(Args&&... args) {
        static_assert(std::is_base_of<Object, T>::value,
                      "heap objects must derive from runtime::Object");
        std::unique_ptr<T> object(new T(std::forward<Args>(args)...));
        object->next_ = objects_;
        objects_ = object.get();
        ++allocated_;
        return object.release();
    }

    void collect();
    void collect(const std::function<void(Tracer&)>& extra_roots);
    std::size_t allocated() const noexcept { return allocated_; }

private:
    friend class Object;
    friend class Tracer;
    friend class RootScope;

    void add_root(Value* value);
    void remove_root(Value* value) noexcept;
    void mark(Object* object) noexcept;
    void sweep() noexcept;

    Object* objects_ = nullptr;
    std::vector<Value*> roots_;
    std::size_t allocated_ = 0;
};

inline void Tracer::mark(Value value) noexcept {
    if (value.is_object())
        mark(value.as_object());
}

inline void Tracer::mark(Object* object) noexcept {
    heap_.mark(object);
}

inline void RootScope::protect(Value& value) {
    roots_.push_back(&value);
    heap_->add_root(&value);
}

inline void RootScope::unprotect(Value& value) noexcept {
    for (auto it = roots_.rbegin(); it != roots_.rend(); ++it) {
        if (*it == &value) {
            roots_.erase(std::next(it).base());
            heap_->remove_root(&value);
            return;
        }
    }
}

inline RootScope::~RootScope() {
    for (Value* value : roots_)
        heap_->remove_root(value);
}

inline void Heap::add_root(Value* value) {
    roots_.push_back(value);
}

inline void Heap::remove_root(Value* value) noexcept {
    for (auto it = roots_.rbegin(); it != roots_.rend(); ++it) {
        if (*it == value) {
            roots_.erase(std::next(it).base());
            return;
        }
    }
}

inline void Heap::mark(Object* object) noexcept {
    if (object == nullptr || object->marked_)
        return;
    object->marked_ = true;
    Tracer tracer(*this);
    object->trace(tracer);
}

inline void Heap::collect() {
    Tracer tracer(*this);
    for (Value* root : roots_)
        if (root != nullptr)
            tracer.mark(*root);
    sweep();
}

inline void Heap::collect(const std::function<void(Tracer&)>& extra_roots) {
    Tracer tracer(*this);
    for (Value* root : roots_)
        if (root != nullptr)
            tracer.mark(*root);
    extra_roots(tracer);
    sweep();
}

inline void Heap::sweep() noexcept {
    Object** cursor = &objects_;
    while (*cursor != nullptr) {
        Object* object = *cursor;
        if (object->marked_) {
            object->marked_ = false;
            cursor = &object->next_;
        } else {
            *cursor = object->next_;
            delete object;
            --allocated_;
        }
    }
}

inline Heap::~Heap() {
    Object* object = objects_;
    while (object != nullptr) {
        Object* next = object->next_;
        delete object;
        object = next;
    }
}

} // namespace goldfish::runtime
