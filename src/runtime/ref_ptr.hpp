#pragma once

#include <cstddef>
#include <utility>

namespace goldfish::runtime {

// Intrusive reference counting for the evaluator's environment frames: the
// runtime is single-threaded, so a shared_ptr control block's atomic
// read-modify-write on every copy and destroy bought no synchronization.
// CRTP supplies the concrete type so release() deletes through the derived
// destructor without a virtual one.
template <typename T>
class RefCounted {
public:
    void retain() const noexcept { count_ += 1; }
    void release() const noexcept {
        if (--count_ == 0)
            delete static_cast<const T*>(this);
    }

protected:
    RefCounted() noexcept = default;
    // Refcounts live in the handles, never in the counted object.
    RefCounted(const RefCounted&) noexcept = delete;
    RefCounted& operator=(const RefCounted&) noexcept = delete;

private:
    mutable std::size_t count_ = 0;
};

// A non-atomic shared handle over a RefCounted object.  Mirrors the
// shared_ptr surface the evaluator used: get, ->, *, bool conversion,
// pointer comparison, reset/swap.
template <typename T>
class RefPtr final {
public:
    RefPtr() noexcept = default;
    RefPtr(std::nullptr_t) noexcept {}
    explicit RefPtr(T* raw) noexcept : raw_(raw) {
        if (raw_) raw_->retain();
    }
    RefPtr(const RefPtr& other) noexcept : raw_(other.raw_) {
        if (raw_) raw_->retain();
    }
    RefPtr(RefPtr&& other) noexcept
        : raw_(std::exchange(other.raw_, nullptr)) {}
    RefPtr& operator=(const RefPtr& other) noexcept {
        if (raw_ != other.raw_) {
            if (other.raw_) other.raw_->retain();
            if (raw_) raw_->release();
            raw_ = other.raw_;
        }
        return *this;
    }
    RefPtr& operator=(RefPtr&& other) noexcept {
        if (this != &other) {
            if (raw_) raw_->release();
            raw_ = std::exchange(other.raw_, nullptr);
        }
        return *this;
    }
    ~RefPtr() {
        if (raw_) raw_->release();
    }

    explicit operator bool() const noexcept { return raw_ != nullptr; }
    T* get() const noexcept { return raw_; }
    T* operator->() const noexcept { return raw_; }
    T& operator*() const noexcept { return *raw_; }
    void reset(T* raw = nullptr) noexcept {
        RefPtr replacement(raw);
        swap(replacement);
    }
    void swap(RefPtr& other) noexcept { std::swap(raw_, other.raw_); }

    friend bool operator==(const RefPtr& a, const RefPtr& b) noexcept {
        return a.raw_ == b.raw_;
    }
    friend bool operator!=(const RefPtr& a, const RefPtr& b) noexcept {
        return a.raw_ != b.raw_;
    }
    friend bool operator==(const RefPtr& a, std::nullptr_t) noexcept {
        return a.raw_ == nullptr;
    }
    friend bool operator!=(const RefPtr& a, std::nullptr_t) noexcept {
        return a.raw_ != nullptr;
    }
    friend bool operator==(std::nullptr_t, const RefPtr& a) noexcept {
        return a.raw_ == nullptr;
    }
    friend bool operator!=(std::nullptr_t, const RefPtr& a) noexcept {
        return a.raw_ != nullptr;
    }

private:
    T* raw_ = nullptr;
};

template <typename T, typename... Args>
RefPtr<T> make_ref(Args&&... args) {
    return RefPtr<T>(new T(std::forward<Args>(args)...));
}

} // namespace goldfish::runtime
