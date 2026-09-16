#include "runtime/heap.hpp"

#include <cassert>

using goldfish::runtime::Heap;
using goldfish::runtime::Object;
using goldfish::runtime::Tracer;
using goldfish::runtime::Value;

namespace {

class Pair final : public Object {
public:
    Pair(Value car, Value cdr) : car(car), cdr(cdr) {}

    Value car;
    Value cdr;

protected:
    void trace(Tracer& tracer) override {
        tracer.mark(car);
        tracer.mark(cdr);
    }
};

} // namespace

int main() {
    Heap heap;
    assert(heap.allocated() == 0);

    Value root = Value::object(heap.make<Pair>(Value::integer(1), Value::null()));
    goldfish::runtime::RootScope roots(heap);
    roots.protect(root);

    heap.make<Pair>(Value::integer(2), Value::null());
    assert(heap.allocated() == 2);
    heap.collect();
    assert(heap.allocated() == 1);
    assert(root.as_object<Pair>()->car.as_integer() == 1);

    Value child = Value::object(heap.make<Pair>(Value::integer(3), Value::null()));
    root.as_object<Pair>()->cdr = child;
    roots.protect(child);
    roots.unprotect(child);
    child = Value::null();
    heap.collect();
    assert(heap.allocated() == 2);

    root = Value::null();
    heap.collect();
    assert(heap.allocated() == 0);

    Pair* cycle = heap.make<Pair>(Value::integer(4), Value::null());
    Value cycle_root = Value::object(cycle);
    cycle->cdr = cycle_root;
    roots.protect(cycle_root);
    heap.collect();
    assert(heap.allocated() == 1);
    roots.unprotect(cycle_root);
    cycle_root = Value::null();
    heap.collect();
    assert(heap.allocated() == 0);
}
