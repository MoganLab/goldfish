// GC-backed global allocation for the native runtime (native-only; the
// host keeps plain new/delete).
//
// Every C++ allocation -- heap objects, Values vectors, environment
// binding maps, shared_ptr control blocks, string buffers -- lives on the
// BDWGC heap.  The collector scans stacks, the data segment and GC blocks
// conservatively, so all of those containers root their contents without
// per-container allocator plumbing; int64 immediates that happen to look
// like pointers only cause harmless false retention.
//
// Consequences to keep in mind:
//   - Object destructors never run at collection time: they must not own
//     non-GC resources (ports release their streams in close-port, not in
//     ~XxxPort).
//   - A Value that is only reachable from an exception payload (malloc'd
//     by the unwinder) is invisible to the scan; catch sites copy it onto
//     their stack frame first.

#include "gc/gc.h"

#include "runtime/heap.hpp"

#include <cstddef>
#include <new>

namespace {
// Single-threaded runtime: a plain flag beats std::call_once on the hot
// path (operator new is called millions of times per run).
bool g_gc_initialized = false;

void ensure_gc_init() {
    if (!g_gc_initialized) {
        GC_init();
        g_gc_initialized = true;
        // Precise mode runs its own exact sweep over the same arena; the
        // conservative collector must not run alongside it (stale stack
        // slots would re-mark blocks the exact sweep already freed).
        // The GC heap then behaves as a plain arena whose blocks return
        // through explicit delete/GC_free only -- the pre-BDWGC contract.
        if (goldfish::runtime::gc_mode() == goldfish::runtime::GcMode::Precise)
            GC_disable();
    }
}
} // namespace

void* operator new(std::size_t size) {
    ensure_gc_init();
    if (size == 0)
        size = 1;
    if (void* memory = GC_malloc(size))
        return memory;
    throw std::bad_alloc();
}

void* operator new[](std::size_t size) { return operator new(size); }

// Deallocation returns memory eagerly, all sizes.  This is load-bearing:
// transient buffers are full of Value words, so letting them linger --
// even just one top-level form's worth behind the expand-eval boundary
// -- seeds conservative marking's false-retention cascade and blows up
// collection cost (measured: no-op delete ~2x runtime, a 512-byte
// threshold still ~1.6x with mark at ~27% of samples).  GC_free's header
// lookup per call is expensive but dirty-heap size dominates.
void operator delete(void* memory) noexcept {
    if (memory != nullptr)
        GC_free(memory);
}

void operator delete[](void* memory) noexcept {
    if (memory != nullptr)
        GC_free(memory);
}

void operator delete(void* memory, std::size_t) noexcept {
    if (memory != nullptr)
        GC_free(memory);
}

void operator delete[](void* memory, std::size_t) noexcept {
    if (memory != nullptr)
        GC_free(memory);
}
