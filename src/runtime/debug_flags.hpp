#pragma once

#include <cstdlib>
#include <string>

namespace goldfish::runtime {

// Diagnostics live behind ONE knob so they cannot sprawl:
//
//   GOLDFISH_DEBUG=timing,throw,gc,progress   (comma list, no spaces)
//   GOLDFISH_DEBUG=all                        (every key)
//   unset / empty                             (default: everything off)
//
// Adding a diagnostic means adding a key here and calling
// debug_enabled("key") -- never a new environment variable.  Legacy
// GOLDFISH_NATIVE_TIMING / GOLDFISH_TRACE_THROW keep working as
// aliases.  Configuration variables (GOLDFISH_TEST_CHUNK,
// GOLDFISH_CACHE_DIR, GOLDFISH_GC, ...) are not diagnostics and stay
// independent of this group.
inline bool debug_enabled(const char* key, const char* legacy = nullptr) {
    static const std::string group = [] {
        const char* env = std::getenv("GOLDFISH_DEBUG");
        return std::string(env != nullptr ? env : "");
    }();
    if (!group.empty()) {
        if (group == "all")
            return true;
        const std::string padded = "," + group + ",";
        if (padded.find("," + std::string(key) + ",") != std::string::npos)
            return true;
    }
    return legacy != nullptr && std::getenv(legacy) != nullptr;
}

} // namespace goldfish::runtime
