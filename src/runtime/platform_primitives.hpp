#pragma once

#include "runtime/evaluator.hpp"

namespace goldfish::runtime {

// OS, filesystem, environment, clock, and digest adapters.  These primitives
// expose capabilities; policy and cache orchestration remain in Scheme.
void install_platform_primitives(Evaluator& evaluator);

// Capture argv before the primitives are used: g_command-line reports the
// real invocation (the host reads it from its own main), and the test tool
// passes it to argparse as (argv).
void set_native_command_line(int argc, char** argv);

} // namespace goldfish::runtime
