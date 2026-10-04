# Native control stack

The evaluator uses a CEK control loop with explicit continuation frames. It
captures Scheme control state without copying or restoring the C++ call stack.

## Continuations

A continuation stores a snapshot of evaluator frames and can be invoked more
than once. Invoking it installs the captured frames and supplies the invocation
values. Captured environments remain shared and mutable; continuation transfer
does not roll variable assignments back.

Continuation objects trace values in their frames, captured environments and
winder state so the collector keeps live state reachable. A continuation is
bound to its evaluator machine. Transfers through nested native primitive
callbacks propagate to the active evaluator instead of resuming returned C++
frames.

Scheme callbacks that may capture or invoke a continuation must execute as
machine frames. This applies to the supported iteration procedures and
resource-backed port callbacks. Arbitrary external C++ callback frames are not
continuation-safe unless integrated with the evaluator machine.

Native `eval` evaluates its expression on the caller's machine and continuation,
so a tail call through `eval` does not introduce a nested evaluator invocation.

## Dynamic extent

`dynamic-wind` tracks winder identities with each control state. Normal return,
exception transfer and continuation transfer run `after` thunks for exited
winders and `before` thunks for entered winders in the required order. Core
`guard` and `catch` transfer through the evaluator machine. The public
`with-exception-handler` currently uses aborting `catch`; `raise-continuable`
does not yet resume its original continuation. These incompatibilities are
tracked in [R7RS-COMPATIBILITY.tsv](R7RS-COMPATIBILITY.tsv).

File and string port callbacks preserve dynamic port bindings across transfers.
File output ports close on exit and reopen in append mode on re-entry; buffered
input ports retain their read position. Module loading callbacks are outside
the Scheme continuation guarantee.

## Performance

Continuation capture currently copies the frame snapshot. Keep this simple
implementation unless dedicated measurements show snapshot copying is a
material cost. Any shared-segment or copy-on-write design must preserve
multi-shot behavior, GC tracing and winder ordering.
