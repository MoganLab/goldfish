# Lowered core semantics

The expander lowers source programs to the 14 forms in `goldfish/core/ir.scm`.
The native evaluator implements these rules; derived syntax is handled before
evaluation.

## Lowering invariants

- `primitive-ref`, `lexical-ref` and `toplevel-ref` lower to symbols. Resolve
  lexical frames first, then top-level cells, then primitive bindings.
- `<void>` lowers to `(quote void)`. An `if` without an alternate and an empty
  `begin` produce one unspecified value.
- `let-values` lowers to `call-with-values`; named `let` lowers to `letrec` and
  a call. `case-lambda` dispatch is performed before core evaluation.

## Forms

- `quote` returns its datum unchanged, including improper lists.
- `define` accepts a name and expression. Top-level redefinition replaces the
  prior binding; the expression is evaluated before binding.
- `lambda` accepts proper, improper or rest formals. Its body is an implicit
  `begin`, and its closure captures the definition environment.
- `if` treats only `#f` as false. Without an alternate, a false test returns
  one unspecified value.
- `begin` evaluates expressions in order and returns the final result; an
  empty `begin` returns one unspecified value.
- `let` evaluates initializers in the outer environment. `let*` nests bindings.
- `letrec` reserves all bindings before evaluating initializers. Reading an
  uninitialized binding is an error. `letrec*` initializes in order, exposing
  earlier values to later initializers.
- `set!` updates the nearest lexical or top-level cell; an unbound name is an
  error.
- `values` preserves zero, one or multiple values. `call-with-values` passes
  all produced values to its consumer.
- `module-ref` and `module-set` are loader operations, not user-level forms.
- Continuations and `dynamic-wind` preserve evaluator control state. Their
  implementation boundary is described in `CONTROL-STACK.md`.

## Multiple-value calls

Arguments in procedure-call position splice multiple values before arity
checking. This applies to primitives, closures and `apply`; a `values` form
also splices its arguments. `if` consumes the first value, with zero values
being true. `define` and `set!` require one value.

## Errors

- Unbound lookup and assignment use the `unbound-variable` error key.
- Closure arity mismatch and invalid multiple-value arity use
  `wrong-number-of-args`.
- Other error behavior is defined by native regression tests and the applicable
  R7RS rules.
