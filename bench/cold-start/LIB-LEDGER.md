# Per-library cache ledger

Quantifies the SHARED cache mechanism that standard libraries and user
libraries both use, so both can be improved together (no special-casing of
the standard library).

## Method

`lib-timing.sh CACHE LIB [SAMPLES] [MODE]` prepares a warm bootstrap cache,
then measures one library's import in a warm process.  Two cache paths were
instrumented behind `GOLDFISH_DEBUG=timing` (the marks are not in the tree:
the temporary instrumentation was reverted after it perturbed the bootstrap
cache path — see below — but the raw logs under `analysis/lib-*.stderr`
retain its output):

- `install-library-file!` (`install.scm`): boot installs, `load_cached_source`
  (reader/writer/native-scheme-surface), kind `module`.
- `load-library-in-unit!` (`module.scm`): `import` of a `define-library` file,
  kind `libraries`.

They share the gfo backend (`cache-load-checked`, `gfo-write!`) but have
separate orchestration and bundle kinds.  This is the main non-unification:
one shared backend, two front doors.

Segments:
- `lib-cache-lookup` = `cache-load-checked` (read/parse + validity + dep check)
- `lib-read` / `lib-expand` / `lib-optimize` / `lib-save` = cold compile
- `lib-restore` / `lib-eval` = warm replay (`restore-library-cache`,
  `load-library-file-cached!`)

## Warm replay (cache hit)

| Library | lines | artifact | lookup | restore-import | bindings/macros | restore total | eval |
|---|---:|---:|---:|---:|---:|---:|---:|
| liii/base.scm | 68 | 12 KB | 71 ms | — | — | 43 ms | 1 ms |
| liii/ascii.scm | 59 | — | 60 ms | 48 ms | 0 ms | 60 ms | 5 ms |
| srfi/srfi-151.scm | — | — | 73 ms | 33 ms | 4 ms | 42 ms | 7 ms |
| scheme/char.scm | 134 | 26 KB | 370 ms | 965 ms | 0 ms | 945 ms | 5 ms |

`restore-import` for `scheme/char` includes loading its dependency closure
(`liii unicode` → `liii base`/`liii ascii`/`liii bitwise` → `srfi srfi-151`),
so it is the whole closure, not one copy.

`cache-load-checked` split for `liii/ascii` (leaf, two deps):

```
cache-read (parse gfo)     1 ms
cache-envelope (stamp)     0 ms
cache-deps-map (fingerprints) 56 ms   <- dominates
cache-deps-equal           0 ms
```

So the per-library warm floor is ~120 ms, and `cache-deps` — recomputing every
stored dependency fingerprint, which re-reads/parses the dependency source and
walks its declaration inputs — is the single largest component.  This is the
deliberate cache-validity design (see the `library-dep-fingerprint` comment:
fingerprinting the artifact instead would miss an edit because consumers
validate before dependencies reload).

## Cold compile (cache miss)

| Library | read | expand | optimize | save | eval |
|---|---:|---:|---:|---:|---:|
| liii/base.scm | 8 ms | 210 ms | 146 ms | 233 ms | 1 ms |
| scheme/char.scm | 33 ms | 1560 ms | 756 ms | 421 ms | 5 ms |

`expand` and `optimize` scale with source size (~10 ms/line on `scheme/char`);
`save` (serialize + write) is also large.

## Fix 1: cheap dependency declaration-input scan

`library-declaration-inputs` (used only for fingerprinting) called
`normalize-library-declarations`, which deep-converts every body form of a
`define-library` just to discover declarations.  Two changes:

- `scan-declaration-inputs`: walk clause heads for `include`/`cond-expand`
  without converting bodies.
- memoize `library-declaration-inputs` per session by source stat, so a
  dependency fingerprinted by several consumers is read once.

Measured: the `decl` component of a single `srfi/srfi-175` fingerprint drops
from 56 ms to ~0 on repeat; across a warm boot 29 fingerprints cost ~5 ms
total.  Warm `boot total` 4273 ms → 4129 ms, `mode-imports` 941 ms → 721 ms.
The remaining fingerprint cost is the first `read-forms` of each dependency
source (the reader itself is ~0.1 ms/line: `read-forms` of `srfi-175.scm` is
47 ms), which needs a faster reader or artifact-side declaration inputs.

Correctness: `lib-cache-test`, `lib-cache-all-libs-test`,
`lib-cache-import-order-test`, `program-cache-macro-test`,
`program-cache-syntax-test` all pass.

## Attempted fixes (reverted)

Per the plan's rule not to merge unmeasured complexity, four speculative fixes
were tried and reverted after A/B showed no gain:

1. O(n) identity-hash memo in `serialize-cache-sexp`/`deserialize-cache-sexp`
   (replacing the O(n²) `assq` alist): no change (lookup 390→390 ms).
2. Identity-keyed `*interface-cache*` in `import-view`: no change
   (restore-import 965→997 ms).
3. Session memo of `library-dep-fingerprint`: no change (cache-deps 58→59 ms).
4. Session memo of `declaration-input-current`: no change (cache-deps 57 ms).

Conclusion: the warm cost is not repeated hashing or an O(n²) memo; it is
first-time dependency fingerprinting per `(library, dependency)` plus binding
re-import, and cold cost is expansion/optimization/serialization throughput.
These need a design change or profiler-driven work, not a local memo.

A fifth change, per-library timing in `module.scm`, was also reverted: it made
the warm bootstrap loader reject the `scheme/case-lambda` artifact (written as
kind `libraries` by the mode import but read as kind `module` by
`load_cached_runtime`), i.e. the instrumentation itself perturbed boot.  That
collision between the two cache front doors is a latent design smell worth
fixing in the unification work.  A coarse, non-perturbing replacement now
wraps `load-library!` (`[timing] (lib-load <name>)`), giving each library's
total import cost; the finer cache/restore split still needs a design that
does not change the loader's return values or lowered shape.
