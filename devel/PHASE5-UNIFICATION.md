# Phase 5 — cache unification design

Status: design, decisions recorded.  Implementation proceeds as separately
gated commits; this document is the contract they are held to.

## 1. As-is map (2026-10-07)

One backend — the gfo envelope (record + content stamps + pipeline
fingerprint directory) with one validity gate (`cache-load-checked` /
`validate_artifact`) — is orchestrated by TWO paths over THREE bundle
kinds:

| path | orchestrator | kind | users |
|---|---|---|---|
| bootstrap | C++ (`native_main` + `bootstrap.cpp`), fixed order | module (plain-source install units: `expander/lib/*.scm`) and libraries (`scheme/base`, `scheme/case-lambda`) | the 12 boot artifacts, deferred base |
| installer | C++ capture/replay pair (`load_cached_installer` + `load_cached_source`) | module | install.scm, install-boot.scm, base-functions, native-hash-adapter, native-abi, reader, native-write |
| loader | Scheme (`module.scm`: `load-library!` → `restore-library-cache` + `load-library-file-cached!`) | libraries (define-library files) | all `(scheme *)`, `(srfi *)`, `(liii *)`, user libraries |
| program | Scheme (`compile-file-cached`) | program | toplevel scripts |

The bootstrap path exists by necessity: the Scheme loader, the installer,
and the cache backend they define all load AFTER the boot files that
bootstrapped them.  No reordering removes that cycle (2c options A/C
rejected for layering; see PERF-PLAN Appendix C).

## 2. The core invariant: a growing export surface

The implementation library's export surface grows across boot stages:

```
kernel load → boot artifact replays → install.scm defines →
install-boot publishes/registrations → native-scheme-surface →
liii/reader → mode-imports
```

Every importer of `(goldfish)` snapshots the surface AT ITS IMPORT TIME
(`import-set-pairs` → `import-view`): that per-importer snapshot is
semantic, not waste.  Memoizing it freezes the first importer's view and
breaks later programs with unbound identifiers (observed: write-roundtrip;
see PERF-PLAN Appendix G).  Registering the base library directly as the
view widens visibility to base's own table and changes cold-capture
resolution (also measured, also reverted).

**Decision D1 (surface snapshots):** keep per-importer snapshots.  The
cost is intrinsic to the growth semantics; removing it requires a
semantic decision (below), not a local cache.

**Decision D2 (growth propagation):** when a shared interface's source
surface grows, extend the interface in place (`exp-library-define!` into
the cached view object) instead of rebuilding per importer.  All holders
of the view see the growth — matching what a fresh snapshot at a later
import time would see, without the rebuild.  This is the design that
replaces the reverted pairs memo: the cache stays, the invalidation
becomes explicit extension.  Cold-expansion resolution effects are
bounded by the post-first-import growth (publishes + surface
registrations) and must be re-measured under the cold gate.

## 3. Decisions

**D3 (one manifest, not one process).**  The bootstrap cannot move into
Scheme, so unification means: the boot chain is described by ONE manifest
(single source of truth, currently duplicated between
`native_bootstrap_artifacts` and the Scheme-side expectations), executed
by the C++ bootstrap for the pre-loader tier and by the Scheme loader for
everything after.  Adding a boot file becomes a manifest edit, never a
C++ edit.

**D4 (kinds are views of one schema).**  module bundles exist because
plain-source install units extend the base library instead of declaring
their own library — structurally necessary, kept.  libraries bundles are
the shape all `define-library` files share.  The unification deliverable
is a documented schema mapping: module = one install unit into an
existing library; libraries = N library declarations; program = one
lowered script.  No kind collapse.

**D5 (the freeze point for imports).**  A library's import-time snapshot
is taken from the interface as extended up to that moment (D2).  The
freeze point for CACHED ARTIFACTS is unchanged: an artifact's validity is
its content stamp — source edits invalidate naturally.  No boot-stage
freeze is introduced; the growth semantics stay observable.

**D6 (install-boot is the template).**  The install.scm / install-boot.scm
capture-replay pattern (definitions-only file + side-effecting defines +
tiny conditional driver) is the pattern any future boot-adjacent file
follows.  install-macrolayer.scm stays source-only: it is cold-only work,
and caching it would help no warm boot.

## 4. Implementation plan (each step gated like 2c)

1. **D2 — in-place interface extension.**  When `import-view` hits its
   cache but the source's export list has grown since the cached build
   (detect: compare a recorded export count/hash), extend the view.
   Re-measure warm lib-import and the cold gate (the D2 widening affects
   cold-capture resolution; the gate and the import-sets tests arbitrate).
2. **D3 — the boot manifest.**  One Scheme-data (or embedded table)
   manifest: file, kind, unit target, condition.  `bootstrap.cpp` reads
   it; `tools/warm-bootstrap-cache.sh` and the Scheme side consume the
   same list.  Behaviour-neutral; gate = changed-since + cold/warm suites.
3. **D4 — schema documentation + round-trip test.**  A test that replays
   one artifact of each kind through one entry point and asserts the
   post-state (the contract test pattern from
   `tools/test-cache-concurrency.sh`).
4. **Format hardening residue.**  Stale `.tmp.PID` sweeping is documented
   as out of scope (harmless garbage; sweeping races with in-flight
   writers).

## 5. What this unlocks

- **Phase 6 (runtime image):** the image is the boot manifest's end-state
  serialized.  D2's growth semantics define the image boundary: an image
  captures the surface AS OF ITS FREEZE — loading an image replaces the
  boot chain up to the freeze point, after which the manifest resumes.
  The image design (separate document) needs exactly this vocabulary.
- **Phase 4 (memory):** the interface builder's materialized tables and
  the shared views are the measured allocation centers; the unification
  keeps them single-instance.
