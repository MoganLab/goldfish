;;; expander/boot-manifest.scm
;;; The boot chain manifest: the single source of truth for which cached
;;; units the native bootstrap replays, in dependency order.  Consumed by
;;; src/runtime/bootstrap.cpp (warm replay, validation, distribution
;;; precompile) and tools/warm-bootstrap-cache.sh; both fingerprints
;;; include this file, so any edit moves the cache version.
;;;
;;; Entries: ("relative-path" tier kind)
;;;   tier:  boot | deferred | precompile
;;;          boot      replayed by load_cached_runtime, in order
;;;          deferred  replayed later, after the native-scheme-surface
;;;                    stage (scheme/base: its replay needs the reader
;;;                    registrations that load ahead of it)
;;;          precompile distribution warm-up: the optimizer pipeline, not
;;;                    validated on every start
;;;   kind:  module | libraries (documentation; the replay auto-detects)
;;;
;;; Out of scope by design: the cold-only files (bootstrap-prelude, the
;;; preludes, install-macrolayer.scm -- per-file expansion-helper gating)
;;; and the boot-adjacent cached-source files whose load order interleaves
;;; with semantic anchors in native_main (install.scm, install-boot.scm,
;;; base-functions, native-hash-adapter, native-abi, liii/reader.scm,
;;; native-write.scm).  Those stay orchestrated in native_main.
(
  ("expander/lib/syntax-runtime.scm" boot module)
  ("expander/lib/syntax-case.scm" boot module)
  ("expander/lib/define-record-type.scm" boot module)
  ("expander/lib/core-macros.scm" boot module)
  ("expander/lib/cond-expand.scm" boot module)
  ("expander/lib/defmacro.scm" boot module)
  ("expander/lib/define-star.scm" boot module)
  ("expander/lib/module-registry.scm" boot module)
  ("expander/lib/module.scm" boot module)
  ("expander/lib/standard.scm" boot module)
  ("scheme/case-lambda.scm" boot libraries)
  ("scheme/base.scm" deferred libraries)
  ("goldfish/core/ir.scm" precompile module)
  ("goldfish/match.scm" precompile module)
  ("goldfish/match/expansion.scm" precompile module)
  ("goldfish/compiler/patterns.scm" precompile module)
  ("goldfish/compiler/passes.scm" precompile module)
  ("goldfish/compiler.scm" precompile module)
  ("goldfish/expander/tree-il.scm" precompile module)
)
