;; load-kernel.scm: MANIFEST synced with goldfish/expander/kernel.scm includes.
;; Order matters. Kernel files are plain source (no define-library) so bootstrap-0
;; s7 can eval them directly; self-hosted path uses kernel.scm instead.

(load-source-file "expander/kernel/substrate.scm")
(load-source-file "expander/kernel/exp-library.scm")
(load-source-file "expander/kernel/sets.scm")
(load-source-file "expander/kernel/env.scm")
(load-source-file "expander/kernel/store.scm")
(load-source-file "expander/kernel/syntax-objects.scm")
(load-source-file "expander/kernel/context.scm")
(load-source-file "expander/kernel/expand.scm")
(load-source-file "expander/kernel/transformer.scm")
(load-source-file "expander/kernel/intdef.scm")
(load-source-file "expander/kernel/core-forms.scm")
(load-source-file "expander/kernel/libbody.scm")
(load-source-file "expander/kernel/primitives.scm")
(load-source-file "expander/kernel/driver.scm")
;; install.scm not here: needs R7RS reader (X ... ellipsis). Loaded after reader.

