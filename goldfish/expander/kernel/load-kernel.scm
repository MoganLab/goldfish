;;; kernel/load-kernel.scm
;;; MANIFEST of the expander kernel: the list of kernel modules in
;;; dependency order.  Each kernel file is ordinary object-level source
;;; (no define-library, no macros beyond what the host can evaluate), so
;;; at bootstrap-0 this list is loaded directly by s7: it produces a
;;; running expander with no pre-expanded artifact.
;;;
;;; The SELF-HOSTED path (artifact build in build-combined.scm, and any
;;; future runtime load through the expander) does NOT read this file
;;; directly: it loads the same sources as the library goldfish/expander/
;;; kernel.scm (define-library (goldfish expander)), whose include list
;;; must stay in sync with this manifest.
;;;
;;; Order matters: dependencies first.

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
;;; Host convenience: load the user-space macro library on top of the core
;;; (the bootstrap-0 equivalent of the runtime's separate install.scm load
;;; after the artifact; the library/artifact path excludes it).
(load-source-file "expander/lib/install.scm")

