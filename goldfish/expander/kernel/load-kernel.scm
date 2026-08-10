;;; kernel/load-kernel.scm
;;; MANIFEST of the expander kernel: the list of kernel modules in
;;; dependency order.  Each kernel file is ordinary object-level source
;;; (no define-library, no macros beyond what the host can evaluate), so
;;; at bootstrap-0 this list is loaded directly by s7: it produces a
;;; running expander with no pre-expanded artifact.  Later bootstrap
;;; stages re-read the same list through the self-hosted expander to
;;; produce the pre-expanded artifact (build-combined.scm).
;;;
;;; Order matters: dependencies first.

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
