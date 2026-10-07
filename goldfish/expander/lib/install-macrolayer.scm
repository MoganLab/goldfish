;;; lib/install-macrolayer.scm
;;; The cold-start macro-layer install: installs the user-space macro
;;; library files into the base library.  The native driver loads this file
;;; ONLY when the bootstrap cache is unavailable (GOLDFISH_NATIVE_ARTIFACTS
;;; unset) -- on warm boots the same layer comes from the replayed boot
;;; artifacts, and this file is never read.  Split out of install-boot.scm
;;; so that file is definitions-only and cacheable.

;;; Boot: install the user-space macro layer into the base library.  Order:
;;; syntax-runtime (value definitions: pattern matching / instantiation /
;;; dispatch) and syntax-case / syntax-rules first, then the object-level
;;; define-record-type macro, then core-macros (whose syntax-rules
;;; desugaring needs syntax-case bound at phase+1), then cond-expand (uses
;;; core-macros' let / and / or), then standard.

(if (not (getenv "GOLDFISH_NATIVE_ARTIFACTS"))
  (begin
  (install-library-file! the-base-library "expander/lib/syntax-runtime.scm")

;;; Expansion-time helper surface of the boot macro layer (v5): own value
;;; definitions are phase-0 only, so helpers called from a transformer body
;;; (parse-template inside the syntax-case transformer, the cond-expand
;;; feature checker, the record-macro expanders) need phase-free
;;; resolution: a primitive binding (emitting the bare name) passes the
;;; phase gate at expansion time, and the host binding under the source
;;; name gives the emitted reference its run-time meaning -- exactly like
;;; the kernel syntax API (datum->syntax ...).  These names are part of
;;; the implementation's expansion machinery, not a user-facing phase
;;; exception: a user library's own value definitions stay phase-0.
;;; Call before the defining file installs (gate) and again after (host
;;; binding); the first call no-ops when the definition is not there yet.

(define (install-expansion-helper! name)
  (let ((b (exp-library-ref-own the-base-library name)))
    (when b
      (module-define! the-expander-library name
        (eval (toplevel-ref-gensym (binding-value b))
              (module-eval-environment the-expander-library))))
    (exp-library-define! the-base-library name (make-primitive-binding name))))

;;; install-with-helpers! : lib path gate-names capture-names -> void
;;; Install a boot file whose transformer bodies call own helpers: gate
;;; every name as a primitive first (phase-free resolution while the file
;;; itself expands; the capture no-ops for names not defined yet), install
;;; the file, then capture the values of the names it defines.  The two
;;; lists differ when earlier files already defined some helpers.

(define (install-with-helpers! lib path gate-names capture-names)
  (for-each install-expansion-helper! gate-names)
  (install-library-file! lib path)
  (for-each install-expansion-helper! capture-names))

  (install-with-helpers! the-base-library "expander/lib/syntax-case.scm"
  '(parse-template syntax-case-dispatch fast-instantiate
                   sr-build-transformer subst-ellipsis)
  '(sr-build-transformer subst-ellipsis))

  (install-with-helpers! the-base-library "expander/lib/define-record-type.scm"
  '(dr-field-datum dr-record-defs dr-register-def dr-interleave-register)
  '(dr-field-datum dr-record-defs dr-register-def dr-interleave-register))

  (install-library-file! the-base-library "expander/lib/core-macros.scm")

  (install-with-helpers! the-base-library "expander/lib/cond-expand.scm"
  '(cond-expand-feature-satisfied? cond-expand-requirement-valid? cond-expand-select *cond-expand-features*)
  '(cond-expand-feature-satisfied? cond-expand-requirement-valid? cond-expand-select *cond-expand-features*))
;; Legacy procedural macro forms (depend on syntax-case).
  (install-library-file! the-base-library "expander/lib/defmacro.scm")
;; Optional-argument procedure forms (depend on syntax-case).
  (install-library-file! the-base-library "expander/lib/define-star.scm")
;; The R7RS library surface (define-library/import/define-module/use-modules)
;; is self-hosted lib-layer code, not part of the core artifact; installing
;; it registers the module-form bindings in the-base-library (the trailing
;; define in lib/module.scm runs install-module-forms!).  The registry
;; prefix lives in module-registry.scm (installed first: everything below
;; references it, and cross-file references must point backward -- the
;; loader/cache/import/expand core is mutually recursive and stays whole).
    (install-library-file! the-base-library "expander/lib/module-registry.scm")
    (install-library-file! the-base-library "expander/lib/module.scm")))
