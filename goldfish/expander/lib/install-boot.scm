;;; lib/install-boot.scm
;;; The driver half of the bootstrap installer, loaded after the
;;; definitions-only expander/lib/install.scm (from source, or replayed
;;; from its install-cache bundle).  Holds everything with boot-order
;;; side effects, in the order the unsplit install.scm ran them:
;;;
;;;   - the macro-layer boot install (cold start only: skipped when
;;;     GOLDFISH_NATIVE_ARTIFACTS marks the cached artifact set as loaded),
;;;   - the expander-module publishes for the install/ccache surface,
;;;   - the internal-runtime-surface registration scans, which must run
;;;     after module.scm is installed (cold: by the boot block above;
;;;     warm: by the cached artifacts).

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

(module-define! the-expander-library 'install-library-forms! install-library-forms!)
(module-define! the-expander-library 'install-library-file! install-library-file!)
(module-define! the-expander-library 'install-standard-library! install-standard-library!)

(module-define! the-expander-library 'compile-file-cached compile-file-cached)
;; reader.scm's `load' preloads the deps of a compiled program artifact
;; through this (the same collector the cache writer uses).
(module-define! the-expander-library 'collect-cache-module-refs collect-cache-module-refs)
(module-define! the-expander-library 'gfo-dir gfo-dir)
(module-define! the-expander-library 'gfo-key gfo-key)
(module-define! the-expander-library 'gfo-path gfo-path)
(module-define! the-expander-library 'gfo-stamp gfo-stamp)
(module-define! the-expander-library 'gfo-valid? gfo-valid?)
(module-define! the-expander-library 'gfo-format-version gfo-format-version)
(module-define! the-expander-library 'gfo-load gfo-load)
(module-define! the-expander-library 'gfo-write! gfo-write!)
(module-define! the-expander-library 'compile-file-stamp compile-file-stamp)
;; Serializer shared with the user-library cache (lib/module.scm): a macro
;; definition caches its lowered transformer form, exactly as the boot
;; library installs do, so user libraries and the boot library build their
;; caches through one mechanism.
(module-define! the-expander-library 'serialize-cache-sexp serialize-cache-sexp)
(module-define! the-expander-library 'deserialize-cache-sexp deserialize-cache-sexp)
(module-define! the-expander-library 'make-bundle make-bundle)
(module-define! the-expander-library 'bundle? bundle?)
(module-define! the-expander-library 'bundle-kind bundle-kind)
(module-define! the-expander-library 'bundle-section bundle-section)

;;; ------------------------------------------------------------------------
;;; Internal runtime surface
;;; ------------------------------------------------------------------------
;;; Reader, install, and module functions live in the expander module, not in
;;; the (goldfish) base library's binding table. Internal scripts and the
;;; goldtest runner) are programs too and import (goldfish); two
;;; complementary registrations make them resolve:
;;;
;;;   * the dynamic scan registers every the-expander-library export that
;;;     is a runtime VALUE (core forms and module forms keep their real
;;;     bindings; only value functions become primitives);
;;;   * %internal-names below is the explicitly audited surface -- kernel
;;;     definitions that were not registered with module-define!, as data.

(define %internal-surface-registered!
  (for-each
    (lambda (name)
      (exp-library-define! the-base-library name (make-primitive-binding name)))
    (let ((module-forms
            ;; Names that are NOT value bindings: the core forms and module
            ;; forms stay as their real bindings in the base library; only
            ;; the runtime VALUE functions are re-registered as primitives.
            '(lambda if begin define set! quote quasiquote quote-syntax syntax
              letrec letrec* define-syntax let-syntax letrec-syntax eval-when
              define-library import define-module use-modules
              core-form-handlers)))
      (filter (lambda (name)
                (and (symbol? name)
                     (not (memq name module-forms))
                     (not (not (module-ref the-expander-library name)))))
              (module-exports the-expander-library)))))

;;; %internal-names: the audited surface above, as data, so the
;;; post-boot assert can check every entry resolves.
(define %internal-names
    '(;; reader
      read read-forms read-line read-string read-char write-roundtrip load
      expand-eval auto-compile-enabled?
      ;; loader
      load-source-file load-find-module-file
      ;; install
      install-standard-library! install-library-file! install-library-forms!
      compile-file compile-file-into compile-file-cached
      collect-cache-module-refs
      compile-file-stamp
      ;; gfo backend canonical names (cache paths/keys for tools/tests)
      gfo-dir gfo-key
      cacheable-expansion?
      install-cache-save! install-cache-load!
      ;; kernel entry points (expand-time API not already exported)
      expand expand-stx expand-library-body expand-library-finalize
      initial-context make-exp-library wrap-expression
      expand-lib-define-bind expand-lib-define-syntax
      ;; kernel exp-library / binding accessors (the base library's live
      ;; bindings hold the primitives + macros, but NOT the kernel defines
      ;; -- register them so (import (goldfish)) provides the kernel API)
      base-library set-base-library! exp-library?
      exp-library-name exp-library-bindings set-exp-library-bindings!
      exp-library-ref exp-library-define!
      exp-library-uses exp-library-ref-own exp-library-ref-at-phase
      binding? binding-kind binding-value make-binding
      lexical-binding? toplevel-binding? primitive-binding?
      transformer-binding? core-form-binding? module-form-binding?
      tstop-binding? binding-unstop make-toplevel-binding
      make-primitive-binding make-transformer-binding make-core-form-binding
      make-module-form-binding
      ;; substrate accessors not module-define!'d in the kernel
      make-record-type record-type? record-type-name record-type-fields
      record-instance? record-predicate record-accessor record-modifier
      record-field-index next-fresh make-fresh-name next-record-rtd
      lookup-module module? make-module module-name module-ref module-define!
      context-empty context-resolve env-lookup context-env
      syntax? syntax-e syntax-form syntax-context syntax-library
      make-syntax syntax->datum datum->syntax identifier?
      free-identifier=? bound-identifier=? generate-temporaries
      make-syntax-introducer syntax-local-introduce syntax-local-value
      local-expand local-binder
      the-expander-library the-base-library *base-library*
      ;; module machinery
      expand-define-library import-into-library! import-spec-into-library!
      library-registry-ref library-registry-set! library-record load-library! load-library-file-cached!
      library-file-cacheable? capture-file-cache restore-library-cache
      capture-library-cache make-lib-record lib-record-library lib-record-exports
      runtime-registered-add! runtime-registered? register-runtime-module
      make-program-library program-library reset-program-library!
      make-program-environment eval-in-program-environment))

(define %internal-names-registered!
  (for-each
    (lambda (name)
      (exp-library-define! the-base-library name (make-primitive-binding name)))
    %internal-names))

;;; Reader variables (*load-path*, *eval-ctx*) are REAL variables, not
;;; functions: a primitive binding would make (set! *load-path* ...) fail
;;; with "cannot assign primitive".  Register them as toplevel bindings
;;; with no home, so a reference emits the name of the runtime variable.

(define %internal-vars-registered!
  (for-each
    (lambda (name)
      (exp-library-define! the-base-library name
                           (make-toplevel-binding
                             (make-toplevel-ref name #f name #f))))
    '(*load-path* *eval-ctx*)))

;;; Every %internal-names entry must be visible in the expander module now
;;; installation is complete (module.scm, the last lib file, is installed
;;; above; note this is bucket membership, not the export list -- most
;;; lib-layer defines are plain defines).  A stale entry would otherwise
;;; install a primitive emitting an unresolvable bare reference.  Names
;;; covered only by the dynamic scan need no listing.
;;; The invariant -- every entry usable from (import (goldfish)) programs --
;;; is checked by tests/expander/internal-surface-test.scm.
(module-define! the-expander-library '%internal-names %internal-names)
