;;; lib/install-boot.scm
;;; The driver half of the bootstrap installer, as DEFINITIONS ONLY so the
;;; file caches like any other install unit: the native driver replays it
;;; from its captured bundle on warm boots and expands + captures it on
;;; cold ones.  Every define here performs boot side effects at evaluation
;;; time, in the order the unsplit install.scm ran them:
;;;
;;;   - %expander-surface-published!: the expander-module publishes for the
;;;     install/ccache surface,
;;;   - the internal-runtime-surface registration scans, which must run
;;;     after module.scm is installed (cold: by install-macrolayer.scm,
;;;     which the driver loads before this file; warm: by the replayed boot
;;;     artifacts).
;;;
;;; The cold-start macro-layer install itself lives in
;;; install-macrolayer.scm, loaded by the native driver only when the
;;; bootstrap cache is unavailable.

;;; The expander-module publishes for the install/ccache surface: the
;;; installer API, the Guile-style ccache backend, and the serializer
;;; shared with the user-library cache (lib/module.scm: a macro definition
;;; caches its lowered transformer form, exactly as the boot library
;;; installs do).

(define %expander-surface-published!
  (begin
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
    (module-define! the-expander-library 'serialize-cache-sexp serialize-cache-sexp)
    (module-define! the-expander-library 'deserialize-cache-sexp deserialize-cache-sexp)
    (module-define! the-expander-library 'make-bundle make-bundle)
    (module-define! the-expander-library 'bundle? bundle?)
    (module-define! the-expander-library 'bundle-kind bundle-kind)
    (module-define! the-expander-library 'bundle-section bundle-section)))

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
      ;; native bulk import-interface builder (bootstrap_primitives)
      %interface-table
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
(define %internal-names-published!
  (module-define! the-expander-library '%internal-names %internal-names))
