;;; kernel.scm -- the expander core as one R7RS library.
;;;
;;; The Sets-of-Scopes expander core, organized as a library so the kernel
;;; can be re-expanded by the expander itself (the artifact build in
;;; build-combined.scm is this self-bootstrap: the committed artifact is the
;;; expander, and it expands this file's library body to produce the next
;;; artifact).
;;;
;;; bootstrap-0 (GOLDFISH_BOOTSTRAP / EXPANDER_BOOT=from-source) bypasses
;;; this file: s7 evaluates the same sources directly via the manifest
;;; expander/kernel/load-kernel.scm (which lists the identical files).
;;;
;;; The export list is the kernel's public API surface.  build-combined.scm
;;; verifies that every top-level binding of the library body is exported,
;;; so the list stays complete.  NOTE: the runtime still re-binds every
;;; exported name into the rootlet (the lib layer references the core API as
;;; rootlet free identifiers); narrowing that surface is a separate cleanup.
;;;
;;; The trailing driver module-define! registrations and the install of the
;;; lib layer are host-loading conveniences only (see load-kernel.scm).

(define-library (goldfish expander)
  (export
    *base-library*
    base-library
    biggest-subset
    binding-unstop
    body-def-head
    body-output-source
    body-stop-list
    bound-identifier=?
    build-lambda-stx
    build-stop-frame
    check-eval-when-situations
    collect-macro-record!
    compile-file
    compile-program
    compile-program*
    compile-toplevel
    context-add-prune-scope
    context-add-use-scope
    context-alloc-box
    context-alloc-def-env
    context-alloc-name
    context-alloc-scope
    context-at-phase
    context-bind
    context-empty
    context-extend-env
    context-reset-use-scopes
    context-resolve
    context-return
    context-with-env
    context-with-intro-scope
    context-with-store
    context-with-use-scopes
    core-begin
    core-define
    core-define-syntax
    core-eval-when
    core-form-binding?
    core-form-handlers
    core-if
    core-lambda
    core-letrec
    core-letrec*
    core-letrec-syntax
    core-let-syntax
    core-quasiquote
    core-quote
    core-quote-syntax
    core-set!
    core-syntax
    ctx-local-expand
    ctx-local-expand-defs
    ctx-local-expand-defs*
    *current-expand-context*
    current-expand-context
    *current-intro-scope*
    current-intro-scope
    datum->stx-ctx
    datum->stx-ctx-source
    datum->syntax
    def-bind!
    defs-scope
    emit-toplevel-ref
    env-empty
    env-extend
    env-lookup
    env-map-values
    eval-transformer
    eval-when-expand!
    expand
    expand-application
    expand-atom
    expand-body
    expand-body-finalize
    expand-body-form
    expand-body-seq
    expand-box
    expand-expr
    expand-lambda-binding
    expand-lambda-bindings
    expand-letrec-allocate
    expand-letrec-form
    expand-letrec-inits
    expand-lib-define-bind
    expand-lib-define-syntax
    expand-library-body
    expand-library-finalize
    expand-list
    expand-macro
    expand-macro-once
    expand-pair
    expand-set-box!
    expand-stx
    expand-syntax-bindings
    expand-syntax-bindings/rec
    expand-unbox
    exp-library-define!
    exp-library-ref
    free-identifier=?
    generate-temporaries
    host-forms
    identifier?
    initial-context
    install-core-forms!
    install-primitives!
    lexical-binding?
    lib-output-source
    lib-resolve-head
    local-binder
    local-expand
    local-expand-body
    lower
    lower-head
    *macro-records*
    make-core-form-binding
    make-exp-library
    make-lexical-binding
    make-module-form-binding
    make-primitive-binding
    make-syntax-introducer
    make-syntax-rules-transformer
    make-toplevel-binding
    make-transformer-binding
    make-tstop-binding
    map-spine
    module-form-binding?
    new-defs
    parse-internal-define
    parse-lambda-params
    primitive-binding?
    primitive-variables
    qq-atom
    qq-expand
    qq-head
    qq-list
    qq-unquote-form?
    qq-vector
    require-identifier
    resolve-identifier
    scan-body-form
    scan-def-form
    scan-head-loop
    scan-lib-head
    self-evaluating?
    set
    set<=?
    set=?
    set-add
    set-base-library!
    set-current-expand-context!
    set-current-intro-scope!
    set-flip
    set-fold
    set-member?
    set-remove
    set-subtract
    set-union
    stopped-form?
    store-alloc
    store-alloc-box
    store-alloc-def-env
    store-alloc-name
    store-alloc-scope
    store-bind
    store-box-ref
    store-box-set
    store-def-env-ref
    store-def-env-set
    store-empty
    store-lookup
    store-resolve
    stx-add-scope
    stx-add-scope-unchecked
    stx-add-then-flip
    stx-apply-ctx
    stx-cadr
    stx-ctx-add
    stx-ctx-add-then-flip
    stx-ctx-add-unchecked
    stx-ctx-at
    stx-ctx-empty
    stx-ctx-flip
    stx-ctx-mark-intro
    stx-ctx-prune
    stx-ctx-set
    stx-flip-intro-off
    stx-flip-scope
    stx-maybe-flip
    stx-prune-scopes
    stx-set-library
    stx-vector?
    syntax-case-spec->procedure-form
    syntax->datum
    syntax-e
    syntax-local-introduce
    syntax-local-value
    syntax-scopes
    take-macro-records
    the-base-library
    toplevel-binding?
    transformer-binding?
    transformer-spec->procedure-form
    tstop-binding?
    void-expr
    wrap-expression
    )
  (begin
    (include "expander/kernel/exp-library.scm")
    (include "expander/kernel/sets.scm")
    (include "expander/kernel/env.scm")
    (include "expander/kernel/store.scm")
    (include "expander/kernel/syntax-objects.scm")
    (include "expander/kernel/context.scm")
    (include "expander/kernel/expand.scm")
    (include "expander/kernel/transformer.scm")
    (include "expander/kernel/intdef.scm")
    (include "expander/kernel/core-forms.scm")
    (include "expander/kernel/libbody.scm")
    (include "expander/kernel/primitives.scm")
    (include "expander/kernel/driver.scm")))
