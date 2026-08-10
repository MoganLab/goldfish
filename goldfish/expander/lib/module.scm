;;; lib/module.scm
;;; R7RS library surface as SELF-HOSTED lib-layer source (installed by
;;; lib/install.scm into the-base-library, not pre-expanded into the core
;;; artifact).  It adapts the kernel's minimal module machinery:
;;;
;;;   * Minimal module API (kernel): a module is an exp-library plus a
;;;     registry entry (exp-library . export-names).  expand-library-body
;;;     (expand/libbody.scm) expands a module body, installing its defines
;;;     into the library.  The driver knows nothing about R7RS; it only
;;;     dispatches top-level forms whose head resolves to a `module-form'
;;;     binding (see context.scm).
;;;
;;;   * Runtime modules (Guile-style, S2): define-library also emits a
;;;     registration expression (make-module/module-define!/register-module,
;;;     see common/prelude.scm) so libraries have runtime identity; cross-
;;;     library references are emitted as (module-ref 'lib 'name).  References
;;;     within the defining library stay bare gensyms.  Exported bindings are
;;;     immutable (set! on them is an expansion error): the module inlet holds
;;;     value snapshots, not shared cells.  Re-exports forward via module-ref;
;;;     references resolve straight to the defining module.
;;;
;;;   * On-demand file loading: importing an unknown library loads
;;;     (foo bar) -> foo/bar.scm from *load-path* at expand time.
;;;
;;;   * R7RS adapter: parses the R7RS `define-library' / `import' surface
;;;     syntax (export/import/only/prefix/rename clauses) and calls the
;;;     minimal API.  Guile-style `define-module' / `use-modules' desugar
;;;     to the R7RS forms (explicit-body semantics).  All surface forms are
;;;     installed into the-base-library as module-form bindings.
;;;
;;; The kernel core keeps only: exp-library (module/exp-library.scm),
;;; expand-library-body (expand/libbody.scm), the binding types, and the
;;; runtime module substrate (common/prelude.scm).

;;; ------------------------------------------------------------------------
;;; Minimal module API
;;; ------------------------------------------------------------------------

;;; Registry: maps module name -> (exp-library . export-names)

(define *library-registry* '())

(define (library-registry-ref name)
  (let ((entry (assoc name *library-registry*)))
    (and entry (cdr entry))))

(define (library-registry-set! name record)
  (set! *library-registry*
        (cons (cons name record)
              (filter (lambda (e) (not (equal? (car e) name)))
                      *library-registry*))))

(define (make-lib-record lib exports)
  (cons lib exports))

(define (lib-record-library rec) (car rec))
(define (lib-record-exports rec) (cdr rec))

;;; On-demand file loading.  A library name (foo bar) maps to the file
;;; "foo/bar.scm" searched over *load-path* (via the loader's single
;;; load-find-module-file).  The file must contain define-library forms; it
;;; is compiled (registering the expand-time record, recursively loading
;;; imports) and evaluated (registering the runtime module).

(define (library-file-name lib-name)
  (let loop ((parts (map symbol->string lib-name)) (acc ""))
    (if (null? parts)
        (string-append acc ".scm")
        (loop (cdr parts)
              (if (string=? acc "")
                  (car parts)
                  (string-append acc "/" (car parts)))))))

;;; load-library! : name -> void
;;; Compile (registering the expand-time record, recursively loading imports)
;;; and evaluate (registering the runtime module) a define-library file.
;;; Circular loads are an error (stricter than R7RS, which tolerates some
;;; import cycles).  File lookup reuses the loader's load-find-module-file.

(define *libraries-being-loaded* '())

(define (load-library! lib-name)
  (when (member lib-name *libraries-being-loaded*)
    (error "import: circular library dependency" lib-name))
  (let ((file (load-find-module-file (library-file-name lib-name))))
    (unless file
      (error "import: unknown library" lib-name))
    ;; compile-program is a global in both the host path (driver.scm) and the
    ;; self-hosted path (the artifact), so no (require 'driver) here -- that
    ;; would reload the expander from source and clash with the artifact.
    (let ((forms (call-with-input-file file read-forms)))
      (dynamic-wind
        (lambda ()
          (set! *libraries-being-loaded*
                (cons lib-name *libraries-being-loaded*)))
        (lambda ()
          (eval (compile-program forms) (rootlet)))
        (lambda ()
          (set! *libraries-being-loaded*
                (filter (lambda (n) (not (equal? n lib-name)))
                        *libraries-being-loaded*)))))))

;;; library-record : name -> (exp-library . exports)
;;; Look up a library record, loading the library from file on demand.

(define (library-record lib-name)
  (let ((base (base-library)))
    (if (and base (equal? lib-name (exp-library-name base)))
        ;; (scsyntax): the implementation kernel is not an on-disk library;
        ;; treat it as a record of its live bindings so only/prefix/rename
        ;; imports of it work too.
        (make-lib-record base (map car (exp-library-bindings base)))
        (or (library-registry-ref lib-name)
            (begin (load-library! lib-name)
                   (library-registry-ref lib-name))
            (error "import: unknown library" lib-name)))))

;;; ------------------------------------------------------------------------
;;; R7RS adapter
;;; ------------------------------------------------------------------------

;;; Import specs: (only lib id ...) / (prefix lib p) / (rename lib (from to) ...)
;;; / plain library name.  Copies exported bindings from the source library
;;; into the target library.

(define (import-into-library! lib imports)
  (for-each (lambda (spec-group)
              (for-each (lambda (spec)
                          (import-spec-into-library! lib spec))
                        spec-group))
            imports))

(define (import-spec-into-library! lib spec)
  (cond
    ((and (pair? spec) (eq? (car spec) 'only))
     (import-only-into-library! lib spec))
    ((and (pair? spec) (eq? (car spec) 'prefix))
     (import-prefix-into-library! lib spec))
    ((and (pair? spec) (eq? (car spec) 'rename))
     (import-rename-into-library! lib spec))
    (else
     (import-plain-into-library! lib spec))))

(define (import-plain-into-library! lib lib-name)
  ;; (scsyntax): the implementation kernel module is the primitive library
  ;; itself, not a registered on-disk library.  Import all of its live
  ;; bindings so the common `(import (scsyntax))' idiom works; the
  ;; r7rs-small `(import (scheme base))' loads the on-disk
  ;; scsyntax/scheme/base.scm which re-exports the standard surface.
  ;; Exports are computed at import time so base macros installed later
  ;; (standard.scm) are included.
  (let* ((base (base-library))
         (base? (and base (equal? lib-name (exp-library-name base))))
         (src (if base? base (lib-record-library (library-record lib-name))))
         (exports (if base?
                      (map car (exp-library-bindings base))
                      (lib-record-exports (library-record lib-name)))))
    (for-each (lambda (name)
                (let ((binding (exp-library-ref src name)))
                  (unless binding
                    (error "import: exported identifier has no binding" name))
                  (exp-library-define! lib name binding)))
              exports)))

(define (import-only-into-library! lib spec)
  (let* ((lib-name (cadr spec))
         (ids (cddr spec))
         (rec (library-record lib-name)))
    (let ((src (lib-record-library rec))
          (exports (lib-record-exports rec)))
      (for-each (lambda (id)
                  (unless (memq id exports)
                    (error "import only: not exported" id))
                  (let ((binding (exp-library-ref src id)))
                    (unless binding
                      (error "import: exported identifier has no binding" id))
                    (exp-library-define! lib id binding)))
                ids))))

(define (import-prefix-into-library! lib spec)
  (let* ((lib-name (cadr spec))
         (prefix (caddr spec))
         (rec (library-record lib-name)))
    (let ((src (lib-record-library rec))
          (exports (lib-record-exports rec)))
      (for-each (lambda (name)
                  (let ((binding (exp-library-ref src name)))
                    (unless binding
                      (error "import: exported identifier has no binding" name))
                    (let ((prefixed (string->symbol
                                     (string-append (symbol->string prefix)
                                                    (symbol->string name)))))
                      (exp-library-define! lib prefixed binding))))
                exports))))

(define (import-rename-into-library! lib spec)
  (let* ((lib-name (cadr spec))
         (renames (cddr spec))
         (rec (library-record lib-name)))
    (let ((src (lib-record-library rec))
          (exports (lib-record-exports rec)))
      (for-each (lambda (name)
                  (let ((binding (exp-library-ref src name)))
                    (unless binding
                      (error "import: exported identifier has no binding" name))
                    (let ((rename-entry (assq name renames)))
                      (if rename-entry
                          (exp-library-define! lib (cadr rename-entry) binding)
                          (exp-library-define! lib name binding)))))
                exports))))

;;; define-library clause parsing: (export id ...) / (import spec ...) / body.

(define (parse-library-clauses clauses)
  (let loop ((clauses clauses) (exports '()) (imports '()) (body '()))
    (if (null? clauses)
        (values exports (reverse imports) (reverse body))
        (let* ((clause (syntax-form (car clauses)))
               (head (syntax->datum (car clause))))
          (cond
            ((eq? head 'export)
             (loop (cdr clauses)
                   (append exports (map syntax->datum (cdr clause)))
                   imports
                   body))
            ((eq? head 'import)
             (loop (cdr clauses)
                   exports
                   (cons (map syntax->datum (cdr clause)) imports)
                   body))
            (else
             (loop (cdr clauses) exports imports (cons (car clauses) body))))))))

;;; module-form handlers (installed into the-base-library below).

;;; expand-define-library : syntax context -> (values defs ctx)
;;; Expands a define-library form, registers it, returns its defs for
;;; emission.  The defs end with a runtime module registration expression
;;; (make-module/module-define!/register-module) so the library has
;;; runtime identity; exported macros have no runtime representation.
;;; Re-exports forward through module-ref (snapshot semantics).

(define (expand-define-library stx ctx)
  (let* ((form (syntax-form stx))
         (name (syntax->datum (cadr form)))
         (clauses (cddr form)))
    (let*-values (((exports imports body-stxs) (parse-library-clauses clauses)))
      (let ((lib (make-exp-library name)))
        (import-into-library! lib imports)
        (let ((body-stxs (map (lambda (s) (stx-set-library s lib)) body-stxs)))
          (let*-values (((defs ctx1) (expand-library-body body-stxs lib ctx)))
            ;; An exported identifier not defined in the library body is
            ;; inherited from the base library when the base library has it
            ;; (host primitives / core forms / ambient syntax re-exported
            ;; without a body definition, as goldfish/scheme/base.scm does).
            (for-each (lambda (export)
                        (let ((binding (or (exp-library-ref lib export)
                                           (exp-library-ref the-base-library export))))
                          (unless binding
                            (error "define-library: exported identifier not defined"
                                   export))
                          (when (toplevel-binding? binding)
                            (set-toplevel-ref-exported! (binding-value binding) #t))))
                      exports)
            (library-registry-set! name (make-lib-record lib exports))
            (values (append defs
                            (list (datum->syntax lib-output-source
                                    (library-register-expression lib name exports))))
                    ctx1)))))))

;;; library-register-expression : exp-library name exports -> sexp
;;; (let ((m (make-module 'name)))
;;;   (module-define! m 'export ref) ...
;;;   (register-module m))

(define (library-register-expression lib name exports)
  (let ((entries
         (reverse
          (fold (lambda (acc export)
                  (let ((binding (exp-library-ref lib export)))
                    (cond
                      ((transformer-binding? binding) acc)
                      ((toplevel-binding? binding)
                       (let ((ref (binding-value binding)))
                         (cons (cons export
                                     (if (eq? (toplevel-ref-home ref) lib)
                                         (toplevel-ref-gensym ref)
                                         (list 'module-ref
                                               (list 'quote
                                                     (exp-library-name
                                                      (toplevel-ref-home ref)))
                                               (list 'quote
                                                     (toplevel-ref-original ref)))))
                               acc)))
                      ((primitive-binding? binding)
                       ;; Re-exported host primitive: the register expression
                       ;; stores its name; the reference resolves at eval time
                       ;; (self-eval -> symbol->value).
                       (cons (cons export (binding-value binding)) acc))
                      ((or (core-form-binding? binding)
                           (module-form-binding? binding))
                       ;; Ambient syntax (lambda/if/define/...): no runtime
                       ;; value to store.
                       acc)
                      (else
                       (error "define-library: cannot export binding" export)))))
                '()
                exports))))
    ;; Built with list/append, not backquote: s7's eval of the standard
    ;; (quasiquote ...) form does not implement unquote-splicing (only its
    ;; native reader's #_list-values representation does), so backquote
    ;; templates with ,@ fail when the kernel is host-loaded through our
    ;; reader.  The datum is identical either way.
    (cons 'let
          (cons (list (list 'm (list 'make-module (list 'quote name))))
                (append (map (lambda (entry)
                               (list 'module-define! 'm
                                     (list 'quote (car entry))
                                     (cdr entry)))
                             entries)
                        (list (list 'register-module 'm)))))))

;;; expand-import : syntax context -> (values defs ctx)
;;; Top-level import: installs the imported bindings into the-base-library so
;;; subsequent top-level forms resolve them.  Emits no definitions.

(define (expand-import stx ctx)
  (for-each (lambda (spec)
              (import-spec-into-library! (base-library) spec))
            (cdr (syntax->datum stx)))
  (values '() ctx))

;;; ------------------------------------------------------------------------
;;; Guile-style surface syntax
;;; ------------------------------------------------------------------------
;;; Explicit-body semantics (NOT Guile's file-level declaration):
;;;   (define-module (name ...) #:export (x y) body ...)
;;;     == (define-library (name ...) (export x y) body ...)
;;;   (use-modules spec ...) == (import spec ...)
;;; #:export is read by s7 as the keyword symbol :export.  These are
;;; module-form handlers (not plain macros): the driver dispatches
;;; module forms only on raw top-level heads, so a macro expanding to
;;; define-library/import would never be re-dispatched.

(define (expand-define-module stx ctx)
  (let* ((form (syntax-form stx))
         (name-stx (cadr form))
         (rest (cddr form)))
    (let*-values (((exports body) (parse-module-options rest)))
      (let ((clauses (if (null? exports)
                         body
                         (cons (datum->syntax stx (cons 'export exports))
                               body))))
        (expand-define-library
         (datum->syntax stx (cons 'define-library (cons name-stx clauses)))
         ctx)))))

;;; parse-module-options : (list syntax) -> (values exports body)

(define (parse-module-options rest)
  (if (and (pair? rest)
           (syntax? (car rest))
           (eq? (syntax-form (car rest)) ':export))
      (values (syntax->datum (cadr rest)) (cddr rest))
      (values '() rest)))

(define (expand-use-modules stx ctx)
  (expand-import
   (datum->syntax stx (cons 'import (cdr (syntax-form stx))))
   ctx))

;;; ------------------------------------------------------------------------
;;; Installation
;;; ------------------------------------------------------------------------

(define (install-module-forms!)
  (exp-library-define! the-base-library 'define-library
    (make-module-form-binding expand-define-library))
  (exp-library-define! the-base-library 'import
    (make-module-form-binding expand-import))
  (exp-library-define! the-base-library 'define-module
    (make-module-form-binding expand-define-module))
  (exp-library-define! the-base-library 'use-modules
    (make-module-form-binding expand-use-modules)))

;;; Register the module forms now.  Wrapped in a define so
;;; install-library-forms! (which only evals value definitions) runs it.
(define %module-forms-installed! (install-module-forms!))

;;; ------------------------------------------------------------------------
;;; Exports (wrapped in a define so install-library-forms! runs them)
;;; ------------------------------------------------------------------------

(define %module-api-exported!
  (begin
    (module-define! the-expander-library 'expand-define-library expand-define-library)
    (module-define! the-expander-library 'expand-import expand-import)
    (module-define! the-expander-library 'expand-define-module expand-define-module)
    (module-define! the-expander-library 'expand-use-modules expand-use-modules)
    (module-define! the-expander-library 'install-module-forms! install-module-forms!)
    (module-define! the-expander-library 'library-registry-ref library-registry-ref)
    (module-define! the-expander-library 'lib-record-library lib-record-library)
    (module-define! the-expander-library 'lib-record-exports lib-record-exports)))
