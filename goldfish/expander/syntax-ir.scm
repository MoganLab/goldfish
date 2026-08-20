;;; syntax-ir.scm -- bridge: fully-expanded syntax -> IR record tree.
;;;
;;; The expander core resolves every identifier to a binding (lexical /
;;; primitive / toplevel / core-form / transformer) during expansion, but
;;; its lowered output (syntax objects) encodes that decision as a bare
;;; symbol (a gensym for lexical/toplevel, the primitive name for
;;; primitives).  The compiler's core->ir must therefore re-derive what an
;;; identifier refers to by name matching alone.
;;;
;;; syntax->ir re-walks a FULLY-EXPANDED syntax tree (the value expand-expr
;;; returns, before lower), re-resolving each identifier against the
;;; expansion context so the binding KIND is preserved in the IR:
;;;
;;;   - a primitive binding produces a <primitive-ref> node, so the
;;;     backend never has to guess "is this symbol a primitive?";
;;;   - a lexical binding produces a <lexical-ref> node carrying its
;;;     (depth . index) frame address, computed here during the walk;
;;;   - toplevel / unbound keep their emitted name.
;;;
;;; The lexical addressing mirrors the backend's frame model: every lambda
;;; opens a frame whose slots are its formals followed by let/letrec
;;; bindings and internal defines, in allocation order.  A reference
;;; resolves to (depth . index) with depth = frames out from the current
;;; one (0 = current) and index = slot within that frame.  Computing this
;;; here (instead of in bytecode.scm) lets the backend consume pre-resolved
;;; addresses instead of re-deriving them from names.
;;;
;;; This is the first step toward the expander emitting the IR directly:
;;; it proves the binding-kind information is available on fully-expanded
;;; syntax and can flow into the IR without re-architecting the emitter.
;;;
;;; Note: this file is NOT part of the expander kernel artifact
;;; (kernel-combined.scm).  It is loaded after both the kernel and the
;;; compiler are available, and re-exports the IR constructors it needs.

(define-library (goldfish expander syntax-ir)
  (import (scheme base)
          (goldfish)
          (goldfish compiler ir)
          (goldfish compiler passes)
          (goldfish compiler bytecode))
  (export syntax->ir
    syntax->ir/sexp
    expand->ir
    compile-syntax-defs
    compile-syntax-program
    vm-load-syntax-defs)
  (begin

    ;; binding-kind : binding -> symbol/#f
    ;; Map a resolved binding to its kind, or #f if the identifier did not
    ;; resolve to a value binding.
    (define (binding-kind binding)
      (cond
        ((not binding) #f)
        ((primitive-binding? binding) 'primitive)
        ((lexical-binding? binding) 'lexical)
        ((toplevel-binding? binding) 'toplevel)
        ((core-form-binding? binding) 'core-form)
        ((transformer-binding? binding) 'transformer)
        (else 'other)))

    ;; resolve-name : syntax ctx -> (values name kind)
    ;; Resolve an identifier syntax to its emitted name and binding kind.
    (define (resolve-name id ctx)
      (let*-values (((name binding) (resolve-identifier id ctx)))
        (values name (binding-kind binding))))

    ;; datum-of : syntax -> datum
    ;; The plain datum of a syntax node (used for structural fields such as
    ;; lambda formals and let binding names, which the IR keeps as data).
    (define (datum-of s)
      (if (syntax? s) (syntax->datum s) s))

    ;; ------------------------------------------------------------------
    ;; Lexical environment: a stack of frames.  A frame is an alist
    ;; (name . index); frame 0 (the stack head) is the innermost lambda's
    ;; slots.  env-of / env-lookup mirror bytecode.scm's frame-envs /
    ;; resolve-var.
    ;;   env-lookup : env name -> (depth . index) or #f

    (define (env-lookup env name)
      (let loop ((es env) (d 0))
        (if (null? es)
          #f
          (let ((cell (assq name (car es))))
            (if cell
              (cons d (cdr cell))
              (loop (cdr es) (+ d 1)))))))

    ;; env-extend-frame : env (list name) -> env
    ;; Open a new innermost frame whose slots are the given names in order.
    (define (env-extend-frame env names)
      (let loop ((ns names) (i 0) (frame '()))
        (if (null? ns)
          (cons frame env)
          (loop (cdr ns) (+ i 1) (cons (cons (car ns) i) frame)))))

    ;; env-next-slot : env -> integer
    ;; The first free slot index of the innermost frame (one past its current
    ;; bindings' maximum index).
    (define (env-next-slot env)
      (if (null? env)
        0
        (let loop ((frame (car env)) (max-idx -1))
          (if (null? frame)
            (+ max-idx 1)
            (loop (cdr frame) (max max-idx (cdar frame)))))))

    ;; env-add-bindings : env (list (name . slot)) -> env
    ;; Append bindings to the innermost frame (let/letrec/define slots).
    (define (env-add-bindings env bindings)
      (if (null? env)
        env
        (cons (append (car env) bindings) (cdr env))))

    ;; ------------------------------------------------------------------
    ;; syntax->ir* : syntax ctx env (bool) -> ir
    ;; The recursive walk with a lexical environment.  A lexical reference
    ;; becomes a <lexical-ref> node (resolve-lexical? true) or keeps its
    ;; name symbol (false, for the s7-eval path where ir->core cannot
    ;; render depth/index); a primitive stays a <primitive-ref>; anything
    ;; else keeps its name symbol.

    ;; binding-name / binding-init : syntax -> datum / ir
    ;; A binding syntax (form (name init)) is a syntax record whose form is a
    ;; pair of syntax records; unwrap with syntax-form before car/cadr.
    (define (binding-name b)
      (datum-of (car (syntax-form b))))
    (define (binding-init b)
      (cadr (syntax-form b)))

    (define (syntax->ir* stx ctx env resolve-lexical?)
      (cond
        ((not (syntax? stx)) stx)
        (else
         (let ((form (syntax-form stx)))
           (cond
             ((not (pair? form))
              (if (symbol? form)
                (let*-values (((name kind) (resolve-name stx ctx)))
                  ;; A lexical name resolves to its allocated gensym; the
                  ;; binding is consumed at expansion time (resolve returns
                  ;; #f), so match the name against the lexical env FIRST.
                  (let ((loc (env-lookup env name)))
                    (cond
                      ((and loc resolve-lexical?)
                       (make-lexical-ref #f (car loc) (cdr loc)))
                      (loc
                       ;; lexical but not resolving addresses: keep the name
                       (if (eq? kind 'primitive)
                         (make-primitive-ref #f name)
                         name))
                      ((eq? kind 'primitive)
                       (make-primitive-ref #f name))
                      (else name))))
                form))
             (else
              (let ((head (car form))
                    (head-name (if (syntax? (car form)) (syntax-form (car form)) (car form))))
                (case head-name
                  ((quote) (make-const #f (datum-of (cadr form))))
                  ((quote-syntax) (make-const #f (datum-of (cadr form))))
                  ((define)
                   (if (symbol? (syntax-form (cadr form)))
                     (let* ((dname (datum-of (cadr form)))
                            (env1 (env-add-bindings env (list (cons dname 0)))))
                       (make-define #f dname
                                    (syntax->ir* (caddr form) ctx env1 resolve-lexical?)))
                     (let* ((df (syntax-form (cadr form)))
                            (dname (datum-of (car df)))
                            (dformals (map datum-of (cdr df)))
                            (env1 (env-extend-frame env dformals)))
                       (make-define #f dname
                                    (make-lambda #f dformals
                                                 (map (lambda (b) (syntax->ir* b ctx env1 resolve-lexical?))
                                                      (cddr form)))))))
                  ((lambda)
                   (let* ((dformals (map datum-of (syntax-form (cadr form))))
                          (env1 (env-extend-frame env dformals)))
                     (make-lambda #f dformals
                                  (map (lambda (b) (syntax->ir* b ctx env1 resolve-lexical?))
                                       (cddr form)))))
                  ((if)
                   (let ((else-stx (and (pair? (cdddr form)) (cadddr form))))
                     (make-if #f (syntax->ir* (cadr form) ctx env resolve-lexical?)
                              (syntax->ir* (caddr form) ctx env resolve-lexical?)
                              (if else-stx (syntax->ir* else-stx ctx env resolve-lexical?) #f))))
                  ((begin)
                   (make-begin #f (map (lambda (b) (syntax->ir* b ctx env resolve-lexical?))
                                       (cdr form))))
                  ((let)
                   (if (symbol? (syntax-form (cadr form)))
                     ;; named let: (let name ((v i) ...) body ...) becomes
                     ;; (letrec ((name (lambda (v ...) body ...))) (name i ...))
                     (let* ((name (datum-of (cadr form)))
                            (bindings (syntax-form (caddr form)))
                            (body (cdddr form))
                            (bnames (map binding-name bindings))
                            (env-b (env-extend-frame env bnames)))
                       (make-letrec 'letrec
                                    (list (list name
                                                (make-lambda #f bnames
                                                             (map (lambda (b) (syntax->ir* b ctx env-b resolve-lexical?))
                                                                  body))))
                                    (list (make-call #f name
                                                     (map (lambda (b) (syntax->ir* (binding-init b) ctx env resolve-lexical?))
                                                          bindings)))))
                     (let* ((bindings (syntax-form (cadr form)))
                            (bnames (map binding-name bindings))
                            (slot-alist (let loop ((ns bnames) (i (env-next-slot env)) (acc '()))
                                          (if (null? ns)
                                            acc
                                            (loop (cdr ns) (+ i 1)
                                                  (cons (cons (car ns) i) acc)))))
                            (env1 (env-add-bindings env (reverse slot-alist))))
                       (make-let #f
                                 (map (lambda (b) (list (binding-name b)
                                                        (syntax->ir* (binding-init b) ctx env resolve-lexical?)))
                                      bindings)
                                 (map (lambda (b) (syntax->ir* b ctx env1 resolve-lexical?))
                                      (cddr form))))))
                   ((letrec letrec*)
                    (let* ((bindings (syntax-form (cadr form)))
                           (bnames (map binding-name bindings))
                           (slot-alist (let loop ((ns bnames) (i (env-next-slot env)) (acc '()))
                                         (if (null? ns)
                                           acc
                                           (loop (cdr ns) (+ i 1)
                                                 (cons (cons (car ns) i) acc)))))
                           (env1 (env-add-bindings env (reverse slot-alist))))
                     (make-letrec head-name
                                  (map (lambda (b) (list (binding-name b)
                                                         (syntax->ir* (binding-init b) ctx env1 resolve-lexical?)))
                                       bindings)
                                  (map (lambda (b) (syntax->ir* b ctx env1 resolve-lexical?))
                                       (cddr form)))))
                  ((set!)
                   (let*-values (((name kind) (resolve-name (cadr form) ctx)))
                     (let ((loc (and resolve-lexical? (env-lookup env name))))
                       (make-set! #f
                                  (cond
                                    (loc (make-lexical-ref #f (car loc) (cdr loc)))
                                    ((eq? kind 'primitive) (make-primitive-ref #f name))
                                    (else name))
                                  (syntax->ir* (caddr form) ctx env resolve-lexical?)))))
                  ((values)
                   (make-values #f (map (lambda (b) (syntax->ir* b ctx env resolve-lexical?))
                                        (cdr form))))
                  ((call-with-values)
                   (make-call-with-values #f (syntax->ir* (cadr form) ctx env resolve-lexical?)
                                          (syntax->ir* (caddr form) ctx env resolve-lexical?)))
                  (else
                   (make-call #f (syntax->ir* head ctx env resolve-lexical?)
                              (map (lambda (a) (syntax->ir* a ctx env resolve-lexical?))
                                   (cdr form))))))))))))

    ;; syntax->ir : syntax ctx -> ir
    ;; Convert a fully-expanded syntax object into an IR record tree.
    ;; The lexical environment starts empty (no enclosing frame).
    ;; Lexical references become <lexical-ref> nodes (bytecode path).
    (define (syntax->ir stx ctx)
      (syntax->ir* stx ctx '() #t))

    ;; syntax->ir/sexp : syntax ctx -> ir
    ;; Like syntax->ir but keeps lexical references as name symbols: for the
    ;; s7-eval path, where the passes output must survive ir->core (a
    ;; <lexical-ref> carries depth/index that has no datum rendering).
    (define (syntax->ir/sexp stx ctx)
      (syntax->ir* stx ctx '() #f))

    ;; expand->ir : datum -> ir
    ;; Expand a datum expression in the base library and convert it to IR.
    (define (expand->ir expr)
      (let*-values (((stx ctx) (expand-expr (wrap-expression expr) (initial-context))))
        (syntax->ir stx ctx)))

    ;; compile-syntax-defs : (list syntax) context (list pass) -> (list sexp)
    ;; The library-def pipeline: expander output (syntax defs) -> IR ->
    ;; passes -> lowered core sexp.  This replaces the
    ;; (lower defs) -> (map core->ir defs) boundary in compile-defs-on-load:
    ;; syntax->ir keeps the binding-kind information (primitive references
    ;; stay <primitive-ref> nodes through the passes), which core->ir cannot
    ;; (it sees only lowered bare symbols).
    (define (compile-syntax-defs defs ctx passes)
      ;; Explicit recursion (not `map') so this library does not depend on
      ;; the map binding during bootstrap, when map may resolve to a
      ;; not-yet-installed Scheme definition.
      (let rec ((ds defs) (acc '()))
        (if (null? ds)
          (reverse acc)
          (rec (cdr ds)
               (cons (ir->core (run-passes (syntax->ir/sexp (car ds) ctx) passes))
                     acc)))))

    ;; compile-syntax-program : (list syntax) context (list pass) -> program
    ;; The library-def pipeline to BYTECODE: expander output (syntax defs)
    ;; -> IR (with binding-kind + lexical addressing) -> passes -> a VM
    ;; bytecode program.  This is the VM-loading counterpart of
    ;; compile-syntax-defs (which lowers to sexp for s7 eval); vm-loading it
    ;; keeps the library definitions executing on our VM instead of s7.
    (define (compile-syntax-program defs ctx passes)
      (to-bytecode (map (lambda (d) (syntax->ir d ctx)) defs)))

    ;; vm-load-syntax-defs : (list syntax) context (list pass) global-env -> irs
    ;; Compile the defs to a VM program and load it, storing each top-level
    ;; define into global-env (e.g. the-expander-library).  Returns the IR
    ;; list so callers can map gensym names (define-name) to values.
    (define (vm-load-syntax-defs defs ctx passes global-env)
      (let ((irs (map (lambda (d) (syntax->ir d ctx)) defs)))
        (vm-load (to-bytecode irs) global-env)
        irs))))
