;;; passes.scm -- L2: nanopass-style IR passes.
;;;
;;; The compiler passes rewritten against the record IR (goldfish/compiler/ir.scm)
;;; using (goldfish match) patterns ($const/$lambda/...).  A pass is a
;;; function ir -> ir that rewrites the tree; run-passes applies a list of
;;; passes in order.  The output is either converted back to core sexp for
;;; the s7-eval path (ir->core) or fed to the bytecode compiler.
;;;
;;; This library is the nanopass home of the pass pipeline that
;;; compiler.scm (the s7-eval / load-path integration) consumes.

(define-library (goldfish compiler passes)
  (import (scheme base)
          (goldfish match)
          (goldfish compiler ir))
  (export run-passes
    compile-defs
    constant-fold
    simplify-if
    inline
    *inline-max-effort*
    *inline-max-depth*
    eliminate-dead-defs
    tail-call-positions
    collect-free-symbols
    lambda-valued-def?
    *foldable-functions*)
  (begin

    ;; fold-left : (a b -> a) a (list b) -> a
    ;; R7RS (scheme base) has no fold-left; implement the left fold here.
    (define (fold-left f acc ls)
      (if (null? ls)
        acc
        (fold-left f (f acc (car ls)) (cdr ls))))

    (define (run-passes ir passes)
      (fold-left (lambda (s pass) (pass s)) ir passes))

    ;; compile-defs : (list sexp) (list pass) -> (list sexp)
    ;; Apply the pipeline to each def of an expanded library body.  The
    ;; boundary conversion keeps this interface sexp -> sexp (the load
    ;; path evaluates the result with the host s7); passes themselves
    ;; operate on the record IR.
    (define (compile-defs defs passes)
      (map (lambda (d) (ir->core (run-passes (core->ir d) passes))) defs))

    ;; ------------------------------------------------------------------
    ;; Foldable primitive table.  Only total, side-effect-free functions
    ;; with self-evaluating results belong here: folding a call must never
    ;; turn a runtime error into a compile-time one, nor change observable
    ;; behavior.  `catch' guards the fold itself (e.g. (expt 2 100000)
    ;; overflows), leaving the call untouched on any error.

    (define *foldable-functions*
      (list (cons '+ +)
            (cons '- -)
            (cons '* *)
            (cons 'quotient quotient)
            (cons 'remainder remainder)
            (cons 'modulo modulo)
            (cons 'abs abs)
            (cons 'min min)
            (cons 'max max)
            (cons 'expt expt)
            (cons '= =)
            (cons '< <)
            (cons '> >)
            (cons '<= <=)
            (cons '>= >=)
            (cons 'string-length string-length)
            (cons 'string-append string-append)
            (cons 'string->symbol string->symbol)
            (cons 'symbol->string symbol->string)
            (cons 'string->number string->number)
            (cons 'number->string number->string)
            (cons 'char->integer char->integer)
            (cons 'integer->char integer->char)
            (cons 'not not)
            (cons 'boolean? boolean?)
            (cons 'eq? eq?)
            (cons 'eqv? eqv?)))

    ;; self-evaluating? : any -> boolean
    ;; Literals that may appear bare in core IR (no quote wrapper).
    (define (self-evaluating? v)
      (or (number? v)
          (string? v)
          (char? v)
          (boolean? v)
          (null? v)
          (eof-object? v)))

    ;; try-fold-call-ir : proc (list ir) -> ir or #f
    ;; Fold (proc arg...) when proc is a primitive reference whose name is
    ;; in *foldable-functions*, every argument is a constant (a const
    ;; record or a self-evaluating atom), and the application succeeds.
    ;; Returns a const IR node, or #f to leave the call untouched.  A
    ;; plain symbol proc (a non-primitive name, e.g. a toplevel function)
    ;; is never folded.
    (define (try-fold-call-ir proc args)
      (and (or (symbol? proc) (primitive-ref? proc))
           (let ((p (if (primitive-ref? proc) (primitive-ref-name proc) proc)))
             (and (symbol? p)
                  (let ((entry (assq p *foldable-functions*)))
                    (and entry
                         (let loop ((as args) (values '()))
                           (if (null? as)
                             (catch
                               #t
                               (lambda ()
                                 (let ((result (apply (cdr entry) (reverse values))))
                                   (make-const #f result)))
                               (lambda (tag . info) #f))
                             (let ((a (car as)))
                               (cond
                                 ((const? a)
                                  (loop (cdr as) (cons (const-value a) values)))
                                 ((self-evaluating? a)
                                  (loop (cdr as) (cons a values)))
                                 (else #f)))))))))))

    ;; constant-fold : ir -> ir
    ;; Fold primitive calls whose arguments are all constants, recursively
    ;; through the IR tree.  const contents are data and never entered.
    (define (constant-fold ir)
      (match ir
        (($const v) ir)
        (($define name value)
         (make-define #f name (constant-fold value)))
        (($lambda formals body ...)
         (make-lambda #f formals (map constant-fold body)))
        (($if test then else)
         (make-if #f (constant-fold test) (constant-fold then)
                  (if else (constant-fold else) #f)))
        (($begin body ...)
         (make-begin #f (map constant-fold body)))
        (($let bindings body ...)
         (make-let #f
                   (map (lambda (b) (list (car b) (constant-fold (cadr b))))
                        bindings)
                   (map constant-fold body)))
        (($letrec src bindings body ...)
         (make-letrec src
                      (map (lambda (b) (list (car b) (constant-fold (cadr b))))
                           bindings)
                      (map constant-fold body)))
        (($set! name expr)
         (make-set! #f name (constant-fold expr)))
        (($values args ...)
         (make-values #f (map constant-fold args)))
        (($call-with-values p c)
         (make-call-with-values #f (constant-fold p) (constant-fold c)))
        (($call proc args ...)
         (let ((p (constant-fold proc))
               (as (map constant-fold args)))
           (or (try-fold-call-ir p as)
               (make-call #f p as))))
        (($primitive-ref name)
         (make-primitive-ref #f name))
        ((? symbol? s) s)
        (_ ir)))

    ;; simplify-if : ir -> ir
    ;; Resolve if whose test folds to a known boolean constant:
    ;; (if #t t e) -> t, (if #f t e) -> e.  A dead branch disappears, so
    ;; this is also the first dead-code elimination.  An if with NO else
    ;; arm whose test is #f is left as-is: R7RS says (if #f t) returns an
    ;; unspecified value, NOT #f, so it cannot be folded to #f.
    (define (const-boolean? ir val)
      (or (eq? ir val)
          (and (const? ir) (eq? (const-value ir) val))))

    (define (simplify-if ir)
      (match ir
        (($const v) ir)
        (($define name value)
         (make-define #f name (simplify-if value)))
        (($lambda formals body ...)
         (make-lambda #f formals (map simplify-if body)))
        (($if test then else)
         (let* ((t (simplify-if test))
                (th (simplify-if then))
                (el (if else (simplify-if else) #f)))
           (cond
             ((const-boolean? t #t) th)
             ((const-boolean? t #f)
              (if else el (make-if #f t th #f)))
             (else (make-if #f t th el)))))
        (($begin body ...)
         (make-begin #f (map simplify-if body)))
        (($let bindings body ...)
         (make-let #f
                   (map (lambda (b) (list (car b) (simplify-if (cadr b))))
                        bindings)
                   (map simplify-if body)))
        (($letrec src bindings body ...)
         (make-letrec src
                      (map (lambda (b) (list (car b) (simplify-if (cadr b))))
                           bindings)
                      (map simplify-if body)))
        (($set! name expr)
         (make-set! #f name (simplify-if expr)))
        (($values args ...)
         (make-values #f (map simplify-if args)))
        (($call-with-values p c)
         (make-call-with-values #f (simplify-if p) (simplify-if c)))
        (($call proc args ...)
         (make-call #f (simplify-if proc) (map simplify-if args)))
        ((? symbol? s) s)
        (_ ir)))

    ;; ------------------------------------------------------------------
    ;; inline (L2-2): the peval core -- copy propagation + beta reduction
    ;; (IR version).
    ;;
    ;; Rewrites the IR by beta-reducing applications of lambda literals
    ;; and copy-propagating safe lexical bindings: const nodes and lambda
    ;; closures.  These are immutable, side-effect-free values, so
    ;; duplicating them at every reference is observationally equivalent.
    ;; A binding whose name is assigned (set!) in scope, and any binding
    ;; not statically known, is left alone.
    ;;
    ;; Cost control follows Waddell & Dybvig's effort counter: one budget
    ;; per pass bounds both the total inlining work and the nesting depth.
    ;; When it is exhausted the pass residualizes applications instead of
    ;; inlining them, keeping the rewrite O(N) and bounding code growth.
    ;; Propagated bindings are kept in their let/letrec, so a reference
    ;; left behind by a depth cut is still bound.

    (define (lambda-formals->list formals)
      (if (symbol? formals)
        (list formals)
        (let loop ((f formals) (acc '()))
          (cond ((null? f) (reverse acc))
                ((pair? f) (loop (cdr f) (cons (car f) acc)))
                (else (reverse (cons f acc)))))))

    ;; ir-children : ir -> (list ir)
    ;; The direct child expressions of a node (not atoms like formals or
    ;; binding names).  A plain list (a lambda/let body, a call's args)
    ;; yields its elements.
    (define (ir-children ir)
      (cond
        ((lambda? ir) (lambda-body ir))
        ((define? ir) (list (define-value ir)))
        ((if? ir) (if (if-else ir)
                    (list (if-test ir) (if-then ir) (if-else ir))
                    (list (if-test ir) (if-then ir))))
        ((begin? ir) (begin-body ir))
        ((let? ir) (append (map cadr (let-bindings ir)) (let-body ir)))
        ((letrec? ir) (append (map cadr (letrec-bindings ir)) (letrec-body ir)))
        ((set!? ir) (list (set!-expr ir)))
        ((values? ir) (values-args ir))
        ((call-with-values? ir) (list (cwv-producer ir) (cwv-consumer ir)))
        ((call? ir) (cons (call-proc ir) (call-args ir)))
        ((pair? ir) ir)
        (else '())))

    ;; collect-assigned : ir -> (list symbol)
    ;; Names assigned by set! anywhere in the form (not entering const
    ;; data).  An assigned name is a mutable binding: copy propagation of
    ;; its initializer would miss later writes, so it is never propagated.
    (define (collect-assigned ir)
      (let loop ((s ir) (acc '()))
        (cond
          ((symbol? s) acc)
          ((or (const? s) (void? s)) acc)
          ((set!? s)
           (loop (set!-expr s)
                 (if (member (set!-target s) acc)
                   acc
                   (cons (set!-target s) acc))))
          ((lambda? s)
           (let loop2 ((bs (lambda-body s)) (acc acc))
             (if (null? bs)
               acc
               (loop2 (cdr bs) (loop (car bs) acc)))))
          ((define? s)
           (let ((val (define-value s)))
             (if (or (const? val) (void? val))
               acc
               (loop val acc))))
          (else
           (let loop2 ((cs (ir-children s)) (acc acc))
             (if (null? cs)
               acc
               (loop2 (cdr cs) (loop (car cs) acc))))))))

    ;; safe-inline-value? : ir -> boolean
    ;; A value that may be freely duplicated by copy propagation.  Only
    ;; const nodes, lambda closures, and plain self-evaluating atoms
    ;; (records are vectors, so they are excluded here).
    (define (safe-inline-value? v)
      (or (const? v)
          (lambda? v)
          (and (not (pair? v))
               (not (symbol? v))
               (not (vector? v)))))

    ;; The effort budget: a pair (remaining-effort . remaining-depth).
    (define (make-inline-budget effort depth) (list effort depth))
    (define (inline-budget-spent? b) (or (<= (car b) 0) (<= (cadr b) 0)))
    (define (inline-spend-effort! b n)
      (set-car! b (- (car b) n)))
    (define (inline-deepen! b)
      (set-car! (cdr b) (- (cadr b) 1)))

    ;; env helpers: association list of (name . value); a value of the
    ;; sentinel *inline-var* means the name is a variable (resolves to its
    ;; runtime binding, not propagated).  Any other value is a statically
    ;; known safe value to copy-propagate.
    (define *inline-var* (list 'inline-var))

    ;; lambda-self-referential? : symbol ir -> boolean
    ;; True when a lambda value refers to its own binding name anywhere in
    ;; its body (a recursive function).  Propagating such a lambda would
    ;; inline its own recursive calls -- unrolling the recursion until the
    ;; effort budget cuts it off, bloating the residual code.  Leave
    ;; recursive functions as variables.
    (define (lambda-self-referential? name lambda-ir)
      (member name (collect-residual-free (lambda-body lambda-ir))))

    (define (env-extend-vars env names)
      (fold-left (lambda (e n) (cons (cons n *inline-var*) e)) env names))
    (define (env-extend-safes env bindings assigned)
      (fold-left (lambda (e b)
                   (cons (cons (car b)
                               (if (or (member (car b) assigned)
                                       (not (safe-inline-value? (cadr b)))
                                       (and (lambda? (cadr b))
                                            (lambda-self-referential?
                                             (car b) (cadr b))))
                                 *inline-var*
                                 (cadr b)))
                         e))
                 env bindings))

    ;; beta-bindings : formals (list ir) -> (list (name value)) or #f
    ;; Pair the lambda formals with the argument expressions.  With a
    ;; rest formal the remaining args become a (list ...) construction
    ;; (retained as a binding, never propagated, since it allocates).
    ;; Mismatched arity returns #f: leave the call alone.
    (define (beta-bindings formals args)
      (cond
        ((symbol? formals)
         (if (= (length args) 1) (list (list formals (car args))) #f))
        (else
         (let loop ((fs formals) (as args) (acc '()))
           (cond
             ((null? fs) (if (null? as) (reverse acc) #f))
             ((symbol? fs)
              (reverse (cons (list fs (if (null? as)
                                        (make-const #f '())
                                        (make-call #f 'list as)))
                             acc)))
             ((null? as) #f)
             (else (loop (cdr fs) (cdr as) (cons (list (car fs) (car as)) acc))))))))

    ;; collect-residual-free : ir -> (list symbol)
    ;; Free symbols of an inlined body, ENTERING lambda/let bodies (minus
    ;; their bindings) but not const data.  Used to decide which let
    ;; bindings are still referenced after copy propagation.
    (define (collect-residual-free ir)
      (let loop ((s ir) (acc '()))
        (cond
          ((symbol? s) (if (member s acc) acc (cons s acc)))
          ((or (const? s) (void? s)) acc)
          ((lambda? s)
           (let ((bound (lambda-formals->list (lambda-formals s))))
             (filter (lambda (x) (not (member x bound)))
                     (let loop2 ((bs (lambda-body s)) (acc acc))
                       (if (null? bs)
                         acc
                         (loop2 (cdr bs) (loop (car bs) acc)))))))
          ((let? s)
           (let ((bound (map car (let-bindings s))))
             (filter (lambda (x) (not (member x bound)))
                     (let loop2 ((bs (let-bindings s))
                                 (acc (let loop3 ((bd (let-body s)) (acc acc))
                                        (if (null? bd)
                                          acc
                                          (loop3 (cdr bd) (loop (car bd) acc))))))
                       (if (null? bs)
                         acc
                         (loop2 (cdr bs) (loop (cadr (car bs)) acc)))))))
          ((letrec? s)
           (let ((bound (map car (letrec-bindings s))))
             (filter (lambda (x) (not (member x bound)))
                     (let loop2 ((bs (letrec-bindings s))
                                 (acc (let loop3 ((bd (letrec-body s)) (acc acc))
                                        (if (null? bd)
                                          acc
                                          (loop3 (cdr bd) (loop (car bd) acc))))))
                       (if (null? bs)
                         acc
                         (loop2 (cdr bs) (loop (cadr (car bs)) acc)))))))
          ((define? s)
           (let ((val (define-value s)))
             (if (or (const? val) (void? val))
               acc
               (loop val acc))))
          (else
           (let loop2 ((cs (ir-children s)) (acc acc))
             (if (null? cs)
               acc
               (loop2 (cdr cs) (loop (car cs) acc))))))))

    ;; prune-let-bindings : head (list binding) (list ir) -> ir
    ;; After inlining the body, drop bindings whose name is no longer
    ;; referenced anywhere (binding values and body).  When none survive
    ;; the let collapses to its body.
    (define (pure-ir? ir)
      (cond
        ((or (const? ir) (void? ir) (primitive-ref? ir) (symbol? ir)) #t)
        ((lambda? ir) #t)
        ((begin? ir)
         (let loop ((es (begin-body ir)))
           (if (null? es) #t (and (pure-ir? (car es)) (loop (cdr es))))))
        ((if? ir) (and (pure-ir? (if-test ir))
                       (pure-ir? (if-then ir))
                       (or (not (if-else ir)) (pure-ir? (if-else ir)))))
        ((values? ir)
         (let loop ((es (values-args ir)))
           (if (null? es) #t (and (pure-ir? (car es)) (loop (cdr es))))))
        (else #f)))

    (define (prune-let-bindings head src new-bindings body-inl)
      (let* ((free (collect-residual-free
                    (append (map (lambda (b) (cadr b)) new-bindings) body-inl)))
             (survivors (filter (lambda (b)
                                  (or (member (car b) free)
                                      (not (pure-ir? (cadr b)))))
                                new-bindings)))
        (if (null? survivors)
          (if (null? (cdr body-inl)) (car body-inl) (make-begin #f body-inl))
          (if (eq? head 'let)
            (make-let #f survivors body-inl)
            (make-letrec src survivors body-inl)))))

    (define (inline-walk ir env budget)
      (if (inline-budget-spent? budget)
        ir
        (cond
          ((symbol? ir)
           (let ((v (assq ir env)))
             (if (and v (not (eq? (cdr v) *inline-var*)))
               (begin (inline-spend-effort! budget 1) (cdr v))
               ir)))
          ((or (const? ir) (void? ir)) ir)
          ((lambda? ir)
           (let* ((formals (lambda-formals ir))
                  (env1 (env-extend-vars env (lambda-formals->list formals))))
             (make-lambda #f formals
                          (map (lambda (e) (inline-walk e env1 budget))
                               (lambda-body ir)))))
          ((define? ir)
           (make-define #f (define-name ir)
                        (inline-walk (define-value ir) env budget)))
          ((if? ir)
           (make-if #f (inline-walk (if-test ir) env budget)
                    (inline-walk (if-then ir) env budget)
                    (if (if-else ir) (inline-walk (if-else ir) env budget) #f)))
          ((begin? ir)
           (make-begin #f (map (lambda (e) (inline-walk e env budget))
                               (begin-body ir))))
          ((set!? ir)
           (make-set! #f (set!-target ir) (inline-walk (set!-expr ir) env budget)))
          ((let? ir)
           (let* ((bindings (let-bindings ir))
                  (new-bindings
                   (map (lambda (b) (list (car b) (inline-walk (cadr b) env budget)))
                        bindings))
                  (env1 (env-extend-safes env new-bindings
                                          (collect-assigned (let-body ir))))
                  (body-inl (map (lambda (e) (inline-walk e env1 budget))
                                 (let-body ir))))
             (inline-spend-effort! budget 1)
             (prune-let-bindings 'let #f new-bindings body-inl)))
          ((letrec? ir)
           (let* ((bindings (letrec-bindings ir))
                  (names (map car bindings))
                  (env0 (env-extend-vars env names))
                  (assigned (collect-assigned (letrec-body ir)))
                  (new-bindings
                   (map (lambda (b) (list (car b) (inline-walk (cadr b) env0 budget)))
                        bindings))
                  (env1 (env-extend-safes env new-bindings assigned))
                  (body-inl (map (lambda (e) (inline-walk e env1 budget))
                                 (letrec-body ir))))
              (inline-spend-effort! budget 1)
              (prune-let-bindings 'letrec (letrec-source ir) new-bindings body-inl)))
          ((values? ir)
           (make-values #f (map (lambda (e) (inline-walk e env budget))
                                (values-args ir))))
          ((call-with-values? ir)
           (make-call-with-values #f (inline-walk (cwv-producer ir) env budget)
                                  (inline-walk (cwv-consumer ir) env budget)))
          ((call? ir)
           (let ((f (inline-walk (call-proc ir) env budget))
                 (args (map (lambda (a) (inline-walk a env budget))
                            (call-args ir))))
             (if (and (lambda? f) (not (inline-budget-spent? budget)))
               (let ((bindings (beta-bindings (lambda-formals f) args)))
                 (if bindings
                   (let ((let-form (make-let #f bindings (lambda-body f))))
                     (inline-spend-effort! budget 2)
                     (inline-deepen! budget)
                     (inline-walk let-form env budget))
                   (make-call #f f args)))
               (make-call #f f args))))
          (else ir))))

    (define *inline-max-effort* 40000)
    (define *inline-max-depth* 16)

    ;; inline : ir -> ir
    (define (inline ir)
      (inline-walk ir '() (make-inline-budget *inline-max-effort* *inline-max-depth*)))

    ;; ------------------------------------------------------------------
    ;; Tail-call position analysis (L2-3 backend prerequisite, IR version).
    ;;
    ;; Tail positions in core IR:
    ;;   (lambda (formals) body ...)   last body expression
    ;;   (if test then else)           then and else
    ;;   (begin e ...)                 last expression
    ;;   (let/letrec/letrec* bs body)  last body expression
    ;;   (set! name e)                 e
    ;;   (values e ...)                no (multi-value return)
    ;;   (call-with-values p c)        consumer c is invoked in tail
    ;;                                 position of the whole form
    ;;
    ;; tail-call-positions : ir -> ir
    ;; Mark every subexpression that sits in a tail position by wrapping
    ;; it as (tail-call <ir>).  The wrapper is the analysis product:
    ;; backends consume it to emit jumps instead of push-calls.  The
    ;; returned IR is NOT for direct evaluation.

    (define (tail-call-positions ir)
      (define (mark-body body)
        (if (null? body)
          '()
          (let* ((rev (reverse body))
                 (last (mark-tail (car rev))))
            (append (reverse (cdr rev)) (list last)))))
      (define (mark-tail s)
        (cond
          ((or (const? s) (void? s)) s)
          ((symbol? s) s)
          ((lambda? s)
           (make-lambda #f (lambda-formals s) (mark-body (lambda-body s))))
          ((if? s)
           (make-if #f (if-test s) (mark-tail (if-then s))
                    (if (if-else s) (mark-tail (if-else s)) #f)))
          ((begin? s)
           (make-begin #f (mark-body (begin-body s))))
          ((let? s)
           (make-let #f (let-bindings s) (mark-body (let-body s))))
          ((letrec? s)
           (make-letrec (letrec-source s) (letrec-bindings s) (mark-body (letrec-body s))))
          ((set!? s)
           (make-set! #f (set!-target s) (mark-tail (set!-expr s))))
          ((call-with-values? s)
           ;; consumer is invoked in tail position
           (make-call-with-values #f (cwv-producer s) (mark-tail (cwv-consumer s))))
          ((values? s) s)
          (else
           ;; a bare application in tail position: wrap it
           (list 'tail-call s))))
      (mark-tail ir))

    ;; ------------------------------------------------------------------
    ;; Dead code elimination at the defs level (IR version).
    ;;
    ;; eliminate-dead-defs : (list ir) -> (list ir)
    ;; Drop top-level (define name value) defs whose name is never
    ;; referenced by any surviving def or by the registration/other forms
    ;; (directly or transitively).  Only lambda-valued defs are
    ;; candidates: a non-lambda value (constant, call) may have side
    ;; effects at definition time and is always kept.  Iterates to a
    ;; fixpoint because deleting one def can make another unreferenced.

    ;; collect-free-symbols : ir -> (list symbol)
    ;; Free symbols of an expression: identifiers in operator and operand
    ;; positions, not counting lambda formals / let bindings (bound), and
    ;; not entering const data.
    (define (collect-free-symbols ir)
      (let loop ((s ir) (acc '()))
        (cond
          ((symbol? s) (if (member s acc) acc (cons s acc)))
          ((or (const? s) (void? s)) acc)
          ((lambda? s)
           (let* ((bound (lambda-formals->list (lambda-formals s))))
             (let loop2 ((body (lambda-body s)) (acc acc))
               (if (null? body)
                 (filter (lambda (x) (not (member x bound))) acc)
                 (loop2 (cdr body) (loop (car body) acc))))))
          ((let? s)
           (let ((bound (map car (let-bindings s))))
             (let loop2 ((bs (let-bindings s)) (acc acc))
               (if (null? bs)
                 (let loop3 ((body (let-body s)) (acc acc))
                   (if (null? body)
                     (filter (lambda (x) (not (member x bound))) acc)
                     (loop3 (cdr body) (loop (car body) acc))))
                 (loop2 (cdr bs) (loop (cadr (car bs)) acc))))))
          ((letrec? s)
           (let ((bound (map car (letrec-bindings s))))
             (let loop2 ((bs (letrec-bindings s)) (acc acc))
               (if (null? bs)
                 (let loop3 ((body (letrec-body s)) (acc acc))
                   (if (null? body)
                     (filter (lambda (x) (not (member x bound))) acc)
                     (loop3 (cdr body) (loop (car body) acc))))
                 (loop2 (cdr bs) (loop (cadr (car bs)) acc))))))
          ((define? s)
           ;; (define name value): collect from the value only; the name
           ;; is bound by this definition.
           (let ((val (define-value s)))
             (if (or (const? val) (void? val))
               acc
               (loop val acc))))
          (else
           (let loop2 ((cs (ir-children s)) (acc acc))
             (if (null? cs)
               acc
               (loop2 (cdr cs) (loop (car cs) acc))))))))

    ;; lambda-valued-def? : ir -> boolean
    ;; A def whose value is a plain lambda -- a safe DCE candidate (no
    ;; definition-time side effect).
    (define (lambda-valued-def? d)
      (and (define? d) (lambda? (define-value d))))

    (define (collect-all-free defs)
      (let loop ((ds defs) (acc '()))
        (if (null? ds)
          acc
          (loop (cdr ds)
                (append (collect-free-symbols (car ds)) acc)))))

    (define (eliminate-dead-defs defs)
      (let loop ((current defs))
        (let* ((alive (collect-all-free current))
               (survivors (filter (lambda (d)
                                    (or (not (lambda-valued-def? d))
                                        (member (define-name d) alive)))
                                  current)))
          (if (equal? survivors current)
            survivors
            (loop survivors)))))

    )) ;begin
