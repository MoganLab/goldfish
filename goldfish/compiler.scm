;;; compiler.scm -- L2-1: a self-hosted compilation pass pipeline.
;;;
;;; Input: the expander's output, a lowered core-lambda S-expression
;;; (`lower' in expander/kernel/expand.scm): define/lambda/if/begin/set!/
;;; call-with-values/values/quote plus top-level primitive calls.  The
;;; pass pipeline rewrites this IR in place; the output is still a
;;; core-lambda form that the host s7 evaluator runs unchanged.  This
;;; stage validates the "self-compile" path (our passes process real
;;; expander output) without yet replacing the evaluator -- later stages
;;; (L2 bytecode VM / compile-to-C) reuse the same pipeline and swap only
;;; the backend.
;;;
;;; A pass is a pure function sexp -> sexp.  run-passes applies a list of
;;; passes in order.

(define-library (goldfish compiler)
  (import (scheme base))
  (export run-passes
    compile-defs
    constant-fold
    simplify-if
    inline
    *inline-max-effort*
    *inline-max-depth*
    eliminate-dead-defs
    tail-call-positions
    *foldable-functions*)
  (begin

    ;; ------------------------------------------------------------------
    ;; Pass pipeline

    ;; fold-left : (a b -> a) a (list b) -> a
    ;; R7RS (scheme base) has no fold-left; implement the left fold here.
    (define (fold-left f acc ls)
      (if (null? ls)
        acc
        (fold-left f (f acc (car ls)) (cdr ls))))

    (define (run-passes sexp passes)
      (fold-left (lambda (s pass) (pass s)) sexp passes))

    ;; compile-defs : (list sexp) (list pass) -> (list sexp)
    ;; Apply the pipeline to each def of an expanded library body.
    (define (compile-defs defs passes)
      (map (lambda (d) (run-passes d passes)) defs))

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

    ;; fold-result->sexp : any -> sexp
    ;; A folded value must be lowered back to IR.  Self-evaluating values
    ;; appear bare; anything else (a symbol, pair, vector) needs a quote.
    (define (fold-result->sexp v)
      (if (self-evaluating? v) v (list 'quote v)))

    ;; try-fold-call : symbol (list sexp) -> sexp or #f
    ;; Fold (head arg... ) when head is in *foldable-functions*, every
    ;; argument is a constant, and the application succeeds.  Returns the
    ;; folded sexp, or #f to leave the call untouched.
    (define (try-fold-call head args)
      (let ((entry (assq head *foldable-functions*)))
        (if entry
          (let loop ((as args) (values '()))
            (if (null? as)
              (catch
                #t
                (lambda ()
                  (let ((result (apply (cdr entry) (reverse values))))
                    (if (self-evaluating? result)
                      result
                      (list 'quote result))))
                (lambda (tag . info) #f))
              (let ((a (car as)))
                (cond
                  ((self-evaluating? a)
                   (loop (cdr as) (cons a values)))
                  ((and (pair? a) (eq? (car a) 'quote))
                   (loop (cdr as) (cons (cadr a) values)))
                  (else #f)))))
          #f)))

    ;; constant-fold : sexp -> sexp
    ;; Fold primitive calls whose arguments are all constants, recursively
    ;; through the core forms.  quote/quote-syntax contents are data and
    ;; never entered.
    (define (constant-fold sexp)
      (if (pair? sexp)
        (let ((head (car sexp)))
          (cond
            ((eq? head 'quote) sexp)
            ((eq? head 'quote-syntax) sexp)
            ((eq? head 'lambda)
             (cons 'lambda
                   (cons (cadr sexp)
                         (map constant-fold (cddr sexp)))))
            ((eq? head 'define)
             (cons 'define
                   (cons (cadr sexp)
                         (map constant-fold (cddr sexp)))))
            ((eq? head 'begin)
             (cons 'begin (map constant-fold (cdr sexp))))
            ((eq? head 'if)
             (cons 'if (map constant-fold (cdr sexp))))
            ((eq? head 'set!)
             (cons 'set!
                   (cons (cadr sexp)
                         (map constant-fold (cddr sexp)))))
            ((eq? head 'let)
             (cons 'let
                   (cons (map (lambda (b)
                                (list (car b) (constant-fold (cadr b))))
                              (cadr sexp))
                         (map constant-fold (cddr sexp)))))
            ((eq? head 'letrec)
             (cons 'letrec
                   (cons (map (lambda (b)
                                (list (car b) (constant-fold (cadr b))))
                              (cadr sexp))
                         (map constant-fold (cddr sexp)))))
            ((eq? head 'letrec*)
             (cons 'letrec*
                   (cons (map (lambda (b)
                                (list (car b) (constant-fold (cadr b))))
                              (cadr sexp))
                         (map constant-fold (cddr sexp)))))
            ((eq? head 'call-with-values)
             (cons 'call-with-values
                   (map constant-fold (cdr sexp))))
            ((eq? head 'values)
             (cons 'values (map constant-fold (cdr sexp))))
            (else
             (let ((folded (map constant-fold (cdr sexp))))
               (or (try-fold-call head folded)
                   (cons head folded))))))
        sexp))

    ;; simplify-if : sexp -> sexp
    ;; Resolve if whose test is a known boolean literal: (if #t t e) -> t,
    ;; (if #f t e) -> e.  A dead branch disappears, so this is also the
    ;; first dead-code elimination.
    (define (simplify-if sexp)
      (if (pair? sexp)
        (let ((head (car sexp)))
          (cond
            ((eq? head 'quote) sexp)
            ((eq? head 'quote-syntax) sexp)
            ((eq? head 'lambda)
             (cons 'lambda
                   (cons (cadr sexp)
                         (map simplify-if (cddr sexp)))))
            ((eq? head 'define)
             (cons 'define
                   (cons (cadr sexp)
                         (map simplify-if (cddr sexp)))))
            ((eq? head 'begin)
             (cons 'begin (map simplify-if (cdr sexp))))
            ((eq? head 'if)
             (let* ((t (simplify-if (cadr sexp)))
                    (th (simplify-if (caddr sexp)))
                    (el (if (pair? (cdddr sexp))
                          (simplify-if (cadddr sexp))
                          #f)))
               (cond
                 ((eq? t #t) th)
                 ((eq? t #f) el)
                 (else (if (pair? (cdddr sexp))
                         (list 'if t th el)
                         (list 'if t th))))))
            ((eq? head 'set!)
             (cons 'set!
                   (cons (cadr sexp)
                         (map simplify-if (cddr sexp)))))
            ((eq? head 'let)
             (cons 'let
                   (cons (map (lambda (b)
                                (list (car b) (simplify-if (cadr b))))
                              (cadr sexp))
                         (map simplify-if (cddr sexp)))))
            ((eq? head 'letrec)
             (cons 'letrec
                   (cons (map (lambda (b)
                                (list (car b) (simplify-if (cadr b))))
                              (cadr sexp))
                         (map simplify-if (cddr sexp)))))
            ((eq? head 'letrec*)
             (cons 'letrec*
                   (cons (map (lambda (b)
                                (list (car b) (simplify-if (cadr b))))
                              (cadr sexp))
                         (map simplify-if (cddr sexp)))))
            ((eq? head 'call-with-values)
             (cons 'call-with-values (map simplify-if (cdr sexp))))
            ((eq? head 'values)
             (cons 'values (map simplify-if (cdr sexp))))
            (else
             (cons head (map simplify-if (cdr sexp))))))
        sexp))

    ;; ------------------------------------------------------------------
    ;; inline (L2-2): the peval core -- copy propagation + beta reduction.
    ;;
    ;; Rewrites lowered core IR by beta-reducing applications of lambda
    ;; literals and copy-propagating safe lexical bindings: self-evaluating
    ;; literals, quoted data, and lambda closures.  These are immutable,
    ;; side-effect-free values, so duplicating them at every reference is
    ;; observationally equivalent.  A binding whose name is assigned (set!)
    ;; in scope, and any binding not statically known, is left alone.
    ;;
    ;; Cost control follows Waddell & Dybvig's effort counter: one budget
    ;; per pass bounds both the total inlining work and the nesting depth.
    ;; When it is exhausted the pass residualizes (keeps) applications and
    ;; references instead of inlining them, keeping the rewrite O(N) in the
    ;; program size and bounding code growth.  Propagated bindings are kept
    ;; in their let/letrec, so a reference left behind by a depth cut is
    ;; still bound.

    ;; lambda-formals->list : formals -> (list symbol)
    (define (lambda-formals->list formals)
      (if (symbol? formals)
        (list formals)
        (let loop ((f formals) (acc '()))
          (cond ((null? f) (reverse acc))
                ((pair? f) (loop (cdr f) (cons (car f) acc)))
                (else (reverse (cons f acc)))))))

    ;; collect-assigned : sexp -> (list symbol)
    ;; Names assigned by set! anywhere in the form (not entering quoted
    ;; data).  An assigned name is a mutable binding: copy propagation of
    ;; its initializer would miss later writes, so it is never propagated.
    (define (collect-assigned sexp)
      (let loop ((s sexp) (acc '()))
        (cond
          ((symbol? s) acc)
          ((not (pair? s)) acc)
          ((memq (car s) '(quote quote-syntax)) acc)
          ((eq? (car s) 'set!)
           (let ((name (cadr s)))
             (loop (caddr s)
                   (if (member name acc) acc (cons name acc)))))
          ((eq? (car s) 'lambda)
           ;; formals may be a dotted list; only the body is walked
           (let loop2 ((bs (cddr s)) (acc acc))
             (if (null? bs)
               acc
               (loop2 (cdr bs) (loop (car bs) acc)))))
          ((eq? (car s) 'define)
           (if (and (pair? (cddr s)) (pair? (caddr s)))
             (loop (caddr s) acc)
             acc))
          ((memq (car s) '(let letrec letrec*))
           (let loop2 ((bs (cdr s)) (acc acc))
             (if (null? bs)
               acc
               (loop2 (cdr bs) (loop (car bs) acc)))))
          (else
           (let loop2 ((cs (cdr s)) (acc (loop (car s) acc)))
             (if (null? cs)
               acc
               (loop2 (cdr cs) (loop (car cs) acc))))))))

    ;; safe-inline-value? : sexp -> boolean
    ;; A value that may be freely duplicated by copy propagation.
    (define (safe-inline-value? v)
      (or (self-evaluating? v)
          (and (pair? v) (eq? (car v) 'quote))
          (and (pair? v) (eq? (car v) 'lambda))))

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

    ;; lambda-self-referential? : symbol sexp -> boolean
    ;; True when a lambda value refers to its own binding name anywhere in
    ;; its body (a recursive function).  Propagating such a lambda would
    ;; inline its own recursive calls -- unrolling the recursion until the
    ;; effort budget cuts it off, bloating the residual code (and, at a
    ;; depth cut inside a propagated copy, leaving a dangling reference).
    ;; Leave recursive functions as variables; their recursive calls stay
    ;; calls to the surviving letrec binding.
    (define (lambda-self-referential? name lambda-sexp)
      (member name (collect-residual-free (cddr lambda-sexp))))

    (define (env-extend-vars env names)
      (fold-left (lambda (e n) (cons (cons n *inline-var*) e)) env names))
    (define (env-extend-safes env bindings assigned)
      (fold-left (lambda (e b)
                   (cons (cons (car b)
                               (if (or (member (car b) assigned)
                                       (not (safe-inline-value? (cadr b)))
                                       (and (pair? (cadr b))
                                            (eq? (car (cadr b)) 'lambda)
                                            (lambda-self-referential?
                                             (car b) (cadr b))))
                                 *inline-var*
                                 (cadr b)))
                         e))
                 env bindings))

    ;; beta-bindings : formals (list sexp) -> (list (name value)) or #f
    ;; Pair the lambda formals with the argument expressions.  With a rest
    ;; formal the remaining args become a (list ...) construction (retained
    ;; as a binding, never propagated, since it allocates).  Mismatched
    ;; arity returns #f: leave the call alone so the runtime reports it.
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
                                        '(quote ())
                                        (cons 'list as)))
                             acc)))
             ((null? as) #f)
             (else (loop (cdr fs) (cdr as) (cons (list (car fs) (car as)) acc))))))))

    ;; collect-residual-free : sexp -> (list symbol)
    ;; Free symbols of an inlined body, ENTERING lambda/let bodies (minus
    ;; their bindings) but not quoted data.  Used to decide which let
    ;; bindings are still referenced after copy propagation: a binding kept
    ;; only inside a propagated closure would dangle if its let were
    ;; dropped, so those references must count.
    (define (collect-residual-free body)
      (let loop ((s body) (acc '()))
        (cond
          ((symbol? s) (if (member s acc) acc (cons s acc)))
          ((not (pair? s)) acc)
          ((memq (car s) '(quote quote-syntax)) acc)
          ((eq? (car s) 'lambda)
           (let ((bound (lambda-formals->list (cadr s))))
             (filter (lambda (x) (not (member x bound)))
                     (loop (cons 'begin (cddr s)) acc))))
          ((memq (car s) '(let letrec letrec*))
           (let ((bound (map car (cadr s))))
             (filter (lambda (x) (not (member x bound)))
                     (loop (cons 'begin (cddr s)) (loop (cadr s) acc)))))
          (else
           (let loop2 ((cs (cdr s)) (acc (loop (car s) acc)))
             (if (null? cs)
               acc
               (loop2 (cdr cs) (loop (car cs) acc))))))))

    ;; prune-let-bindings : head (list binding) (list sexp) -> sexp
    ;; After inlining the body, drop bindings whose name is no longer
    ;; referenced anywhere (binding values and body).  When none survive the
    ;; let collapses to its body.
    (define (prune-let-bindings head new-bindings body-inl)
      (let* ((free (collect-residual-free
                    (append (map (lambda (b) (cadr b)) new-bindings) body-inl)))
             (survivors (filter (lambda (b) (member (car b) free)) new-bindings)))
        (if (null? survivors)
          (if (null? (cdr body-inl)) (car body-inl) (cons 'begin body-inl))
          (cons head (cons survivors body-inl)))))

    (define (inline-walk sexp env budget)
      (if (inline-budget-spent? budget)
        sexp
        (cond
          ((symbol? sexp)
           (let ((v (assq sexp env)))
             (if (and v (not (eq? (cdr v) *inline-var*)))
               (begin (inline-spend-effort! budget 1) (cdr v))
               sexp)))
          ((not (pair? sexp)) sexp)
          (else
           (let ((head (car sexp)))
             (cond
               ((eq? head 'quote) sexp)
               ((eq? head 'quote-syntax) sexp)
               ((eq? head 'lambda)
                (let* ((formals (cadr sexp))
                       (env1 (env-extend-vars env (lambda-formals->list formals))))
                  (cons 'lambda
                        (cons formals
                              (map (lambda (e) (inline-walk e env1 budget))
                                   (cddr sexp))))))
               ((eq? head 'define)
                (if (and (pair? (cadr sexp)) (symbol? (caadr sexp)))
                  ;; (define (name args) body) desugars to a lambda value
                  (list 'define
                        (caadr sexp)
                        (inline-walk (cons 'lambda (cons (cdadr sexp) (cddr sexp)))
                                     env budget))
                  (list 'define (cadr sexp)
                        (inline-walk (caddr sexp) env budget))))
               ((eq? head 'if)
                (cons 'if (map (lambda (e) (inline-walk e env budget)) (cdr sexp))))
               ((eq? head 'begin)
                (cons 'begin (map (lambda (e) (inline-walk e env budget)) (cdr sexp))))
               ((eq? head 'set!)
                (list 'set! (cadr sexp) (inline-walk (caddr sexp) env budget)))
               ((eq? head 'let)
                (let* ((bindings (cadr sexp))
                       (new-bindings
                        (map (lambda (b)
                               (list (car b) (inline-walk (cadr b) env budget)))
                             bindings))
                       (env1 (env-extend-safes env new-bindings
                                               (collect-assigned (cddr sexp))))
                       (body-inl (map (lambda (e) (inline-walk e env1 budget))
                                   (cddr sexp))))
                  (inline-spend-effort! budget 1)
                  (prune-let-bindings 'let new-bindings body-inl)))
               ((memq head '(letrec letrec*))
                (let* ((bindings (cadr sexp))
                       (names (map car bindings))
                       (env0 (env-extend-vars env names))
                       (assigned (collect-assigned (cddr sexp)))
                       (new-bindings
                        (map (lambda (b)
                               (list (car b)
                                     (inline-walk (cadr b) env0 budget)))
                             bindings))
                       (env1 (env-extend-safes env new-bindings assigned))
                       (body-inl (map (lambda (e) (inline-walk e env1 budget))
                                   (cddr sexp))))
                  (inline-spend-effort! budget 1)
                  (prune-let-bindings head new-bindings body-inl)))
               ((eq? head 'call-with-values)
                (cons 'call-with-values
                      (map (lambda (e) (inline-walk e env budget)) (cdr sexp))))
               ((eq? head 'values)
                (cons 'values
                      (map (lambda (e) (inline-walk e env budget)) (cdr sexp))))
               (else
                ;; application: inline head and args, then beta-reduce if the
                ;; head is now a lambda literal.
                (let ((f (inline-walk head env budget))
                      (args (map (lambda (a) (inline-walk a env budget))
                                 (cdr sexp))))
                  (if (and (pair? f) (eq? (car f) 'lambda)
                           (not (inline-budget-spent? budget)))
                    (let ((bindings (beta-bindings (cadr f) args)))
                      (if bindings
                        (let ((let-form (cons 'let (cons bindings (cddr f)))))
                          (inline-spend-effort! budget 2)
                          (inline-deepen! budget)
                          (inline-walk let-form env budget))
                        (cons f args)))
                    (cons f args))))))))))

    (define *inline-max-effort* 40000)
    (define *inline-max-depth* 16)

    ;; inline : sexp -> sexp
    (define (inline sexp)
      (inline-walk sexp '() (make-inline-budget *inline-max-effort* *inline-max-depth*)))

    ;; ------------------------------------------------------------------
    ;; Tail-call position analysis (L2-3 backend prerequisite).
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
    ;; tail-call-positions : sexp -> (list sexp)
    ;; Mark every subexpression that sits in a tail position by wrapping
    ;; it as (tail-call <sexp>).  The wrapper is the analysis product:
    ;; backends consume it to emit jumps instead of push-calls.  The
    ;; returned IR is NOT for direct s7 evaluation (tail-call is a marker,
    ;; not an operator), so this is exported for the L2-3 backend rather
    ;; than run in the on-load pipeline.

    ;; tail? : sexp boolean -> boolean
    ;; Whether the given form (a lambda body) has its last expression in
    ;; tail position -- trivially yes; the helper walks down.
    (define (tail-call-positions sexp)
      ;; mark-body : (list sexp) -> (list sexp); mark only the last
      ;; expression of a body as tail.
      (define (mark-body body)
        (if (null? body)
          '()
          (let* ((rev (reverse body))
                 (last (mark-tail (car rev))))
            (append (reverse (cdr rev)) (list last)))))
      (define (mark-tail s)
        (cond
          ((not (pair? s)) s)
          ((memq (car s) '(quote quote-syntax)) s)
          ((eq? (car s) 'lambda)
           (cons 'lambda (cons (cadr s) (mark-body (cddr s)))))
          ((eq? (car s) 'if)
           (list 'if (cadr s) (mark-tail (caddr s))
                 (if (pair? (cdddr s)) (mark-tail (cadddr s)) #f)))
          ((eq? (car s) 'begin)
           (cons 'begin (mark-body (cdr s))))
          ((memq (car s) '(let letrec letrec*))
           (list (car s) (cadr s) (mark-body (cddr s))))
          ((eq? (car s) 'set!)
           (list 'set! (cadr s) (mark-tail (caddr s))))
          ((eq? (car s) 'call-with-values)
           ;; consumer (caddr) is invoked in tail position
           (list 'call-with-values (cadr s) (mark-tail (caddr s))))
          (else
           ;; A bare application in tail position: wrap it.
           (list 'tail-call s))))
      (mark-tail sexp))

    ;; ------------------------------------------------------------------
    ;; Dead code elimination at the defs level.
    ;;
    ;; eliminate-dead-defs : (list sexp) -> (list sexp)
    ;; Drop top-level (define name e) defs whose name is never referenced
    ;; by any surviving def or by the registration/other forms (directly
    ;; or transitively).  Only lambda-valued defs are candidates: a
    ;; non-lambda value (constant, call) may have side effects at
    ;; definition time and is always kept.  Iterates to a fixpoint because
    ;; deleting one def can make another unreferenced.

    ;; collect-free-symbols : sexp -> (list symbol)
    ;; Free symbols of an expression: identifiers in operator and operand
    ;; positions, not counting lambda formals / let bindings (bound), and
    ;; not entering quote/quote-syntax data.
    (define (collect-free-symbols sexp)
      (let loop ((s sexp) (acc '()))
        (cond
          ((symbol? s) (if (member s acc) acc (cons s acc)))
          ((not (pair? s)) acc)
          ((memq (car s) '(quote quote-syntax)) acc)
          ((eq? (car s) 'lambda)
           (let* ((formals (cadr s))
                  (bound (if (symbol? formals) (list formals) formals)))
             (let loop2 ((body (cddr s)) (acc acc))
               (if (null? body)
                 (filter (lambda (x) (not (member x bound))) acc)
                 (loop2 (cdr body) (loop (car body) acc))))))
          ((memq (car s) '(let letrec letrec*))
           (let ((bound (map car (cadr s))))
             (let loop2 ((bs (cadr s)) (acc acc))
               (if (null? bs)
                 (let loop3 ((body (cddr s)) (acc acc))
                   (if (null? body)
                     (filter (lambda (x) (not (member x bound))) acc)
                     (loop3 (cdr body) (loop (car body) acc))))
                 (loop2 (cdr bs) (loop (cadr (car bs)) acc))))))
          ((eq? (car s) 'define)
           ;; (define name value): collect from the value only; the name
           ;; is bound by this definition.
           (let ((val (if (pair? (cddr s)) (caddr s) #f)))
             (if (pair? val) (loop val acc) acc)))
          (else
           (let loop2 ((cs (cdr s)) (acc (loop (car s) acc)))
             (if (null? cs)
               acc
               (loop2 (cdr cs) (loop (car cs) acc))))))))

    ;; def-name : sexp -> symbol or #f
    ;; The name introduced by a top-level define form.
    (define (def-name sexp)
      (if (and (pair? sexp)
               (eq? (car sexp) 'define)
               (pair? (cdr sexp)))
        (if (symbol? (cadr sexp))
          (cadr sexp)
          (caadr sexp))
        #f))

    ;; lambda-valued-def? : sexp -> boolean
    ;; A def whose value is a plain lambda -- a safe DCE candidate (no
    ;; definition-time side effect).
    (define (lambda-valued-def? sexp)
      (and (pair? sexp)
           (eq? (car sexp) 'define)
           (pair? (cdr sexp))
           (symbol? (cadr sexp))
           (pair? (cddr sexp))
           (pair? (caddr sexp))
           (eq? (caaddr sexp) 'lambda)))

    (define (eliminate-dead-defs defs)
      (let loop ((current defs))
        ;; alive : all free symbols of the current defs.  A lambda-valued
        ;; def whose name is not alive is unreferenced -> drop it.
        (let* ((alive (collect-all-free current))
               (survivors (filter (lambda (d)
                                    (or (not (lambda-valued-def? d))
                                        (member (def-name d) alive)))
                                  current)))
          (if (equal? survivors current)
            survivors
            (loop survivors)))))

    (define (collect-all-free defs)
      (let loop ((ds defs) (acc '()))
        (if (null? ds)
          acc
          (loop (cdr ds)
                (append (collect-free-symbols (car ds)) acc)))))

    )) ;begin
