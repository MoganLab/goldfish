;;; passes.scm -- L2: nanopass-style IR passes.
;;;
;;; The compiler passes rewritten against the record IR (goldfish/core/ir.scm,
;;; Guile-aligned tree-il).  A pass is a function ir -> ir that rewrites the
;;; tree; run-passes applies a list of passes in order.  The output is either
;;; converted back to core sexp for the s7-eval path (ir->core) or fed to the
;;; bytecode compiler.
;;;
;;; IR shape notes (Guile-aligned):
;;;   - <begin> is a binary right-nested <seq> (head . tail); a single
;;;     expression is NOT wrapped, an empty sequence is <void>.
;;;   - <if> is <conditional> (test consequent alternate).
;;;   - <set!> is typed: <lexical-set> / <toplevel-set>.
;;;   - <lambda> carries a single body expression (possibly a <seq> tree or
;;;     a <letrec>), never a body list.
;;;   - top-level defs are <toplevel-define>.

(define-library (goldfish compiler passes)
  (import (scheme base)
          (goldfish match)
          (goldfish compiler patterns)
          (goldfish core ir))
  (export run-passes
    constant-fold
    simplify-if
    lower-let
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

    ;; fold-lambda-body : ir pass -> ir
    ;; Apply a pass to the body EXPRESSION of a lambda (the lambda-case
    ;; body), rebuilding the lambda-case so passes recurse into closures.
    (define (fold-lambda-body lc f)
      (if (lambda-case? lc)
        (make-lambda-case #f
                          (lambda-case-req lc) (lambda-case-opt lc)
                          (lambda-case-rest lc) (lambda-case-kw lc)
                          (lambda-case-inits lc) (lambda-case-gensyms lc)
                          (f (lambda-case-body lc))
                          (if (lambda-case-alternate lc)
                            (fold-lambda-body (lambda-case-alternate lc) f)
                            #f))
        (f lc)))

    ;; (compile-defs removed with core->ir: the pipeline's sexp interface
    ;; rebuilt IR from lowered core sexp, which is gone.  The load path
    ;; runs syntax->ir directly (compile-defs-on-load / compile-defs-cached);
    ;; the legacy tests that fed core sexp were retired with it.)

    ;; ------------------------------------------------------------------
    ;; seq helpers: seq trees are binary right-nested <seq> nodes.
    ;; seq-head/seq-tail recursion is O(1) in the tail, unlike a begin list.

    ;; seq->list : ir -> (list ir)
    ;; Flatten a seq tree into a list of expressions (head first).
    (define (seq->list s)
      (let collect ((s s) (acc '()))
        (cond ((void? s) (reverse acc))
              ((seq? s) (collect (seq-tail s) (cons (seq-head s) acc)))
              (else (reverse (cons s acc))))))

    ;; list->seq : (list ir) -> ir
    ;; Join a list of expressions into a binary right-nested seq tree.
    ;;   () -> <void>; (e) -> e; (e1 e2 ...) -> (seq e1 (seq e2 ...)).
    (define (list->seq ls)
      (cond
        ((null? ls) (make-void #f))
        ((null? (cdr ls)) (car ls))
        (else (make-seq #f (car ls) (list->seq (cdr ls))))))

    ;; seq-map : (ir -> ir) seq-tree -> seq-tree
    ;; Rewrite every element of a seq tree with f, preserving the tree shape.
    (define (seq-map f s)
      (cond ((void? s) s)
            ((seq? s) (make-seq #f (f (seq-head s)) (seq-map f (seq-tail s))))
            (else (f s))))

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
                                  (loop (cdr as) (cons (const-exp a) values)))
                                 ((self-evaluating? a)
                                  (loop (cdr as) (cons a values)))
                                 (else #f)))))))))))

    ;; constant-fold : ir -> ir
    ;; Fold primitive calls whose arguments are all constants, recursively
    ;; through the IR tree.  const contents are data and never entered.
    (define (constant-fold ir)
      (match ir
        (($const v) ir)
        ( (? void?) ir)
        (($toplevel-define name value)
         (make-toplevel-define #f name (constant-fold value)))
        (($lambda meta body)
         (make-lambda #f meta (fold-lambda-body body constant-fold)))
        (($conditional test then else)
         (make-conditional #f (constant-fold test) (constant-fold then)
                           (if else (constant-fold else) #f)))
        (($seq head tail)
         (let ((h (constant-fold head))
               (t (constant-fold tail)))
           (cond
             ((void? t) h)
             ((and (void? h) (void? t)) (make-void #f))
             (else (make-seq #f h t)))))
        (($let names gensyms vals body)
         (make-let #f names gensyms
                   (map constant-fold vals)
                   (constant-fold body)))
        (($letrec src in-order? names gensyms vals body)
         (make-letrec src in-order? names gensyms
                      (map constant-fold vals)
                      (constant-fold body)))
        (($lexical-set name depth index expr)
         (make-lexical-set #f name depth index (constant-fold expr)))
        (($toplevel-set name expr)
         (make-toplevel-set #f name (constant-fold expr)))
        (($let-values exp body)
         (make-let-values #f (constant-fold exp) (constant-fold body)))
        (($call proc args ...)
         (let ((p (constant-fold proc))
               (as (map constant-fold args)))
           (or (try-fold-call-ir p as)
               (make-call #f p as))))
        (($primcall name args ...)
         (or (try-fold-call-ir (make-primitive-ref #f name) args)
             (make-primcall #f name (map constant-fold args))))
        (($primitive-ref name)
         (make-primitive-ref #f name))
        (($lexical-ref name depth index)
         (make-lexical-ref #f name depth index))
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
          (and (const? ir) (eq? (const-exp ir) val))))

    (define (simplify-if ir)
      (match ir
        (($const v) ir)
        ( (? void?) ir)
        (($toplevel-define name value)
         (make-toplevel-define #f name (simplify-if value)))
        (($lambda meta body)
         (make-lambda #f meta (fold-lambda-body body simplify-if)))
        (($conditional test then else)
         (let* ((t (simplify-if test))
                (th (simplify-if then))
                (el (if else (simplify-if else) #f)))
           (cond
             ((const-boolean? t #t) th)
             ((const-boolean? t #f)
              (if else el (make-conditional #f t th #f)))
             (else (make-conditional #f t th el)))))
        (($seq head tail)
         (make-seq #f (simplify-if head) (simplify-if tail)))
        (($let names gensyms vals body)
         (make-let #f names gensyms
                   (map simplify-if vals)
                   (simplify-if body)))
        (($letrec src in-order? names gensyms vals body)
         (make-letrec src in-order? names gensyms
                      (map simplify-if vals)
                      (simplify-if body)))
        (($lexical-set name depth index expr)
         (make-lexical-set #f name depth index (simplify-if expr)))
         (($toplevel-set name expr)
          (make-toplevel-set #f name (simplify-if expr)))
         (($let-values exp body)
          (make-let-values #f (simplify-if exp) (simplify-if body)))
         (($call proc args ...)
         (make-call #f (simplify-if proc) (map simplify-if args)))
        (($primcall name args ...)
         (make-primcall #f name (map simplify-if args)))
        (($primitive-ref name)
         (make-primitive-ref #f name))
        (($lexical-ref name depth index)
         (make-lexical-ref #f name depth index))
        ((? symbol? s) s)
        (_ ir)))

    ;; lower-let : ir -> ir
    ;; Desugar let/letrec into lambda/call so the VM sees only core forms.
    ;; let  ((x e) ...) body -> ((lambda (x ...) body) e ...)
    ;; letrec ((x e) ...) body -> (let ((x #f) ...) (set! x e) ... body) lowered
    (define (lower-let ir)
      (match ir
        (($const v) ir)
        ( (? void?) ir)
        (($toplevel-define name value)
         (make-toplevel-define #f name (lower-let value)))
        (($lambda meta body)
         (make-lambda #f meta (fold-lambda-body body lower-let)))
        (($conditional test then else)
         (make-conditional #f (lower-let test) (lower-let then)
                           (if else (lower-let else) #f)))
        (($seq head tail)
         (make-seq #f (lower-let head) (lower-let tail)))
        (($let names gensyms vals body)
         (if (null? names)
           (lower-let body)
           (make-call #f (make-lambda #f #f
                                      (make-lambda-case #f names '() #f #f
                                                        '() gensyms
                                                        (lower-let body) #f))
                      (map lower-let vals))))
        (($letrec src in-order? names gensyms vals body)
         (let ((inits (map lower-let vals))
               (tmp-bindings (map (lambda (n) (list n (make-const #f #f))) names)))
           (lower-let
             (make-let #f names gensyms
                       (map (lambda (n) (make-const #f #f)) names)
                       (list->seq
                         (append
                           (map (lambda (n v) (make-lexical-set #f n 0 0 v))
                                names inits)
                           (list (lower-let body))))))))
        (($lexical-set name depth index expr)
         (make-lexical-set #f name depth index (lower-let expr)))
         (($toplevel-set name expr)
          (make-toplevel-set #f name (lower-let expr)))
         (($let-values exp body)
          (make-let-values #f (lower-let exp) (lower-let body)))
         (($call proc args ...)
         (make-call #f (lower-let proc) (map lower-let args)))
        (($primcall name args ...)
         (make-primcall #f name (map lower-let args)))
        (($primitive-ref name)
         (make-primitive-ref #f name))
        (($lexical-ref name depth index)
         (make-lexical-ref #f name depth index))
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

    ;; lambda-req : ir -> (list symbol) or #f
    ;; The required formal names of a lambda (via its lambda-case), or #f
    ;; if the body is not a lambda-case (degenerate).
    (define (lambda-req lam)
      (let ((b (lambda-body lam)))
        (if (lambda-case? b) (lambda-case-req b) #f)))

    ;; lambda-case-formals : lambda-case -> formals
    ;; Reconstruct a formals list from a lambda-case arity (req opt rest).
    (define (lambda-case-formals lc)
      (let ((req (lambda-case-req lc))
            (opt (lambda-case-opt lc))
            (rest (lambda-case-rest lc)))
        (cond
          ((and (null? opt) rest)
           (append req rest))
          ((and (null? opt) (not rest))
           req)
          (else
           (append req opt (if rest (list rest) '()))))))

    ;; lambda-formals : ir -> formals
    ;; The full formals of a lambda (req/opt/rest reconstructed), or '() if
    ;; the body is not a lambda-case.
    (define (lambda-formals lam)
      (let ((b (lambda-body lam)))
        (if (lambda-case? b) (lambda-case-formals b) '())))

    ;; ir-children : ir -> (list ir)
    ;; The direct child expressions of a node (not atoms like formals or
    ;; binding names).  A seq tree yields its flattened elements; a
    ;; lambda yields its single body.
    (define (ir-children ir)
      (cond
        ((lambda? ir) (list (lambda-body ir)))
        ((toplevel-define? ir) (list (toplevel-define-exp ir)))
        ((conditional? ir)
         (if (conditional-alternate ir)
           (list (conditional-test ir) (conditional-consequent ir) (conditional-alternate ir))
           (list (conditional-test ir) (conditional-consequent ir))))
        ((seq? ir) (seq->list ir))
        ((let? ir) (append (let-vals ir) (list (let-body ir))))
        ((letrec? ir) (append (letrec-vals ir) (list (letrec-body ir))))
        ((let-values? ir) (list (let-values-exp ir) (let-values-body ir)))
        ((lexical-set? ir) (list (lexical-set-exp ir)))
        ((toplevel-set? ir) (list (toplevel-set-exp ir)))
        ((call? ir) (cons (call-proc ir) (call-args ir)))
        ((primcall? ir) (primcall-args ir))
        ((lambda-case? ir)
         (if (lambda-case-alternate ir)
           (list (lambda-case-body ir) (lambda-case-alternate ir))
           (list (lambda-case-body ir))))
        ((primitive-ref? ir) '())
        ((lexical-ref? ir) '())
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
          ((lexical-set? s)
           (loop (lexical-set-exp s)
                 (if (member (lexical-set-name s) acc)
                   acc
                   (cons (lexical-set-name s) acc))))
          ((toplevel-set? s)
           (loop (toplevel-set-exp s) acc))
          ((lambda? s)
           (let ((b (lambda-body s)))
             (loop b acc)))
          ((toplevel-define? s)
           (let ((val (toplevel-define-exp s)))
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
    (define (beta-bindings req args)
      (if (symbol? req)
        (if (= (length args) 1) (list (list req (car args))) #f)
        (let loop ((fs req) (as args) (acc '()))
          (cond
            ((null? fs) (if (null? as) (reverse acc) #f))
            ((symbol? fs)
             (reverse (cons (list fs (if (null? as)
                                         (make-const #f '())
                                         (make-call #f 'list as)))
                            acc)))
            ((null? as) #f)
            (else (loop (cdr fs) (cdr as) (cons (list (car fs) (car as)) acc)))))))

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
           (let* ((b (lambda-body s))
                  (bound (or (and (lambda-case? b) (lambda-case-req b)) '())))
             (filter (lambda (x) (not (member x bound)))
                     (loop b acc))))
          ((let? s)
           (let ((bound (let-names s)))
             (filter (lambda (x) (not (member x bound)))
                     (let ((acc1 (loop (let-body s) acc)))
                       (fold-left (lambda (a v) (loop v a)) acc1 (let-vals s))))))
          ((letrec? s)
           (let ((bound (letrec-names s)))
             (filter (lambda (x) (not (member x bound)))
                     (let ((acc1 (loop (letrec-body s) acc)))
                       (fold-left (lambda (a v) (loop v a)) acc1 (letrec-vals s))))))
          ((toplevel-define? s)
           (let ((val (toplevel-define-exp s)))
             (if (or (const? val) (void? val))
               acc
               (loop val acc))))
          (else
           (let loop2 ((cs (ir-children s)) (acc acc))
             (if (null? cs)
               acc
               (loop2 (cdr cs) (loop (car cs) acc))))))))

    ;; prune-let-bindings : head names vals body-inl -> ir
    ;; After inlining the body, drop bindings whose name is no longer
    ;; referenced anywhere (binding values and body).  When none survive
    ;; the let collapses to its body.
    (define (pure-ir? ir)
      (cond
        ((or (const? ir) (void? ir) (primitive-ref? ir) (symbol? ir)
             (and (not (pair? ir)) (not (vector? ir))))
         #t)
        ((lambda? ir) #t)
        ((seq? ir)
         (and (pure-ir? (seq-head ir)) (pure-ir? (seq-tail ir))))
        ((conditional? ir)
         (and (pure-ir? (conditional-test ir))
              (pure-ir? (conditional-consequent ir))
              (or (not (conditional-alternate ir))
                  (pure-ir? (conditional-alternate ir)))))
        (else #f)))

    (define (prune-let-bindings head names vals body-inl)
      (let* ((free (collect-residual-free
                    (append vals (list body-inl))))
             (surviving-names
               (filter (lambda (n) (member n free)) names))
             (surviving-vals
               (map (lambda (n) (cadr (assoc n (map list names vals))))
                    surviving-names)))
        (cond
          ((null? surviving-names) body-inl)
          ((eq? head 'let)
           (make-let #f surviving-names surviving-names surviving-vals body-inl))
          (else
           (make-letrec #f (eq? head 'letrec*)
                        surviving-names surviving-names surviving-vals body-inl)))))

    ;; inline-walk-lambda-case : lambda-case env budget -> lambda-case
    ;; Recurse a pass into a lambda's body expression (via its lambda-case).
    (define (inline-walk-lambda-case lc env budget)
      (if (lambda-case? lc)
        (make-lambda-case #f
                          (lambda-case-req lc) (lambda-case-opt lc)
                          (lambda-case-rest lc) (lambda-case-kw lc)
                          (lambda-case-inits lc) (lambda-case-gensyms lc)
                          (inline-walk (lambda-case-body lc) env budget)
                          (if (lambda-case-alternate lc)
                            (inline-walk-lambda-case (lambda-case-alternate lc) env budget)
                            #f))
        (inline-walk lc env budget)))

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
          ((primitive-ref? ir) ir)
          ((lexical-ref? ir) ir)
          ((lambda? ir)
           (let* ((b (lambda-body ir))
                  (req (or (and (lambda-case? b) (lambda-case-req b)) '()))
                  (env1 (env-extend-vars env req)))
             (make-lambda #f (lambda-meta ir) (inline-walk-lambda-case b env1 budget))))
          ((toplevel-define? ir)
           (make-toplevel-define #f (toplevel-define-name ir)
                                 (inline-walk (toplevel-define-exp ir) env budget)))
          ((conditional? ir)
           (make-conditional #f (inline-walk (conditional-test ir) env budget)
                             (inline-walk (conditional-consequent ir) env budget)
                             (if (conditional-alternate ir)
                               (inline-walk (conditional-alternate ir) env budget)
                               #f)))
          ((seq? ir)
           (make-seq #f (inline-walk (seq-head ir) env budget)
                     (inline-walk (seq-tail ir) env budget)))
          ((lexical-set? ir)
           (make-lexical-set #f (lexical-set-name ir)
                             (lexical-set-depth ir) (lexical-set-index ir)
                             (inline-walk (lexical-set-exp ir) env budget)))
          ((toplevel-set? ir)
           (make-toplevel-set #f (toplevel-set-name ir)
                              (inline-walk (toplevel-set-exp ir) env budget)))
          ((let? ir)
           (let* ((names (let-names ir))
                  (vals (let-vals ir))
                  (new-vals (map (lambda (v) (inline-walk v env budget)) vals))
                  (env1 (env-extend-safes env
                                          (map list names new-vals)
                                          (collect-assigned (let-body ir))))
                  (body-inl (inline-walk (let-body ir) env1 budget)))
             (inline-spend-effort! budget 1)
             (prune-let-bindings 'let names new-vals body-inl)))
          ((letrec? ir)
           (let* ((names (letrec-names ir))
                  (vals (letrec-vals ir))
                  (assigned names)
                  (env0 (env-extend-safes env (map list names vals) assigned))
                  (new-vals (map (lambda (v) (inline-walk v env0 budget)) vals))
                  (env1 (env-extend-safes env (map list names new-vals) assigned))
                  (body-inl (inline-walk (letrec-body ir) env1 budget)))
             (inline-spend-effort! budget 1)
             (prune-let-bindings (if (letrec-in-order? ir) 'letrec* 'letrec)
                                 names new-vals body-inl)))
          ((let-values? ir)
           (make-let-values #f (inline-walk (let-values-exp ir) env budget)
                            (inline-walk (let-values-body ir) env budget)))
          ((call? ir)
           (let ((f (inline-walk (call-proc ir) env budget))
                 (args (map (lambda (a) (inline-walk a env budget))
                            (call-args ir))))
             (if (and (lambda? f) (not (inline-budget-spent? budget)))
               (let ((bindings (beta-bindings (or (lambda-formals f) '()) args)))
                 (if bindings
                   (let* ((lc (lambda-body f))
                          (body (if (lambda-case? lc)
                                   (lambda-case-body lc)
                                   lc))
                          (let-form (make-let #f
                                              (map car bindings)
                                              (map car bindings)
                                              (map cadr bindings)
                                              body)))
                     (inline-spend-effort! budget 2)
                     (inline-deepen! budget)
                     (inline-walk let-form env budget))
                   (make-call #f f args)))
               (make-call #f f args))))
          ((primcall? ir)
           (make-primcall #f (primcall-name ir)
                          (map (lambda (a) (inline-walk a env budget))
                               (primcall-args ir))))
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
    ;;   (begin e ...)                 last expression (seq tail)
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
      (define (mark-lambda-body lc)
        (if (lambda-case? lc)
          (make-lambda-case #f
                            (lambda-case-req lc) (lambda-case-opt lc)
                            (lambda-case-rest lc) (lambda-case-kw lc)
                            (lambda-case-inits lc) (lambda-case-gensyms lc)
                            (mark-tail (lambda-case-body lc))
                            (if (lambda-case-alternate lc)
                              (mark-lambda-body (lambda-case-alternate lc))
                              #f))
          (mark-tail lc)))
      (define (mark-tail s)
        (cond
          ((or (const? s) (void? s)) s)
          ((symbol? s) s)
          ((primitive-ref? s) s)
          ((lexical-ref? s) s)
          ((lambda? s)
           (make-lambda #f (lambda-meta s) (mark-lambda-body (lambda-body s))))
          ((conditional? s)
           (make-conditional #f (conditional-test s)
                             (mark-tail (conditional-consequent s))
                             (if (conditional-alternate s)
                               (mark-tail (conditional-alternate s))
                               #f)))
          ((seq? s)
           (make-seq #f (seq-head s) (mark-tail (seq-tail s))))
          ((let? s)
           (make-let #f (let-names s) (let-gensyms s) (let-vals s)
                     (mark-tail (let-body s))))
          ((letrec? s)
           (make-letrec (letrec-source s) (letrec-in-order? s)
                        (letrec-names s) (letrec-gensyms s)
                        (letrec-vals s) (mark-tail (letrec-body s))))
          ((lexical-set? s)
           (make-lexical-set #f (lexical-set-name s)
                             (lexical-set-depth s) (lexical-set-index s)
                             (mark-tail (lexical-set-exp s))))
          ((toplevel-set? s)
           (make-toplevel-set #f (toplevel-set-name s) (mark-tail (toplevel-set-exp s))))
          ((let-values? s)
           ;; consumer body is invoked in tail position
           (make-let-values #f (let-values-exp s) (mark-tail (let-values-body s))))
          ((call? s)
           (let ((proc (call-proc s))
                 (args (call-args s)))
             (list 'tail-call
                   (if (primitive-ref? proc)
                     (make-primcall #f (primitive-ref-name proc) args)
                     (make-call #f proc args)))))
          ((primcall? s) s)
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
          ((primitive-ref? s) acc)
          ((lexical-ref? s) acc)
          ((lambda? s)
           (let* ((b (lambda-body s))
                  (bound (or (and (lambda-case? b) (lambda-case-req b)) '())))
             (filter (lambda (x) (not (member x bound)))
                     (loop b acc))))
          ((let? s)
           (let ((bound (let-names s)))
             (filter (lambda (x) (not (member x bound)))
                     (let ((acc1 (loop (let-body s) acc)))
                       (fold-left (lambda (a v) (loop v a)) acc1 (let-vals s))))))
          ((letrec? s)
           (let ((bound (letrec-names s)))
             (filter (lambda (x) (not (member x bound)))
                     (let ((acc1 (loop (letrec-body s) acc)))
                       (fold-left (lambda (a v) (loop v a)) acc1 (letrec-vals s))))))
          ((toplevel-define? s)
           ;; (define name value): collect from the value only; the name
           ;; is bound by this definition.
           (let ((val (toplevel-define-exp s)))
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
      (and (toplevel-define? d) (lambda? (toplevel-define-exp d))))

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
                                        (member (toplevel-define-name d) alive)))
                                  current)))
          (if (equal? survivors current)
            survivors
            (loop survivors)))))

    )) ;begin
