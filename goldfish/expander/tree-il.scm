;;; tree-il.scm -- L4: expander direct tree-il emission.
;;;
;;; The expander's core resolves every identifier to a binding during
;;; expansion, but its lowered output (via `lower`) previously encoded
;;; that decision as a bare symbol.  This library re-walks a
;;; FULLY-EXPANDED syntax tree (the value expand-expr returns) and
;;; preserves the binding KIND in the IR:
;;;
;;;   - primitive -> <primitive-ref>
;;;   - lexical   -> <lexical-ref> with (depth . index)
;;;   - toplevel / unbound -> <toplevel-ref>
;;;
;;; Lexical addressing mirrors the backend's frame model.  Computing it
;;; here lets the backend consume pre-resolved addresses instead of
;;; re-deriving them.
;;;
;;; Node names and structure follow Guile's (language tree-il): <lambda>
;;; carries a single body (a <seq> tree / <letrec>), <begin> joins to a
;;; binary <seq>, <if> is <conditional>, and <set!> is typed.  The IR
;;; definitions live in L2 (goldfish core ir); this bridge lives in L4
;;; and is imported by both the expander API and the compiler's
;;; syntax-ir wrapper.  No L3->L5 dependency.
;;;
;;; Primitive marking: expand-atom emits (primitive-ref name) for primitive
;;; references, so this walk recognizes the marker directly instead of
;;; re-resolving the identifier; lexical/toplevel kinds are still resolved
;;; here (lexical addressing is computed here regardless).

(define-library (goldfish expander tree-il)
  (import (scheme base)
          (goldfish)
          (goldfish core ir))
  (export syntax->ir
    syntax->ir/sexp
    expand->ir)
  (begin

    (define (binding-kind binding)
      (cond
        ((not binding) #f)
        ((primitive-binding? binding) 'primitive)
        ((lexical-binding? binding) 'lexical)
        ((toplevel-binding? binding) 'toplevel)
        ((core-form-binding? binding) 'core-form)
        ((transformer-binding? binding) 'transformer)
        (else 'other)))

    (define (resolve-name id ctx)
      (let*-values (((name binding) (resolve-identifier id ctx)))
        (values name (binding-kind binding))))

    (define (datum-of s)
      (if (syntax? s) (syntax->datum s) s))

    (define (env-lookup env name)
      (let loop ((es env) (d 0))
        (if (null? es)
          #f
          (let ((cell (assq name (car es))))
            (if cell
              (cons d (cdr cell))
              (loop (cdr es) (+ d 1)))))))

    (define (env-extend-frame env names)
      (let loop ((ns names) (i 0) (frame '()))
        (if (null? ns)
          (cons frame env)
          (loop (cdr ns) (+ i 1) (cons (cons (car ns) i) frame)))))

    (define (env-next-slot env)
      (if (null? env)
        0
        (let loop ((frame (car env)) (max-idx -1))
          (if (null? frame)
            (+ max-idx 1)
            (loop (cdr frame) (max max-idx (cdar frame)))))))

    (define (env-add-bindings env bindings)
      (if (null? env)
        env
        (cons (append (car env) bindings) (cdr env))))

    (define (binding-name b)
      (datum-of (car (syntax-form b))))
    (define (binding-init b)
      (cadr (syntax-form b)))

    (define (lambda-formals->list formals)
      (if (symbol? formals)
        (list formals)
        (let loop ((f formals) (acc '()))
          (cond ((null? f) (reverse acc))
                ((pair? f) (loop (cdr f) (cons (car f) acc)))
                (else (reverse (cons f acc)))))))

    (define (formals->datum f)
      ;; The formals datum with a DOTTED rest preserved: (l:2 . r:3) must
      ;; stay (l:2 . r:3) so formals->arity recovers the rest parameter.
      ;; Unwrapping to (l:2 r:3) loses it and the lambda-case gains an
      ;; ordinary formal instead of the rest marker.
      (cond
        ((syntax? f) (formals->datum (syntax-form f)))
        ((symbol? f) f)
        ((null? f) '())
        ((pair? f)
         (let loop ((p f) (acc '()))
           (cond ((null? p) (reverse acc))
                 ((pair? p)
                  (loop (cdr p)
                        (cons (if (syntax? (car p)) (syntax->datum (car p)) (car p))
                              acc)))
                 (else (append (reverse acc)
                               (if (syntax? p) (syntax->datum p) p))))))
        (else f)))

    ;; formals->arity : formals -> (values req opt rest)
    ;; Split a formals list into Guile lambda-case arity parts.
    (define (formals->arity f)
      (let ((d (formals->datum f)))
        (cond
          ((symbol? d) (values '() '() d))
          (else
           (let loop ((p d) (req '()) (opt '()) (rest #f))
             (cond
               ((null? p) (values (reverse req) (reverse opt) rest))
               ((pair? p) (loop (cdr p) (cons (car p) req) opt rest))
               (else (values (reverse req) (reverse opt) p))))))))

    ;; list->seq : (list ir) -> ir
    ;; Join expressions into a binary right-nested seq (Guile join).
    (define (list->seq exps)
      (cond
        ((null? exps) (make-void #f))
        ((null? (cdr exps)) (car exps))
        (else (make-seq #f (car exps) (list->seq (cdr exps))))))

    ;; seq->body : (list ir) -> ir
    ;; A lambda body is a SINGLE expression (possibly a seq tree or letrec).
    (define (seq->body exps) (list->seq exps))

    (define (syntax->ir* stx ctx env)
      (cond
        ((not (syntax? stx)) stx)
        ;; A template literal (marked with the syntax-literal scope by
        ;; (syntax X) sub-templates and the (... ...) escape) is a datum
        ;; VALUE, not code: lower it to a constant.  Matches expand-expr.
        ((and (syntax? stx)
              (let ((sctx (syntax-context stx)))
                (and (pair? sctx)
                     (set-member? (stx-ctx-at sctx 0) 'syntax-literal))))
         (make-const #f (syntax->datum stx)))
        (else
         (let ((form (syntax-form stx)))
           (cond
              ((not (pair? form))
               (if (symbol? form)
                 (let*-values (((name kind) (resolve-name stx ctx)))
                   (let ((loc (env-lookup env name)))
                     (cond
                       (loc
                        (make-lexical-ref #f name (car loc) (cdr loc)))
                       ((eq? kind 'primitive)
                        (make-primitive-ref #f name))
                       (else (make-toplevel-ref #f name)))))
                 form))
             (else
              (let ((head (car form))
                    (head-name (if (syntax? (car form)) (syntax-form (car form)) (car form))))
                (case head-name
                  ((primitive-ref)
                   ;; (primitive-ref name) is emitted by expand-atom for
                   ;; primitive references; no binding re-resolution needed.
                   (make-primitive-ref #f (datum-of (cadr form))))
                  ((quote) (make-const #f (datum-of (cadr form))))
                  ((quote-syntax) (make-const #f (datum-of (cadr form))))
                  ((define)
                   (if (symbol? (syntax-form (cadr form)))
                     (let* ((dname (datum-of (cadr form)))
                            (env1 (env-add-bindings env (list (cons dname 0)))))
                       (make-toplevel-define #f dname
                                             (syntax->ir* (caddr form) ctx env1)))
                     (let* ((df (syntax-form (cadr form)))
                            (dname (datum-of (car df)))
                            (dformals (formals->datum (cdr df)))
                            (dnames (lambda-formals->list dformals)))
                       (make-toplevel-define #f dname
                                             (make-lambda #f #f
                                                          (make-lambda-case #f dnames '() #f #f '()
                                                                           dnames
                                                                           (seq->body
                                                                             (map (lambda (b) (syntax->ir* b ctx (env-extend-frame env dnames)))
                                                                                  (cddr form)))
                                                                           #f))))))
                  ((lambda)
                   (let* ((dformals (formals->datum (syntax-form (cadr form))))
                          (dnames (lambda-formals->list dformals)))
                     (let-values (((req opt rest) (formals->arity dformals)))
                       (make-lambda #f #f
                                    (make-lambda-case #f req opt rest #f '()
                                                     dnames
                                                     (seq->body
                                                       (map (lambda (b) (syntax->ir* b ctx (env-extend-frame env dnames)))
                                                            (cddr form)))
                                                     #f)))))
                  ((if)
                   (let ((else-stx (and (pair? (cdddr form)) (cadddr form))))
                     (let ((test-ir (syntax->ir* (cadr form) ctx env))
                           (then-ir (syntax->ir* (caddr form) ctx env))
                           (else-ir (if else-stx
                                      (let ((ir (syntax->ir* else-stx ctx env)))
                                        (if (eq? ir #f) (make-const #f #f) ir))
                                      #f)))
                       (make-conditional #f test-ir then-ir else-ir))))
                  ((begin)
                   (seq->body (map (lambda (b) (syntax->ir* b ctx env))
                                   (cdr form))))
                  ((let)
                   (if (symbol? (syntax-form (cadr form)))
                     (let* ((name (datum-of (cadr form)))
                            (bindings (syntax-form (caddr form)))
                            (body (cdddr form))
                            (bnames (map binding-name bindings)))
                       (make-letrec 'letrec #f
                                    (list name) (list name)
                                    (list (make-lambda #f #f
                                                       (make-lambda-case #f bnames '() #f #f '()
                                                                        bnames
                                                                        (seq->body
                                                                          (map (lambda (b) (syntax->ir* b ctx (env-extend-frame env bnames)))
                                                                               body))
                                                                        #f)))
                                    (seq->body
                                      (list (make-call #f (make-toplevel-ref #f name)
                                                       (map (lambda (b) (syntax->ir* (binding-init b) ctx env))
                                                            bindings))))))
                     (let* ((bindings (syntax-form (cadr form)))
                            (bnames (map binding-name bindings))
                            (slot-alist (let loop ((ns bnames) (i (env-next-slot env)) (acc '()))
                                          (if (null? ns)
                                            acc
                                            (loop (cdr ns) (+ i 1)
                                                  (cons (cons (car ns) i) acc)))))
                            (env1 (env-add-bindings env (reverse slot-alist))))
                       (make-let #f
                                 bnames
                                 bnames
                                 (map (lambda (b) (syntax->ir* (binding-init b) ctx env))
                                      bindings)
                                 (seq->body
                                   (map (lambda (b) (syntax->ir* b ctx env1))
                                        (cddr form)))))))
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
                                  (eq? head-name 'letrec*)
                                  bnames
                                  bnames
                                  (map (lambda (b) (syntax->ir* (binding-init b) ctx env1))
                                       bindings)
                                  (seq->body
                                    (map (lambda (b) (syntax->ir* b ctx env1))
                                         (cddr form))))))
                  ((set!)
                   (let*-values (((name kind) (resolve-name (cadr form) ctx)))
                     (let ((loc (env-lookup env name))
                           (rhs (syntax->ir* (caddr form) ctx env)))
                       (cond
                         (loc (make-lexical-set #f name (car loc) (cdr loc) rhs))
                         ((eq? kind 'primitive)
                          (error "set!: cannot assign to primitive" name))
                         (else (make-toplevel-set #f name rhs))))))
                  ((module-ref)
                   ;; (module-ref (quote lib) (quote name)) is the
                   ;; cross-library reference emitted by emit-toplevel-ref;
                   ;; a plain (module-ref m name) application is a call.
                   (if (and (syntax? (cadr form))
                            (eq? (syntax-form (cadr form)) 'quote)
                            (syntax? (caddr form))
                            (eq? (syntax-form (caddr form)) 'quote))
                     (let ((lib (cadr (datum-of (cadr form))))
                           (name (cadr (datum-of (caddr form)))))
                       (make-module-ref #f lib name #t))
                     (make-call #f (syntax->ir* head ctx env)
                                (map (lambda (a) (syntax->ir* a ctx env))
                                     (cdr form)))))
                  ((module-set)
                   ;; (module-set (quote lib) (quote name) exp) is the
                   ;; cross-library assignment emitted by emit; a plain
                   ;; (module-set m name v) application is a call.
                   (if (and (syntax? (cadr form))
                            (eq? (syntax-form (cadr form)) 'quote)
                            (syntax? (caddr form))
                            (eq? (syntax-form (caddr form)) 'quote))
                     (let ((lib (cadr (datum-of (cadr form))))
                           (name (cadr (datum-of (caddr form))))
                           (exp (syntax->ir* (cadddr form) ctx env)))
                       (make-module-set #f lib name #t exp))
                     (make-call #f (syntax->ir* head ctx env)
                                (map (lambda (a) (syntax->ir* a ctx env))
                                     (cdr form)))))
                  ((toplevel-ref)
                   ;; (toplevel-ref name) with a literal name is the
                   ;; reference form; anything else is a call.
                   (if (symbol? (datum-of (cadr form)))
                     (make-toplevel-ref #f (datum-of (cadr form)))
                     (make-call #f (syntax->ir* head ctx env)
                                (map (lambda (a) (syntax->ir* a ctx env))
                                     (cdr form)))))
                  ((values)
                   (make-values #f (map (lambda (a) (syntax->ir* a ctx env))
                                        (cdr form))))
                  ((call-with-values)
                   (make-call-with-values #f
                                          (syntax->ir* (cadr form) ctx env)
                                          (syntax->ir* (caddr form) ctx env)))
                  (else
                   (make-call #f (syntax->ir* head ctx env)
                              (map (lambda (a) (syntax->ir* a ctx env))
                                   (cdr form))))))))))))

    (define (syntax->ir stx ctx)
      (syntax->ir* stx ctx '()))

    (define (syntax->ir/sexp stx ctx)
      ;; Historical alias: compile-syntax-defs / the cache path used to ask
      ;; for "no lexical resolution" on top-level defs; the walk resolves
      ;; lexical refs regardless (a top-level def has an empty env anyway),
      ;; so the two entries now behave identically.
      (syntax->ir* stx ctx '()))

    (define (expand->ir expr)
      (let*-values (((stx ctx) (expand-expr (wrap-expression expr) (initial-context))))
        (syntax->ir stx ctx)))))
