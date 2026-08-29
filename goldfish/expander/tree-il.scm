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
;;;   - toplevel / unbound -> bare symbol
;;;
;;; Lexical addressing mirrors the backend's frame model.  Computing it
;;; here lets the backend consume pre-resolved addresses instead of
;;; re-deriving them.
;;;
;;; This is the L4 side of "expander directly emits tree-il": the IR
;;; definition lives in L2 (goldfish core ir), this bridge lives in L4
;;; and is imported by both the expander API and the compiler's
;;; syntax-ir wrapper.  No L3->L5 dependency.

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
      (let ((d (if (syntax? f) (syntax->datum f) f)))
        (cond ((symbol? d) d)
              ((null? d) '())
              ((pair? d)
               (let loop ((p d) (acc '()))
                 (cond ((null? p) (reverse acc))
                       ((pair? p)
                        (loop (cdr p) (cons (if (syntax? (car p)) (syntax->datum (car p)) (car p)) acc)))
                       (else (reverse (cons (if (syntax? p) (syntax->datum p) p) acc))))))
              (else d))))

    (define (syntax->ir* stx ctx env resolve-lexical?)
      (cond
        ((not (syntax? stx)) stx)
        (else
         (let ((form (syntax-form stx)))
           (cond
             ((not (pair? form))
              (if (symbol? form)
                (let*-values (((name kind) (resolve-name stx ctx)))
                  (let ((loc (env-lookup env name)))
                    (cond
                      ((and loc resolve-lexical?)
                       (make-lexical-ref #f (car loc) (cdr loc)))
                      (loc
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
                            (dformals (formals->datum (cdr df)))
                            (dnames (lambda-formals->list dformals))
                            (env1 (env-extend-frame env dnames)))
                       (make-define #f dname
                                    (make-lambda #f dformals
                                                 (map (lambda (b) (syntax->ir* b ctx env1 resolve-lexical?))
                                                      (cddr form)))))))
                  ((lambda)
                   (let* ((dformals (formals->datum (syntax-form (cadr form))))
                          (dnames (lambda-formals->list dformals))
                          (env1 (env-extend-frame env dnames)))
                     (make-lambda #f dformals
                                  (map (lambda (b) (syntax->ir* b ctx env1 resolve-lexical?))
                                       (cddr form)))))
                  ((if)
                   (let ((else-stx (and (pair? (cdddr form)) (cadddr form))))
                     (let ((test-ir (syntax->ir* (cadr form) ctx env resolve-lexical?))
                           (then-ir (syntax->ir* (caddr form) ctx env resolve-lexical?))
                           (else-ir (if else-stx
                                      (let ((ir (syntax->ir* else-stx ctx env resolve-lexical?)))
                                        (if (eq? ir #f) (make-const #f #f) ir))
                                      #f)))
                       (make-if #f test-ir then-ir else-ir))))
                  ((begin)
                   (make-begin #f (map (lambda (b) (syntax->ir* b ctx env resolve-lexical?))
                                       (cdr form))))
                  ((let)
                   (if (symbol? (syntax-form (cadr form)))
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
                  (else
                   (make-call #f (syntax->ir* head ctx env resolve-lexical?)
                              (map (lambda (a) (syntax->ir* a ctx env resolve-lexical?))
                                   (cdr form))))))))))))

    (define (syntax->ir stx ctx)
      (syntax->ir* stx ctx '() #t))

    (define (syntax->ir/sexp stx ctx)
      (syntax->ir* stx ctx '() #f))

    (define (expand->ir expr)
      (let*-values (((stx ctx) (expand-expr (wrap-expression expr) (initial-context))))
        (syntax->ir stx ctx)))))
