;;; ir.scm -- L2: goldfish intermediate representation (record-based).
;;;
;;; A record-based, nanopass-friendly IR for the L2 compiler.  Lowered
;;; core IR sexps are converted into record trees via core->ir; passes
;;; rewrite the tree with (goldfish match) patterns ($const/$lambda/...);
;;; ir->core converts back for the s7-eval path, and the bytecode
;;; compiler consumes the tree directly.
;;;
;;; Records use the self-hosted R7RS vector layout: (vector <rtd>
;;; source field...).  The source field is currently always #f: the
;;; expander's syntax objects carry no location info yet (form/context/
;;; library only), so locations are a future extension.
;;;
;;; Atoms (symbols and self-evaluating data) stay as themselves -- only
;;; compound nodes are records.  A symbol is an identifier reference,
;;; resolved by the backend (frame slot vs. name) as before.

(define-library (goldfish compiler ir)
  (import (scheme base)
          (goldfish match))
  (export core->ir
    ir->core
    make-const const? const-source const-value
    make-void void? void-source
    make-define define? define-source define-name define-value
    make-lambda lambda? lambda-source lambda-formals lambda-body
    make-if if? if-source if-test if-then if-else
    make-begin begin? begin-source begin-body
    make-let let? let-source let-bindings let-body
    make-letrec letrec? letrec-source letrec-bindings letrec-body
    make-set! set!? set!-source set!-target set!-expr
    make-call call? call-source call-proc call-args
    make-values values? values-source values-args
    make-call-with-values call-with-values? cwv-source cwv-producer cwv-consumer
    $const $void $define $lambda $if $begin $let $letrec $set! $call $values
    $call-with-values)
  (begin

    ;; ------------------------------------------------------------------
    ;; IR node records.

    (define-record-type <const>
      (make-const source value)
      const?
      (source const-source)
      (value const-value))

    (define-record-type <void>
      (make-void source)
      void?
      (source void-source))

    (define-record-type <define>
      (make-define source name value)
      define?
      (source define-source)
      (name define-name)
      (value define-value))

    (define-record-type <lambda>
      (make-lambda source formals body)
      lambda?
      (source lambda-source)
      (formals lambda-formals)
      (body lambda-body))

    (define-record-type <if>
      (make-if source test then else)
      if?
      (source if-source)
      (test if-test)
      (then if-then)
      (else if-else))

    (define-record-type <begin>
      (make-begin source body)
      begin?
      (source begin-source)
      (body begin-body))

    (define-record-type <let>
      (make-let source bindings body)
      let?
      (source let-source)
      (bindings let-bindings)
      (body let-body))

    (define-record-type <letrec>
      (make-letrec source bindings body)
      letrec?
      (source letrec-source)
      (bindings letrec-bindings)
      (body letrec-body))

    (define-record-type <set!>
      (make-set! source target expr)
      set!?
      (source set!-source)
      (target set!-target)
      (expr set!-expr))

    (define-record-type <call>
      (make-call source proc args)
      call?
      (source call-source)
      (proc call-proc)
      (args call-args))

    (define-record-type <values>
      (make-values source args)
      values?
      (source values-source)
      (args values-args))

    (define-record-type <call-with-values>
      (make-call-with-values source producer consumer)
      call-with-values?
      (source cwv-source)
      (producer cwv-producer)
      (consumer cwv-consumer))

    ;; ------------------------------------------------------------------
    ;; Pattern-syntax: friendly pattern names for the IR nodes.  The R7RS
    ;; keywords (lambda/if/begin/let/set!/values/call-with-values) cannot
    ;; name patterns, so the aliases use a '$' prefix.

    (define-syntax $const
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'const? (list '=> 'const-value (cadr d)))))))

    (define-syntax $void
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'void? (list '=> 'void-source (cadr d)))))))

    (define-syntax $define
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'define?
                  (list '=> 'define-name (cadr d))
                  (list '=> 'define-value (caddr d)))))))

    (define-syntax $lambda
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'lambda?
                  (list '=> 'lambda-formals (cadr d))
                  (list '=> 'lambda-body (caddr d)))))))

    (define-syntax $if
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'if?
                  (list '=> 'if-test (cadr d))
                  (list '=> 'if-then (caddr d))
                  (list '=> 'if-else (cadddr d)))))))

    (define-syntax $begin
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'begin?
                  (list '=> 'begin-body (cadr d)))))))

    (define-syntax $let
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'let?
                  (list '=> 'let-bindings (cadr d))
                  (list '=> 'let-body (caddr d)))))))

    (define-syntax $letrec
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'letrec?
                  (list '=> 'letrec-source (cadr d))
                  (list '=> 'letrec-bindings (caddr d))
                  (list '=> 'letrec-body (cadddr d)))))))

    (define-syntax $set!
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'set!?
                  (list '=> 'set!-target (cadr d))
                  (list '=> 'set!-expr (caddr d)))))))

    (define-syntax $call
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'call?
                  (list '=> 'call-proc (cadr d))
                  (list '=> 'call-args (caddr d)))))))

    (define-syntax $values
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'values?
                  (list '=> 'values-args (cadr d)))))))

    (define-syntax $call-with-values
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'call-with-values?
                  (list '=> 'cwv-producer (cadr d))
                  (list '=> 'cwv-consumer (caddr d)))))))

    ;; ------------------------------------------------------------------
    ;; core->ir : sexp -> ir
    ;; Convert a lowered core IR form into an IR record tree.  Symbols
    ;; and self-evaluating atoms are left as-is; compound nodes become
    ;; records.  The tail of a core `if' with no alternative is #f.

    (define (core->ir sexp)
      (cond
        ((or (symbol? sexp) (not (pair? sexp))) sexp)
        (else
         (let ((head (car sexp)))
           (case head
             ((quote) (make-const #f (cadr sexp)))
             ((quote-syntax) (make-const #f (cadr sexp)))
             ((define)
              (if (symbol? (cadr sexp))
                (make-define #f (cadr sexp) (core->ir (caddr sexp)))
                (make-define #f (caadr sexp)
                             (core->ir (cons 'lambda
                                             (cons (cdadr sexp) (cddr sexp)))))))
             ((lambda)
              (make-lambda #f (cadr sexp) (map core->ir (cddr sexp))))
             ((if)
              (if (pair? (cdddr sexp))
                ;; an explicit else, even #f, is a real branch: wrap #f as
                ;; a const node so ir->core does not mistake it for the
                ;; absent-else marker (an atomic #f)
                (make-if #f (core->ir (cadr sexp)) (core->ir (caddr sexp))
                         (if (eq? (cadddr sexp) #f)
                           (make-const #f #f)
                           (core->ir (cadddr sexp))))
                (make-if #f (core->ir (cadr sexp)) (core->ir (caddr sexp)) #f)))
             ((begin)
              (make-begin #f (map core->ir (cdr sexp))))
              ((let)
               (if (symbol? (cadr sexp))
                 ;; named let: (let name ((v i) ...) body ...)
                 ;;   -> (letrec ((name (lambda (v ...) body ...)))
                 ;;              (name i ...))
                 (let* ((name (cadr sexp))
                        (bindings (caddr sexp))
                        (body (cdddr sexp)))
                   (make-letrec 'letrec
                                (list (list name
                                            (make-lambda #f (map car bindings)
                                                         (map core->ir body))))
                                (list (make-call #f name
                                                 (map core->ir
                                                      (map cadr bindings))))))
                 (make-let #f
                           (map (lambda (b) (list (car b) (core->ir (cadr b))))
                                (cadr sexp))
                           (map core->ir (cddr sexp)))))
              ((let*)
               ;; let*: sequential bindings -> nested lets (returns a single
               ;; <let> record; the innermost body holds the expression list)
               (let rec ((bs (cadr sexp)))
                 (if (null? (cdr bs))
                   (make-let #f (list (list (caar bs) (core->ir (cadar bs))))
                             (map core->ir (cddr sexp)))
                   (make-let #f (list (list (caar bs) (core->ir (cadar bs))))
                             (list (rec (cdr bs)))))))
              ((letrec letrec*)
              (make-letrec head
                           (map (lambda (b) (list (car b) (core->ir (cadr b))))
                                (cadr sexp))
                           (map core->ir (cddr sexp))))
             ((set!)
              (make-set! #f (cadr sexp) (core->ir (caddr sexp))))
             ((values)
              (make-values #f (map core->ir (cdr sexp))))
             ((call-with-values)
              (make-call-with-values #f (core->ir (cadr sexp))
                                     (core->ir (caddr sexp))))
             (else
              (make-call #f (core->ir head) (map core->ir (cdr sexp)))))))))

    ;; ------------------------------------------------------------------
    ;; ir->core : ir -> sexp
    ;; Convert an IR record tree back to lowered core IR.  Used by the
    ;; s7-eval path (pass output is handed to the host evaluator).
    ;; Records are not pairs, so the node predicates are checked first.

    (define (ir->core ir)
      (cond
        ((const? ir)
         (let ((v (const-value ir)))
           (if (or (number? v) (string? v) (char? v) (boolean? v)
                   (null? v) (eof-object? v))
             v
             (list 'quote v))))
        ((define? ir) (list 'define (define-name ir) (ir->core (define-value ir))))
        ((lambda? ir)
         (cons 'lambda (cons (lambda-formals ir)
                             (map ir->core (lambda-body ir)))))
        ((if? ir)
         (let ((else (if-else ir)))
           (if else
             (list 'if (ir->core (if-test ir)) (ir->core (if-then ir))
                   (ir->core else))
             (list 'if (ir->core (if-test ir)) (ir->core (if-then ir))))))
        ((begin? ir)
         (cons 'begin (map ir->core (begin-body ir))))
        ((let? ir)
         (cons 'let
               (cons (map (lambda (b) (list (car b) (ir->core (cadr b))))
                          (let-bindings ir))
                     (map ir->core (let-body ir)))))
        ((letrec? ir)
         (let ((name (letrec-source ir)))
           (cons (if (symbol? name) name 'letrec)
                 (cons (map (lambda (b) (list (car b) (ir->core (cadr b))))
                            (letrec-bindings ir))
                       (map ir->core (letrec-body ir))))))
        ((set!? ir)
         (list 'set! (set!-target ir) (ir->core (set!-expr ir))))
        ((values? ir)
         (cons 'values (map ir->core (values-args ir))))
        ((call-with-values? ir)
         (list 'call-with-values (ir->core (cwv-producer ir))
               (ir->core (cwv-consumer ir))))
        ((call? ir)
         (cons (ir->core (call-proc ir)) (map ir->core (call-args ir))))
        ((or (symbol? ir) (not (pair? ir))) ir)
        (else (error "ir->core: unknown IR node" ir))))

    )) ;begin
