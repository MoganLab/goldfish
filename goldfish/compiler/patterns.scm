;;; patterns.scm -- L2: tree-il pattern-syntax aliases ($const/$lambda/...).
;;;
;;; Pattern names for the record IR (goldfish/core/ir.scm), defined as
;;; syntax that rewrites to (goldfish match) patterns.  They live in the
;;; compiler layer (not core) because they are compiler-facing conveniences;
;;; the IR itself is plain records.
;;;
;;; $<name> at a use site binds the CALLER's variables (in order) to the
;;; node's fields:
;;;   ($const v)          =>  (? const? (=> const-exp v))
;;;   ($lambda meta body) =>  (? lambda? (=> lambda-meta meta)
;;;                                    (=> lambda-body body))
;;; The source field is skipped (rarely needed in patterns); bindings run
;;; in field order after source.

(define-library (goldfish compiler patterns)
  (import (scheme base)
          (goldfish match))
  (export $void $const $primitive-ref $lexical-ref $lexical-set
    $conditional $call $primcall $seq $lambda $lambda-case
    $let $letrec $let-values $values $call-with-values
    $toplevel-ref $toplevel-set $toplevel-define $module-ref $module-set)
  (begin

    (define-syntax $void
      (lambda (stx)
        (datum->syntax stx '(? void?))))

    (define-syntax $const
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'const? (list '=> 'const-exp (cadr d)))))))

    (define-syntax $primitive-ref
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'primitive-ref? (list '=> 'primitive-ref-name (cadr d)))))))

    (define-syntax $lexical-ref
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'lexical-ref?
                  (list '=> 'lexical-ref-name (cadr d))
                  (list '=> 'lexical-ref-depth (caddr d))
                  (list '=> 'lexical-ref-index (cadddr d)))))))

    (define-syntax $lexical-set
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'lexical-set?
                  (list '=> 'lexical-set-name (cadr d))
                  (list '=> 'lexical-set-depth (caddr d))
                  (list '=> 'lexical-set-index (cadddr d))
                  (list '=> 'lexical-set-exp (car (cddddr d))))))))

    (define-syntax $conditional
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'conditional?
                  (list '=> 'conditional-test (cadr d))
                  (list '=> 'conditional-consequent (caddr d))
                  (list '=> 'conditional-alternate (cadddr d)))))))

    (define-syntax $call
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'call?
                  (list '=> 'call-proc (cadr d))
                  (list '=> 'call-args (caddr d)))))))

    (define-syntax $primcall
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'primcall?
                  (list '=> 'primcall-name (cadr d))
                  (list '=> 'primcall-args (caddr d)))))))

    (define-syntax $seq
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'seq?
                  (list '=> 'seq-head (cadr d))
                  (list '=> 'seq-tail (caddr d)))))))

    (define-syntax $lambda
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'lambda?
                  (list '=> 'lambda-meta (cadr d))
                  (list '=> 'lambda-body (caddr d)))))))

    (define-syntax $lambda-case
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'lambda-case?
                  (list '=> 'lambda-case-req (cadr d))
                  (list '=> 'lambda-case-opt (caddr d))
                  (list '=> 'lambda-case-rest (cadddr d))
                  (list '=> 'lambda-case-kw (car (cddddr d)))
                  (list '=> 'lambda-case-inits (cadr (cddddr d)))
                  (list '=> 'lambda-case-gensyms (caddr (cddddr d)))
                  (list '=> 'lambda-case-body (cadddr (cddddr d)))
                  (list '=> 'lambda-case-alternate (car (cddddr (cddddr d)))))))))

    (define-syntax $let
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'let?
                  (list '=> 'let-names (cadr d))
                  (list '=> 'let-gensyms (caddr d))
                  (list '=> 'let-vals (cadddr d))
                  (list '=> 'let-body (car (cddddr d))))))))

    (define-syntax $letrec
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'letrec?
                  (list '=> 'letrec-source (cadr d))
                  (list '=> 'letrec-in-order? (caddr d))
                  (list '=> 'letrec-names (cadddr d))
                  (list '=> 'letrec-gensyms (car (cddddr d)))
                  (list '=> 'letrec-vals (cadr (cddddr d)))
                  (list '=> 'letrec-body (caddr (cddddr d))))))))

    (define-syntax $let-values
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'let-values?
                  (list '=> 'let-values-exp (cadr d))
                  (list '=> 'let-values-body (caddr d)))))))

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

    (define-syntax $toplevel-ref
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'toplevel-ref? (list '=> 'toplevel-ref-name (cadr d)))))))

    (define-syntax $toplevel-set
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'toplevel-set?
                  (list '=> 'toplevel-set-name (cadr d))
                  (list '=> 'toplevel-set-exp (caddr d)))))))

    (define-syntax $toplevel-define
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'toplevel-define?
                  (list '=> 'toplevel-define-name (cadr d))
                  (list '=> 'toplevel-define-exp (caddr d)))))))

    (define-syntax $module-ref
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'module-ref?
                  (list '=> 'module-ref-module (cadr d))
                  (list '=> 'module-ref-name (caddr d))
                  (list '=> 'module-ref-public? (cadddr d)))))))

    (define-syntax $module-set
      (lambda (stx)
        (let ((d (syntax->datum stx)))
          (datum->syntax stx
            (list '? 'module-set?
                  (list '=> 'module-set-module (cadr d))
                  (list '=> 'module-set-name (caddr d))
                  (list '=> 'module-set-public? (cadddr d))
                  (list '=> 'module-set-exp (car (cddddr d))))))))
    ))
