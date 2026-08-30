;;; ir.scm -- shim re-exporting L2 IR (goldfish/core/ir.scm).
;;;
;;; The authoritative IR definition now lives in L2 (goldfish core ir)
;;; so the expander (L3/L4) can emit tree-il directly.  This library
;;; remains for backward compat and re-exports the same bindings with
;;; the same record identity.
;;;
;;; Node names follow Guile's (language tree-il); lexical addressing
;;; keeps goldfish's (depth . index) alongside the name.  The OLD
;;; goldfish node names (define?/if?/begin?/set!?/... and the list-valued
;;; lambda-body/let-body/begin-body accessors) are provided as a
;;; compatibility layer over the new records, so legacy compiler tests
;;; keep working unchanged.

(define-library (goldfish compiler ir)
  (import (prefix (goldfish core ir) core:)
          (goldfish compiler patterns))
  (export core->ir
    ir->core
    core-language core-form? core-node-of validate-core-sexp
    make-void void? void-source
    make-const const? const-source const-exp
    make-primitive-ref primitive-ref? primitive-ref-source primitive-ref-name
    make-lexical-ref lexical-ref? lexical-ref-source lexical-ref-name lexical-ref-depth lexical-ref-index
    make-lexical-set lexical-set? lexical-set-source lexical-set-name lexical-set-depth lexical-set-index lexical-set-exp
    make-conditional conditional? conditional-source conditional-test conditional-consequent conditional-alternate
    make-call call? call-source call-proc call-args
    make-primcall primcall? primcall-source primcall-name primcall-args
    make-seq seq? seq-source seq-head seq-tail
    make-lambda lambda? lambda-source lambda-meta
    make-lambda-case lambda-case? lambda-case-source
      lambda-case-req lambda-case-opt lambda-case-rest lambda-case-kw
      lambda-case-inits lambda-case-gensyms lambda-case-body lambda-case-alternate
    make-let let? let-source let-names let-gensyms let-vals
    make-letrec letrec? letrec-source letrec-in-order? letrec-names letrec-gensyms letrec-vals
    make-let-values let-values? let-values-source let-values-exp let-values-body
    make-values values? values-source values-args
    make-call-with-values call-with-values? cwv-source cwv-producer cwv-consumer
    make-toplevel-ref toplevel-ref? toplevel-ref-source toplevel-ref-name
    make-toplevel-set toplevel-set? toplevel-set-source toplevel-set-name toplevel-set-exp
    make-toplevel-define toplevel-define? toplevel-define-source toplevel-define-name toplevel-define-exp
    make-module-ref module-ref? module-ref-source module-ref-module module-ref-name module-ref-public?
    make-module-set module-set? module-set-source module-set-module module-set-name module-set-public? module-set-exp
    ;; legacy names (old goldfish node API)
    make-define define? define-source define-name define-value
    make-if if? if-source if-test if-then if-else
    make-begin begin? begin-source begin-body
    make-set! set!? set!-source set!-target set!-expr
    make-const const? const-source const-value
    make-lambda lambda? lambda-source lambda-formals lambda-body
    make-let let? let-source let-bindings let-body
    make-letrec letrec? letrec-source letrec-bindings letrec-body)
  (export $void $const $primitive-ref $lexical-ref $lexical-set
    $conditional $call $primcall $seq $lambda $lambda-case
    $let $letrec $fix $let-values
    $toplevel-ref $toplevel-set $toplevel-define $module-ref $module-set
    $define $if $begin $set!)
  (begin

    ;; --- new-record forwarding ---
    (define core->ir core:core->ir)
    (define ir->core core:ir->core)
    (define core-language core:core-language)
    (define core-form? core:core-form?)
    (define core-node-of core:core-node-of)
    (define validate-core-sexp core:validate-core-sexp)
    (define make-void core:make-void)
    (define void? core:void?)
    (define void-source core:void-source)
    (define make-const core:make-const)
    (define const? core:const?)
    (define const-source core:const-source)
    (define const-exp core:const-exp)
    (define make-primitive-ref core:make-primitive-ref)
    (define primitive-ref? core:primitive-ref?)
    (define primitive-ref-source core:primitive-ref-source)
    (define primitive-ref-name core:primitive-ref-name)
    (define make-lexical-ref core:make-lexical-ref)
    (define lexical-ref? core:lexical-ref?)
    (define lexical-ref-source core:lexical-ref-source)
    (define lexical-ref-name core:lexical-ref-name)
    (define lexical-ref-depth core:lexical-ref-depth)
    (define lexical-ref-index core:lexical-ref-index)
    (define make-lexical-set core:make-lexical-set)
    (define lexical-set? core:lexical-set?)
    (define lexical-set-source core:lexical-set-source)
    (define lexical-set-name core:lexical-set-name)
    (define lexical-set-depth core:lexical-set-depth)
    (define lexical-set-index core:lexical-set-index)
    (define lexical-set-exp core:lexical-set-exp)
    (define make-conditional core:make-conditional)
    (define conditional? core:conditional?)
    (define conditional-source core:conditional-source)
    (define conditional-test core:conditional-test)
    (define conditional-consequent core:conditional-consequent)
    (define conditional-alternate core:conditional-alternate)
    (define make-call core:make-call)
    (define call-source core:call-source)
    (define (call? x)
      (or (core:call? x) (core:values? x) (core:call-with-values? x)))
    (define (call-proc x)
      (cond ((core:call? x) (core:call-proc x))
            ((core:values? x) 'values)
            ((core:call-with-values? x) 'call-with-values)
            (else #f)))
    (define (call-args x)
      (cond ((core:call? x) (core:call-args x))
            ((core:values? x) (core:values-args x))
            ((core:call-with-values? x) (list (core:cwv-producer x) (core:cwv-consumer x)))
            (else #f)))
    (define make-primcall core:make-primcall)
    (define primcall? core:primcall?)
    (define primcall-source core:primcall-source)
    (define primcall-name core:primcall-name)
    (define primcall-args core:primcall-args)
    (define make-seq core:make-seq)
    (define seq? core:seq?)
    (define seq-source core:seq-source)
    (define seq-head core:seq-head)
    (define seq-tail core:seq-tail)
    (define make-lambda core:make-lambda)
    (define lambda? core:lambda?)
    (define lambda-source core:lambda-source)
    (define lambda-meta core:lambda-meta)
    (define make-lambda-case core:make-lambda-case)
    (define lambda-case? core:lambda-case?)
    (define lambda-case-source core:lambda-case-source)
    (define lambda-case-req core:lambda-case-req)
    (define lambda-case-opt core:lambda-case-opt)
    (define lambda-case-rest core:lambda-case-rest)
    (define lambda-case-kw core:lambda-case-kw)
    (define lambda-case-inits core:lambda-case-inits)
    (define lambda-case-gensyms core:lambda-case-gensyms)
    (define lambda-case-body core:lambda-case-body)
    (define lambda-case-alternate core:lambda-case-alternate)
    (define make-let core:make-let)
    (define let? core:let?)
    (define let-source core:let-source)
    (define let-names core:let-names)
    (define let-gensyms core:let-gensyms)
    (define let-vals core:let-vals)
    (define make-letrec core:make-letrec)
    (define letrec? core:letrec?)
    (define letrec-source core:letrec-source)
    (define letrec-in-order? core:letrec-in-order?)
    (define letrec-names core:letrec-names)
    (define letrec-gensyms core:letrec-gensyms)
    (define letrec-vals core:letrec-vals)
    (define make-let-values core:make-let-values)
    (define let-values? core:let-values?)
    (define let-values-source core:let-values-source)
    (define let-values-exp core:let-values-exp)
    (define let-values-body core:let-values-body)
    (define make-values core:make-values)
    (define values? core:values?)
    (define values-source core:values-source)
    (define values-args core:values-args)
    (define make-call-with-values core:make-call-with-values)
    (define call-with-values? core:call-with-values?)
    (define cwv-source core:cwv-source)
    (define cwv-producer core:cwv-producer)
    (define cwv-consumer core:cwv-consumer)
    (define make-toplevel-ref core:make-toplevel-ref)
    (define toplevel-ref? core:toplevel-ref?)
    (define toplevel-ref-source core:toplevel-ref-source)
    (define toplevel-ref-name core:toplevel-ref-name)
    (define make-toplevel-set core:make-toplevel-set)
    (define toplevel-set? core:toplevel-set?)
    (define toplevel-set-source core:toplevel-set-source)
    (define toplevel-set-name core:toplevel-set-name)
    (define toplevel-set-exp core:toplevel-set-exp)
    (define make-toplevel-define core:make-toplevel-define)
    (define toplevel-define? core:toplevel-define?)
    (define toplevel-define-source core:toplevel-define-source)
    (define toplevel-define-name core:toplevel-define-name)
    (define toplevel-define-exp core:toplevel-define-exp)
    (define make-module-ref core:make-module-ref)
    (define module-ref? core:module-ref?)
    (define module-ref-source core:module-ref-source)
    (define module-ref-module core:module-ref-module)
    (define module-ref-name core:module-ref-name)
    (define module-ref-public? core:module-ref-public?)
    (define make-module-set core:make-module-set)
    (define module-set? core:module-set?)
    (define module-set-source core:module-set-source)
    (define module-set-module core:module-set-module)
    (define module-set-name core:module-set-name)
    (define module-set-public? core:module-set-public?)
    (define module-set-exp core:module-set-exp)

    ;; --- legacy compatibility layer ---
    (define (seq->list s)
      (let loop ((s s) (acc '()))
        (cond ((core:void? s) (reverse acc))
              ((core:seq? s) (loop (core:seq-tail s) (cons (core:seq-head s) acc)))
              (else (reverse (cons s acc))))))
    (define (list->seq ls)
      (cond ((null? ls) (core:make-void #f))
            ((null? (cdr ls)) (car ls))
            (else (core:make-seq #f (car ls) (list->seq (cdr ls))))))
    (define (arity->formals req opt rest)
      (cond ((and (null? opt) rest) (append req rest))
            ((and (null? opt) (not rest)) req)
            ((null? rest) (append req opt))
            (else (append req opt (list rest)))))
    (define make-define core:make-toplevel-define)
    (define (define? x) (core:toplevel-define? x))
    (define define-source core:toplevel-define-source)
    (define define-name core:toplevel-define-name)
    (define define-value core:toplevel-define-exp)
    (define make-if core:make-conditional)
    (define (if? x) (core:conditional? x))
    (define if-source core:conditional-source)
    (define if-test core:conditional-test)
    (define if-then core:conditional-consequent)
    (define if-else core:conditional-alternate)
    (define (make-begin body) (list->seq body))
    (define (begin? x) (core:seq? x))
    (define begin-source core:seq-source)
    (define (begin-body ir) (seq->list ir))
    (define (make-set! source target expr) (core:make-lexical-set source target 0 0 expr))
    (define (set!? x)
      (or (core:lexical-set? x) (core:toplevel-set? x) (core:module-set? x)))
    (define (set!-source x)
      (cond ((core:lexical-set? x) (core:lexical-set-source x))
            ((core:toplevel-set? x) (core:toplevel-set-source x))
            ((core:module-set? x) (core:module-set-source x))
            (else #f)))
    (define (set!-target x)
      (cond ((core:lexical-set? x) (core:lexical-set-name x))
            ((core:toplevel-set? x) (core:toplevel-set-name x))
            ((core:module-set? x) (list 'module-ref (core:module-set-module x)
                                        (core:module-set-name x)))
            (else #f)))
    (define (set!-expr x)
      (cond ((core:lexical-set? x) (core:lexical-set-exp x))
            ((core:toplevel-set? x) (core:toplevel-set-exp x))
            ((core:module-set? x) (core:module-set-exp x))
            (else #f)))
    (define (lambda-formals ir)
      (let ((b (core:lambda-body ir)))
        (if (core:lambda-case? b)
          (arity->formals (core:lambda-case-req b)
                          (core:lambda-case-opt b)
                          (core:lambda-case-rest b))
          '())))
    (define (lambda-body ir)
      (let ((b (core:lambda-body ir)))
        (if (core:lambda-case? b)
          (seq->list (core:lambda-case-body b))
          '())))
    (define (let-bindings ir)
      (map (lambda (n v) (list n v)) (core:let-names ir) (core:let-vals ir)))
    (define (let-body ir) (seq->list (core:let-body ir)))
    (define (letrec-bindings ir)
      (map (lambda (n v) (list n v)) (core:letrec-names ir) (core:letrec-vals ir)))
    (define (letrec-body ir) (seq->list (core:letrec-body ir)))
    (define (const-value ir) (core:const-exp ir))))
