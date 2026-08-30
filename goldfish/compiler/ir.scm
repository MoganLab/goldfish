;;; ir.scm -- thin forwarder for the L2 IR (goldfish/core/ir.scm).
;;;
;;; The authoritative IR definition lives in L2 (goldfish core ir) so the
;;; expander (L4) and compiler (L5) share one record contract.  This library
;;; forwards every binding of (goldfish core ir) plus the $ pattern-syntax
;;; aliases (goldfish compiler patterns), keeping the historical
;;; (goldfish compiler ir) library name for the load-path integration and
;;; the test suite.  There is NO legacy node API here anymore: consumers use
;;; the canonical names (toplevel-define?/conditional?/seq?/const-exp/...,
;;; <lambda> body is the <lambda-case> record, <let>/<letrec> carry flat
;;; names/gensyms/vals plus a single body expression).

(define-library (goldfish compiler ir)
  (import (prefix (goldfish core ir) core:)
          (goldfish compiler patterns))
  (export core->ir
    ir->core
    core-language core-form? core-node-of validate-core-sexp
    make-void void? void-source
    make-const const? const-source const-exp
    make-primitive-ref primitive-ref? primitive-ref-source primitive-ref-name
    make-lambda lambda? lambda-source lambda-meta lambda-body
    make-lambda-case lambda-case? lambda-case-source
      lambda-case-req lambda-case-opt lambda-case-rest lambda-case-kw
      lambda-case-inits lambda-case-gensyms lambda-case-body lambda-case-alternate
    make-toplevel-define toplevel-define? toplevel-define-source toplevel-define-name toplevel-define-exp
    make-lexical-ref lexical-ref? lexical-ref-source lexical-ref-name lexical-ref-depth lexical-ref-index
    make-lexical-set lexical-set? lexical-set-source lexical-set-name lexical-set-depth lexical-set-index lexical-set-exp
    make-let let? let-source let-names let-gensyms let-vals let-body
    make-letrec letrec? letrec-source letrec-in-order? letrec-names letrec-gensyms letrec-vals letrec-body
    make-let-values let-values? let-values-source let-values-exp let-values-body
    make-values values? values-source values-args
    make-call-with-values call-with-values? cwv-source cwv-producer cwv-consumer
    make-toplevel-ref toplevel-ref? toplevel-ref-source toplevel-ref-name
    make-toplevel-set toplevel-set? toplevel-set-source toplevel-set-name toplevel-set-exp
    make-module-ref module-ref? module-ref-source module-ref-module module-ref-name module-ref-public?
    make-module-set module-set? module-set-source module-set-module module-set-name module-set-public? module-set-exp
    make-conditional conditional? conditional-source conditional-test conditional-consequent conditional-alternate
    make-seq seq? seq-source seq-head seq-tail
    make-call call? call-source call-proc call-args
    make-primcall primcall? primcall-source primcall-name primcall-args
    $void $const $primitive-ref $lexical-ref $lexical-set
    $conditional $call $primcall $seq $lambda $lambda-case
    $let $letrec $let-values $values $call-with-values
    $toplevel-ref $toplevel-set $toplevel-define $module-ref $module-set)
  (begin
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
    (define make-lambda core:make-lambda)
    (define lambda? core:lambda?)
    (define lambda-source core:lambda-source)
    (define lambda-meta core:lambda-meta)
    (define lambda-body core:lambda-body)
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
    (define make-toplevel-define core:make-toplevel-define)
    (define toplevel-define? core:toplevel-define?)
    (define toplevel-define-source core:toplevel-define-source)
    (define toplevel-define-name core:toplevel-define-name)
    (define toplevel-define-exp core:toplevel-define-exp)
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
    (define make-let core:make-let)
    (define let? core:let?)
    (define let-source core:let-source)
    (define let-names core:let-names)
    (define let-gensyms core:let-gensyms)
    (define let-vals core:let-vals)
    (define let-body core:let-body)
    (define make-letrec core:make-letrec)
    (define letrec? core:letrec?)
    (define letrec-source core:letrec-source)
    (define letrec-in-order? core:letrec-in-order?)
    (define letrec-names core:letrec-names)
    (define letrec-gensyms core:letrec-gensyms)
    (define letrec-vals core:letrec-vals)
    (define letrec-body core:letrec-body)
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
    (define make-conditional core:make-conditional)
    (define conditional? core:conditional?)
    (define conditional-source core:conditional-source)
    (define conditional-test core:conditional-test)
    (define conditional-consequent core:conditional-consequent)
    (define conditional-alternate core:conditional-alternate)
    (define make-seq core:make-seq)
    (define seq? core:seq?)
    (define seq-source core:seq-source)
    (define seq-head core:seq-head)
    (define seq-tail core:seq-tail)
    (define make-call core:make-call)
    (define call-source core:call-source)
    (define call? core:call?)
    (define call-proc core:call-proc)
    (define call-args core:call-args)
    (define make-primcall core:make-primcall)
    (define primcall? core:primcall?)
    (define primcall-source core:primcall-source)
    (define primcall-name core:primcall-name)
    (define primcall-args core:primcall-args)))
