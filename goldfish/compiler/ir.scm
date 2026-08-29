;;; ir.scm -- shim re-exporting L2 IR (goldfish/core/ir.scm).
;;;
;;; The authoritative IR definition now lives in L2 (goldfish core ir)
;;; so the expander (L3/L4) can emit tree-il directly.  This library
;;; remains for backward compat and re-exports the same bindings with
;;; the same record identity.

(define-library (goldfish compiler ir)
  (import (goldfish core ir))
  (export core->ir
    ir->core
    core-language core-form? core-node-of validate-core-sexp
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
    make-primitive-ref primitive-ref? primitive-ref-source primitive-ref-name
    make-lexical-ref lexical-ref? lexical-ref-source lexical-ref-depth lexical-ref-index
    make-values values? values-source values-args
    make-call-with-values call-with-values? cwv-source cwv-producer cwv-consumer
    $const $void $define $lambda $if $begin $let $letrec $set! $call $values
    $call-with-values))
