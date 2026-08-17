;;; compiler.scm -- L2: the self-hosted compilation pipeline.
;;;
;;; Aggregate library re-exporting the L2 compiler front-end, split into
;;; three sub-libraries under goldfish/compiler/:
;;;   (goldfish compiler ir)       -- record IR + core->ir / ir->core
;;;   (goldfish compiler passes)   -- nanopass-style IR passes
;;;   (goldfish compiler bytecode) -- IR -> bytecode + validation
;;;
;;; The pipeline: expander lowers core IR (sexp), core->ir converts it to
;;; a record tree, passes rewrite the tree, and either ir->core hands the
;;; result back to the s7 evaluator or to-bytecode emits the VM bytecode.
;;; This aggregate keeps the (goldfish compiler) interface for the
;;; load-path integration and the existing test suite.

(define-library (goldfish compiler)
  (import (goldfish compiler ir)
          (goldfish compiler passes)
          (goldfish compiler bytecode))
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
    $call-with-values
    run-passes
    compile-defs
    constant-fold
    simplify-if
    inline
    *inline-max-effort*
    *inline-max-depth*
    eliminate-dead-defs
    tail-call-positions
    *foldable-functions*
    to-bytecode
    valid-bytecode?
    *bytecode-version*))
