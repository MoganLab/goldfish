;;; compiler.scm -- L2: the self-hosted compilation pipeline.
;;;
;;; Aggregate library re-exporting the compiler front-end:
;;;   (goldfish core ir)          -- record IR (L2, tree-il, Guile-aligned)
;;;   (goldfish compiler patterns) -- $ pattern-syntax aliases
;;;   (goldfish compiler passes)   -- nanopass-style IR passes
;;;
;;; The pipeline: the expander emits tree-il directly (syntax->ir); passes
;;; rewrite the tree; ir->core hands the result back to the s7 evaluator
;;; (the single execution host).  This aggregate keeps the (goldfish
;;; compiler) interface for the load-path integration and the test suite.

(define-library (goldfish compiler)
  (import (goldfish))
  (import (goldfish core ir)
          (goldfish compiler patterns)
          (goldfish compiler passes))
  (export ir->core
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
    make-lambda lambda? lambda-source lambda-meta lambda-body
    make-lambda-case lambda-case? lambda-case-source
      lambda-case-req lambda-case-opt lambda-case-rest lambda-case-kw
      lambda-case-inits lambda-case-gensyms lambda-case-body lambda-case-alternate
    make-let let? let-source let-names let-gensyms let-vals let-body
    make-letrec letrec? letrec-source letrec-in-order? letrec-names letrec-gensyms letrec-vals letrec-body
    make-let-values let-values? let-values-source let-values-exp let-values-body
    make-values values? values-source values-args
    make-call-with-values call-with-values? cwv-source cwv-producer cwv-consumer
    make-toplevel-ref toplevel-ref? toplevel-ref-source toplevel-ref-name
    make-toplevel-set toplevel-set? toplevel-set-source toplevel-set-name toplevel-set-exp
    make-toplevel-define toplevel-define? toplevel-define-source toplevel-define-name toplevel-define-exp
    make-module-ref module-ref? module-ref-source module-ref-module module-ref-name module-ref-public?
    make-module-set module-set? module-set-source module-set-module module-set-name module-set-public? module-set-exp
    $void $const $primitive-ref $lexical-ref $lexical-set
    $conditional $call $primcall $seq $lambda $lambda-case
    $let $letrec $let-values $values $call-with-values
    $toplevel-ref $toplevel-set $toplevel-define $module-ref $module-set
    run-passes
    constant-fold
    simplify-if
    inline
    eliminate-dead-defs
    tail-call-positions
    *foldable-functions*

))
