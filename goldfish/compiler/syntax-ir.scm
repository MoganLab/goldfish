;;; syntax-ir.scm -- bridge: compiler pipeline via L4 tree-il.
;;;
;;; The pure syntax->ir walk now lives in (goldfish expander tree-il)
;;; (L4) so the expander can emit tree-il directly.  This library
;;; remains as the compiler's facade: it re-exports the L4 bridge and
;;; adds the pass/bytecode-aware helpers.

(define-library (goldfish compiler syntax-ir)
  (import (scheme base)
          (goldfish)
          (goldfish core ir)
          (goldfish expander tree-il)
          (goldfish compiler passes)
          (goldfish compiler bytecode))
  (export syntax->ir
    syntax->ir/sexp
    expand->ir
    compile-syntax-defs
    compile-syntax-program)
  (begin

    ;; compile-syntax-defs : (list syntax) context (list pass) -> (list sexp)
    (define (compile-syntax-defs defs ctx passes)
      (let rec ((ds defs) (acc '()))
        (if (null? ds)
          (reverse acc)
          (rec (cdr ds)
               (cons (ir->core (run-passes (syntax->ir/sexp (car ds) ctx) passes))
                     acc)))))

    ;; compile-syntax-program : (list syntax) context (list pass) -> program
    (define (compile-syntax-program defs ctx passes)
      (to-bytecode (map (lambda (d) (syntax->ir d ctx)) defs)))))
