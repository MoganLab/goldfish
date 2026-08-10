;;; transformer.scm
;;; Transformer evaluation: the phase seam between the expander and
;;; macro code.  eval-transformer expands a transformer expression at
;;; phase+1, lowers it, and evaluates it to a procedure.
;;;
;;; The only transformer-position syntax special-cased here is
;;; syntax-case, which is wrapped in a lambda; syntax-rules is an
;;; ordinary macro (defined in lib/syntax-case.scm) and expands through
;;; the normal macro path.  eval-transformer is thus the sole eval seam.

(define (make-syntax-rules-transformer stx . maybe-ctx)
  (let ((ctx (if (null? maybe-ctx) (context-empty) (car maybe-ctx))))
    (let-values (((proc _) (eval-transformer stx ctx)))
      proc)))

;;; transformer-spec->procedure-form : syntax -> syntax
;;; Rewrite a transformer spec into a procedure expression.
;;;   (syntax-case _ (lit ...) clause ...)   ; transformer position
;;;     -> (lambda (stx) (syntax-case stx (lit ...) clause ...))
;;; Anything already a procedure expression (e.g. (lambda ...)) is returned
;;; unchanged.  syntax-rules is not handled here: it is a macro.

(define (transformer-spec->procedure-form stx)
  (let ((form (syntax-form stx)))
    (if (and (pair? form) (identifier? (car form)))
        (let ((head (syntax-form (car form))))
          (if (eq? head 'syntax-case)
              (syntax-case-spec->procedure-form stx)
              stx))
        stx)))

(define (syntax-case-spec->procedure-form stx)
  (let* ((form (syntax-form stx))
         (ctx (syntax-context stx))
         (lib (syntax-library stx))
         (param (make-syntax (make-fresh-name 'stx) ctx lib)))
    (datum->syntax stx
      (list 'lambda
            (list param)
            (cons (car form) (cons param (cddr form)))))))

;;; eval-transformer : syntax context -> (values procedure context)
;;; Evaluate a transformer expression at expand time.  Transformer specs
;;; (syntax-rules, and syntax-case in transformer position) are first desugared
;;; to a procedural transformer; the result is then expanded at phase+1
;;; (phases-model let-syntax rule), lowered to core Scheme, and evaluated
;;; to a procedure in the expander API module (s7 eval falls back to
;;; rootlet for names the module does not define).

(define (eval-transformer stx ctx)
  (let* ((stx (transformer-spec->procedure-form stx))
         (ph (context-phase ctx))
         (ctx-up (context-at-phase ctx (+ ph 1))))
    (let*-values (((sexp ctx2) (expand-expr stx ctx-up)))
      (let ((proc (eval (lower sexp) the-expander-library)))
        (unless (procedure? proc)
          (error "eval-transformer: transformer must evaluate to a procedure"
                 (syntax->datum stx)))
        (values proc (context-return ctx ctx2))))))

;;; Library exports

(module-define! the-expander-library 'make-syntax-rules-transformer make-syntax-rules-transformer)
(module-define! the-expander-library 'eval-transformer eval-transformer)
