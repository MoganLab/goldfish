;;; libbody.scm
;;; Expand a program body (define / define-syntax / begin / expressions)
;;; into an exp-library: definitions install their bindings into the
;;; library at scan time; their value expressions and body expressions
;;; are deferred until all definitions are bound (R7RS 5.5: definitions
;;; are visible throughout the body), mirroring the intdef scan/finish
;;; split.  Macro-generated definitions are detected via
;;; expand-body-form's define/define-syntax stop frame.
;;;
;;; This is the generic library mechanism behind both the driver's
;;; base-library boot (lib/core-macros.scm is ordinary object-level
;;; source expanded by the expander itself) and the R7RS define-library
;;; surface (module/library.scm).

(define (lib-resolve-head stx ctx)
  (and (syntax? stx)
       (pair? (syntax-form stx))
       (identifier? (car (syntax-form stx)))
       (context-resolve ctx (car (syntax-form stx)))))

(define (expand-library-body stxs lib ctx)
  (let loop ((stxs stxs) (ctx ctx) (var-defs '()) (exprs '()))
    (if (null? stxs)
        (expand-library-finalize (reverse var-defs) (reverse exprs) ctx)
        (let* ((stx (car stxs))
               (resolved (lib-resolve-head stx ctx)))
          (cond
            ((eq? resolved 'define)
             (let-values (((var-def ctx1) (expand-lib-define-bind stx lib ctx)))
               (loop (cdr stxs) ctx1 (cons var-def var-defs) exprs)))
            ((eq? resolved 'define-syntax)
             (let-values (((ctx1) (expand-lib-define-syntax stx lib ctx)))
               (loop (cdr stxs) ctx1 var-defs exprs)))
            ((eq? resolved 'begin)
             (loop (append (cdr (syntax-form stx)) (cdr stxs)) ctx var-defs exprs))
            (else
             (let*-values (((result ctx1) (expand-body-form stx ctx)))
               (let ((resolved2 (lib-resolve-head result ctx1)))
                 (cond
                   ((eq? resolved2 'define)
                    (let-values (((var-def ctx2) (expand-lib-define-bind result lib ctx1)))
                      (loop (cdr stxs) ctx2 (cons var-def var-defs) exprs)))
                   ((eq? resolved2 'define-syntax)
                    (let-values (((ctx2) (expand-lib-define-syntax result lib ctx1)))
                      (loop (cdr stxs) ctx2 var-defs exprs)))
                   ((eq? resolved2 'begin)
                    (loop (append (cdr (syntax-form result)) (cdr stxs)) ctx1 var-defs exprs))
                   (else
                    (loop (cdr stxs) ctx1 var-defs (cons stx exprs))))))))))))

;;; expand-library-finalize : (list (name . val-stx)) (list syntax) context
;;;                           -> (values defs ctx)
;;; Expand definition values, then body expressions, with every
;;; definition bound.  Emits (define name val) forms followed by the
;;; initialization expressions.  Output wrappers carry an empty
;;; scope-set context (see intdef.scm body-output-source).

(define lib-output-source (make-syntax 'empty (stx-ctx-empty) #f))

(define (expand-library-finalize var-defs exprs ctx)
  (let*-values (((defs ctx1)
                 (let loop ((ds var-defs) (c ctx) (out '()))
                   (if (null? ds)
                       (values (reverse out) c)
                       (let*-values (((val-sexp c1) (expand-expr (cdar ds) c)))
                         (loop (cdr ds) c1
                               (cons (datum->syntax lib-output-source
                                       `(define ,(caar ds) ,val-sexp))
                                     out)))))))
    (let*-values (((expr-sexps ctx2)
                   (let loop ((es exprs) (c ctx1) (out '()))
                     (if (null? es)
                         (values (reverse out) c)
                         (let*-values (((sexp c1) (expand-expr (car es) c)))
                           (loop (cdr es) c1 (cons sexp out)))))))
      (values (append defs expr-sexps) ctx2))))

;;; expand-lib-define-bind : syntax exp-library context
;;;                        -> (values (name . val-stx) context)
;;; Scan phase of a value definition: allocate the name and install the
;;; bindings (env + library table); the value expression is deferred.

(define (expand-lib-define-bind stx lib ctx)
  (let*-values (((id val-stx) (parse-internal-define stx)))
    (let*-values (((name ctx) (context-alloc-name ctx id))
                  ((ctx) (context-bind ctx id name))
                  ((ref) (make-toplevel-ref name lib (syntax-form id) #f))
                  ((ctx) (context-extend-env ctx name (make-toplevel-binding ref))))
      (exp-library-define! lib (syntax-form id) (make-toplevel-binding ref))
      (values (cons name val-stx) ctx))))

(define (expand-lib-define-syntax stx lib ctx)
  (let* ((form (syntax-form stx))
         (id (cadr form))
         (transformer-stx (caddr form)))
    (let*-values (((proc ctx) (eval-transformer transformer-stx ctx)))
      (let*-values (((name ctx) (context-alloc-name ctx id)))
        (exp-library-define! lib (syntax-form id) (make-transformer-binding proc))
        (values (context-extend-env (context-bind ctx id name)
                                    name
                                    (make-transformer-binding proc)))))))

(module-define! the-expander-library 'expand-library-body expand-library-body)
(module-define! the-expander-library 'expand-library-finalize expand-library-finalize)
(module-define! the-expander-library 'expand-lib-define-bind expand-lib-define-bind)
(module-define! the-expander-library 'expand-lib-define-syntax expand-lib-define-syntax)
