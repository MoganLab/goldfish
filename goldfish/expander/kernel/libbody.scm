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
  (let loop ((stxs stxs) (ctx ctx) (var-defs '()) (exprs '()) (n 0))
    (when (> n 50000)
      (error "expand-library-body: expansion limit exceeded"))
    (if (null? stxs)
        (expand-library-finalize (reverse var-defs) (reverse exprs) ctx)
        (let* ((stx (car stxs))
               (resolved (lib-resolve-head stx ctx)))
          (cond
            ((eq? resolved 'define)
             (let-values (((var-def ctx1) (expand-lib-define-bind stx lib ctx)))
               (loop (cdr stxs) ctx1 (cons var-def var-defs) exprs (+ n 1))))
            ((eq? resolved 'define-syntax)
             (let-values (((ctx1) (expand-lib-define-syntax stx lib ctx)))
               (loop (cdr stxs) ctx1 var-defs exprs (+ n 1))))
            ((eq? resolved 'eval-when)
             ;; R7RS 7.1.3 library-body form: expand situation runs the
             ;; exprs at expand time; load/eval situations re-scan the
             ;; exprs as ordinary library-body forms (definitions get
             ;; recognized, expressions are deferred to finalize).
             (let* ((form (syntax-form stx))
                    (sit-datum (map syntax->datum (syntax-form (cadr form))))
                    (body-exprs (cddr form)))
               (let*-values (((ctx1)
                              (if (memq 'expand sit-datum)
                                (eval-when-expand! body-exprs ctx)
                                (values ctx))))
                 (if (or (memq 'load sit-datum) (memq 'eval sit-datum))
                   (loop (append body-exprs (cdr stxs))
                         ctx1 var-defs exprs (+ n 1))
                   (loop (cdr stxs) ctx1 var-defs exprs (+ n 1))))))
            ((eq? resolved 'begin)
             (loop (append (cdr (syntax-form stx)) (cdr stxs))
                   ctx var-defs exprs (+ n 1)))
            (else
             ;; Macro-headed form (e.g. define-macro): expand the head one
             ;; step at a time until the definition kind is revealed, WITHOUT
             ;; recursing into an expression body (mirrors intdef's
             ;; scan-head-loop).  Non-definition heads fall through and the
             ;; form is expanded as a body expression at finish time.
             (let*-values (((result ctx1) (scan-lib-head stx ctx)))
               (let ((resolved2 (lib-resolve-head result ctx1)))
                 (cond
                   ((eq? resolved2 'define)
                    (let-values (((var-def ctx2) (expand-lib-define-bind result lib ctx1)))
                      (loop (cdr stxs) ctx2 (cons var-def var-defs) exprs (+ n 1))))
                   ((eq? resolved2 'define-syntax)
                    (let-values (((ctx2) (expand-lib-define-syntax result lib ctx1)))
                      (loop (cdr stxs) ctx2 var-defs exprs (+ n 1))))
                   ((eq? resolved2 'begin)
                    (loop (append (cdr (syntax-form result)) (cdr stxs))
                          ctx1 var-defs exprs (+ n 1)))
                   (else
                    (loop (cdr stxs) ctx1 var-defs (cons stx exprs) (+ n 1))))))))))))

;;; scan-lib-head : syntax context -> (values syntax context)
;;; Expand the head of a top-level library form one macro step at a time
;;; (the scan phase), stopping at a definition head (define / define-syntax
;;; / begin) or a non-macro head.  This lets macro-generated definitions
;;; (e.g. define-macro -> define-syntax) be detected and dispatched by
;;; expand-library-body without expanding the body as an expression.

(define (scan-lib-head stx ctx)
  (let ((form (syntax-form stx)))
    (if (and (pair? form) (identifier? (car form)))
        (let*-values (((name binding)
                       (resolve-identifier (car form) ctx)))
          (if (and binding (transformer-binding? binding))
              (let*-values (((out ctx1)
                             (expand-macro-once stx ctx (binding-value binding))))
                (scan-lib-head out ctx1))
              (values stx ctx)))
        (values stx ctx))))

;;; expand-library-finalize : (list (name . val-stx)) (list syntax) context
;;;                           -> (values defs ctx)
;;; Expand definition values, then body expressions, with every
;;; definition bound.  Emits (define name val) forms followed by the
;;; initialization expressions.  Output wrappers carry an empty
;;; scope-set context (see intdef.scm body-output-source).

(define lib-output-source (make-syntax 'empty (stx-ctx-empty) #f))

(define (expand-library-finalize var-defs exprs ctx)
  (set-current-expand-context! ctx)
  (let*-values (((defs ctx1)
                 (let loop ((ds var-defs) (c ctx) (out '()))
                   (if (null? ds)
                       (values (reverse out) c)
                       ;; Prune use scopes off a macro-generated value so its
                       ;; free identifiers resolve against the library rather
                       ;; than the macro use site (mirrors def-bind!'s
                       ;; stx-prune-scopes on binders in expand.scm).
                       (let* ((ph (context-phase c))
                              (val (stx-prune-scopes (cdar ds)
                                                     (context-use-scopes c)
                                                     ph)))
                         (let*-values (((val-sexp c1) (expand-expr val c)))
                           (loop (cdr ds) c1
                                 (cons (datum->syntax lib-output-source
                                         `(define ,(caar ds) ,val-sexp))
                                       out))))))))
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
    ;; Macro alias: (define name macro) where macro resolves to a
    ;; transformer binding -- an s7 idiom (e.g. (liii raw-string)'s
    ;; (define deindent stx-deindent), (define &- stx-deindent)).  Register
    ;; name as an alias of the same transformer; the value expression is a
    ;; no-op (macro aliases have no runtime value).
    (let*-values (((vname vbinding) (resolve-identifier val-stx ctx)))
      (if (transformer-binding? vbinding)
          (let*-values (((name ctx2) (context-alloc-name ctx id)))
            (exp-library-define! lib (syntax-form id) vbinding)
            (values (cons name (datum->syntax val-stx '(if #f #f)))
                    (context-extend-env ctx2 name vbinding)))
          (let* ((ph (context-phase ctx))
                 (scp-i (context-intro-scope ctx))
                 (id (if (and scp-i (memq scp-i (syntax-scopes id ph)))
                         (stx-flip-scope id scp-i ph)
                         id))
                 (val-stx (if (and scp-i (memq scp-i (syntax-scopes val-stx ph)))
                              (stx-flip-scope val-stx scp-i ph)
                              val-stx)))
            (let*-values (((name ctx) (context-alloc-name ctx id))
                          ((ctx) (context-bind ctx id name))
                          ((ref) (make-toplevel-ref name lib (syntax-form id) #f))
                          ((ctx) (context-extend-env ctx name (make-toplevel-binding ref))))
              (exp-library-define! lib (syntax-form id) (make-toplevel-binding ref))
              (values (cons name val-stx) ctx)))))))

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
