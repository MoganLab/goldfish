;;; core-forms.scm
;;; Handlers for core special forms.
;;; Each handler has signature (stx ctx) -> (values syntax ctx).
;;; Output is fully-expanded syntax; `lower' (expand.scm) converts it
;;; to evaluable core Scheme at the compile-program boundary.

(define (require-identifier stx msg)
  (unless (identifier? stx)
    (error msg stx)))

(define (stx-cadr stx) (cadr (syntax-form stx)))

(define void-expr '(if #f #f))

;;; lambda
;;; Parameter lists follow R7RS 7.3: proper (x y), rest-only x, or
;;; dotted (x y . z).  Output mirrors the input shape with allocated
;;; gensyms (s7 evaluates dotted lambdas natively).

(define (core-lambda stx ctx)
  (let* ((form (syntax-form stx))
         (params-stx (cadr form))
         (body-stxs (cddr form))
         (ph (context-phase ctx)))
    (let*-values (((fixed-ids rest-id) (parse-lambda-params params-stx)))
      (let*-values (((scp ctx1) (context-alloc-scope ctx)))
        (let*-values (((names ctx2)
                       (expand-lambda-bindings fixed-ids ctx1 scp ph))
                      ((rest-name ctx3)
                       (if rest-id
                           (expand-lambda-binding rest-id ctx2 scp ph)
                           (values #f ctx2))))
          (let ((ctx4 (context-reset-use-scopes (context-add-prune-scope ctx3 scp))))
            (let*-values (((body-sexp ctx5)
                           (expand-body (map (lambda (s) (stx-add-scope s scp ph)) body-stxs) ctx4)))
              (let ((params (if rest-name
                                (append names rest-name)
                                names)))
                (values (datum->syntax stx `(lambda ,params ,body-sexp))
                        (context-return ctx ctx5))))))))))

;;; parse-lambda-params : syntax -> (values fixed-ids rest-id)

(define (parse-lambda-params params-stx)
  (cond
    ((identifier? params-stx)
     (values '() params-stx))
    ((and (syntax? params-stx) (pair? (syntax-form params-stx)))
     (let loop ((p (syntax-form params-stx)) (fixed '()))
       (cond
         ((pair? p)
          (loop (cdr p)
                (cons (car p) fixed)))
         ((null? p) (values (reverse fixed) #f))
         ((identifier? p) (values (reverse fixed)
                                  p))
         (else (error "lambda: malformed parameter list"
                      (syntax->datum params-stx))))))
    ((and (syntax? params-stx) (null? (syntax-form params-stx)))
     (values '() #f))
    (else
     (error "lambda: malformed parameter list"
            (syntax->datum params-stx)))))

;;; expand-lambda-binding : syntax context scope phase -> (values name ctx)

(define (expand-lambda-binding id ctx scp ph)
  (let ((id (stx-add-scope id scp ph)))
    (require-identifier id "lambda: expected identifier")
    (let*-values (((name ctx1) (context-alloc-name ctx id)))
      (values name
              (context-extend-env (context-bind ctx1 id name)
                                  name
                                  (make-lexical-binding name))))))

(define (expand-lambda-bindings ids ctx scp ph)
  (let loop ((ids ids) (c ctx) (names '()))
    (if (null? ids)
        (values (reverse names) c)
        (let*-values (((name c1) (expand-lambda-binding (car ids) c scp ph)))
          (loop (cdr ids) c1 (cons name names))))))

;;; quote

(define (core-quote stx ctx)
  (let ((datum (syntax->datum (stx-cadr stx))))
    (values (datum->syntax stx `(quote ,datum)) ctx)))

;;; syntax
;;; (syntax stx) -> (quote-syntax stx_pruned), where stx_pruned is the
;;; template with the accumulated prune-scopes (scps_p) removed at the
;;; current phase.  quote-syntax is an internal form (cf. Racket): it
;;; passes through expansion untouched and `lower' renders it as
;;; (quote <syntax>), so the quoted value stays a syntax object --
;;; instantiate (transformer code) and the pruning tests rely on this.
;;; Plain `quote' datums its content instead.

(define (core-syntax stx ctx)
  (let* ((template (stx-cadr stx))
         (scps-p (context-prune-scopes ctx))
         (pruned (if (null? scps-p)
                     template
                     (stx-prune-scopes template scps-p (context-phase ctx)))))
    (values (datum->syntax stx (list 'quote-syntax pruned)) ctx)))

;;; quote-syntax: syntax literal; passes through expansion as-is.

(define (core-quote-syntax stx ctx)
  (values stx ctx))

;;; if

(define (core-if stx ctx)
  (let ((form (syntax-form stx)))
    (cond
      ((= 3 (length form))
       (let*-values (((c-sexp c1) (expand-expr (cadr form) ctx))
                     ((t-sexp c2) (expand-expr (caddr form) c1)))
         (values (datum->syntax stx `(if ,c-sexp ,t-sexp ,void-expr)) c2)))
      ((= 4 (length form))
       (let*-values (((c-sexp c1) (expand-expr (cadr form) ctx))
                     ((t-sexp c2) (expand-expr (caddr form) c1))
                     ((e-sexp c3) (expand-expr (cadddr form) c2)))
         (values (datum->syntax stx `(if ,c-sexp ,t-sexp ,e-sexp)) c3)))
      (else
       (error "if: expected (if cond then [else])" form)))))

;;; begin
;;; A begin with a stopped (tstop-headed) subform is returned unexpanded
;;; so the enclosing definition-context scan can see the stopped
;;; definition (core-begin and body expansion trap).

(define (stopped-form? stx ctx)
  (and (syntax? stx)
       (pair? (syntax-form stx))
       (identifier? (car (syntax-form stx)))
       (let*-values (((name binding)
                      (resolve-identifier (car (syntax-form stx)) ctx)))
         (tstop-binding? binding))))

(define (core-begin stx ctx)
  (let* ((form (syntax-form stx))
         (body (cdr form)))
    (if (any (lambda (s) (stopped-form? s ctx)) body)
        (values stx ctx)
        ;; Expand the body through intdef so internal definitions in an
        ;; expression begin work ((begin (define x 1) x) -- s7 allows
        ;; statements in begin bodies; cf. the (expected (begin (define
        ;; ans 42) (expt ...))) idiom in liii/packrat's tests).
        (if (null? body)
            (values (datum->syntax stx '(if #f #f)) ctx)
            (let*-values (((body-sexp ctx1) (expand-body body ctx)))
              (values body-sexp ctx1))))))

;;; set!
;;; (set! var val) assigns an identifier.  (set! (proc args ...) val) is the
;;; SRFI-17 generalized form, lowered to ((setter proc) args ... val);
;;; goldfish's s7-derived libraries (e.g. (liii logging)'s exit-hook
;;; registration) rely on it, as do the R7RS (set! (car x) v) positions.

(define (core-set! stx ctx)
  (let ((form (syntax-form stx)))
    (unless (= 3 (length form))
      (error "set!: expected (set! var val)" form))
    (let ((var-stx (cadr form))
          (val-stx (caddr form)))
      (if (pair? (syntax-form var-stx))
        (let* ((target-form (syntax-form var-stx))
               (proc-stx (car target-form))
               (arg-stxs (cdr target-form)))
          (let*-values (((proc-sexp ctx1) (expand-expr proc-stx ctx))
                        ((args-sexp ctx2) (expand-list arg-stxs ctx1))
                        ((val-sexp ctx3) (expand-expr val-stx ctx2)))
            (values (datum->syntax stx
                     `((setter ,proc-sexp) ,@args-sexp ,val-sexp))
                    ctx3)))
        (begin
          (require-identifier var-stx "set!: expected identifier")
          (let*-values (((name binding) (resolve-identifier var-stx ctx)))
            (cond
              ((core-form-binding? binding)
               (error "set!: cannot assign keyword" (syntax-form var-stx)))
              ((primitive-binding? binding)
               (error "set!: cannot assign primitive" (syntax-form var-stx)))
              ((and (toplevel-binding? binding)
                    (toplevel-ref-exported? (binding-value binding)))
               (error "set!: cannot assign exported module binding"
                      (syntax-form var-stx))))
            (let*-values (((val-sexp ctx1) (expand-expr val-stx ctx)))
              (let ((target (if (toplevel-binding? binding)
                                (emit-toplevel-ref (binding-value binding) var-stx)
                                name)))
                (values (datum->syntax stx `(set! ,target ,val-sexp)) ctx1)))))))))

;;; letrec* / letrec -- the recursive-binding core forms.
;;; `letrec' is a core form (not a macro): R7RS gives implementations a
;;; choice for letrec's init evaluation order, and s7's native letrec
;;; enforces the strict semantics (referencing an as-yet-uninitialized
;;; binding in an init is an error), which the letrec* expansion would
;;; silently permit.  The expander therefore emits `letrec' as-is and
;;; lets the host evaluate it with its R7RS semantics; letrec* remains
;;; the expander's own emission target for internal defines.

(define (expand-letrec-form stx ctx form-name)
  (let* ((form (syntax-form stx))
         (binding-stxs (syntax-form (cadr form)))
         (body-stxs (cddr form))
         (ph (context-phase ctx)))
    (let*-values (((scp ctx1) (context-alloc-scope ctx)))
      (let*-values (((names ctx2) (expand-letrec-allocate binding-stxs ctx1 scp ph)))
        (let*-values (((inits ctx3)
                       (expand-letrec-inits binding-stxs ctx2 names scp ph)))
          (let*-values (((body-sexp ctx4)
                         (expand-body (map (lambda (s) (stx-add-scope s scp ph)) body-stxs) (context-reset-use-scopes (context-add-prune-scope ctx3 scp)))))
            (values (datum->syntax stx `(,form-name ,(map list names inits) ,body-sexp))
                    (context-return ctx ctx4))))))))

(define (core-letrec* stx ctx) (expand-letrec-form stx ctx 'letrec*))
(define (core-letrec stx ctx) (expand-letrec-form stx ctx 'letrec))

(define (expand-letrec-allocate binding-stxs ctx scp ph)
  (let loop ((bs binding-stxs) (c ctx) (names '()))
    (if (null? bs)
        (values (reverse names) c)
        (let* ((bs-stx (car bs))
               (b (syntax-form bs-stx))
               (id (stx-add-scope (car b) scp ph)))
          (require-identifier id "letrec: expected identifier")
          (let*-values (((name c*) (context-alloc-name c id)))
            (loop (cdr bs)
                  (context-extend-env (context-bind c* id name)
                                      name
                                      (make-lexical-binding name))
                  (cons name names)))))))

(define (expand-letrec-inits binding-stxs ctx names scp ph)
  (let loop ((bs binding-stxs) (c (context-reset-use-scopes ctx)) (inits '()))
    (if (null? bs)
        (values (reverse inits) c)
        (let* ((bs-stx (car bs))
               (b (syntax-form bs-stx))
               (init-stx (stx-add-scope (cadr b) scp ph)))
          (let*-values (((init-sexp c*) (expand-expr init-stx c)))
            (loop (cdr bs) c* (cons init-sexp inits)))))))

;;; define (not valid in expression position)

(define (core-define stx ctx)
  (error "define: not valid in expression position" (syntax-form stx)))

;;; define-syntax (not valid in expression position)

(define (core-define-syntax stx ctx)
  (error "define-syntax: not valid in expression position" (syntax-form stx)))

;;; let-syntax

(define (core-let-syntax stx ctx)
  (let* ((form (syntax-form stx))
         (binding-stxs (syntax-form (cadr form)))
         (body-stxs (cddr form))
         (ph (context-phase ctx)))
    (let*-values (((scp ctx1) (context-alloc-scope ctx)))
      (let*-values (((ctx2) (expand-syntax-bindings binding-stxs ctx1 scp ph)))
        (let ((ctx3 (context-reset-use-scopes (context-add-prune-scope ctx2 scp))))
          (let*-values (((sexp ctx4)
                         (expand-body (map (lambda (s) (stx-add-scope s scp ph)) body-stxs) ctx3)))
            (values sexp (context-return ctx ctx4))))))))

;;; letrec-syntax
;;; Like let-syntax, but scp is also added to the transformer RHS
;;; so template identifiers carry the binding scope, enabling
;;; recursive macro definitions (paper §3, pattern-macros.scrbl:241).

(define (core-letrec-syntax stx ctx)
  (let* ((form (syntax-form stx))
         (binding-stxs (syntax-form (cadr form)))
         (body-stxs (cddr form))
         (ph (context-phase ctx)))
    (let*-values (((scp ctx1) (context-alloc-scope ctx)))
      (let*-values (((ctx2) (expand-syntax-bindings/rec binding-stxs ctx1 scp ph)))
        (let ((ctx3 (context-reset-use-scopes (context-add-prune-scope ctx2 scp))))
          (let*-values (((sexp ctx4)
                         (expand-body (map (lambda (s) (stx-add-scope s scp ph)) body-stxs) ctx3)))
            (values sexp (context-return ctx ctx4))))))))

(define (expand-syntax-bindings/rec binding-stxs ctx scp ph)
  (if (null? binding-stxs)
      (values ctx)
      (let* ((bs-stx (car binding-stxs))
             (b (syntax-form bs-stx))
             (id (stx-add-scope (car b) scp ph))
             (transformer-stx (stx-add-scope (cadr b) scp ph)))
        (require-identifier id "letrec-syntax: expected identifier")
        (let*-values (((proc ctx0 _) (eval-transformer transformer-stx ctx)))
          (let*-values (((name ctx1) (context-alloc-name ctx0 id)))
            (let ((ctx2 (context-extend-env (context-bind ctx1 id name)
                                            name
                                            (make-transformer-binding proc))))
              (expand-syntax-bindings/rec (cdr binding-stxs) ctx2 scp ph)))))))

(define (expand-syntax-bindings binding-stxs ctx scp ph)
  (if (null? binding-stxs)
      (values ctx)
      (let* ((bs-stx (car binding-stxs))
             (b (syntax-form bs-stx))
             (id (stx-add-scope (car b) scp ph))
             (transformer-stx (cadr b)))
        (require-identifier id "let-syntax: expected identifier")
        (let*-values (((proc ctx0 _) (eval-transformer transformer-stx ctx)))
          (let*-values (((name ctx1) (context-alloc-name ctx0 id)))
            (let ((ctx2 (context-extend-env (context-bind ctx1 id name)
                                            name
                                            (make-transformer-binding proc))))
              (expand-syntax-bindings (cdr binding-stxs) ctx2 scp ph)))))))

;;; quasiquote
;;; Fully desugar (quasiquote template) into core Scheme list/cons/append
;;; construction, so templates may reference scope-renamed bindings (a
;;; simple passthrough would leave bare identifiers unrenamed and break
;;; references to renamed lambda params).  Literal (non-unquoted)
;;; identifiers are quoted as datum symbols; `unquote`/`unquote-splicing`
;;; at depth 1 are expanded as expressions.

(define (qq-head form)
  (let ((h (if (pair? form) (car form) form)))
    (if (syntax? h) (syntax-form h) h)))

(define (qq-atom stx ctx)
  (let ((form (syntax-form stx)))
    (if (symbol? form)
        (values (datum->syntax stx (list 'quote form)) ctx)
        (values stx ctx))))

(define (qq-unquote-form? stx)
  (and (syntax? stx)
       (pair? (syntax-form stx))
       (let ((h (qq-head (syntax-form stx))))
         (or (eq? h 'unquote) (eq? h 'unquote-splicing)))))

(define (qq-list stx ctx depth)
  (let ((form (syntax-form stx)))
    (cond
      ((null? form)
       (values (datum->syntax stx '()) ctx))
      ((pair? form)
       (let* ((e (syntax-e stx))
              (first (car e))
              (rest (cdr e))
              (f (syntax-form first))
              (is-splice (and (pair? f)
                              (eq? (qq-head f) 'unquote-splicing)
                              (= depth 1))))
         (let*-values (((head ctx1)
                        (if is-splice
                            (expand-expr (stx-cadr first) ctx)
                            (qq-expand first ctx depth))))
           (let*-values (((tail ctx2)
                          (cond
                            ((qq-unquote-form? rest)
                             (qq-expand rest ctx1 depth))
                            ((syntax? rest)
                             (qq-list rest ctx1 depth))
                            (else
                             (qq-expand (make-syntax rest
                                                     (syntax-context stx)
                                                     (syntax-library stx))
                                        ctx1 depth)))))
             (values (datum->syntax stx (if is-splice
                                            (list 'append head tail)
                                            (list 'cons head tail)))
                     ctx2)))))
      (else
       (qq-expand stx ctx depth)))))

(define (qq-vector stx ctx depth)
  (let* ((form (syntax-form stx))
         (items (vector->list
                 (vector-map (lambda (s)
                               (datum->syntax stx s))
                             form))))
    (let*-values (((lst-expr ctx1)
                   (qq-list (datum->syntax stx items)
                            ctx depth)))
      (values (datum->syntax stx (list 'list->vector lst-expr)) ctx1))))

(define (qq-expand stx ctx depth)
  (let ((form (syntax-form stx)))
    (cond
      ((stx-vector? form)
       (qq-vector stx ctx depth))
      ((pair? form)
       (let ((head (qq-head form)))
         (cond
           ((eq? head 'unquote)
            (if (= depth 1)
                (expand-expr (stx-cadr stx) ctx)
                ;; Deeper than the template's own level: the (unquote X) is
                ;; literal data.  Emit (list 'unquote <datum-expr>) -- a
                ;; real `list' call -- not (unquote ...), which would be
                ;; mis-evaluated as an application of the unquote keyword.
                (let*-values (((inner ctx1)
                               (qq-expand (stx-cadr stx) ctx (- depth 1))))
                  (values (datum->syntax stx
                            (list 'list (datum->syntax stx ''unquote) inner))
                          ctx1))))
           ((eq? head 'unquote-splicing)
            (if (= depth 1)
                (error "unquote-splicing outside list context")
                (let*-values (((inner ctx1)
                               (qq-expand (stx-cadr stx) ctx (- depth 1))))
                  (values (datum->syntax stx
                            (list 'list
                                  (datum->syntax stx ''unquote-splicing)
                                  inner))
                          ctx1))))
           ((eq? head 'quasiquote)
            (let*-values (((inner ctx1)
                           (qq-expand (stx-cadr stx) ctx (+ depth 1))))
              (values (datum->syntax stx
                        (list 'list (datum->syntax stx ''quasiquote) inner))
                      ctx1)))
           (else
            (qq-list stx ctx depth)))))
      ((null? form)
       (values (datum->syntax stx '()) ctx))
      (else
       (qq-atom stx ctx)))))

(define (core-quasiquote stx ctx)
  (qq-expand (stx-cadr stx) ctx 1))

;;; eval-when : (eval-when (situation ...) expr ...) -> value
;;; R7RS 7.1.3.  situations are expand / load / eval (any subset):
;;;   - expand: exprs are evaluated at expand time, in the phase+1
;;;     (implementation) environment, immediately -- their effects (e.g.
;;;     (set! *load-path* ...)) are visible to later expansion of the
;;;     same body (imports, macro expansion).
;;;   - load / eval: exprs are kept in the phase-0 output (evaluated when
;;;     the program is loaded / when eval'd).
;;; At least one situation must be present.

(define (eval-when-expand! exprs ctx)
  ;; Evaluate each expr at phase+1 in the implementation environment,
  ;; threading the phase+1 expansion context through the exprs (so later
  ;; exprs see the expansion-time bindings made by earlier ones) and
  ;; merging it back.  A definition expr is expanded in a library
  ;; (definition) context; other exprs are expanded as expressions.
  ;; Effects land in the expander library / rootlet (s7 eval falls back
  ;; to the rootlet for names the expander library does not define).
  (let* ((ph (context-phase ctx))
         (ctx-up (context-at-phase ctx (+ ph 1))))
    (let loop ((es exprs) (c ctx-up))
      (if (null? es)
        (context-return ctx c)
        (let* ((form (syntax-form (car es)))
               (head (and (pair? form)
                          (identifier? (car form))
                          (context-resolve c (car form)))))
          (if (memq head '(define define-syntax))
            (let*-values (((defs c1)
                           ;; Register the expand-time definition in the
                           ;; library the form expands against (the program
                           ;; library for a strict program, the base library
                           ;; otherwise), so later forms in the SAME library
                           ;; context see the macro / value.
                           (expand-library-body (list (car es))
                                                (syntax-library (car es)) c)))
              (eval (if (null? defs)
                      '(if #f #f)
                      (lower (cons 'begin defs)))
                    the-expander-library)
              (loop (cdr es) c1))
            (let*-values (((sexp c1) (expand-expr (car es) c)))
              (eval (lower sexp) the-expander-library)
              (loop (cdr es) c1))))))))

(define (check-eval-when-situations sit-datum stx)
  (for-each
    (lambda (s)
      (unless (memq s '(expand load eval))
        (error "eval-when: invalid situation" sit-datum)))
    sit-datum))

(define (core-eval-when stx ctx)
  (let* ((form (syntax-form stx))
         (sit-datum (map syntax->datum (syntax-form (cadr form))))
         (exprs (cddr form))
         (do-expand (memq 'expand sit-datum))
         (do-keep (or (memq 'load sit-datum) (memq 'eval sit-datum))))
    (check-eval-when-situations sit-datum stx)
    (let*-values (((ctx1)
                   (if do-expand
                     (eval-when-expand! exprs ctx)
                     (values ctx))))
      (if do-keep
        (let*-values (((sexps ctx2) (expand-list exprs ctx1)))
          (values (datum->syntax stx (cons 'begin sexps)) ctx2))
        (values (datum->syntax stx '(if #f #f)) ctx1)))))

;;; Core form table

(define core-form-handlers
  ;; Written as explicit list/cons -- NOT a quasiquote template: a
  ;; `(quasiquote . ,X)' template would be a nested-quasiquote form per
  ;; R7RS/Racket (s7's native quasiquote substitutes it, a host-ism that
  ;; breaks the self-hosted re-expansion and the artifact).
  (list (cons 'lambda core-lambda)
        (cons 'quote core-quote)
        (cons 'quasiquote core-quasiquote)
        (cons 'quote-syntax core-quote-syntax)
        (cons 'syntax core-syntax)
        (cons 'if core-if)
        (cons 'begin core-begin)
        (cons 'set! core-set!)
        (cons 'letrec* core-letrec*)
        (cons 'letrec core-letrec)
        (cons 'define core-define)
        (cons 'define-syntax core-define-syntax)
        (cons 'let-syntax core-let-syntax)
        (cons 'letrec-syntax core-letrec-syntax)
        (cons 'eval-when core-eval-when)))

(module-define! the-expander-library 'core-form-handlers core-form-handlers)
