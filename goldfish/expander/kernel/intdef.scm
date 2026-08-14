;;; Internal definitions (user space, built on expand.scm primitives).
;;;
;;; Body expansion follows Racket's body.rkt shape:
;;;
;;;   1. A definition context (inside-edge scope + def-env) is allocated
;;;      EAGERLY at body start, so every form -- including expressions
;;;      that precede all definitions -- carries the inside-edge scope.
;;;
;;;   2. Scan phase (expand-body-seq): each form is expanded with the
;;;      definition context and a define/define-syntax stop list, but
;;;      only to detect (macro-generated) definitions.  Expression forms
;;;      are NOT expanded here: the original form tagged with the
;;;      inside-edge scope is deferred.  This is what makes
;;;      (let () (+ x 1) (define x 41)) resolve x to the definition:
;;;      the expression is expanded only after all definitions are bound
;;;      (Racket's finish-expanding-body).
;;;
;;;   3. Finish phase (expand-body-finalize): definition RHSs and the
;;;      deferred expressions are expanded with every definition in
;;;      scope, producing (letrec* ((g init) ...) body).
;;;
;;; Transformers of deferred expressions run twice (scan for detection,
;;; finish for real).  Harmless for pure transformers; it also makes
;;; body macros visible body-wide, which R7RS 5.2.2 permits (scanning
;;; definitions before expanding expressions).
;;;
;;; Hygiene: body forms are already in expansion space (the macro-output
;;; flip happened at the expand-macro boundary), so body scan/finish
;;; expand WITHOUT the intro-scope flip -- flipping again would re-add
;;; the introduction scope to use-site identifiers and let macro-
;;; introduced bindings spuriously match (cf. local-expand, the
;;; transformer-facing primitive, which does flip).  Macro-generated
;;; definitions carry the output-flip scope until def-bind! normalizes
;;; it off (stx-flip-intro-off).

(define (expand-body stxs ctx)
  (if (null? stxs)
      (error "expand-body: empty body")
      (let ((saved-ctx (current-expand-context)))
        (set-current-expand-context! ctx)
        (let ((defs (new-defs)))
          (expand-body-seq stxs defs '() '() saved-ctx)))))

(define body-stop-list '(define define-syntax))

;;; scan-body-form : expand head macros to detect (macro-generated)
;;; definitions WITHOUT recursing into expression bodies.  Returns the form
;;; whose head reveals whether it is a definition (body-def-head).  Recursing
;;; into a non-definition body here would re-expand continuation-passing
;;; macros (match.scm) once per scan and again at finish -- exponential.
;;; Begin is left for body-def-head / scan-def-form to splice, so a macro
;;; expanding to (begin (define ...) ...) is still detected.

(define (scan-body-form stx defs)
  (let ((ctx (current-expand-context)))
    (unless ctx
      (error "scan-body-form: no expansion context"))
    (let* ((ph (context-phase ctx))
           (scp-in (defs-scp-in defs))
           (addr-env (defs-addr defs))
           (env-defs (store-def-env-ref (context-store ctx) addr-env))
           (env-unstops (env-map-values binding-unstop env-defs))
           (stx1 (stx-add-scope (stx-eager-flush stx) scp-in ph))
           (ctx0 (context-with-env ctx env-unstops)))
      (let*-values (((form c) (scan-head-loop stx1 ctx0)))
        (let ((c1 (context-with-use-scopes (context-return ctx c)
                                           (context-use-scopes c))))
          (set-current-expand-context! c1)
          form)))))

(define (scan-head-loop form c)
  (if (or (not (syntax? form)) (not (pair? (syntax-form form))))
      (values form c)
      (let ((head (stx-propagate-wrap form (car (syntax-form form)))))
        (if (not (identifier? head))
            (values form c)
            (let*-values (((name binding) (resolve-identifier head c)))
              (cond
                ((tstop-binding? binding) (values form c))
                ((core-form-binding? binding) (values form c))
                ((transformer-binding? binding)
                 (let*-values (((out c1)
                                (expand-macro-once form c (binding-value binding))))
                   (scan-head-loop out c1)))
                (else (values form c))))))))

(define (expand-body-seq stxs defs var-defs exprs saved-ctx)
  (if (null? stxs)
      (let ((final-ctx (current-expand-context)))
        (set-current-expand-context! saved-ctx)
        (expand-body-finalize defs (reverse var-defs) (reverse exprs) final-ctx))
      (let* ((stx (car stxs))
             (ctx (current-expand-context))
             (head (body-def-head stx ctx)))
        (if head
            (let*-values (((stxs1 var-defs1 exprs1)
                           (scan-def-form head stx stxs defs var-defs exprs)))
              (expand-body-seq stxs1 defs var-defs1 exprs1 saved-ctx))
            (let* ((result (scan-body-form stx defs))
                   (ctx1 (current-expand-context))
                   (rhead (body-def-head result ctx1)))
              (if rhead
                  (let*-values (((stxs1 var-defs1 exprs1)
                                 (scan-def-form rhead result stxs defs var-defs exprs)))
                    (expand-body-seq stxs1 defs var-defs1 exprs1 saved-ctx))
                  (let ((deferred (stx-add-scope (stx-eager-flush stx)
                                                 (defs-scp-in defs)
                                                 (context-phase ctx1))))
                    (expand-body-seq (cdr stxs) defs var-defs
                                     (cons deferred exprs) saved-ctx))))))))

(define (expand-body-form stx ctx)
  (let* ((stop-frame (map (lambda (name) (cons name (make-tstop-binding #f)))
                          body-stop-list))
         (ctx-stopped (context-with-env ctx (cons stop-frame (context-env ctx)))))
    (let*-values (((result ctx1) (expand-expr stx ctx-stopped)))
      (let ((restored (context-with-use-scopes (context-return ctx ctx1)
                                               (context-use-scopes ctx1))))
        (set-current-expand-context! restored)
        (values result restored)))))

;;; expand-body-finalize : expand deferred forms with every definition
;;; bound.  Plain expand-expr, no intro-scope flip: deferred body forms
;;; are in expansion space, like the scan itself.  A body with no
;;; expressions (definitions only, e.g. a definition-only program file)
;;; yields void, like Racket (R7RS requires one expression; the
;;; relaxation is needed for bootstrap, where source files are mostly
;;; definitions).

;;; Output wrapper forms carry no lexical context: every subnode is
;;; already fully expanded, and a well-formed (empty) scope-set context
;;; keeps the tree traversable by scope operations (a context record in
;;; the context slot would crash stx-ctx-at on any later flip/resolve).
(define body-output-source (make-syntax 'empty (stx-ctx-empty) #f))

(define (expand-body-finalize defs var-defs exprs ctx)
  (if (null? exprs)
      (expand-body-finalize defs var-defs
                            (list (datum->syntax body-output-source '(if #f #f))) ctx)
      (let* ((scp-in (defs-scp-in defs))
             (ph (context-phase ctx))
             (ctx1 (let loop ((ds var-defs) (c ctx))
                     (if (null? ds)
                         c
                         (loop (cdr ds)
                               (context-extend-env c
                                                   (caar ds)
                                                   (make-lexical-binding (caar ds))))))))
        (let*-values (((inits ctx2)
                       (let loop ((ds var-defs) (c ctx1) (inits '()))
                         (if (null? ds)
                             (values (reverse inits) c)
                             (let ((val-stx (stx-add-scope (stx-eager-flush (cdar ds)) scp-in ph)))
                               (let*-values (((init-sexp c1) (expand-expr val-stx c)))
                                 (loop (cdr ds) c1 (cons init-sexp inits))))))))
          (let*-values (((body-sexps ctx3)
                         (let loop ((es exprs) (c ctx2) (out '()))
                           (if (null? es)
                               (values (reverse out) c)
                               (let*-values (((sexp c1) (expand-expr (car es) c)))
                                 (loop (cdr es) c1 (cons sexp out)))))))
            (let ((body (if (= 1 (length body-sexps))
                            (car body-sexps)
                            (datum->syntax body-output-source (cons 'begin body-sexps)))))
              (if (null? var-defs)
                  (values body ctx3)
                  (values (datum->syntax body-output-source
                          `(letrec* ,(map list (map car var-defs) inits) ,body))
                          ctx3))))))))

;;; body-def-head : syntax context -> symbol/#f
;;; The definition head (define / define-syntax / begin) if the form is a
;;; definition form, else #f.

(define (body-def-head stx ctx)
  (and (syntax? stx)
       (pair? (syntax-form stx))
       (identifier? (car (syntax-form stx)))
       (let ((h (context-resolve ctx (stx-propagate-wrap stx (car (syntax-form stx))))))
         (and (memq h '(define define-syntax begin)) h))))

;;; scan-def-form : process one detected definition form, returning
;;; (values stxs var-defs exprs) for the continued scan.

(define (scan-def-form head form stxs defs var-defs exprs)
  (cond
    ((eq? head 'define)
     (let*-values (((id val-stx) (parse-internal-define form)))
       (let ((name (def-bind! defs id)))
         (values (cdr stxs) (cons (cons name val-stx) var-defs) exprs))))
    ((eq? head 'define-syntax)
     (let* ((ef (stx-eager-flush form))
            (f (syntax-form ef)))
       (def-bind! defs (stx-propagate-wrap ef (cadr f))
                  (stx-propagate-wrap ef (caddr f)))
       (values (cdr stxs) var-defs exprs)))
    (else
     (let* ((ef (stx-eager-flush form))
            (f (syntax-form ef)))
       (values (append (map (lambda (s) (stx-propagate-wrap ef s))
                            (cdr f))
                       (cdr stxs))
               var-defs exprs)))))

(define (parse-internal-define stx)
  (let* ((estx (stx-eager-flush stx))
         (form (syntax-form estx))
         (head (stx-propagate-wrap estx (cadr form))))
    (cond
      ((identifier? head)
       (values head (stx-propagate-wrap estx (caddr form))))
      ((and (syntax? head)
            (pair? (syntax-form head))
            (identifier? (car (syntax-form head))))
       (let ((params (cdr (syntax-form head))))
         (values (stx-propagate-wrap head (car (syntax-form head)))
                 (build-lambda-stx estx
                                   (map-spine (lambda (p) (stx-propagate-wrap head p))
                                              params)
                                   (map (lambda (s) (stx-propagate-wrap estx s))
                                        (cddr form))))))
      (else
       (error "define: bad syntax in body" (syntax->datum stx))))))

(define (build-lambda-stx src-stx param-stxs body-stxs)
  (let ((ctx (syntax-context src-stx))
        (lib (syntax-library src-stx)))
    (make-syntax (cons (make-syntax 'lambda ctx lib)
                       (cons (if (syntax? param-stxs)
                                 param-stxs
                                 (make-syntax param-stxs ctx lib))
                             body-stxs))
                 ctx lib)))

(module-define! the-expander-library 'expand-body expand-body)
(module-define! the-expander-library 'expand-body-form expand-body-form)
(module-define! the-expander-library 'parse-internal-define parse-internal-define)
(module-define! the-expander-library 'build-lambda-stx build-lambda-stx)
