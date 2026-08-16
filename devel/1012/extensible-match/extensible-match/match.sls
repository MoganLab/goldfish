(library (extensible-match match)
  (export match-lambda
          match
          match-values
          match-let
          match-let*
          match-let-values
          match-let*-values
          match-define
          match-define-values
          match-letrec
          match-letrec*
          if-match

          &match
          make-match-violation
          match-violation?)
  (import (rnrs (6))
          (only (srfi :1 lists)
                append-map
                make-list)
          (srfi :244 define-values)
          (extensible-match core-pattern) ; for match-define
          (extensible-match expand)
          (extensible-match ast)
          (extensible-match decision-tree)
          (extensible-match code-generator)
          (extensible-match util))

  (define-syntax match-lambda
    (lambda (stx)
      (syntax-case stx ()
        ((_ ((pat ...) body_0 body_1 ...) ...)
         ;; group the clauses by the number of pats in each and then
         ;; use case-lambda to delegate to %match-lambda
         (let ((clause-table (make-eqv-hashtable)))
           (for-each
            (lambda (pats body)
              (let ((n-pats (length pats)))
                (hashtable-update! clause-table
                                   n-pats
                                   (lambda (old-pats)
                                     (cons (cons pats body) old-pats))
                                   '())))
            #'((pat ...) ...)
            #'((body_0 body_1 ...) ...))
           (let ((arg-nrs
                   (vector->list
                    (vector-sort < (hashtable-keys clause-table)))))
             #`(case-lambda
                 #,@(map
                     (lambda (nr)
                       (with-syntax
                           ((((pats body ...) ...)
                             (reverse (hashtable-ref clause-table nr #f)))
                            ((temp-arg ...)
                             (generate-temporaries (make-list nr))))
                         #'((temp-arg ...)
                            ((%match-lambda
                               (pats body ...) ...)
                             temp-arg ...))))
                     arg-nrs))))))))

  ;; %match-lambda is like match-lambda, but requires that all the
  ;; clauses have the same number of pats (this is not checked)
  (define-syntax %match-lambda
    (syntax-rules ()
      ((_ ((pat ...) body_0 body_1 ...) ...)
       (expand-patternses ((pat ...) ...) %core-match-lambda ((body_0 body_1 ...) ...)))))

  (define-syntax %core-match-lambda
    (lambda (stx)
      (syntax-case stx ()
        ((_ ((core-pat ...) ...) ((body_0 body_1 ...) ...))
         (with*-syntax (((action-id ...)
                         (generate-temporaries #'(body_0 ...)))
                        ((input-id ...)
                         (generate-temporaries
                          (car #'((core-pat ...) ...))))
                        (ast-patterns
                         (core-patterns->ast
                          #'((core:row
                              (core:subject input-id core-pat) ...) ...)))
                        (nowt
                         (begin
                           (for-each raise-if-non-linear #'ast-patterns)
                           #f))
                        (((action-arg ...) ...)
                         (map pattern-vars #'ast-patterns))
                        (((disjointed-var ...) ...)
                         (map pattern-disjointed-vars #'ast-patterns))
                        ((action-syntax ...)
                         #'((action-id action-arg ...) ...))
                        (ast-actions
                         (map action-syntax->ast #'(action-syntax ...)))
                        (patacts
                         (patacts-source-transform
                          (map make-patact #'ast-patterns #'ast-actions)))
                        (dt
                         (remove-decision-tree-renames
                          (patacts->decision-tree #'patacts)))
                        (generated-code (decision-tree->syntax #'dt)))
           #`(lambda (input-id ...)
               (let ((fail
                      (lambda ()
                        (raise
                         (condition
                          (make-match-violation)
                          (make-irritants-condition (list input-id ...))))))
                     (action-id
                      (lambda (action-arg ...)
                        (let-syntax
                            ((disjointed-var
                              (lambda (stx)
                                (syntax-violation 'match
                                                  "variable is not bound in all subpatterns of a disjunction"
                                                  #'disjointed-var))) ...)
                          body_0 body_1 ...))) ...)
                 generated-code)))))))

  (define-condition-type &match &assertion
    make-match-violation match-violation?)

  ;; convenience syntax
  (define-syntax match
    (syntax-rules ()
      ((_ expr (pattern body_0 body_1 ...) ...)
       ((match-lambda ((pattern) body_0 body_1 ...) ...) expr))))

  (define-syntax match-values
    (syntax-rules ()
      ((_ expr ((pattern ...) body_0 body_1 ...) ...)
       (call-with-values
           (lambda () expr)
         (match-lambda ((pattern ...) body_0 body_1 ...) ...)))))

  (define-syntax match-let
    (syntax-rules ()
      ((_ ((pat init) ...) body_0 body_1 ...)
       (match-values (values init ...)
         ((pat ...)
          body_0 body_1 ...)))))
  (define-syntax match-let*
    (syntax-rules ()
      ((_ () body_0 body_1 ...)
       (let ()
         body_0 body_1 ...))
      ((_ ((pat init) more ...) body_0 body_1 ...)
       (match-let ((pat init))
         (match-let* (more ...)
           body_0 body_1 ...)))))

  (define-syntax match-let-values
    (lambda (stx)
      (syntax-case stx ()
        ((_ (((pat ...) init) ...) body_0 body_1 ...)
         (with-syntax
             ((((temp ...) ...)
               (map generate-temporaries #'((pat ...) ...))))
           #'(let-values
                 (((temp ...) init) ...)
               ((match-lambda
                  ((pat ... ...) body_0 body_1 ...))
                temp ... ...)))))))
  (define-syntax match-let*-values
    (syntax-rules ()
      ((_ () body_0 body_1 ...)
       (let ()
         body_0 body_1 ...))
      ((_ (((pat ...) init) more ...) body_0 body_1 ...)
       (match-let-values (((pat ...) init))
         (match-let*-values (more ...)
           body_0 body_1 ...)))))

  (define-syntax match-define
    (syntax-rules ()
      ((_ pat expr)
       (match-define-values (pat) expr))))

  (define-syntax match-define-values
    (syntax-rules ()
      ((_ (pat ...) expr)
       (expand-patterns (pat ...) match-define-values-aux expr))))
  (define-syntax match-define-values-aux
    (lambda (stx)
      (syntax-case stx ()
        ((_ (core-pat ...) expr)
         (with*-syntax
             ((ast (core-patterns->ast
                     #'((core:subject dummy core-pat) ...)))
              (((var ...) ...) (map pattern-vars #'ast))
              (((disjointed-var ...) ...)
               (map pattern-disjointed-vars #'ast)))
           #'(begin
               (define-values
                   (var ... ...)
                 ((%core-match-lambda
                   ((core-pat ...))
                   (((values var ... ...))))
                  expr))
               (define-syntax disjointed-var
                 (lambda (stx)
                   (syntax-violation 'match
                                     "variable is not bound in all subpatterns of a disjunction"
                                     #'disjointed-var))) ... ...))))))

  (define-syntax match-letrec
    (syntax-rules ()
      ((_ ((pat init) ...) body_0 body_1 ...)
       (expand-patterns (pat ...) match-letrec-aux (init ...) body_0 body_1 ...))))
  (define-syntax match-letrec-aux
    (lambda (stx)
      (syntax-case stx ()
        ((_ (core-pat ...) (init ...) body_0 body_1 ...)
         (let* ((ast (core-patterns->ast
                      #'((core:subject dummy core-pat) ...)))
                (vars (append-map pattern-vars ast))
                (disjointed-vars (append-map pattern-disjointed-vars ast)))
           (with-syntax (((var ...) vars)
                         ((disjointed-var ...) disjointed-vars)
                         ((tmp ...) (generate-temporaries vars)))
             #'(let ()
                 (define-values (tmp ...)
                   ((%core-match-lambda
                     ((core-pat ...))
                     (((values var ...))))
                     init ...))
                 (define var tmp) ...
                 (define-syntax disjointed-var
                   (lambda (stx)
                     (syntax-violation 'match
                                       "variable is not bound in all subpatterns of a disjunction"
                                       #'disjointed-var))) ...
                 (let ()
                   body_0 body_1 ...))))))))

  (define-syntax match-letrec*
    (syntax-rules ()
      ((_ ((pat init) ...) body_0 body_1 ...)
       (let ()
         (match-define pat init) ...
         (let ()
           body_0 body_1 ...)))))

  (define-syntax if-match
    (lambda (stx)
      (syntax-case stx ()
        ((_ ((pat init) ...) conseq alter)
         (with-syntax ((else (make-list (length #'(pat ...)) #'_)))
           #'(match-values (values init ...)
               ((pat ...) conseq)
               (else alter))))))))
