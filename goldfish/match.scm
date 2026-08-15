;;; match.scm -- SRFI-262 core subset: extensible pattern matching.
;;;
;;; A self-hosted implementation of the SRFI-262 pattern matcher core,
;;; following the Racket-style extension mechanism: a pattern syntax
;;; keyword is bound to a *transformer* via define-pattern-syntax; at
;;; expansion time the matcher resolves the keyword's binding with
;;; syntax-local-value and calls the transformer to rewrite the pattern
;;; into core primitives.
;;;
;;; Implemented patterns (SRFI-262 core subset):
;;;   _  id  datum  (quote d)  (list ...)  (cons a d)  (vector ...)
;;;   (? proc subpat ...)  (=> proc subpat ...)  (and ...)  (or ...)  (not ...)
;;;
;;; Code generation: each clause compiles to nested if/let code
;;;
;;;   (if guard (let ((v ...)) body) <next-clause-code>)
;;;
;;; where the failure branch of every guard is the code of the
;;; following clause (or a match-error for the last).  `or' and `not'
;;; rebind failure locally.

(define-library (goldfish match)
  (import (scheme base))
  (export match
    match-lambda
    match-values
    match-let
    match-let*
    match-define
    if-match
    define-pattern-syntax)
  (begin

    ;; ------------------------------------------------------------------
    ;; Pattern-syntax registry (Racket-style, binding-backed).
    ;;
    ;; define-pattern-syntax : id transformer-expr
    ;;   Binds id to a transformer whose value is a procedure.  The
    ;;   matcher recovers it with syntax-local-value at expansion time.
    ;;   Lexical scoping gives local shadowing and import propagation.
    ;;
    ;; The transformer receives the whole pattern (including the
    ;; keyword) and must return a syntax object for the rewritten
    ;; pattern.

    (define-syntax define-pattern-syntax
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (datum->syntax
           stx
           (list 'define-syntax
                 (cadr form)
                 ;; The transformer is (lambda (stx) (proc stx)) where
                 ;; proc is the user's pattern transformer expression.
                 (list 'lambda (list 'stx)
                       (list (caddr form) 'stx)))))))

    ;; pattern-transformer : syntax -> procedure or #f
    ;;   The transformer bound to a pattern keyword's identifier, or #f
    ;;   if it has none.  syntax-local-value returns a <binding> record;
    ;;   we look for a transformer binding whose value is a procedure.
    (define (pattern-transformer id)
      (let ((binding (syntax-local-value id)))
        (if (and (binding? binding)
                 (eq? (binding-kind binding) 'transformer))
            (let ((proc (binding-value binding)))
              (if (procedure? proc) proc #f))
            #f)))

    ;; ------------------------------------------------------------------
    ;; Pattern expansion to core form.
    ;;
    ;;   (wildcard) (var id) (quote datum)
    ;;   (seq-list pat ...) (seq-cons a d) (seq-vector pat ...)
    ;;   (seq-and pat ...) (seq-or pat ...) (seq-not pat)
    ;;   (seq-pred proc (pat ...)) (seq-proj proc (pat ...))

    (define (expand-pattern pat)
      (let* ((form (if (syntax? pat) (syntax-form pat) pat))
             (d (if (syntax? pat) (syntax->datum pat) pat)))
        (cond
          ((symbol? d)
           (if (eq? d '_)
             '(wildcard)
             (list 'var d)))
          ((not (pair? d))
           (list 'quote d))
          ((symbol? (car d))
           (let ((head (car d)))
             (cond
               ((eq? head 'quote)
                (list 'quote (cadr d)))
               ((eq? head 'list)
                (cons 'seq-list (map expand-pattern (cdr form))))
               ((eq? head 'cons)
                (list 'seq-cons (expand-pattern (cadr form))
                      (expand-pattern (caddr form))))
               ((eq? head 'vector)
                (cons 'seq-vector (map expand-pattern (cdr form))))
               ((eq? head 'and)
                (cons 'seq-and (map expand-pattern (cdr form))))
               ((eq? head 'or)
                (cons 'seq-or (map expand-pattern (cdr form))))
               ((eq? head 'not)
                (list 'seq-not (expand-pattern (cadr form))))
               ((eq? head '?)
                (list 'seq-pred (cadr d)
                      (map expand-pattern (cddr form))))
               ((eq? head '=>)
                (list 'seq-proj (cadr d)
                      (map expand-pattern (cddr form))))
               (else
                ;; user-defined pattern syntax?
                (let ((tx (if (syntax? pat)
                            (pattern-transformer (car form))
                            #f)))
                  (if tx
                    (let ((result (tx pat)))
                      (if (syntax? result)
                        (expand-pattern result)
                        (error "define-pattern-syntax: transformer must return syntax"
                               result)))
                    ;; A bare list pattern (a b c) matches a proper list
                    ;; of elements; a dotted pair (a . b) matches
                    ;; (cons a b).
                    (if (proper-list? d)
                      (cons 'seq-list (map expand-pattern form))
                      ;; dotted pair (a . b): form = (a . b)
                      (list 'seq-cons (expand-pattern (car form))
                            (expand-pattern (cdr form))))))))))
          (else
           ;; The head is not a symbol (e.g. a nested list (a b) c):
           ;; treat as a list pattern.
           (if (proper-list? d)
             (cons 'seq-list (map expand-pattern form))
             (list 'seq-cons (expand-pattern (car form))
                   (expand-pattern (cdr form))))))))

    ;; ------------------------------------------------------------------
    ;; Code generation.
    ;;
    ;; gen : pat (sexp subject-expr) (sexp fail) -> (values code binds)
    ;;   Generate code matching pat against subject-expr.  On success
    ;;   the code evaluates `success' (passed as the continuation);
    ;;   on failure it evaluates (fail).  Pattern variables are bound
    ;;   to *hygienic temporary identifiers* (generate-temporaries);
    ;;   the returned `binds' is an alist (user-name . temp-identifier)
    ;;   used by the clause compiler to rewrite the body.
    ;;
    ;; gen* : pat (sexp subject-expr) (sexp fail) (sexp success)
    ;;        (list binds) -> (values code binds)
    ;;   gen* is the workhorse; gen threads success and accumulates
    ;;   binds.

    (define (gen* pat subject fail success binds)
      (cond
        ((eq? (car pat) 'wildcard)
         (values success binds))
        ((eq? (car pat) 'var)
         (let* ((user (cadr pat))
                (entry (assq user binds))
                (tmp (if entry (cdr entry)
                         (car (generate-temporaries (list user))))))
           (values `(let ((,tmp ,subject)) ,success)
                   (if entry binds (cons (cons user tmp) binds)))))
        ((eq? (car pat) 'quote)
         (values `(if (equal? ,subject ',(cadr pat)) ,success ,fail)
                 binds))
        ((eq? (car pat) 'seq-list)
         (gen-list* (cdr pat) subject fail success binds))
        ((eq? (car pat) 'seq-cons)
         (let ((a (cadr pat)) (d (caddr pat)))
           (let*-values (((d-code d-binds)
                          (gen* d `(cdr ,subject) fail success binds))
                         ((a-code a-binds)
                          (gen* a `(car ,subject) fail d-code d-binds)))
             (values `(if (pair? ,subject) ,a-code ,fail) a-binds))))
        ((eq? (car pat) 'seq-vector)
         (gen-vector* (cdr pat) subject fail success binds))
        ((eq? (car pat) 'seq-and)
         (let loop ((subs (cdr pat)) (s success) (b binds))
           (if (null? subs)
             (values s b)
             (let*-values (((code binds2) (gen* (car subs) subject fail s b)))
               (loop (cdr subs) code binds2)))))
        ((eq? (car pat) 'seq-or)
         (gen-or* (cdr pat) subject fail success binds))
        ((eq? (car pat) 'seq-not)
         (gen-not* (cadr pat) subject fail success binds))
        ((eq? (car pat) 'seq-pred)
         (let ((proc (cadr pat)) (subs (caddr pat)))
           (if (null? subs)
             (values `(if (,proc ,subject) ,success ,fail) binds)
             (gen-and-pred* proc subs subject fail success binds))))
        ((eq? (car pat) 'seq-proj)
         (gen-proj* (cadr pat) (caddr pat) subject fail success binds))
        (else (error "match: unknown pattern" pat))))

    (define (gen pat subject fail success)
      (let*-values (((code binds) (gen* pat subject fail success '())))
        code))

    (define (gen-and-pred* proc subs subject fail success binds)
      (if (null? subs)
        (values `(if (,proc ,subject) ,success ,fail) binds)
        (let loop ((ss subs) (s success) (b binds))
          (if (null? ss)
            (values `(if (,proc ,subject) ,s ,fail) b)
            (let*-values (((code binds2) (gen* (car ss) subject fail s b)))
              (loop (cdr ss) code binds2))))))

    (define (gen-list* subs subject fail success binds)
      (if (null? subs)
        (values `(if (null? ,subject) ,success ,fail) binds)
        (let ((head (car subs)))
          (let*-values (((rest-code rest-binds)
                         (gen-list* (cdr subs) `(cdr ,subject) fail success binds))
                        ((head-code head-binds)
                         (gen* head `(car ,subject) fail rest-code rest-binds)))
            (values `(if (pair? ,subject) ,head-code ,fail) head-binds)))))

    (define (gen-vector* subs subject fail success binds)
      (if (null? subs)
        (values `(if (and (vector? ,subject) (zero? (vector-length ,subject)))
                     ,success ,fail)
                binds)
        (let*-values (((elems-code elems-binds)
                       (gen-vector-elems* subs subject 0 fail success binds)))
          (values `(if (and (vector? ,subject)
                            (= (vector-length ,subject) ,(length subs)))
                       ,elems-code ,fail)
                  elems-binds))))

    (define (gen-vector-elems* subs subject idx fail success binds)
      (if (null? subs)
        (values success binds)
        (let*-values (((rest-code rest-binds)
                       (gen-vector-elems* (cdr subs) subject (+ idx 1)
                                          fail success binds))
                      ((head-code head-binds)
                       (gen* (car subs) `(vector-ref ,subject ,idx)
                             fail rest-code rest-binds)))
          (values head-code head-binds))))

    (define (gen-or* pats subject fail success binds)
      (cond
        ((null? pats) (values fail binds))
        ((null? (cdr pats)) (gen* (car pats) subject fail success binds))
        (else
         (let*-values (((rest-code rest-binds)
                        (gen-or* (cdr pats) subject fail success binds)))
           (let ((rest-fail (car (generate-temporaries (list 'or-fail)))))
             (let*-values (((first-code first-binds)
                            (gen* (car pats) subject `(,rest-fail) success
                                  rest-binds)))
               (values `(let ((,rest-fail (lambda () ,rest-code)))
                          ,first-code)
                       first-binds)))))))

    (define (gen-not* sub subject fail success binds)
      (let ((inner-ok (car (generate-temporaries (list 'not-ok)))))
        ;; (not sub): succeed (no bindings) iff sub does NOT match.
        ;;   sub matches  -> call inner-ok -> (fail)
        ;;   sub fails    -> the original success
        (let*-values (((sub-code sub-binds)
                       (gen* sub subject success `(,inner-ok) binds)))
          (values `(let ((,inner-ok (lambda () ,fail))) ,sub-code)
                  sub-binds))))

    (define (gen-proj* proc subs subject fail success binds)
      (let ((res (car (generate-temporaries (list 'proj-res)))))
        (if (null? subs)
          (values `(let ((,res (,proc ,subject))) ,success) binds)
          (let loop ((ss subs) (s success) (b binds))
            (if (null? ss)
              (values `(let ((,res (,proc ,subject))) ,s) b)
              (let*-values (((code binds2) (gen* (car ss) res fail s b)))
                (loop (cdr ss) code binds2)))))))

    ;; rename-body : (list (user . temp)) (list sexp body) -> (list sexp)
    ;;   Substitute pattern-variable references (by name) in the body
    ;;   with their hygienic temporaries.
    (define (rename-body binds body)
      (define (ren expr)
        (cond
          ((symbol? expr)
           (let ((entry (assq expr binds)))
             (if entry (cdr entry) expr)))
          ((pair? expr)
           (cons (ren (car expr)) (ren (cdr expr))))
          ((vector? expr)
           (vector-map ren expr))
          (else expr)))
      (map ren body))

    ;; collect-vars : pat -> (list symbol)
    ;;   All pattern variables (user names) in a core pattern, in order.
    (define (collect-vars pat)
      (cond
        ((eq? (car pat) 'var) (list (cadr pat)))
        ((memq (car pat) '(seq-list seq-vector seq-and))
         (apply append (map collect-vars (cdr pat))))
        ((eq? (car pat) 'seq-cons)
         (append (collect-vars (cadr pat)) (collect-vars (caddr pat))))
        ((eq? (car pat) 'seq-not)
         (collect-vars (cadr pat)))
        ((memq (car pat) '(seq-pred seq-proj))
         (apply append (map collect-vars (caddr pat))))
        ((eq? (car pat) 'seq-or)
         ;; all branches must bind the same vars
         (let ((first (collect-vars (cadr pat))))
           (for-each (lambda (b)
                       (if (not (equal? (collect-vars b) first))
                         (error "match: or branches bind different variables")))
                     (cddr pat))
           first))
        (else
         '())))

    ;; compile-pats-gen* : (list datum-pat) (list symbol args)
    ;;                     (list body) (sexp fail) -> sexp
    ;;   Match each pat (datum form) against the corresponding arg
    ;;   symbol using gen*; on success evaluate the body with all
    ;;   pattern variables bound.  Pre-collects pattern variables from
    ;;   all pats so the body can be rewritten to the hygienic
    ;;   temporaries, then threads the shared binds through gen*.
    (define (compile-pats-gen* pats args body fail)
      (let* ((cores (map (lambda (p) (expand-pattern p)) pats))
             (vars (apply append (map collect-vars cores)))
             (binds (map (lambda (v)
                           (cons v (car (generate-temporaries (list v)))))
                         vars))
             (renamed-body (rename-body binds body))
             (success `(begin ,@renamed-body)))
        (let loop ((cs cores) (as args) (s success) (b binds))
          (if (null? cs)
            s
            (let*-values (((code new-binds)
                           (gen* (car cs) (car as) fail s b)))
              (loop (cdr cs) (cdr as) code new-binds))))))

    ;; ------------------------------------------------------------------
    ;; Public syntax.
    ;;
    ;; match : expr clause ... -> value
    ;;   clause = (pattern body ...)
    (define-syntax match
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (let ((expr (syntax-form (cadr form)))
                (clauses (map syntax-form (cddr form))))
            (if (null? clauses)
              (error "match: no clauses")
              (let ((code
                     (let loop ((cls clauses))
                       (if (null? (cdr cls))
                         (let* ((pat (caar cls))
                                (body (cdar cls)))
                           (let* ((core (expand-pattern pat))
                                  (vars (collect-vars core))
                                  (binds (map (lambda (v)
                                                (cons v (car (generate-temporaries (list v)))))
                                              vars))
                                  (renamed-body (rename-body binds
                                                             (map syntax->datum body))))
                             (let*-values (((code ignored)
                                            (gen* core expr
                                                  '(error 'match "no matching pattern")
                                                  `(begin ,@renamed-body)
                                                  binds)))
                               code)))
                         (let* ((pat (caar cls))
                                (body (cdar cls)))
                           (let* ((core (expand-pattern pat))
                                  (vars (collect-vars core))
                                  (binds (map (lambda (v)
                                                (cons v (car (generate-temporaries (list v)))))
                                              vars))
                                  (renamed-body (rename-body binds
                                                             (map syntax->datum body))))
                             (let*-values (((code ignored)
                                            (gen* core expr
                                                  (loop (cdr cls))
                                                  `(begin ,@renamed-body)
                                                  binds)))
                               code)))))))
                (datum->syntax stx code)))))))

    ;; match-lambda : ((pattern ...) body ...) ... -> procedure
    ;;   A procedure matching clauses by argument count (case-lambda
    ;;   semantics): clauses are grouped by the number of patterns and
    ;;   each group becomes one case-lambda branch.
    (define-syntax match-lambda
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (let ((clauses (cdr form)))
            (if (null? clauses)
              (error "match-lambda: no clauses")
              ;; group clauses by pattern count
              (let* ((groups
                      (let loop ((cls clauses) (acc '()))
                        (if (null? cls)
                          acc
                          (let* ((cl (syntax-form (car cls)))
                                 (arity (length (syntax->datum (car cl)))))
                            (let ((entry (assv arity acc)))
                              (if entry
                                (begin
                                  (set-cdr! entry (cons (car cls) (cdr entry)))
                                  (loop (cdr cls) acc))
                                (loop (cdr cls)
                                      (cons (cons arity (list (car cls)))
                                            acc))))))))
                     (compile-group
                      (lambda (arity group-clauses)
                        (let* ((args (generate-temporaries
                                      (make-list arity 'a)))
                               (compile-one
                                (lambda (cl-syntax next-code)
                                  (let* ((cl (syntax-form cl-syntax))
                                         (pats (syntax->datum (car cl)))
                                         (body (map syntax->datum (cdr cl))))
                                    (list 'lambda args
                                          (compile-pats-gen* pats args
                                                             body next-code))))))
                          (letrec ((build-chain
                                    (lambda (cls)
                                      (if (null? (cdr cls))
                                        (compile-one (car cls)
                                                     '(error 'match "no matching pattern"))
                                        (let ((next-code (build-chain (cdr cls))))
                                          (compile-one (car cls)
                                                       (cons next-code args)))))))
                            (build-chain group-clauses))))))
                ;; dispatch on argument count at run time (no
                ;; case-lambda in R7RS): (lambda args (cond ((= (length
                ;; args) n) (apply group-n args)) ... (else error)))
                (let ((rest-arg (car (generate-temporaries (list 'args)))))
                  (datum->syntax
                   stx
                   (list 'lambda rest-arg
                         (cons 'cond
                               (append
                                (map (lambda (g)
                                       (list (list '= (list 'length rest-arg) (car g))
                                             (cons 'apply
                                                   (cons (compile-group (car g) (cdr g))
                                                         (list rest-arg)))))
                                     groups)
                                (list (list 'else
                                            '(error 'match "no matching pattern"))))))))))))))


    ;; ------------------------------------------------------------------
    ;; Derived forms.
    ;;
    ;; match-values : expr ((pattern ...) body ...) ... -> value
    (define-syntax match-values
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (let ((expr (cadr form))
                (clauses (cddr form)))
            (datum->syntax
             stx
             `(call-with-values
               (lambda () ,expr)
               ,(cons 'match-lambda clauses)))))))

    ;; match-let : ((pat init) ...) body ... -> value
    (define-syntax match-let
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (let ((binds (syntax-form (cadr form)))
                (body (cddr form)))
            (datum->syntax
             stx
             (list 'match-values
                   (cons 'values
                         (map (lambda (b)
                                (syntax->datum (cadr (syntax-form b))))
                              binds))
                   (cons (map (lambda (b) (car (syntax-form b))) binds)
                         body)))))))

    ;; match-let* : sequential version
    (define-syntax match-let*
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (let ((binds (syntax-form (cadr form)))
                (body (cddr form)))
            (datum->syntax
             stx
             (if (null? binds)
               `(let () ,@body)
               (let* ((first (syntax-form (car binds)))
                      (first-pat (car first))
                      (first-init (syntax->datum (cadr first))))
                 (list 'match-values
                       first-init
                       (list (list first-pat)
                             (cons 'match-let*
                                   (cons (cdr binds) body)))))))))))

    ;; pattern-vars-datum : datum-pattern -> (list symbol)
    ;;   All pattern variables in a datum-form pattern (name only, no
    ;;   structure; used by match-define).
    (define (pattern-vars-datum pat)
      (cond
        ((symbol? pat)
         (if (eq? pat '_) '() (list pat)))
        ((not (pair? pat)) '())
        ((memq (car pat) '(quote vector))
         '())
        ((memq (car pat) '(list cons and or not))
         (apply append (map pattern-vars-datum (cdr pat))))
        ((eq? (car pat) '?)
         (apply append (map pattern-vars-datum (cddr pat))))
        ((eq? (car pat) '=>)
         (apply append (map pattern-vars-datum (cddr pat))))
        (else
         (apply append (map pattern-vars-datum pat)))))

    ;; match-define : (match-define pattern expr) -> void
    ;;   Binds the pattern variables by matching expr (a single value).
    ;;   Expands to:
    ;;     (begin
    ;;       (define tmp expr)
    ;;       (define v1 (match tmp (pattern v1) (_ (match-error-thunk))))
    ;;       ...)
    ;;   which works at top level (all defines hoisted).
    (define-syntax match-define
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (let ((pat (cadr form))
                (expr (caddr form))
                (tmp (car (generate-temporaries (list 'match-tmp)))))
            (let* ((pat-datum (syntax->datum pat))
                   (vars (pattern-vars-datum pat-datum)))
              (datum->syntax
               stx
               (cons 'begin
                     (cons (list 'define tmp (syntax->datum expr))
                           (map (lambda (v)
                                  (list 'define v
                                        (list 'match tmp
                                              (list (datum->syntax stx pat)
                                                    v)
                                              (list '_ '(error 'match "no matching pattern")))))
                                vars)))))))))

    ;; if-match : expr (pattern body ...) else-expr -> value
    (define-syntax if-match
      (lambda (stx)
        (let ((form (syntax-form stx)))
          (let ((expr (cadr form))
                (clause (caddr form))
                (else-expr (cadddr form))
                (tmp (car (generate-temporaries (list 'ifm-result)))))
            (datum->syntax
             stx
             (list 'let (list (list tmp (syntax->datum expr)))
                   (list 'match tmp
                         clause
                         (list '_ (syntax->datum else-expr)))))))))

    )) ;begin
