;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
(define-library (liii goldcheck arity internal scan)
  (import (scheme base)
    (liii goldcheck arity internal scope)
    (liii goldcheck arity internal signature)
    (liii goldcheck type-hint parse)
    (liii goldcheck path)
  ) ;import
  (export %scan-file-arity)
  (begin
    (define (%definition-form? form)
      (and (pair? form) (memq (car form) '(define define*)))
    ) ;define
    (define (%macro-name form)
      (and (pair? form)
        (memq (car form) '(define-syntax define-macro))
        (pair? (cdr form))
        (symbol? (cadr form))
        (cadr form)
      ) ;and
    ) ;define
    (define (%binding-value form)
      (or (%definition-signature form) 'shadow)
    ) ;define
    (define (%add-binding name value env)
      (let ((old (assq name env)))
        (if old (cons (cons name 'shadow) env) (cons (cons name value) env))
      ) ;let
    ) ;define
    (define (%collect-bindings-loop forms env)
      (if (not (pair? forms))
        env
        (let* ((form (car forms))
               (name (or (%definition-name form) (%macro-name form)))
               (value (if (%macro-name form) 'shadow (%binding-value form)))
              ) ;
          (%collect-bindings-loop
            (if (and (pair? form) (eq? (car form) 'begin))
              (append (cdr form) (cdr forms))
              (cdr forms)
            ) ;if
            (if name (%add-binding name value env) env)
          ) ;%collect-bindings-loop
        ) ;let*
      ) ;if
    ) ;define
    (define (%collect-bindings forms env)
      (%scope-extend (%collect-bindings-loop forms '()) env)
    ) ;define
    (define (%expand-begins forms)
      (let loop
        ((rest forms) (acc '()))
        (if (not (pair? rest))
          (reverse acc)
          (let ((form (car rest)))
            (if (and (pair? form) (eq? (car form) 'begin))
              (loop (cdr rest) (append (reverse (%expand-begins (cdr form))) acc))
              (loop (cdr rest) (cons form acc))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%collect-set-targets form names)
      (cond ((not (pair? form)) names)
            ((eq? (car form) 'quote) names)
            ((and (eq? (car form) 'set!) (pair? (cdr form)) (symbol? (cadr form)))
             (%collect-set-targets (cddr form) (cons (cadr form) names))
            ) ;
            (else (%collect-set-targets (cdr form) (%collect-set-targets (car form) names)))
      ) ;cond
    ) ;define

    (define (%mutation-env forms)
      (let loop
        ((rest forms) (env '()))
        (if (not (pair? rest))
          env
          (loop (cdr rest)
            (%scope-extend (map (lambda (name) (cons name 'mutated)) (%collect-set-targets (car rest) '()))
              env
            ) ;%scope-extend
          ) ;loop
        ) ;if
      ) ;let
    ) ;define

    (define (%source-body forms)
      (let loop
        ((rest forms))
        (cond ((null? rest) forms)
              ((and (pair? (car rest)) (eq? (caar rest) 'define-library))
               (if (pair? (cddar rest)) (cddar rest) '())
              ) ;
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%scan-sequence file forms env)
      (let loop
        ((rest forms) (acc '()))
        (if (not (pair? rest))
          acc
          (loop (cdr rest) (append acc (%scan-form file (car rest) env)))
        ) ;if
      ) ;let
    ) ;define

    (define (%leading-definitions forms)
      (let loop
        ((rest forms) (acc '()))
        (if (not (pair? rest))
          acc
          (let ((form (car rest)))
            (cond ((%definition-form? form) (loop (cdr rest) (cons form acc)))
                  ((and (pair? form) (eq? (car form) 'begin))
                   (loop (append (cdr form) (cdr rest)) acc)
                  ) ;
                  (else (reverse acc))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%body-env forms env)
      (%collect-bindings (%leading-definitions forms) env)
    ) ;define

    (define (%scan-body file forms env)
      (let ((body-env (%body-env forms env)))
        (%scan-sequence file forms body-env)
      ) ;let
    ) ;define

    (define (%scan-procedure file expr env)
      (cond
       ((and (pair? expr)
          (eq? (car expr) 'lambda)
          (pair? (cdr expr))
          (pair? (cddr expr))
        ) ;and
        (let ((names (%formals-names (cadr expr))))
          (if names (%scan-body file (cddr expr) (%scope-shadow names env)) '())
        ) ;let
       ) ;
       ((and (pair? expr) (eq? (car expr) 'case-lambda))
        (%scan-case-lambda file (cdr expr) env)
       ) ;
       (else '())
      ) ;cond
    ) ;define

    (define (%scan-case-lambda file clauses env)
      (let loop
        ((rest clauses) (acc '()))
        (if (not (pair? rest))
          acc
          (let* ((clause (car rest)) (names (and (pair? clause) (%formals-names (car clause)))))
            (if (and names (pair? (cdr clause)) (proper-list? clause))
              (loop (cdr rest)
                (append acc (%scan-body file (cdr clause) (%scope-shadow names env)))
              ) ;loop
              acc
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%scan-definition file form env)
      (let ((target (and (pair? (cdr form)) (cadr form))))
        (cond
         ((and (pair? target) (pair? (cddr form)))
          (let ((names (if (eq? (car form) 'define*)
                         (%star-formals-names (cdr target))
                         (%formals-names (cdr target))
                       ) ;if
                ) ;names
               ) ;
            (if names (%scan-body file (cddr form) (%scope-shadow names env)) '())
          ) ;let
         ) ;
         ((and (symbol? target) (pair? (cddr form)))
          (let ((value (caddr form)))
            (if (%procedure-signature value)
              (%scan-procedure file value env)
              (%scan-form file value env)
            ) ;if
          ) ;let
         ) ;
         (else '())
        ) ;cond
      ) ;let
    ) ;define

    (define (%binding-name binding)
      (and (pair? binding) (symbol? (car binding)) (car binding))
    ) ;define

    (define (%binding-init binding)
      (and (pair? binding) (pair? (cdr binding)) (cadr binding))
    ) ;define

    (define (%let-bindings bindings)
      (and (proper-list? bindings)
        (let loop
          ((rest bindings) (acc '()))
          (if (null? rest)
            (reverse acc)
            (let ((binding (car rest)))
              (if (and (proper-list? binding) (= (length binding) 2) (%binding-name binding))
                (loop (cdr rest) (cons binding acc))
                #f
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;and
    ) ;define

    (define (%unique-let-bindings? bindings)
      (let loop
        ((rest bindings) (names '()))
        (cond ((null? rest) #t)
              ((memq (%binding-name (car rest)) names) #f)
              (else
                (loop (cdr rest) (cons (%binding-name (car rest)) names))
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (%let-scope bindings env)
      (let loop
        ((rest bindings) (scope env))
        (if (null? rest)
          scope
          (let* ((binding (car rest))
                 (name (%binding-name binding))
                 (signature (%procedure-signature (%binding-init binding)))
                ) ;
            (loop (cdr rest)
              (%scope-extend (list (cons name (or signature 'shadow))) scope)
            ) ;loop
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%scan-let-inits file bindings env)
      (%scan-sequence file (map %binding-init bindings) env)
    ) ;define

    (define (%scan-let file form env)
      (let* ((rest (cdr form))
             (named? (and (pair? rest) (pair? (cdr rest)) (symbol? (car rest))))
             (name (and named? (car rest)))
             (bindings (if named? (cadr rest) (car rest)))
             (body (if named? (cddr rest) (cdr rest)))
             (valid-bindings (%let-bindings bindings))
            ) ;
        (if (and valid-bindings (%unique-let-bindings? valid-bindings) (pair? body))
          (let* ((inits (%scan-let-inits file valid-bindings env))
                 (loop-scope
                   (if name
                     (%scope-extend
                       (list (cons name (list 'fixed (length valid-bindings))))
                       env
                     ) ;%scope-extend
                     env
                   ) ;if
                 ) ;loop-scope
                 (body-scope (%scope-shadow (map %binding-name valid-bindings) loop-scope))
                 (body-scope (%let-scope valid-bindings body-scope))
                ) ;
            (append inits (%scan-body file body body-scope))
          ) ;let*
          '()
        ) ;if
      ) ;let*
    ) ;define

    (define (%scan-let-star file bindings body env)
      (if (not (and (%let-bindings bindings) (pair? body)))
        '()
        (let loop
          ((rest bindings) (scope env) (acc '()))
          (if (null? rest)
            (append acc (%scan-body file body scope))
            (let* ((binding (car rest))
                   (init-result (%scan-form file (%binding-init binding) scope))
                   (name (%binding-name binding))
                   (signature (%procedure-signature (%binding-init binding)))
                   (next
                     (%scope-extend (list (cons name (or signature 'shadow))) scope)
                   ) ;next
                  ) ;
              (loop (cdr rest) next (append acc init-result))
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (%scan-letrec file bindings body env)
      (if (not (and (%let-bindings bindings) (%unique-let-bindings? bindings) (pair? body))
          ) ;not
        '()
        (let* ((scope (%let-scope bindings env))
               (inits (%scan-let-inits file bindings scope))
              ) ;
          (append inits (%scan-body file body scope))
        ) ;let*
      ) ;if
    ) ;define

    (define (%scan-special file form env)
      (let ((head (car form)))
        (cond ((memq head
                 '(quote quasiquote cond-expand eval apply define-syntax
                    define-macro syntax-rules)
               ) ;memq
               '()
              ) ;
              ((eq? head 'begin) (%scan-body file (cdr form) env))
              ((eq? head 'if) (%scan-sequence file (cdr form) env))
              ((eq? head 'set!)
               (if (and (pair? (cdr form)) (pair? (cddr form)))
                 (%scan-sequence file (cddr form) env)
                 '()
               ) ;if
              ) ;
              ((memq head '(and or)) (%scan-sequence file (cdr form) env))
              ((eq? head 'lambda) (%scan-procedure file form env))
              ((eq? head 'case-lambda) (%scan-case-lambda file (cdr form) env))
              ((memq head '(define define*)) (%scan-definition file form env))
              ((and (eq? head 'let) (pair? (cdr form)) (pair? (cddr form)))
               (%scan-let file form env)
              ) ;
              ((and (eq? head 'let*) (pair? (cdr form)) (pair? (cddr form)))
               (%scan-let-star file (cadr form) (cddr form) env)
              ) ;
              ((and (eq? head 'letrec) (pair? (cdr form)) (pair? (cddr form)))
               (%scan-letrec file (cadr form) (cddr form) env)
              ) ;
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%scan-form file form env)
      (if (not (and (pair? form) (proper-list? form)))
        '()
        (let* ((head (car form))
               (special (and (symbol? head) (%scan-special file form env)))
               (binding (and (symbol? head) (%resolve-binding head env)))
              ) ;
          (cond (special special)
                ((and (symbol? head) (pair? binding))
                 (let ((args (cdr form)) (signature binding))
                   (append
                     (if (or (%keyword-call? args) (%signature-accepts? signature (length args)))
                       '()
                       (list (list 'arity
                               (relativize-path file)
                               (form-line form)
                               head
                               (length args)
                               signature
                             ) ;list
                       ) ;list
                     ) ;if
                     (%scan-sequence file args env)
                   ) ;append
                 ) ;let
                ) ;
                (else '())
          ) ;cond
        ) ;let*
      ) ;if
    ) ;define

    (define (%scan-file-arity file)
      (let* ((forms (read-all-forms file))
             (body (%expand-begins (%source-body forms)))
             (mutations (%mutation-env body))
             (bindings (%collect-bindings body mutations))
            ) ;
        (%scan-sequence file body bindings)
      ) ;let*
    ) ;define
  ) ;begin
) ;define-library
