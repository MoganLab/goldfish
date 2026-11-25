;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;

(define-library (liii goldcheck free-variables internal scope)
  (import (scheme base) (liii goldcheck path) (liii goldcheck type-hint parse))
  (export %filter %scan-sequence)
  (begin

    (define %special-forms
      '(begin define define* define-record-type lambda let let* letrec letrec*
         let-values let*-values named-let do case-lambda if cond and or not set!
         quote quasiquote unquote unquote-splicing import export define-library
         define-syntax syntax-rules cond-expand else =>)
    ) ;define

    (define %global-bindings
      '(car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar
         cddr cdddr cadddr cddddr caddddr apply call-with-values values
         dynamic-wind error type-error when unless for-each map filter every any
         identity catch pair-line-number read)
    ) ;define

    (define (keyword-symbol? name)
      (and (symbol? name)
        (let ((text (symbol->string name)))
          (and (> (string-length text) 0) (char=? (string-ref text 0) #\:))
        ) ;let
      ) ;and
    ) ;define

    (define (%filter predicate xs)
      (let loop
        ((rest xs) (acc '()))
        (if (null? rest)
          (reverse acc)
          (loop (cdr rest) (if (predicate (car rest)) (cons (car rest) acc) acc))
        ) ;if
      ) ;let
    ) ;define

    (define (bound? name env)
      (memq name env)
    ) ;define

    (define (formals form)
      (cond ((symbol? form) (list form))
            ((null? form) '())
            ((pair? form)
             (let loop
               ((xs form) (acc '()))
               (cond ((null? xs) (reverse acc))
                     ((symbol? xs) (reverse (cons xs acc)))
                     ((pair? xs)
                      (let ((formal (car xs)))
                        (loop (cdr xs)
                          (cons
                            (if (and (pair? formal) (symbol? (car formal))) (car formal) formal)
                            acc
                          ) ;cons
                        ) ;loop
                      ) ;let
                     ) ;
                     (else #f)
               ) ;cond
             ) ;let
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (warning file form name)
      (list 'free-variables file (form-line form) (symbol->string name))
    ) ;define

    (define (append-result a b)
      (if (and a b) (append a b) #f)
    ) ;define

    (define (%scan-sequence file forms env)
      (let loop
        ((xs forms) (acc '()))
        (if (or (null? xs) (not (pair? xs)))
          acc
          (let ((result (scan-form file (car xs) env)))
            (if (not result) (loop (cdr xs) acc) (loop (cdr xs) (append acc result)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (binding-name binding)
      (cond ((symbol? binding) binding)
            ((and (pair? binding) (symbol? (car binding))) (car binding))
            (else #f)
      ) ;cond
    ) ;define

    (define (binding-expression binding)
      (and (pair? binding) (pair? (cdr binding)) (cadr binding))
    ) ;define

    (define (binding-names bindings)
      (let loop
        ((xs bindings) (acc '()))
        (if (null? xs)
          (reverse acc)
          (let ((name (binding-name (car xs))))
            (if name (loop (cdr xs) (cons name acc)) #f)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (scan-bindings file bindings env)
      (let loop
        ((xs bindings) (acc '()))
        (if (null? xs)
          acc
          (let ((expr (binding-expression (car xs))))
            (if (not expr)
              #f
              (let ((result (scan-form file expr env)))
                (if result (loop (cdr xs) (append acc result)) #f)
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (definition-name form)
      (and (pair? form)
        (memq (car form) '(define define*))
        (pair? (cdr form))
        (if (symbol? (cadr form))
          (cadr form)
          (and (pair? (cadr form)) (car (cadr form)))
        ) ;if
      ) ;and
    ) ;define

    (define (scan-body file body env)
      ;; Internal definitions are visible throughout a body.
      (let* ((names (%filter symbol? (map definition-name body)))
             (body-env (append names env))
            ) ;
        (%scan-sequence file body body-env)
      ) ;let*
    ) ;define

    (define (scan-lambda file form env)
      (if (and (pair? (cdr form)) (pair? (cddr form)))
        (let ((names (formals (cadr form))))
          (if names (scan-body file (cddr form) (append names env)) #f)
        ) ;let
        #f
      ) ;if
    ) ;define

    (define (scan-let file form env kind)
      (let* ((rest (cdr form))
             (named? (and (eq? kind 'let) (pair? rest) (symbol? (car rest))))
             (name (and named? (car rest)))
             (bindings (if named? (cadr rest) (car rest)))
             (body (if named? (cddr rest) (cdr rest)))
             (names (and (pair? bindings) (binding-names bindings)))
             (rhs-env (if (and (eq? kind 'letrec) names) (append names env) env))
            ) ;
        (if (and names (pair? body))
          (let ((rhs (scan-bindings file bindings rhs-env)))
            (if rhs
              (let ((body-env (append (if name (cons name names) names) env)))
                (append-result rhs (scan-body file body body-env))
              ) ;let
              #f
            ) ;if
          ) ;let
          #f
        ) ;if
      ) ;let*
    ) ;define

    (define (scan-let-star file form env)
      (if (and (pair? (cdr form)) (pair? (cddr form)))
        (let loop
          ((bindings (cadr form)) (current env) (acc '()))
          (if (null? bindings)
            (append-result acc (scan-body file (cddr form) current))
            (let* ((binding (car bindings))
                   (name (binding-name binding))
                   (expr (binding-expression binding))
                  ) ;
              (if (and name expr)
                (let ((result (scan-form file expr current)))
                  (if result (loop (cdr bindings) (cons name current) (append acc result)) #f)
                ) ;let
                #f
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
        #f
      ) ;if
    ) ;define

    (define (value-binding-names form)
      (formals form)
    ) ;define

    (define (scan-let-values file form env sequential?)
      (if (and (pair? (cdr form)) (pair? (cddr form)))
        (let loop
          ((bindings (cadr form)) (current env) (acc '()))
          (if (null? bindings)
            (append-result acc (scan-body file (cddr form) current))
            (let* ((binding (car bindings))
                   (names (and (pair? binding) (value-binding-names (car binding))))
                   (expr (and (pair? binding) (pair? (cdr binding)) (cadr binding)))
                   (rhs-env (if sequential? current env))
                  ) ;
              (if (and names expr)
                (let ((result (scan-form file expr rhs-env)))
                  (if result (loop (cdr bindings) (append names current) (append acc result)) #f)
                ) ;let
                #f
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
        #f
      ) ;if
    ) ;define

    (define (scan-do-steps file bindings env)
      (if (null? bindings)
        '()
        (let* ((binding (car bindings))
               (result
                 (if (and (pair? binding) (pair? (cddr binding)))
                   (scan-form file (caddr binding) env)
                   '()
                 ) ;if
               ) ;result
              ) ;
          (if result (append-result result (scan-do-steps file (cdr bindings) env)) #f)
        ) ;let*
      ) ;if
    ) ;define

    (define (scan-do file form env)
      (if (and (pair? (cdr form)) (pair? (cddr form)))
        (let* ((bindings (cadr form)) (names (binding-names bindings)) (body (cddr form)))
          (if (and names (pair? body) (pair? (car body)))
            (let ((init-result (scan-bindings file bindings env)) (loop-env (append names env)))
              (if init-result
                (let* ((termination (car body))
                       (test-result (if (pair? termination) (scan-form file (car termination) loop-env) #f)
                       ) ;test-result
                       (value-result (if (and test-result (pair? termination))
                                       (%scan-sequence file (cdr termination) loop-env)
                                       #f
                                     ) ;if
                       ) ;value-result
                       (command-result (if value-result (%scan-sequence file (cdr body) loop-env) #f))
                       (step-result (if command-result (scan-do-steps file bindings loop-env) #f))
                      ) ;
                  (append-result init-result
                    (append-result test-result
                      (append-result value-result (append-result command-result step-result))
                    ) ;append-result
                  ) ;append-result
                ) ;let*
                #f
              ) ;if
            ) ;let
            #f
          ) ;if
        ) ;let*
        #f
      ) ;if
    ) ;define

    (define (scan-case-lambda file form env)
      (let loop
        ((clauses (cdr form)) (acc '()))
        (if (null? clauses)
          acc
          (let* ((clause (car clauses)) (names (and (pair? clause) (formals (car clause)))))
            (if (and names (pair? (cdr clause)))
              (let ((result (scan-body file (cdr clause) (append names env))))
                (if result (loop (cdr clauses) (append acc result)) #f)
              ) ;let
              #f
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (scan-quasiquote file form env)
      (let scan
        ((x (cadr form)) (depth 1))
        (cond ((not (pair? x)) '())
              ((and (eq? (car x) 'unquote) (pair? (cdr x)))
               (if (= depth 1) (scan-form file (cadr x) env) (scan (cadr x) (- depth 1)))
              ) ;
              ((and (eq? (car x) 'quasiquote) (pair? (cdr x))) (scan (cadr x) (+ depth 1)))
              (else (append-result (scan (car x) depth) (scan (cdr x) depth)))
        ) ;cond
      ) ;let
    ) ;define

    (define (scan-definition file form env)
      (if (and (pair? (cdr form)) (symbol? (cadr form)))
        (%scan-sequence file (cddr form) env)
        (if (and (pair? (cdr form)) (pair? (cadr form)))
          (let* ((target (cadr form)) (names (and (pair? (cdr target)) (formals (cdr target)))))
            (if names (scan-body file (cddr form) (append names env)) #f)
          ) ;let*
          #f
        ) ;if
      ) ;if
    ) ;define

    (define (scan-form file form env)
      (cond
       ((not (pair? form))
        (if (or (not (symbol? form))
              (bound? form env)
              (memq form %special-forms)
              (memq form %global-bindings)
              (keyword-symbol? form)
            ) ;or
          '()
          (list (warning file form form))
        ) ;if
       ) ;
       ((null? form) '())
       (else
         (let ((head (car form)))
           (cond ((eq? head 'quote) '())
                 ((eq? head 'quasiquote) (scan-quasiquote file form env))
                 ((memq head '(define-syntax syntax-rules cond-expand eval
                                include)) #f)
                 ((eq? head 'define-record-type) '())
                 ((eq? head 'lambda) (scan-lambda file form env))
                 ((eq? head 'case-lambda) (scan-case-lambda file form env))
                 ((and (memq head '(let letrec)) (pair? (cdr form)))
                  (scan-let file form env head)
                 ) ;
                 ((memq head '(let* letrec*)) (scan-let-star file form env))
                 ((eq? head 'let-values) (scan-let-values file form env #f))
                 ((eq? head 'let*-values) (scan-let-values file form env #t))
                 ((eq? head 'do) (scan-do file form env))
                 ((memq head '(define define*)) (scan-definition file form env))
                 ((memq head '(import export define-library)) '())
                 ((and (symbol? head)
                    (not (bound? head env))
                    (not (memq head %global-bindings))
                    (not (memq head %special-forms))
                  ) ;and
                  (list (warning file form head))
                 ) ;
                 (else (%scan-sequence file (cdr form) env))
           ) ;cond
         ) ;let
       ) ;else
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
