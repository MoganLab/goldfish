;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;

(define-library (liii goldcheck free-variables internal check)
  (import (scheme base)
    (liii goldcheck analysis)
    (liii goldcheck free-variables internal scope)
    (liii goldcheck path)
    (liii goldcheck type-hint env)
    (liii goldcheck type-hint parse)
  ) ;import
  (export %check-free-variables)
  (begin

    (define (library-body forms info)
      (if (not info)
        forms
        (let loop
          ((xs forms))
          (cond ((null? xs) '())
                ((and (pair? (car xs)) (eq? (caar xs) 'define-library)) (cddr (car xs)))
                (else (loop (cdr xs)))
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (record-names-in forms)
      (if (null? forms)
        '()
        (let ((form (car forms)))
          (append
            (if (and (pair? form) (eq? (car form) 'define-record-type))
              (record-names form)
              '()
            ) ;if
            (if (and (pair? form) (eq? (car form) 'begin)) (record-names-in (cdr form)) '())
            (record-names-in (cdr forms))
          ) ;append
        ) ;let
      ) ;if
    ) ;define

    (define (import-env clauses roots)
      (let loop
        ((xs clauses) (env '()))
        (if (null? xs)
          env
          (let ((mapping (normalize-import-mapping (car xs) roots)))
            (if (or (not (pair? mapping)) (eq? (car mapping) 'indeterminate))
              #f
              (loop (cdr xs) (append (map car mapping) env))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (record-field-names spec)
      (if (and (pair? spec) (pair? (cdr spec)) (symbol? (cadr spec)))
        (cons (cadr spec)
          (if (and (pair? (cddr spec)) (symbol? (caddr spec))) (list (caddr spec)) '())
        ) ;cons
        '()
      ) ;if
    ) ;define

    (define (record-fields-names specs)
      (if (null? specs)
        '()
        (append (record-field-names (car specs)) (record-fields-names (cdr specs)))
      ) ;if
    ) ;define

    (define (record-clause-names clause)
      (if (and (pair? clause) (eq? (car clause) 'fields))
        (record-fields-names (cdr clause))
        '()
      ) ;if
    ) ;define

    (define (record-clauses-names clauses)
      (if (null? clauses)
        '()
        (append (record-clause-names (car clauses))
          (record-clauses-names (cdr clauses))
        ) ;append
      ) ;if
    ) ;define

    (define (record-base-names spec ctor pred)
      (append (list (car spec))
        (if (symbol? ctor) (list ctor) (if (pair? ctor) (list (car ctor)) '()))
        (if (symbol? pred) (list pred) '())
      ) ;append
    ) ;define

    (define (record-names form)
      (let ((spec (and (pair? (cddr form)) (caddr form))))
        (if (and (pair? spec) (pair? (cdr spec)))
          (append
            (record-base-names spec (cadr spec) (and (pair? (cdddr form)) (cadddr form)))
            (record-clauses-names (cddddr form))
          ) ;append
          '()
        ) ;if
      ) ;let
    ) ;define

    (define (top-define-names forms)
      (%filter symbol?
        (map
          (lambda (form)
            (and (pair? form)
              (memq (car form) '(define define*))
              (pair? (cdr form))
              (if (symbol? (cadr form))
                (cadr form)
                (and (pair? (cadr form)) (car (cadr form)))
              ) ;if
            ) ;and
          ) ;lambda
          forms
        ) ;map
      ) ;%filter
    ) ;define

    (define (top-level-env info clauses roots)
      (let ((imports (import-env clauses roots)))
        (and imports (append imports (map def-name (lib-defines info))))
      ) ;let
    ) ;define

    (define (%check-free-variables file . maybe-roots)
      (let* ((forms (read-all-forms file))
             (info (parse-library-file file))
             (roots (if (null? maybe-roots) (all-hint-roots) (car maybe-roots)))
             (clauses
               (if info
                 (lib-import-clauses info)
                 (apply append
                   (map
                     (lambda (x) (if (and (pair? x) (eq? (car x) 'import)) (cdr x) '()))
                     forms
                   ) ;map
                 ) ;apply
               ) ;if
             ) ;clauses
             (env
               (if info
                 (top-level-env info clauses roots)
                 (let ((imports (import-env clauses roots)))
                   (and imports (append (top-define-names forms) imports))
                 ) ;let
               ) ;if
             ) ;env
            ) ;
        (if (not env)
          '()
          (let ((body (library-body forms info)))
            (%scan-sequence file body (append (record-names-in body) env))
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
