;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
(define-library (liii goldcheck analysis arity)
  (import (scheme base)
    (liii base)
    (liii goldcheck analysis library)
    (liii goldcheck analysis reexport)
    (liii goldcheck type-hint parse)
    (liii goldcheck type-hint env)
    (liii goldcheck type-hint hint-data)
  ) ;import
  (export procedure-arity)
  (begin
    (define (star-descriptor d)
      (let* ((e (def-expr d))
             (target (and (pair? e) (pair? (cdr e)) (cadr e)))
             (formals (and (pair? target) (cdr target)))
            ) ;
        (if (proper-list? formals)
          (let loop
            ((ps formals) (required 0) (optional 0) (rest? #f) (keyword? #f))
            (if (null? ps)
              (cond (rest? (list 'rest required))
                    (keyword? (list 'keyword required (+ required optional) #t))
                    ((> optional 0) (list 'optional required (+ required optional) #t))
                    (else (list 'fixed required required))
              ) ;cond
              (let ((p (car ps)))
                (cond ((eq? p :rest) (loop (cdr ps) required optional #t keyword?))
                      ((eq? p :allow-other-keys) (loop (cdr ps) required optional rest? #t))
                      ((pair? p) (loop (cdr ps) required (+ optional 1) rest? keyword?))
                      ((symbol? p) (loop (cdr ps) (+ required 1) optional rest? keyword?))
                      (else (loop (cdr ps) required optional rest? keyword?))
                ) ;cond
              ) ;let
            ) ;if
          ) ;let
          (list 'rest 0)
        ) ;if
      ) ;let*
    ) ;define

    (define (rest-required-count d)
      (let* ((e (def-expr d))
             (target (and (pair? e) (pair? (cdr e)) (cadr e)))
             (formals (and (pair? target) (cdr target)))
            ) ;
        (if (pair? formals)
          (let loop
            ((f formals) (count 0))
            (cond ((null? f) count)
                  ((pair? f) (if (symbol? (car f)) (loop (cdr f) (+ count 1)) count))
                  (else count)
            ) ;cond
          ) ;let
          0
        ) ;if
      ) ;let*
    ) ;define

    (define (d-arity d)
      (let* ((kind (def-kind d)) (arity (def-arity d)))
        (cond ((eq? kind 'lambda) (list 'fixed arity arity))
              ((eq? kind 'star) (star-descriptor d))
              ((eq? kind 'rest) (list 'rest (rest-required-count d)))
              ((eq? kind 'case-lambda)
               (if (and (list? arity) (pair? arity))
                 (let loop
                   ((rest arity) (lo (car arity)) (hi (car arity)))
                   (if (null? rest)
                     (list 'fixed lo hi)
                     (loop (cdr rest)
                       (if (< (car rest) lo) (car rest) lo)
                       (if (> (car rest) hi) (car rest) hi)
                     ) ;loop
                   ) ;if
                 ) ;let
                 (list 'indeterminate "case-lambda arity could not be resolved")
               ) ;if
              ) ;
              (else (list 'indeterminate "procedure arity could not be resolved"))
        ) ;cond
      ) ;let*
    ) ;define

    (define (procedure-arity lib-name name . maybe-roots)
      (let ((roots (if (null? maybe-roots) (all-hint-roots) (car maybe-roots))))
        (let* ((resolved (resolve-library lib-name roots))
               (file (and (eq? (car resolved) 'resolved) (cadr resolved)))
               (info (and file (cached-library-info file)))
               (d (and info (find-def name (lib-defines info))))
              ) ;
          (if d
            (d-arity d)
            (let ((owner (reexport-owner lib-name name roots)))
              (if (eq? (car owner) 'owner)
                (let* ((owner-info
                         (cached-library-info (cadr (resolve-library (cadr owner) roots)))
                       ) ;owner-info
                       (owner-name (caddr owner))
                       (owner-def (and owner-info (find-def owner-name (lib-defines owner-info))))
                      ) ;
                  (if owner-def
                    (d-arity owner-def)
                    (list 'indeterminate "procedure definition could not be resolved")
                  ) ;if
                ) ;let*
                owner
              ) ;if
            ) ;let
          ) ;if
        ) ;let*
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
