;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
(define-library (liii goldcheck analysis)
  (import (scheme base)
    (liii base)
    (liii list)
    (liii string)
    (liii goldcheck type-hint parse)
    (liii goldcheck type-hint env)
    (liii goldcheck type-hint hint-data)
  ) ;import
  (export resolve-library reexport-owner normalize-import-mapping procedure-arity)
  (begin

    (define (resolve-library lib-name . maybe-roots)
      (let* ((roots (if (null? maybe-roots) (all-hint-roots) (car maybe-roots)))
             (file (library-file roots lib-name #t))
            ) ;
        (if (not file)
          (list 'indeterminate "library source file could not be resolved")
          (let ((info (cached-library-info file)))
            (if (not info)
              (list 'indeterminate "library source file could not be parsed")
              (list 'resolved file (lib-exports info))
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (dedup-items xs)
      (let loop
        ((rest xs) (acc '()))
        (if (null? rest)
          (reverse acc)
          (if (member (car rest) acc equal?)
            (loop (cdr rest) acc)
            (loop (cdr rest) (cons (car rest) acc))
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (symbols->names xs)
      (map symbol->string xs)
    ) ;define

    (define (import-base-name clause)
      (cond ((symbol? clause) clause)
            ((and (pair? clause)
               (memq (car clause) '(only except prefix rename for))
               (pair? (cdr clause))
             ) ;and
             (import-base-name (cadr clause))
            ) ;
            ((and (pair? clause)
               (let loop
                 ((xs clause))
                 (cond ((null? xs) #t)
                       ((symbol? (car xs)) (loop (cdr xs)))
                       (else #f)
                 ) ;cond
               ) ;let
             ) ;and
             clause
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (export-name-list info)
      (symbols->names (lib-exports info))
    ) ;define

    (define (normalize-simple-import lib-name . maybe-roots)
      (let ((resolved (apply resolve-library (append (list lib-name) maybe-roots))))
        (if (eq? (car resolved) 'resolved)
          (map string->symbol (export-name-list (cached-library-info (cadr resolved))))
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (symbol-prefix prefix name)
      (string->symbol (string-append (symbol->string prefix) (symbol->string name)))
    ) ;define

    (define (normalize-import-mapping clause . maybe-roots)
      (cond
       ((symbol? clause)
        (let ((names (apply normalize-simple-import (append (list clause) maybe-roots))))
          (if names
            (map (lambda (n) (cons n n)) names)
            (list 'indeterminate "import library could not be resolved")
          ) ;if
        ) ;let
       ) ;
       ((and (pair? clause)
          (let loop
            ((xs clause))
            (cond ((null? xs) #t)
                  ((symbol? (car xs)) (loop (cdr xs)))
                  (else #f)
            ) ;cond
          ) ;let
        ) ;and
        (let ((names (apply normalize-simple-import (append (list clause) maybe-roots))))
          (if names
            (map (lambda (n) (cons n n)) names)
            (list 'indeterminate "import library could not be resolved")
          ) ;if
        ) ;let
       ) ;
       ((and (pair? clause) (eq? (car clause) 'only) (>= (length clause) 3))
        (let ((names
                (apply normalize-import-mapping (append (list (cadr clause)) maybe-roots))
              ) ;names
             ) ;
          (if
            (and (pair? names) (not (eq? (car names) 'indeterminate)))
            (let ((wanted (cddr clause)))
              (filter (lambda (m) (memq (cdr m) wanted)) names)
            ) ;let
            names
          ) ;if
        ) ;let
       ) ;
       ((and (pair? clause) (eq? (car clause) 'except) (>= (length clause) 3))
        (let ((names
                (apply normalize-import-mapping (append (list (cadr clause)) maybe-roots))
              ) ;names
             ) ;
          (if
            (and (pair? names) (not (eq? (car names) 'indeterminate)))
            (let ((excluded (cddr clause)))
              (filter
                (lambda (m) (not (memq (cdr m) excluded)))
                names
              ) ;filter
            ) ;let
            names
          ) ;if
        ) ;let
       ) ;
       ((and (pair? clause) (eq? (car clause) 'prefix) (= (length clause) 3))
        (let ((names
                (apply normalize-import-mapping (append (list (cadr clause)) maybe-roots))
              ) ;names
             ) ;
          (if
            (and (pair? names) (not (eq? (car names) 'indeterminate)))
            (map
              (lambda (m) (cons (symbol-prefix (caddr clause) (cdr m)) (cdr m)))
              names
            ) ;map
            names
          ) ;if
        ) ;let
       ) ;
       ((and (pair? clause) (eq? (car clause) 'rename) (>= (length clause) 3))
        (let ((names
                (apply normalize-import-mapping (append (list (cadr clause)) maybe-roots))
              ) ;names
             ) ;
          (if
            (and (pair? names) (not (eq? (car names) 'indeterminate)))
            (let loop-rename
              ((rs (cddr clause)) (current names))
              (if (null? rs)
                current
                (let ((r (car rs)))
                  (cond
                   ((and (pair? r) (= (length r) 2) (symbol? (car r)) (symbol? (cadr r)))
                    (let ((old (car r)) (new (cadr r)))
                      (loop-rename (cdr rs)
                        (map
                          (lambda (m) (if (eq? (cdr m) old) (cons new old) m))
                          current
                        ) ;map
                      ) ;loop-rename
                    ) ;let
                   ) ;
                   ((and (symbol? r) (pair? (cdr rs)) (symbol? (cadr rs)))
                    (let ((old r) (new (cadr rs)))
                      (loop-rename (cddr rs)
                        (map
                          (lambda (m) (if (eq? (cdr m) old) (cons new old) m))
                          current
                        ) ;map
                      ) ;loop-rename
                    ) ;let
                   ) ;
                   (else (loop-rename (cdr rs) current))
                  ) ;cond
                ) ;let
              ) ;if
            ) ;let
            names
          ) ;if
        ) ;let
       ) ;
       (else
         (let ((lib-name (import-base-name clause)))
           (if lib-name
             (apply normalize-import-mapping (append (list lib-name) maybe-roots))
             (list 'indeterminate "unsupported import form")
           ) ;if
         ) ;let
       ) ;else
      ) ;cond
    ) ;define

    (define (local-definition? info name)
      (and (find-def name (lib-defines info)) #t)
    ) ;define

    (define (reexport-owner lib-name external-name . maybe-roots)
      (let ((roots (if (null? maybe-roots) (all-hint-roots) (car maybe-roots))))
        (let loop-owner
          ((current-lib lib-name) (current-name external-name) (seen '()))
          (if (member current-lib seen equal?)
            (list 'indeterminate
              "ambiguous re-export source"
              (list current-lib)
              current-name
            ) ;list
            (let ((resolved (resolve-library current-lib roots)))
              (if (not (eq? (car resolved) 'resolved))
                resolved
                (let* ((file (cadr resolved))
                       (info (cached-library-info file))
                       (entries (lib-export-entries info))
                       (entry (assq current-name entries))
                      ) ;
                  (if (not entry)
                    (list 'not-exported current-name)
                    (let* ((internal-name (cdr entry)) (local? (local-definition? info internal-name)))
                      (if local?
                        (list 'owner current-lib internal-name)
                        (let ((sources '()) (indeterminate? #f))
                          (let scan
                            ((clauses (lib-import-clauses info)))
                            (if (null? clauses)
                              (let ((unique (dedup-items sources)))
                                (cond
                                 ((not (null? unique))
                                  (if (null? (cdr unique))
                                    (loop-owner (car unique) internal-name (cons current-lib seen))
                                    (list 'indeterminate "ambiguous re-export source" unique internal-name)
                                  ) ;if
                                 ) ;
                                 (indeterminate? (list 'indeterminate "re-export source could not be resolved"))
                                 (else (list 'indeterminate "re-export source could not be resolved"))
                                ) ;cond
                              ) ;let
                              (let* ((clause (car clauses))
                                     (base-lib (import-base-name clause))
                                     (mapping (if base-lib
                                                (normalize-import-mapping clause roots)
                                                (list 'indeterminate "unsupported import")
                                              ) ;if
                                     ) ;mapping
                                    ) ;
                                (if
                                  (and (pair? mapping) (not (eq? (car mapping) 'indeterminate)))
                                  (let ((matches (filter (lambda (m) (eq? (cdr m) internal-name)) mapping)))
                                    (if (pair? matches) (set! sources (cons base-lib sources)) #f)
                                  ) ;let
                                  (set! indeterminate? #t)
                                ) ;if
                                (scan (cdr clauses))
                              ) ;let*
                            ) ;if
                          ) ;let
                        ) ;let
                      ) ;if
                    ) ;let*
                  ) ;if
                ) ;let*
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

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
