;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
(define-library (liii goldcheck analysis reexport)
  (import (scheme base)
    (liii base)
    (liii list)
    (liii goldcheck analysis library)
    (liii goldcheck analysis import)
    (liii goldcheck type-hint parse)
    (liii goldcheck type-hint env)
    (liii goldcheck type-hint hint-data)
  ) ;import
  (export reexport-owner)
  (begin
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

    (define (import-base-name clause)
      (cond ((symbol? clause) clause)
            ((and (pair? clause) (memq (car clause) '(only except prefix rename
                                                       for)))
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
                       (entry (assq current-name (lib-export-entries info)))
                      ) ;
                  (if (not entry)
                    (list 'not-exported current-name)
                    (let* ((internal-name (cdr entry))
                           (local? (find-def internal-name (lib-defines info)))
                          ) ;
                      (if local?
                        (list 'owner current-lib internal-name)
                        (let ((sources '()) (indeterminate? #f))
                          (let scan
                            ((clauses (lib-import-clauses info)))
                            (if (null? clauses)
                              (let ((unique (dedup-items sources)))
                                (cond
                                 ((and (pair? unique) (pair? (cdr unique)))
                                  (list 'indeterminate "ambiguous re-export source" unique internal-name)
                                 ) ;
                                 ((pair? unique) (loop-owner (car unique) internal-name (cons current-lib seen)))
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
                                    (when (pair? matches)
                                      (set! sources (cons base-lib sources))
                                    ) ;when
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
  ) ;begin
) ;define-library
