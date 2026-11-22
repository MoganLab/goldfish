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

    (define (source-for-clause clause roots internal-name)
      (let* ((base-lib (import-base-name clause))
             (mapping (if base-lib
                        (normalize-import-mapping clause roots)
                        (list 'indeterminate "unsupported import")
                      ) ;if
             ) ;mapping
            ) ;
        (cond
         ((or (not (pair? mapping)) (eq? (car mapping) 'indeterminate))
          (list 'indeterminate)
         ) ;
         ((find (lambda (m) (eq? (cdr m) internal-name)) mapping)
          (list 'source base-lib)
         ) ;
         (else (list 'none))
        ) ;cond
      ) ;let*
    ) ;define

    (define (scan-reexport-sources clauses roots internal-name)
      (let loop
        ((rest clauses) (sources '()) (indeterminate? #f))
        (if (null? rest)
          (list sources indeterminate?)
          (let ((source (source-for-clause (car rest) roots internal-name)))
            (cond
             ((eq? (car source) 'source)
              (loop (cdr rest) (cons (cadr source) sources) indeterminate?)
             ) ;
             ((eq? (car source) 'indeterminate) (loop (cdr rest) sources #t))
             (else (loop (cdr rest) sources indeterminate?))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (finish-reexport-sources scan-result current-lib internal-name seen loop-owner)
      (let* ((sources (dedup-items (car scan-result))) (indeterminate? (cadr scan-result)))
        (cond
         ((and (pair? sources) (pair? (cdr sources)))
          (list 'indeterminate "ambiguous re-export source" sources internal-name)
         ) ;
         ((pair? sources)
          (loop-owner (car sources) internal-name (cons current-lib seen))
         ) ;
         (else (list 'indeterminate
                 (if indeterminate?
                   "re-export source could not be resolved"
                   "re-export source could not be resolved"
                 ) ;if
               ) ;list
         ) ;else
        ) ;cond
      ) ;let*
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
                        (finish-reexport-sources (scan-reexport-sources (lib-import-clauses info) roots internal-name)
                          current-lib
                          internal-name
                          seen
                          loop-owner
                        ) ;finish-reexport-sources
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
