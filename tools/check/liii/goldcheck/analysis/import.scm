;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
(define-library (liii goldcheck analysis import)
  (import (scheme base)
    (liii base)
    (liii list)
    (liii string)
    (liii goldcheck analysis library)
    (liii goldcheck type-hint parse)
    (liii goldcheck type-hint env)
  ) ;import
  (export normalize-import-mapping)
  (begin
    (define (library-name? clause)
      (and (pair? clause)
        (let loop
          ((xs clause))
          (cond ((null? xs) #t)
                ((symbol? (car xs)) (loop (cdr xs)))
                (else #f)
          ) ;cond
        ) ;let
      ) ;and
    ) ;define

    (define (import-base-name clause)
      (cond ((symbol? clause) clause)
            ((and (pair? clause)
               (memq (car clause) '(only except prefix rename for))
               (pair? (cdr clause))
             ) ;and
             (import-base-name (cadr clause))
            ) ;
            ((library-name? clause) clause)
            (else #f)
      ) ;cond
    ) ;define

    (define (simple-import lib-name . maybe-roots)
      (let ((resolved (apply resolve-library (append (list lib-name) maybe-roots))))
        (if (eq? (car resolved) 'resolved)
          (map string->symbol
            (map symbol->string (lib-exports (cached-library-info (cadr resolved))))
          ) ;map
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (symbol-prefix prefix name)
      (string->symbol (string-append (symbol->string prefix) (symbol->string name)))
    ) ;define

    (define (%resolved-mapping? names)
      (and (pair? names) (not (eq? (car names) 'indeterminate)))
    ) ;define

    (define (normalize-only clause maybe-roots names)
      (if (%resolved-mapping? names)
        (filter (lambda (m) (memq (cdr m) (cddr clause))) names)
        names
      ) ;if
    ) ;define

    (define (normalize-except clause names)
      (if (%resolved-mapping? names)
        (filter
          (lambda (m) (not (memq (cdr m) (cddr clause))))
          names
        ) ;filter
        names
      ) ;if
    ) ;define

    (define (normalize-prefix clause names)
      (if (%resolved-mapping? names)
        (map
          (lambda (m) (cons (symbol-prefix (caddr clause) (cdr m)) (cdr m)))
          names
        ) ;map
        names
      ) ;if
    ) ;define

    (define (normalize-rename-pairs rs names)
      (let loop
        ((rest rs) (current names))
        (if (null? rest)
          current
          (let ((r (car rest)))
            (cond
             ((and (pair? r) (= (length r) 2) (symbol? (car r)) (symbol? (cadr r)))
              (let ((old (car r)) (new (cadr r)))
                (loop (cdr rest)
                  (map
                    (lambda (m) (if (eq? (cdr m) old) (cons new old) m))
                    current
                  ) ;map
                ) ;loop
              ) ;let
             ) ;
             ((and (symbol? r) (pair? (cdr rest)) (symbol? (cadr rest)))
              (let ((old r) (new (cadr rest)))
                (loop (cddr rest)
                  (map
                    (lambda (m) (if (eq? (cdr m) old) (cons new old) m))
                    current
                  ) ;map
                ) ;loop
              ) ;let
             ) ;
             (else (loop (cdr rest) current))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (normalize-import-mapping clause . maybe-roots)
      (cond
       ((symbol? clause)
        (let ((names (apply simple-import (append (list clause) maybe-roots))))
          (if names
            (map (lambda (n) (cons n n)) names)
            (list 'indeterminate "import library could not be resolved")
          ) ;if
        ) ;let
       ) ;
       ((library-name? clause)
        (let ((names (apply simple-import (append (list clause) maybe-roots))))
          (if names
            (map (lambda (n) (cons n n)) names)
            (list 'indeterminate "import library could not be resolved")
          ) ;if
        ) ;let
       ) ;
       ((and (pair? clause)
          (memq (car clause) '(only except prefix rename))
          (pair? (cdr clause))
        ) ;and
        (let* ((names
                 (apply normalize-import-mapping (append (list (cadr clause)) maybe-roots))
               ) ;names
               (kind (car clause))
              ) ;
          (cond ((eq? kind 'only) (normalize-only clause maybe-roots names))
                ((eq? kind 'except) (normalize-except clause names))
                ((eq? kind 'prefix) (normalize-prefix clause names))
                ((%resolved-mapping? names) (normalize-rename-pairs (cddr clause) names))
                (else names)
          ) ;cond
        ) ;let*
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
  ) ;begin
) ;define-library
