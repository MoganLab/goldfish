;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
(define-library (liii goldcheck arity internal signature)
  (import (scheme base) (liii list) (liii goldcheck type-hint parse))
  (export %definition-name %definition-signature %procedure-signature
    %formals-names %star-formals-names %signature-accepts? %keyword-call?
  ) ;export
  (begin
    (define (%keyword-symbol? name)
      (and (symbol? name)
        (let ((text (symbol->string name)))
          (and (> (string-length text) 0) (char=? (string-ref text 0) #\:))
        ) ;let
      ) ;and
    ) ;define

    (define (%valid-formal? name)
      (and (symbol? name) (not (keyword? name)) (not (%keyword-symbol? name)))
    ) ;define

    (define (%unique-name? name names)
      (not (memq name names))
    ) ;define

    (define (%ordinary-signature formals)
      (let loop
        ((rest formals) (count 0) (names '()))
        (cond ((null? rest) (list 'fixed count))
              ((symbol? rest)
               (and (%valid-formal? rest) (%unique-name? rest names) (list 'rest count))
              ) ;
              ((pair? rest)
               (let ((name (car rest)))
                 (and (%valid-formal? name)
                   (%unique-name? name names)
                   (loop (cdr rest) (+ count 1) (cons name names))
                 ) ;and
               ) ;let
              ) ;
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%star-signature formals)
      (if (not (proper-list? formals))
        #f
        (let loop
          ((rest formals) (required 0) (optional 0) (optional-seen? #f) (names '()))
          (cond
           ((null? rest)
            (if (= optional 0)
              (list 'fixed required)
              (list 'range required (+ required optional))
            ) ;if
           ) ;
           ((eq? (car rest) :allow-other-keys)
            (loop (cdr rest) required optional optional-seen? names)
           ) ;
           ((eq? (car rest) :rest)
            (if (and (pair? (cdr rest))
                  (%valid-formal? (cadr rest))
                  (%unique-name? (cadr rest) names)
                  (null? (cddr rest))
                ) ;and
              (list 'rest required)
              #f
            ) ;if
           ) ;
           ((keyword? (car rest)) #f)
           ((symbol? (car rest))
            (and (not optional-seen?)
              (%valid-formal? (car rest))
              (%unique-name? (car rest) names)
              (loop (cdr rest) (+ required 1) optional #f (cons (car rest) names))
            ) ;and
           ) ;
           ((and (pair? (car rest))
              (proper-list? (car rest))
              (= (length (car rest)) 2)
              (%valid-formal? (caar rest))
              (%unique-name? (caar rest) names)
            ) ;and
            (loop (cdr rest) required (+ optional 1) #t (cons (caar rest) names))
           ) ;
           (else #f)
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (%formals-names formals)
      (let loop
        ((rest formals) (names '()))
        (cond ((null? rest) (reverse names))
              ((symbol? rest) (if (%valid-formal? rest) (reverse (cons rest names)) #f))
              ((pair? rest)
               (let ((formal (car rest)))
                 (cond ((%valid-formal? formal) (loop (cdr rest) (cons formal names)))
                       ((and (pair? formal)
                          (proper-list? formal)
                          (= (length formal) 2)
                          (%valid-formal? (car formal))
                        ) ;and
                        (loop (cdr rest) (cons (car formal) names))
                       ) ;
                       (else #f)
                 ) ;cond
               ) ;let
              ) ;
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%star-formals-names formals)
      (if (not (proper-list? formals))
        #f
        (let loop
          ((rest formals) (names '()))
          (cond ((null? rest) (reverse names))
                ((eq? (car rest) :allow-other-keys) (loop (cdr rest) names))
                ((eq? (car rest) :rest)
                 (if (and (pair? (cdr rest))
                       (%valid-formal? (cadr rest))
                       (%unique-name? (cadr rest) names)
                       (null? (cddr rest))
                     ) ;and
                   (reverse (cons (cadr rest) names))
                   #f
                 ) ;if
                ) ;
                ((%valid-formal? (car rest))
                 (if (%unique-name? (car rest) names)
                   (loop (cdr rest) (cons (car rest) names))
                   #f
                 ) ;if
                ) ;
                ((and (pair? (car rest))
                   (proper-list? (car rest))
                   (= (length (car rest)) 2)
                   (%valid-formal? (caar rest))
                 ) ;and
                 (if (%unique-name? (caar rest) names)
                   (loop (cdr rest) (cons (caar rest) names))
                   #f
                 ) ;if
                ) ;
                (else #f)
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (%case-signature clauses)
      (if (not (proper-list? clauses))
        #f
        (let loop
          ((rest clauses) (signatures '()))
          (if (null? rest)
            (and (pair? signatures) (cons 'case (reverse signatures)))
            (let* ((clause (car rest))
                   (signature (and (pair? clause)
                                (pair? (cdr clause))
                                (proper-list? clause)
                                (%ordinary-signature (car clause))
                              ) ;and
                   ) ;signature
                  ) ;
              (if signature (loop (cdr rest) (cons signature signatures)) #f)
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (%procedure-signature expr)
      (cond
       ((and (pair? expr)
          (eq? (car expr) 'lambda)
          (pair? (cdr expr))
          (pair? (cddr expr))
        ) ;and
        (%ordinary-signature (cadr expr))
       ) ;
       ((and (pair? expr) (eq? (car expr) 'case-lambda) (pair? (cdr expr)))
        (%case-signature (cdr expr))
       ) ;
       (else #f)
      ) ;cond
    ) ;define

    (define (%definition-name form)
      (and (pair? form)
        (memq (car form) '(define define*))
        (pair? (cdr form))
        (let ((target (cadr form)))
          (if (symbol? target)
            target
            (and (pair? target) (symbol? (car target)) (car target))
          ) ;if
        ) ;let
      ) ;and
    ) ;define

    (define (%definition-signature form)
      (if (and (pair? form)
            (memq (car form) '(define define*))
            (pair? (cdr form))
            (pair? (cddr form))
          ) ;and
        (let ((target (cadr form)))
          (if (pair? target)
            (if (pair? (cddr form))
              (if (eq? (car form) 'define*)
                (%star-signature (cdr target))
                (%ordinary-signature (cdr target))
              ) ;if
              #f
            ) ;if
            (if (and (symbol? target) (= (length form) 3))
              (%procedure-signature (caddr form))
              #f
            ) ;if
          ) ;if
        ) ;let
        #f
      ) ;if
    ) ;define

    (define (%signature-accepts? signature count)
      (cond ((eq? (car signature) 'fixed) (= count (cadr signature)))
            ((eq? (car signature) 'range)
             (and (>= count (cadr signature)) (<= count (caddr signature)))
            ) ;
            ((eq? (car signature) 'rest) (>= count (cadr signature)))
            ((eq? (car signature) 'case)
             (any (lambda (clause) (%signature-accepts? clause count)) (cdr signature))
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (%keyword-call? args)
      (and (proper-list? args)
        (any (lambda (arg) (or (keyword? arg) (%keyword-symbol? arg))) args)
      ) ;and
    ) ;define
  ) ;begin
) ;define-library
