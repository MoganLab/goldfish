(define-library (liii alist)
  (import (liii base) (liii list) (liii error) (scheme case-lambda))
  (export alist? alist-cons alist-ref alist-ref/default vector->alist)
  (begin

    (define (alist? l)
      (and (list? l) (every pair? l))
    ) ;define

    (define alist-ref
      (case-lambda
       ((alist key)
        (alist-ref alist key (lambda () (key-error "alist-ref: key not found " key)))
       ) ;
       ((alist key thunk) (alist-ref alist key thunk eqv?))
       ((alist key thunk =)
        (let ((value (assoc key alist =)))
          (if value (cdr value) (thunk))
        ) ;let
       ) ;
      ) ;case-lambda
    ) ;define

    (define alist-ref/default
      (case-lambda
       ((alist key default) (alist-ref alist key (lambda () default)))
       ((alist key default =) (alist-ref alist key (lambda () default) =))
      ) ;case-lambda
    ) ;define

    (define (vector->alist x)
      (unless (vector? x)
        (error 'type-error "vector->alist: argument must be a vector" x)
      ) ;unless
      (if (zero? (length x))
        '()
        (let loop
          ((x (vector->list x)) (n 0))
          (cons (cons n (car x)) (if (null? (cdr x)) '() (loop (cdr x) (+ n 1))))
        ) ;let
      ) ;if
    ) ;define
  ) ;begin
) ;define-library
