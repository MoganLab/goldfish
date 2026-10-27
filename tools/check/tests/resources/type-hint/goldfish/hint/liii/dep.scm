(define-library (hint liii dep)
  (import (scheme base))
  (export hint-liii/dep)
  (begin

    ;; my-assoc : (any? alist? -> (or #f pair?))
    (define hint-my-assoc
      '((any? alist? -> (or #f pair?)))
    ) ;define

    ;; my-cdr : (pair? -> any?)
    (define hint-my-cdr
      '((pair? -> any?))
    ) ;define

    ;; my-cons : (any? any? -> pair?)
    (define hint-my-cons
      '((any? any? -> pair?))
    ) ;define

    (define (hint-liii/dep name)
      (case name
       ((my-assoc) hint-my-assoc)
       ((my-cdr) hint-my-cdr)
       ((my-cons) hint-my-cons)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
