(define-library (hint liii dep)
  (import (scheme base))
  (export hint-liii/dep)
  (begin

    ;; my-assoc : (any? alist? -> (or #f pair?))
    (define hint-my-assoc '((any? alist? -> (or #f pair?))))

    ;; my-cdr : (pair? -> any?)
    (define hint-my-cdr '((pair? -> any?)))

    ;; my-cons : (any? any? -> pair?)
    (define hint-my-cons '((any? any? -> pair?)))

    ;; my-number : (string? -> (or #f number?))
    (define hint-my-number '((string? -> (or #f number?))))

    ;; my-mul : (number? number? -> number?)
    (define hint-my-mul '((number? number? -> number?)))

    (define (hint-liii/dep name)
      (case name
       ((my-assoc) hint-my-assoc)
       ((my-cdr) hint-my-cdr)
       ((my-cons) hint-my-cons)
       ((my-number) hint-my-number)
       ((my-mul) hint-my-mul)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
