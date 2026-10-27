(define-library (hint liii arity)
  (import (scheme base))
  (export hint-liii/arity)
  (begin

    ;; f1 签名 1 参，实际 2 参
    (define hint-f1
      '((any? -> any?))
    ) ;define

    ;; f2 签名 1/3 参，子句实际 1/2 参
    (define hint-f2
      '((any? -> any?)
        (any? any? any? -> any?))
    ) ;define

    (define (hint-liii/arity name)
      (case name
       ((f1) hint-f1)
       ((f2) hint-f2)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
