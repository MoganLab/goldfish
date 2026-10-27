(define-library (hint liii dupsig)
  (import (scheme base))
  (export hint-liii/dupsig)
  (begin

    ;; 两条完全相同的签名
    (define hint-f1
      '((any? -> any?) (any? -> any?))
    ) ;define

    (define (hint-liii/dupsig name)
      (case name
       ((f1) hint-f1)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
