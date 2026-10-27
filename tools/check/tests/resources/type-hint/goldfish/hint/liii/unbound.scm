(define-library (hint liii unbound)
  (import (scheme base))
  (export hint-liii/unbound)
  (begin

    (define hint-f1
      '((any? -> any?))
    ) ;define

    ;; 登记了 f2，但 hint-f2 未定义
    (define (hint-liii/unbound name)
      (case name
       ((f1) hint-f1)
       ((f2) hint-f2)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
