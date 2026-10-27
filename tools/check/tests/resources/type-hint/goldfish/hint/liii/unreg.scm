(define-library (hint liii unreg)
  (import (scheme base))
  (export hint-liii/unreg)
  (begin

    (define hint-f1
      '((any? -> any?))
    ) ;define

    ;; 定义了 hint-f2，但查询函数未登记 f2
    (define hint-f2
      '((any? -> any?))
    ) ;define

    (define (hint-liii/unreg name)
      (case name
       ((f1) hint-f1)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
