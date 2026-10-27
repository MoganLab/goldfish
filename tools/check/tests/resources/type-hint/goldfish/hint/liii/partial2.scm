(define-library (hint liii partial2)
  (import (scheme base))
  (export hint-liii/partial2)
  (begin

    ;; f1 签名 1 参，实际定义 2 参 -> 报 B2；f2 未标记 -> 跳过
    (define hint-f1
      '((any? -> any?))
    ) ;define

    (define (hint-liii/partial2 name)
      (case name
       ((f1) hint-f1)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
