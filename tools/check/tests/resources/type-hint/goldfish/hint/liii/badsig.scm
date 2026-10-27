(define-library (hint liii badsig)
  (import (scheme base))
  (export hint-liii/badsig)
  (begin

    ;; 缺 ->
    (define hint-f1
      '((any? any?))
    ) ;define

    ;; -> 后两个返回类型
    (define hint-f2
      '((any? -> any? any?))
    ) ;define

    ;; 参数类型非符号
    (define hint-f3
      '((1 -> any?))
    ) ;define

    (define (hint-liii/badsig name)
      (case name
       ((f1) hint-f1)
       ((f2) hint-f2)
       ((f3) hint-f3)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
