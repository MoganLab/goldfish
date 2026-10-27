(define-library (hint liii restskip)
  (import (scheme base))
  (export hint-liii/restskip)
  (begin

    ;; f4 是 rest 参数、f5 绑定非 lambda，均保守跳过数量校验
    (define hint-f4
      '((any? -> any?))
    ) ;define

    (define hint-f5
      '((any? -> any?))
    ) ;define

    (define (hint-liii/restskip name)
      (case name
       ((f4) hint-f4)
       ((f5) hint-f5)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
