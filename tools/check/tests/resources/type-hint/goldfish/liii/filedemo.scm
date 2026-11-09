(define-library (liii demo-bad)
  (import (scheme base))
  (export bad-good)
  (begin
    ;; string->number 结果未守卫即参与 <：确定冲突
    (define (bad-good s)
      (let ((n (string->number s)))
        (< n 10)
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
