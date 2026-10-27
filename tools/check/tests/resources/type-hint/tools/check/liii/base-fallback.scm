(define-library (liii base-fallback)
  (import (scheme base))
  (export bad-cadr)
  (begin

    ;; tools/check 下没有局部 hint 时，也应使用 goldfish/hint/scheme/base。
    (define (bad-cadr x)
      (cadr 1)
    ) ;define
  ) ;begin
) ;define-library
