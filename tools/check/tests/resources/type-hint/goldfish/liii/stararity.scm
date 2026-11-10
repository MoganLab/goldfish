(define-library (liii stararity)
  (import (scheme base))
  (export g1 g2 g3)
  (begin

    ;; define*：1 个必选参数 + 2 个可选参数，可接受 1~3 个参数
    (define* (g1 a (b 2) (c 3)) #t)

    ;; define*：1 个必选参数 + 1 个可选参数，可接受 1~2 个参数
    (define* (g2 a (b 2)) #t)

    ;; define* 带 :rest：参数个数无上界，数量校验跳过
    (define* (g3 a :rest rest) #t)
  ) ;begin
) ;define-library
