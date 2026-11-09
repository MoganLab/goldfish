(define-library (hint liii stararity)
  (import (scheme base))
  (export hint-liii/stararity)
  (begin

    ;; g1 签名 1~3 参，与 define* 的 1 必选 + 2 可选一致
    (define hint-g1
      '((any? -> any?)
        (any? any? -> any?)
        (any? any? any? -> any?))
    ) ;define

    ;; g2 签名只登记 1 参，实际可接受 1~2 参 -> 违规
    (define hint-g2 '((any? -> any?)))

    ;; g3 带 :rest，数量校验跳过
    (define hint-g3 '((any? -> any?)))

    (define (hint-liii/stararity name)
      (case name
       ((g1) hint-g1)
       ((g2) hint-g2)
       ((g3) hint-g3)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
