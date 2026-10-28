;; cond 值聚合测试的 hint 库。
;; 签名刻意用粗的 (any? -> any?)：hint 在这里只起"引导检查"作用，
;; 检查依据来自 (liii reexport-dep) 穿透到的依赖签名。

(define-library (hint liii aggcheck)
  (import (scheme base))
  (export hint-liii/aggcheck)
  (begin

    ;; cond-ok : (any? -> any?)
    (define hint-cond-ok '((any? -> any?)))
    ;; cond-bad : (any? -> any?)
    (define hint-cond-bad '((any? -> any?)))
    ;; cond-no-else : (any? -> any?)
    (define hint-cond-no-else '((any? -> any?)))
    ;; cond-arrow : (any? -> any?)
    (define hint-cond-arrow '((any? -> any?)))
    ;; nested-ok : (any? -> any?)
    (define hint-nested-ok '((any? -> any?)))
    ;; nested-bad : (any? -> any?)
    (define hint-nested-bad '((any? -> any?)))

    (define (hint-liii/aggcheck name)
      (case name
       ((cond-ok) hint-cond-ok)
       ((cond-bad) hint-cond-bad)
       ((cond-no-else) hint-cond-no-else)
       ((cond-arrow) hint-cond-arrow)
       ((nested-ok) hint-nested-ok)
       ((nested-bad) hint-nested-bad)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
