;; 未校验可能为 #f 的数值结果：
;; my-number 返回 (or #f number?)，my-mul 需要 number? -> 确定冲突

(define-library (liii numcheck)
  (import (liii reexport-dep))
  (export bad-number)
  (begin

    ;; 数字解析结果可能为 #f，直接参与乘法 -> 确定的类型冲突
    (define (bad-number s)
      (my-mul 2 (my-number s))
    ) ;define
  ) ;begin
) ;define-library
