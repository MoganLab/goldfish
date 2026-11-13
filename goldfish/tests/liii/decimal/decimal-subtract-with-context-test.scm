;; decimal-subtract-with-context
;; 计算两个 decimal 的差，并按 math-context 约束结果精度。
;;
;; 语法
;; ----
;; (decimal-subtract-with-context a b mc)
;;
;; 参数
;; ----
;; a : decimal?
;;   被减数。
;; b : decimal?
;;   减数。
;; mc : math-context?
;;   精度与舍入模式约束。
;;
;; 返回值
;; ----
;; decimal?
;;   按约束舍入后的差。

(import (liii check)
        (liii decimal)
        (liii decimal math-context)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(let ((mc (make-math-context 4 'half-up)))
  (check (decimal->string (decimal-subtract-with-context
                            (d "10.2345")
                            (d "1.11111")
                            mc))
         => "9.123")
) ;let

(check-report)
