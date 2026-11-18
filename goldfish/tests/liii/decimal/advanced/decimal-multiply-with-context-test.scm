;; decimal-multiply-with-context
;; 计算两个 decimal 的积，并按 math-context 约束结果精度。
;;
;; 语法
;; ----
;; (decimal-multiply-with-context a b mc)
;;
;; 参数
;; ----
;; a : decimal?
;;   第一个乘数。
;; b : decimal?
;;   第二个乘数。
;; mc : math-context?
;;   精度与舍入模式约束。
;;
;; 返回值
;; ----
;; decimal?
;;   按约束舍入后的积。

(import (liii check)
        (liii decimal)
        (liii decimal math-context)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(let ((mc (make-math-context 5 'half-up)))
  (check (decimal->string (decimal-multiply-with-context
                            (d "1.23456")
                            (d "2.0")
                            mc))
         => "2.4691")
) ;let

(check-report)
