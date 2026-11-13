;; decimal-negate-with-context
;; 计算 decimal 的相反数，并按 math-context 约束结果精度。
;;
;; 语法
;; ----
;; (decimal-negate-with-context n mc)
;;
;; 参数
;; ----
;; n : decimal?
;;   输入 decimal。
;; mc : math-context?
;;   精度与舍入模式约束。
;;
;; 返回值
;; ----
;; decimal?
;;   按约束舍入后的相反数。

(import (liii check)
        (liii decimal)
        (liii decimal math-context)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(let ((mc (make-math-context 4 'half-up)))
  (check (decimal->string (decimal-negate-with-context (d "1.23456") mc))
         => "-1.235")
) ;let

(check-report)
