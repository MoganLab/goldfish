;; decimal-sqrt-with-context
;; 计算 decimal 的平方根，按 math-context 的精度作为 scale。
;;
;; 语法
;; ----
;; (decimal-sqrt-with-context n mc)
;;
;; 参数
;; ----
;; n : decimal?
;;   被开方数。
;; mc : math-context?
;;   精度与舍入模式约束；precision=0 表示 UNLIMITED（scale 20）。
;;
;; 返回值
;; ----
;; decimal?
;;   按约束舍入后的平方根。

(import (liii check)
        (liii decimal)
        (liii decimal math-context)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(let ((mc (make-math-context 5 'half-up)))
  (check (decimal->string (decimal-sqrt-with-context (d "2") mc))
         => "1.41421")
) ;let

(check-report)
