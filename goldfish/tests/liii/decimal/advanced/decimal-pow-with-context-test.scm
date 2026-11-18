;; decimal-pow-with-context
;; 计算 decimal 的幂，并按 math-context 约束结果精度。
;;
;; 语法
;; ----
;; (decimal-pow-with-context n exponent mc)
;;
;; 参数
;; ----
;; n : decimal?
;;   底数。
;; exponent : integer?
;;   指数。
;; mc : math-context?
;;   精度与舍入模式约束。
;;
;; 返回值
;; ----
;; decimal?
;;   按约束舍入后的幂。

(import (liii check)
        (liii decimal)
        (liii decimal math-context)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(let ((mc (make-math-context 5 'half-up)))
  (check (decimal->string (decimal-pow-with-context (d "1.05") 10 mc))
         => "1.6289")
) ;let

(check-report)
