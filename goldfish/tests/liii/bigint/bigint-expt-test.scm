;; bigint-expt
;; 使用快速幂计算非负整数次幂。
;;
;; 语法
;; ----
;; (bigint-expt base exponent)
;;
;; 参数
;; ----
;; base : bigint?
;;   底数。
;; exponent : integer? or bigint?
;;   非负整数指数。
;;
;; 返回值
;; ----
;; bigint?
;;   base 的 exponent 次幂。
;;
;; 错误处理
;; ----
;; type-error / value-error
;;   当 base 不是 bigint 时抛出 type-error；指数值不合法时抛出 value-error。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(define (b str)
  (string->bigint str)
) ;define

(check (bigint->string (bigint-expt (b "2") 0)) => "1")
(check (bigint->string (bigint-expt (b "2") 10)) => "1024")
(check (bigint->string (bigint-expt (b "-2") 3)) => "-8")
(check (bigint->string (bigint-expt (b "2") (b "16"))) => "65536")

(check-catch 'value-error (bigint-expt (b "2") -1))
(check-catch 'type-error (bigint-expt 2 10))

(check-report)
