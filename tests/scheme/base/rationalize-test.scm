(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; rationalize
;; 将给定的实数简化为一个具有较小分母的近似有理数。
;;
;; 语法
;; ----
;; (rationalize x [within])
;;
;; 参数
;; ----
;; x : real?
;; 要简化的实数
;;
;; within : real?(可选)
;; 容差范围；省略时使用扩展默认值 1/1000000000000，零表示不允许误差。
;;
;; 返回值
;; ------
;; real?
;; 返回区间内最简单的有理数；任一参数非精确时，结果也非精确。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是实数时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为1或2个时抛出错误。
(check (rationalize 1/2 1/10) => 1/2)
(check (rationalize 33/100 1/100) => 1/3)
(check (rationalize 333/1000 1/50) => 1/3)
(check (rationalize 314159265359/100000000000 1/100) => 22/7)
(check (rationalize 314159265359/100000000000 1/1000) => 201/64)
(check (rationalize 0 1/10) => 0)
(check (rationalize 1 0) => 1)
(check (rationalize 999/1000 1/1000) => 1)
(check (rationalize -1/2 1/10) => -1/2)
(check (rationalize 67957/25000 1/10000) => 193/71)
(check (rationalize 7071/5000 1/1000) => 41/29)
(check (rationalize 2/3 1/20) => 2/3)

;; Exactness follows both arguments, including the optional default.
(check (rationalize (exact 0.3) 1/10) => 1/3)
(check (rationalize 0.3 1/10) => (inexact 1/3))
(check (rationalize 3/10 0.1) => (inexact 1/3))
(check (rationalize 0.3 0.1) => (inexact 1/3))
(check (rationalize -0.3 0.1) => (inexact -1/3))
(check (rationalize 0.0 0.1) => 0.0)
(check (rationalize 1.0 0.0) => 1.0)
(check (rationalize 1/3 0.0) => (inexact 1/3))
(check (rationalize 0.3 0) => 0.3)
(check (rationalize 1/2) => 1/2)
(check (rationalize 0.5) => 0.5)

;; Mixed intervals must not overflow or lose their exact endpoints.
(let ((huge (expt 10 400)))
  (check (rationalize huge 0) => huge)
  (check (rationalize huge 0.0) => +inf.0)
  (check (rationalize (- huge) 0.0) => -inf.0)
  (check (rationalize 0.5 huge) => 0.0)
  (check (rationalize 0.5 (- 1/2 (/ 1 huge))) => 0.5))
(check (rationalize 1/3 1/3) => 0)
(check (rationalize 1/3 -1/3) => 0)
(check-catch 'wrong-type-arg (rationalize "hello" 0.1))
(check-catch 'wrong-number-of-args (rationalize 3.14 0.01 0.02))
(check-report)
