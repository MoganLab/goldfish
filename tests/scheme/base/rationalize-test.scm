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
;; 容差范围，表示结果与原始值之间的最大允许误差，输入0或仅有一个参数时within默认为0.000000000001。
;;
;; 返回值
;; ------
;; real?
;; 返回满足|(result - x)| <= within且分母最小的有理数
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是实数时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为2个时抛出错误。
(check (rationalize 0.5 0.1) => 1/2)
(check (rationalize 0.33 0.01) => 1/3)
(check (rationalize 0.333 0.02) => 1/3)
(check (rationalize 3.14159265359 0.01) => 22/7)
(check (rationalize 3.14159265359 0.001) => 201/64)
(check (rationalize 0.0 0.1) => 0)
(check (rationalize 1.0 0.0) => 1)
(check (rationalize 0.999 0.001) => 1)
(check (rationalize -0.5 0.1) => -1/2)
(check (rationalize 2.71828 0.0001) => 193/71)
(check (rationalize 1.4142 0.001) => 41/29)
(check (rationalize 2/3 0.05) => 2/3)
(check-catch 'wrong-type-arg (rationalize "hello" 0.1))
(check-catch 'wrong-number-of-args
  (rationalize 3.14 0.01 0.02)
) ;check-catch
(check-report)