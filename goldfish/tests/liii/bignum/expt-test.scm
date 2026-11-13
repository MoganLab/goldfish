;; expt
;; 计算幂运算（精确整数大幂自动返回大整数）。
;;
;; 语法
;; ----
;; (expt z1 z2)
;;
;; 参数
;; ----
;; z1, z2 : number? 底数与指数。
;;
;; 返回值
;; ----
;; number? 幂结果。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (number->string (expt 2 100)) => "1267650600228229401496703205376")

(check-report)
