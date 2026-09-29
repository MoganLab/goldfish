(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; expt
;; 计算幂运算。
;;
;; 语法
;; ----
;; (expt z1 z2)
;;
;; 参数
;; ----
;; z1 : number?
;; 底数。
;; z2 : number?
;; 指数。
;;
;; 返回值
;; ------
;; number?
;; z1 的 z2 次幂。
;;
;; 说明
;; ----
;; 1. 支持整数和浮点数
;; 2. 0 的负数次幂可能引发错误
(check (expt 2 3) => 8)
(check (expt 2 0) => 1)
(check (expt 2 -1) => 1/2)
(check (expt 4 1/2) => 2)
(check (exact? (expt 4 1/2)) => #t)
(check (expt 8 2/3) => 4)
(check (expt 81/16 3/4) => 27/8)
(check (expt -8 1/3) => -2)
(check (expt 10000000000000000000000000000000000000000 1/4)
       => 10000000000)
(check (exact? (expt 2 1/2)) => #f)
(check (expt 0 0) => 1)
(check (expt 2 10) => 1024)
(check (expt 2 64) => 18446744073709551616)
(check-catch 'division-by-zero (expt 0 -1))
(define huge-even-exponent (expt 2 64))
(check (exact? huge-even-exponent) => #t)
(check (expt 1 huge-even-exponent) => 1)
(check (expt -1 huge-even-exponent) => 1)
(check (expt -1 (+ huge-even-exponent 1)) => -1)
(check (expt 0 huge-even-exponent) => 0)
(check-catch 'division-by-zero
  (expt 0 (- 0 huge-even-exponent)))

(check-report)
