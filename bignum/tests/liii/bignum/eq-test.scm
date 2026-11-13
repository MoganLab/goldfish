;; =
;; 判断数值是否相等（支持大整数与混合数值）。
;;
;; 语法
;; ----
;; (= z1 z2 ...)
;;
;; 参数
;; ----
;; z1, z2 : number? 数值。
;;
;; 返回值
;; ----
;; boolean? 全部相等返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (= (+ 9223372036854775807 1) (+ 9223372036854775806 2)) => #t)
(check (= (expt 10 30) 10) => #f)
(check (= (+ (expt 10 20) 1) 1e20) => #f)
(check (= (expt 10 20) 1e20) => #t)
(check (= (expt 10 400) +inf.0) => #f)
(check (= (- (expt 10 400)) -inf.0) => #f)
(check (= (expt 10 20) +nan.0) => #f)
(check (= (expt 10 20)) => #t)
(check (= 100000000000000000000 100000000000000000000+0i) => #t)

(check-report)
