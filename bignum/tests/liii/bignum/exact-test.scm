;; exact
;; 将数值转换为精确表示。
;;
;; 语法
;; ----
;; (exact z)
;;
;; 参数
;; ----
;; z : number? 数值。
;;
;; 返回值
;; ----
;; number? 对应的精确数值。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (exact (expt 10 30)) => (expt 10 30))
(check (exact 2.5) => 5/2)
(check (exact 1e+20) => 100000000000000000000)
(check (exact 1e+30) => 1000000000000000019884624838656)
(check (exact -1e+20) => -100000000000000000000)
(check #e1e20 => 100000000000000000000)

(check-report)
