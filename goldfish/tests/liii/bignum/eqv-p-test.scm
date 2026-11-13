;; eqv?
;; 判断两个对象是否等价（支持大整数值等价）。
;;
;; 语法
;; ----
;; (eqv? a b)
;;
;; 参数
;; ----
;; a : any? 对象。
;; b
;; :
;; any?
;; 对象。
;;
;; 返回值
;; ----
;; boolean? 等价返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (eqv? (+ 9223372036854775807 1) (+ 9223372036854775806 2)) => #t)
(check (eqv? (expt 10 30) 42) => #f)

(check-report)
