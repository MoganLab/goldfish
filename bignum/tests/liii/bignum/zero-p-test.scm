;; zero?
;; 判断数值是否为 0。
;;
;; 语法
;; ----
;; (zero? z)
;;
;; 参数
;; ----
;; z : number? 数值。
;;
;; 返回值
;; ----
;; boolean? 若等于 0 返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (zero? 0) => #t)
(check (zero? (expt 10 30)) => #f)

(check-report)
