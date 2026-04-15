(import (liii check) (scheme inexact))
(check-set-mode! 'report-failed)
;; infinite?
;; 判断一个数值是否无限。
;;
;; 语法
;; ----
;; (infinite? obj)
;;
;; 参数
;; ----
;; obj : number?
;; 要判断的数值。支持整数、浮点数、有理数、复数。
;;
;; 返回值
;; ------
;; boolean?
;; - 若 obj 是数值，且其实部或虚部中存在 +inf.0 或 -inf.0，返回 #t。
;; - 否则返回 #f。
;;
;; 错误处理
;; ------
;; 无错误情况，非数值将返回 #f。
(check (infinite? 0) => #f)
(check (infinite? 0.0) => #f)
(check (infinite? 1/2) => #f)
(check (infinite? 0.5+1.0i) => #f)
(check (infinite? 1.0+0.5i) => #f)
(check (infinite? 1.0+2.0i) => #f)
(check (infinite? 1.0+2.0i) => #f)
(check (infinite? +inf.0) => #t)
(check (infinite? -inf.0) => #t)
(check (infinite? +inf.0+2.0i) => #t)
(check (infinite? +inf.0+2.0i) => #t)
(check (infinite? +inf.0+0.5i) => #t)
(check (infinite? 2.0-inf.0i) => #t)
(check (infinite? 2.0-inf.0i) => #t)
(check (infinite? 0.5-inf.0i) => #t)
(check (infinite? +inf.0-inf.0i) => #t)
(check (infinite? -inf.0+inf.0i) => #t)
(check (infinite? +nan.0) => #f)
(check (infinite? +nan.0) => #f)
(check (infinite? (* +nan.0 2.0)) => #f)
(check (infinite? (* 0.0 +nan.0)) => #f)
(check (infinite? +nan.0+5.0i) => #f)
(check (infinite? 5.0+nan.0i) => #f)
(check (infinite? +nan.0+5.0i) => #f)
(check (infinite? 5.0+nan.0i) => #f)
(check (infinite? +nan.0+0.4i) => #f)
(check (infinite? 0.4+nan.0i) => #f)
(check (infinite? #t) => #f)
(check (infinite? "hello") => #f)
(check (infinite? 'symbol) => #f)
(check (infinite? '(+inf.0)) => #f)
(check (infinite? '#(+inf.0)) => #f)
(check-report)