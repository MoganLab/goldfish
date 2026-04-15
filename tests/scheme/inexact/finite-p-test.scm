(import (liii check) (scheme inexact))
(check-set-mode! 'report-failed)
;; finite?
;; 判断一个数值是否有限。
;;
;; 语法
;; ----
;; (finite? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意类型的对象。
;;
;; 返回值
;; ------
;; boolean?
;; - 若 obj 是数值，且实部与虚部都为有限数，返回 #t。
;; - 若是非数值、包含 inf.0、nan.0 的实部或虚部，返回 #f。
;;
;; 错误处理
;; ------
;; 无错误情况，非数值将返回 #f。
(check (finite? 0) => #t)
(check (finite? 0.0) => #t)
(check (finite? 1/2) => #t)
(check (finite? 0.5+1.0i) => #t)
(check (finite? 1.0+0.5i) => #t)
(check (finite? 1.0+2.0i) => #t)
(check (finite? 1.0+2.0i) => #t)
(check (finite? +inf.0) => #f)
(check (finite? -inf.0) => #f)
(check (finite? +inf.0+2.0i) => #f)
(check (finite? +inf.0+2.0i) => #f)
(check (finite? +inf.0+0.5i) => #f)
(check (finite? 2.0-inf.0i) => #f)
(check (finite? 2.0-inf.0i) => #f)
(check (finite? 0.5-inf.0i) => #f)
(check (finite? +inf.0-inf.0i) => #f)
(check (finite? -inf.0+inf.0i) => #f)
(check (finite? +nan.0) => #f)
(check (finite? +nan.0) => #f)
(check (finite? (* +nan.0 2.0)) => #f)
(check (finite? (* 0.0 +nan.0)) => #f)
(check (finite? +nan.0+5.0i) => #f)
(check (finite? 5.0+nan.0i) => #f)
(check (finite? +nan.0+5.0i) => #f)
(check (finite? 5.0+nan.0i) => #f)
(check (finite? +nan.0+0.4i) => #f)
(check (finite? 0.4+nan.0i) => #f)
(check (finite? #t) => #f)
(check (finite? "hello") => #f)
(check (finite? 'symbol) => #f)
(check (finite? '(+inf.0)) => #f)
(check (finite? '#(+inf.0)) => #f)
(check-report)