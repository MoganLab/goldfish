(import (liii check) (scheme inexact))
(check-set-mode! 'report-failed)
;; nan?
;; 判断一个数值是否为 NaN（Not a Number）。
;;
;; 语法
;; ----
;; (nan? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意类型的对象。
;;
;; 返回值
;; ------
;; boolean?
;; - 若 obj 是数值，且其实部或虚部中存在 NaN，返回 #t。
;; - 否则返回 #f。
;;
;; 注意
;; ----
;; 1. NaN 表示"非数字"值，通常由无效的数学运算产生
;; 2. 支持检测各种数值类型中的 NaN
;; 3. 非数值类型将返回 #f
;; 4. 复数中只要实部或虚部任一为 NaN，就返回 #t
;;
;; 错误处理
;; ------
;; 无错误情况，非数值将返回 #f。
;; nan? 基本测试
(check (nan? +nan.0) => #t)
(check (nan? +nan.0) => #t)
(check (nan? +nan.0+5.0i) => #t)
(check (nan? 5.0+nan.0i) => #t)
(check (nan? +nan.0+5.0i) => #t)
(check (nan? 5.0+nan.0i) => #t)
(check (nan? +nan.0+0.4i) => #t)
(check (nan? 0.4+nan.0i) => #t)
;; nan? 非 NaN 数值测试
(check (nan? 32) => #f)
(check (nan? 3.14) => #f)
(check (nan? 1.0+2.0i) => #f)
(check (nan? +inf.0) => #f)
(check (nan? -inf.0) => #f)
(check (nan? 0) => #f)
(check (nan? 0.0) => #f)
(check (nan? 1/2) => #f)
(check (nan? 0.5+1.0i) => #f)
(check (nan? 1.0+0.5i) => #f)
(check (nan? 1.0+2.0i) => #f)
;; nan? 运算产生的 NaN 测试
(check (nan? (* +nan.0 2.0)) => #t)
(check (nan? (* 0.0 +nan.0)) => #t)
(check (nan? (+ +nan.0 1)) => #t)
(check (nan? (- +nan.0 0.5)) => #t)
(check (nan? (sqrt -1.0)) => #f)
;; nan? 非数值类型测试
(check (nan? #t) => #f)
(check (nan? #f) => #f)
(check (nan? "hello") => #f)
(check (nan? 'symbol) => #f)
(check (nan? '(+nan.0)) => #f)
(check (nan? '#(+nan.0)) => #f)
(check (nan? '()) => #f)
(check (nan? '(1 2 3)) => #f)
(check (nan? #\a) => #f)
(check-report)
