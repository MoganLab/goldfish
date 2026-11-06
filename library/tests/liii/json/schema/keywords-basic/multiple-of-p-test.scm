;; multiple-of?
;; 判断实例是否为目标除数的整数倍。
;;
;; 语法
;; ----
;; (multiple-of? instance divisor)
;;
;; 参数
;; ----
;; instance : number
;; 待检查的数值实例。
;;
;; divisor : number
;; 除数，必须是非零数值。
;;
;; 返回值
;; ----
;; boolean
;; instance 是 divisor 的整数倍返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 比较在精确数域内进行，1 与 1.0 这类混用不会产生浮点误差。
;;
;; 错误处理
;; ----
;; divisor 不是数值或为 0 时抛出 json-schema-schema-error。

(import (liii check) (liii json schema keywords-basic))

(check-set-mode! 'report-failed)

;; 整数倍判定。

(check-true (multiple-of? 4 2))
(check-false (multiple-of? 3 2))
(check-true (multiple-of? 0 2))
(check-true (multiple-of? -4 2))
(check-false (multiple-of? -3 2))

;; 精确数与浮点混用时仍按精确值比较。

(check-true (multiple-of? 1.0 1))
(check-true (multiple-of? 9 0.3))
(check-false (multiple-of? 1 0.3))
(check-true (multiple-of? 0.0075 0.0001))
(check-false (multiple-of? 0.00751 0.0001))
(check-true (multiple-of? 12345678900 1234567890))

;; 除数非法时报错。

(check-catch 'json-schema-schema-error (multiple-of? 1 0))
(check-catch 'json-schema-schema-error (multiple-of? 1 "2"))

(check-report)
