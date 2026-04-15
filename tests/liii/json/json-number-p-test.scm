(import (liii check)
  (liii json)
  (liii base)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

;; json-number?
;; 判断值是否为 JSON 数字。
;;
;; 语法
;; ----
;; (json-number? x)
;;
;; 参数
;; ----
;; x : any?
;; 要检查的值。
;;
;; 返回值
;; ----
;; boolean
;; 当 x 为数值时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 同时覆盖整数和浮点数。
;;
;; 示例
;; ----
;; (json-number? 123) => #t
;; (json-number? "123") => #f
;;
;; 错误处理
;; ----
;; 无。

(check-true (json-number? 123))
(check-true (json-number? 3.14))
(check-true (json-number? -10))
(check-true (json-number? 0))
(check-false (json-number? "123"))

(check-report)
