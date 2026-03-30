(import (liii check)
        (liii json)
        (liii base)
        (liii error)
) ;import

(check-set-mode! 'report-failed)

;; json-integer?
;; 判断值是否为 JSON 整数。
;;
;; 语法
;; ----
;; (json-integer? x)
;;
;; 参数
;; ----
;; x : any?
;; 要检查的值。
;;
;; 返回值
;; ----
;; boolean
;; 当 x 为整数时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; `1.0` 不会被视为整数。
;;
;; 示例
;; ----
;; (json-integer? 100) => #t
;; (json-integer? 3.14) => #f
;;
;; 错误处理
;; ----
;; 无。

(check-true (json-integer? 100))
(check-true (json-integer? 0))
(check-true (json-integer? -5))
(check-false (json-integer? 3.14))
(check-false (json-integer? 1.0))

(check-report)

