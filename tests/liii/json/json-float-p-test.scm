(import (liii check)
        (liii json)
        (liii base)
        (liii error)
) ;import

(check-set-mode! 'report-failed)

;; json-float?
;; 判断值是否为 JSON 浮点数。
;;
;; 语法
;; ----
;; (json-float? x)
;;
;; 参数
;; ----
;; x : any?
;; 要检查的值。
;;
;; 返回值
;; ----
;; boolean
;; 当 x 为浮点数时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 整数不会被视为浮点数。
;;
;; 示例
;; ----
;; (json-float? 3.14) => #t
;; (json-float? 100) => #f
;;
;; 错误处理
;; ----
;; 无。

(check-true (json-float? 3.14))
(check-true (json-float? -0.01))
(check-false (json-float? 100))

(check-report)

