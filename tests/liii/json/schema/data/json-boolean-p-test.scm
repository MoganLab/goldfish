(import (liii check) (liii json schema data))

(check-set-mode! 'report-failed)

;; json-boolean?
;; 判断值是否为 JSON 布尔值。
;;
;; 语法
;; ----
;; (json-boolean? value)
;;
;; 参数
;; ----
;; value : any?
;; 待判断的值。
;;
;; 返回值
;; ----
;; boolean
;; 是 JSON 布尔值返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 除 Scheme 布尔值 #t/#f 外，符号 true 与 false 也被视为 JSON 布尔值。
;;
;; 示例
;; ----
;; (json-boolean? #t) => #t
;; (json-boolean? 'true) => #t
;; (json-boolean? 1) => #f
;;
;; 错误处理
;; ----
;; 无。

(check-true (json-boolean? #t))
(check-true (json-boolean? #f))
(check-true (json-boolean? 'true))
(check-true (json-boolean? 'false))
(check-false (json-boolean? 'null))
(check-false (json-boolean? 1))
(check-false (json-boolean? 0))
(check-false (json-boolean? "true"))
(check-false (json-boolean? '()))

(check-report)
