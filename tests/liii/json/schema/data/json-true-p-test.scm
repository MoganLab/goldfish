(import (liii check) (liii json schema data))

(check-set-mode! 'report-failed)

;; json-true?
;; 判断值是否为 JSON 布尔真值。
;;
;; 语法
;; ----
;; (json-true? value)
;;
;; 参数
;; ----
;; value : any?
;; 待判断的值。
;;
;; 返回值
;; ----
;; boolean
;; 是 JSON 布尔真值返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 数字 0 与空列表都不是 JSON 布尔真值。
;;
;; 示例
;; ----
;; (json-true? #t) => #t
;; (json-true? 'true) => #t
;; (json-true? #f) => #f
;;
;; 错误处理
;; ----
;; 无。

(check-true (json-true? #t))
(check-true (json-true? 'true))
(check-false (json-true? #f))
(check-false (json-true? 'false))
(check-false (json-true? 0))
(check-false (json-true? 'null))
(check-false (json-true? '()))

(check-report)
