(import (liii check) (liii json schema data))

(check-set-mode! 'report-failed)

;; json-false?
;; 判断值是否为 JSON 布尔假值。
;;
;; 语法
;; ----
;; (json-false? value)
;;
;; 参数
;; ----
;; value : any?
;; 待判断的值。
;;
;; 返回值
;; ----
;; boolean
;; 是 JSON 布尔假值返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 空列表与 null 都不是 JSON 布尔假值。
;;
;; 示例
;; ----
;; (json-false? #f) => #t
;; (json-false? 'false) => #t
;; (json-false? #t) => #f
;;
;; 错误处理
;; ----
;; 无。

(check-true (json-false? #f))
(check-true (json-false? 'false))
(check-false (json-false? #t))
(check-false (json-false? 'true))
(check-false (json-false? 0))
(check-false (json-false? 'null))
(check-false (json-false? '()))

(check-report)
