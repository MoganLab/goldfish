(import (liii check)
        (liii json)
        (liii base)
        (liii error)
) ;import

(check-set-mode! 'report-failed)

;; json-boolean?
;; 判断值是否为 JSON 布尔值。
;;
;; 语法
;; ----
;; (json-boolean? x)
;;
;; 参数
;; ----
;; x : any?
;; 要检查的值。
;;
;; 返回值
;; ----
;; boolean
;; 当 x 为布尔值时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 本库中的 JSON 布尔值使用 Scheme 的 `#t` 和 `#f` 表示。
;;
;; 示例
;; ----
;; (json-boolean? #t) => #t
;; (json-boolean? 0) => #f
;;
;; 错误处理
;; ----
;; 无。

(check-true (json-boolean? #t))
(check-true (json-boolean? #f))
(check-false (json-boolean? 0))
(check-false (json-boolean? "true"))

(check-report)

