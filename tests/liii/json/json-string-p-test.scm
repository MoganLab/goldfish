(import (liii check)
  (liii json)
  (liii base)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

;; json-string?
;; 判断值是否为 JSON 字符串。
;;
;; 语法
;; ----
;; (json-string? x)
;;
;; 参数
;; ----
;; x : any?
;; 要检查的值。
;;
;; 返回值
;; ----
;; boolean
;; 当 x 为字符串时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 符号不会被视为 JSON 字符串。
;;
;; 示例
;; ----
;; (json-string? "hello") => #t
;; (json-string? 'hello) => #f
;;
;; 错误处理
;; ----
;; 无。

(check-true (json-string? "hello"))
(check-true (json-string? ""))
(check-false (json-string? 'hello))
(check-false (json-string? 123))

(check-report)
