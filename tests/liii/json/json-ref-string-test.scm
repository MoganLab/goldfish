;; json-ref-string
;; 安全读取 JSON 中的字符串值。
;;
;; 语法
;; ----
;; (json-ref-string json key default)
;;
;; 参数
;; ----
;; json : any?
;; 目标 JSON 对象。
;;
;; key : symbol? | string? | integer? | boolean?
;; 要读取的键名。
;;
;; default : string?
;; 当键不存在或值不是字符串时返回的默认值。
;;
;; 返回值
;; -----
;; string
;; 若读取到字符串则返回该值，否则返回默认值。
;;
;; 注意
;; ----
;; 这是类型安全获取器，不会抛类型错误。

(import (liii check) (liii json))

(let* ((j0 '((name . "Alice") (age . 25) (active . #t))))
  (check (json-ref-string j0 'name "unknown") => "Alice")
  (check (json-ref-string j0 'age "not-a-number") => "not-a-number")
  (check (json-ref-string j0 'active "not-a-boolean") => "not-a-boolean")
  (check (json-ref-string j0 'nonexistent "fallback") => "fallback")
) ;let*

(check-report)
