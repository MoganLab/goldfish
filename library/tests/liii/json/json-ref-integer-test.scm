;; json-ref-integer
;; 安全读取 JSON 中的整数值。
;;
;; 语法
;; ----
;; (json-ref-integer json key default)
;;
;; 参数
;; ----
;; json : any?
;; 目标 JSON 对象。
;;
;; key : symbol? | string? | integer? | boolean?
;; 要读取的键名。
;;
;; default : integer?
;; 当键不存在或值不是整数时返回的默认值。
;;
;; 返回值
;; -----
;; integer
;; 若读取到整数则返回该值，否则返回默认值。
;;
;; 注意
;; ----
;; 这是类型安全获取器，不会抛类型错误。浮点数不会被视为整数。

(import (liii check) (liii json))

(let* ((j0 '((name . "Alice") (age . 25) (score . 99.5) (active . #t))))
  (check (json-ref-integer j0 'age 0) => 25)
  (check (json-ref-integer j0 'score -1) => -1)
  (check (json-ref-integer j0 'name -1) => -1)
  (check (json-ref-integer j0 'active -1) => -1)
  (check (json-ref-integer j0 'nonexistent 7) => 7)
) ;let*

(check-report)
