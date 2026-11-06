;; json-ref-number
;; 安全读取 JSON 中的数值（整数或浮点数）。
;;
;; 语法
;; ----
;; (json-ref-number json key default)
;;
;; 参数
;; ----
;; json : any?
;; 目标 JSON 对象。
;;
;; key : symbol? | string? | integer? | boolean?
;; 要读取的键名。
;;
;; default : number?
;; 当键不存在或值不是数值时返回的默认值。
;;
;; 返回值
;; -----
;; number
;; 若读取到数值则返回该值，否则返回默认值。
;;
;; 注意
;; ----
;; 这是类型安全获取器，不会抛类型错误。

(import (liii check) (liii json))

(let* ((j0 '((name . "Alice") (age . 25) (score . 99.5) (active . #t))))
  (check (json-ref-number j0 'age 0) => 25)
  (check (json-ref-number j0 'score 0.0) => 99.5)
  (check (json-ref-number j0 'name -1) => -1)
  (check (json-ref-number j0 'active -1) => -1)
  (check (json-ref-number j0 'nonexistent 3.14) => 3.14)
) ;let*

(check-report)
