(import (liii check) (liii json schema data))

(check-set-mode! 'report-failed)

;; object-ref
;; 读取 JSON 对象中与给定键匹配的值。
;;
;; 语法
;; ----
;; (object-ref object key default)
;;
;; 参数
;; ----
;; object : any?
;; JSON 对象，键值对 alist。
;;
;; key : string | symbol
;; 待查找的键。
;;
;; default : any?
;; 键不存在时返回的默认值。
;;
;; 返回值
;; ----
;; any?
;; 键存在时返回对应的值，否则返回 default。
;;
;; 注意
;; ----
;; 字符串键与同名符号键等价；键存在但值为 #f 时返回 #f 而不是 default。
;;
;; 示例
;; ----
;; (object-ref '(("age" . 18)) "age" #f) => 18
;; (object-ref '(() ) "age" 'missing) => 'missing
;;
;; 错误处理
;; ----
;; type-error 当键既不是字符串也不是符号时。

(define object '(("name" . "Liii") (age . 18) ("enabled" . #f)))

(check (object-ref object "name" #f) => "Liii")
(check (object-ref object 'name #f) => "Liii")
(check (object-ref object "age" #f) => 18)
(check (object-ref object "enabled" #t) => #f)
(check (object-ref object "missing" 'fallback) => 'fallback)
(check (object-ref '() "missing" 'fallback) => 'fallback)

(check-catch 'type-error (object-ref object 1 #f))
(check-catch 'type-error (object-ref '() 1 #f))

(check-report)
