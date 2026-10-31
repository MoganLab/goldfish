(import (liii check) (liii json schema data))

(check-set-mode! 'report-failed)

;; key->string
;; 把 JSON 对象的键转换为字符串。
;;
;; 语法
;; ----
;; (key->string key)
;;
;; 参数
;; ----
;; key : string | symbol
;; JSON 对象的键。
;;
;; 返回值
;; ----
;; string
;; 键对应的字符串。
;;
;; 注意
;; ----
;; 字符串键原样返回，符号键取符号名；
;; 字符串键与同名符号键在查找时等价。
;;
;; 示例
;; ----
;; (key->string "name") => "name"
;; (key->string 'name) => "name"
;;
;; 错误处理
;; ----
;; type-error 当键既不是字符串也不是符号时。

(check (key->string "name") => "name")
(check (key->string 'name) => "name")
(check (key->string "$schema") => "$schema")
(check-catch 'type-error (key->string 1))
(check-catch 'type-error (key->string #f))

(check-report)
