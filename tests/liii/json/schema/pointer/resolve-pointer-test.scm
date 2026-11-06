;; resolve-pointer
;; 按 JSON Pointer 解析出根资源中的值。
;;
;; 语法
;; ----
;; (resolve-pointer root pointer)
;;
;; 参数
;; ----
;; root : any?
;; 根资源，可以是 JSON 对象、数组或标量。
;;
;; pointer : string
;; JSON Pointer 字符串。
;;
;; 返回值
;; ----
;; any?
;; 指针指向的值。
;;
;; 注意
;; ----
;; 空指针指向根资源本身；
;; 引用记号中的 ~0 表示 ~，~1 表示 /。
;;
;; 错误处理
;; ----
;; 指针格式非法、目标不存在或数组下标越界时抛出 json-schema-unresolvable。

(import (liii check) (liii json schema pointer))

(check-set-mode! 'report-failed)

(define root
  '(("" . "empty")
    ("a/b" . "slash")
    ("m~n" . "tilde")
    ("items" . #(10 20 30))
    ("nested" ("x" . 1)))
) ;define

;; 空指针指向根资源。

(check (resolve-pointer root "") => root)

;; 对象成员。

(check (resolve-pointer root "/") => "empty")
(check (resolve-pointer root "/nested/x") => 1)
(check (resolve-pointer root "/m~0n") => "tilde")

;; 转义记号。

(check (resolve-pointer root "/a~1b") => "slash")

;; 数组元素。

(check (resolve-pointer root "/items/0") => 10)
(check (resolve-pointer root "/items/2") => 30)

;; 目标不存在时报错。

(check-catch 'json-schema-unresolvable (resolve-pointer root "/missing"))
(check-catch 'json-schema-unresolvable (resolve-pointer root "/items/3"))

;; 指针格式非法时报错。

(check-catch 'json-schema-unresolvable (resolve-pointer root "items"))
(check-catch 'json-schema-unresolvable (resolve-pointer root "/items/-"))
(check-catch 'json-schema-unresolvable (resolve-pointer root "/items/01"))

(check-report)
