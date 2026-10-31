(import (liii check) (liii json schema data))

(check-set-mode! 'report-failed)

;; object-cell
;; 查找 JSON 对象中与给定键匹配的第一个键值对。
;;
;; 语法
;; ----
;; (object-cell object key)
;;
;; 参数
;; ----
;; object : any?
;; JSON 对象，键值对 alist。
;;
;; key : string | symbol
;; 待查找的键。
;;
;; 返回值
;; ----
;; pair | boolean
;; 找到时返回键值对，未找到时返回 #f。
;;
;; 注意
;; ----
;; 字符串键与同名符号键等价；返回的键值对保留对象中存储的原始键。
;;
;; 示例
;; ----
;; (object-cell '(("name" . "Liii")) 'name) => ("name" . "Liii")
;;
;; 错误处理
;; ----
;; type-error 当键既不是字符串也不是符号时。

(define object '(("name" . "Liii") (age . 18) ("enabled" . #f)))

(check (object-cell object "name") => '("name" . "Liii"))
(check (object-cell object 'name) => '("name" . "Liii"))
(check (object-cell object 'age) => '(age . 18))
(check (object-cell object "enabled") => '("enabled" . #f))
(check-false (object-cell object "missing"))
(check-false (object-cell object 'missing))
(check-false (object-cell '() "missing"))

(check-catch 'type-error (object-cell object 1))

(check-report)
