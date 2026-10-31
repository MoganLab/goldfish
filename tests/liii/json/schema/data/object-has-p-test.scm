(import (liii check) (liii json schema data))

(check-set-mode! 'report-failed)

;; object-has?
;; 判断 JSON 对象中是否存在与给定键匹配的键。
;;
;; 语法
;; ----
;; (object-has? object key)
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
;; boolean
;; 键存在返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 键存在但值为 #f 时同样返回 #t。
;;
;; 示例
;; ----
;; (object-has? '(("enabled" . #f)) "enabled") => #t
;;
;; 错误处理
;; ----
;; type-error 当键既不是字符串也不是符号时。

(define object '(("name" . "Liii") (age . 18) ("enabled" . #f)))

(check-true (object-has? object "name"))
(check-true (object-has? object 'name))
(check-true (object-has? object 'age))
(check-true (object-has? object "enabled"))
(check-false (object-has? object "missing"))
(check-false (object-has? object 'missing))
(check-false (object-has? '() "missing"))

(check-catch 'type-error (object-has? object 1))

(check-report)
