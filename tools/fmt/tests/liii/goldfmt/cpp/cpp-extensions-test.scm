;; cpp-extensions
;; cpp-extensions 对应语言的后缀名列表常量。
;;
;; 语法
;; ----
;; cpp-extensions
;;
;; 参数
;; ----
;; 无（常量）。
;;
;; 返回值
;; ------
;; list of string，后缀名列表。

(import (scheme base) (liii check) (liii goldfmt cpp))

(check-set-mode! 'report-failed)

;; 常量内容断言
(check-true (not (not (member ".hpp" cpp-extensions))))
(check-true (list? cpp-extensions))

(check-report)
