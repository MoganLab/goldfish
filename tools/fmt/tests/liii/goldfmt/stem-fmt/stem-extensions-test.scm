;; stem-extensions
;; stem-extensions 对应语言的后缀名列表常量。
;;
;; 语法
;; ----
;; stem-extensions
;;
;; 参数
;; ----
;; 无（常量）。
;;
;; 返回值
;; ------
;; list of string，后缀名列表。

(import (scheme base) (liii check) (liii goldfmt stem-fmt))

(check-set-mode! 'report-failed)

;; 常量内容断言
(check-true (not (not (member ".stem" stem-extensions))))
(check-true (list? stem-extensions))

(check-report)
