;; scheme-extensions
;; scheme-extensions 对应语言的后缀名列表常量。
;;
;; 语法
;; ----
;; scheme-extensions
;;
;; 参数
;; ----
;; 无（常量）。
;;
;; 返回值
;; ------
;; list of string，后缀名列表。

(import (scheme base) (liii check) (liii goldfmt scheme))

(check-set-mode! 'report-failed)

;; 常量内容断言
(check-true (not (not (member ".scm" scheme-extensions))))
(check-true (list? scheme-extensions))

(check-report)
