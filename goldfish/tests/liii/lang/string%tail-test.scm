;; string%tail
;; 去掉字符串首字符（对标 Scala String.tail）。
;;
;; 语法
;; ----
;; ($ s :tail)
;;
;; 参数
;; ----
;; s : string?
;; 源字符串，不可为空。
;;
;; 返回值
;; ------
;; string?
;; 去掉首字符的新字符串；空串抛 value-error。UTF-8 语义。

(import (liii check) (liii lang) (liii error))

(check-set-mode! 'report-failed)

(check ($ "abc" :tail) => "bc")
(check ($ "测试法" :tail) => "试法")
(check ($ "a" :tail) => "")
(check-catch 'value-error ($ "" :tail))

(check-report)
