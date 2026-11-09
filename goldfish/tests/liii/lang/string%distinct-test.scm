;; string%distinct
;; 字符串去重，保留首次出现顺序（对标 Scala String.distinct）。
;;
;; 语法
;; ----
;; ($ s :distinct)
;;
;; 参数
;; ----
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; string?
;; 去除重复字符后的新字符串。UTF-8 语义。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "aXbX" :distinct) => "aXb")
(check ($ "测试测试" :distinct) => "测试")
(check ($ "" :distinct) => "")
(check ($ "abc" :distinct) => "abc")

(check-report)
