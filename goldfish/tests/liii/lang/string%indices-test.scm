;; string%indices
;; 获取字符串全部字符的合法索引列表（对标 Scala String.indices）。
;;
;; 语法
;; ----
;; ($ s :indices)
;;
;; 参数
;; ----
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; list?
;; (0 1 ... len-1)，UTF-8 字符序；空串返回 '()。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "abc" :indices) => '(0 1 2))
(check ($ "测试" :indices) => '(0 1))
(check ($ "" :indices) => '())

(check-report)
