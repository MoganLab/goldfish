;; raw-string-datum?
;; 判断 datum 是否为原始字符串字面量的内部形式。
;;
;; 语法
;; ----
;; (raw-string-datum? datum)
;;
;; 参数
;; ----
;; datum : any?
;; 待检查的 datum。
;;
;; 返回值
;; ----
;; boolean?
;; 返回 #t 表示 datum 是 (*raw-string* 源码 值) 形式。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(check (raw-string-datum? (list '*raw-string* "#\"x\"" "x")) => #t)
(check (raw-string-datum? (list '*raw-string* "#\"x\"")) => #f)

(check-report)
