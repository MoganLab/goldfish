;; dotted-tail?
;; 判断一个值是否为点尾部记录。
;;
;; 语法
;; ----
;; (dotted-tail? value)
;;
;; 参数
;; ----
;; value : any?
;; 待检查的 Scheme 值。
;;
;; 返回值
;; ----
;; boolean?
;; 返回 #t 表示 value 是点尾部记录；否则返回 #f。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

(check (dotted-tail? (make-dotted-tail)) => #t)
(check (dotted-tail? '()) => #f)
(check (dotted-tail? "string") => #f)
(check (dotted-tail? (make-env)) => #f)

(check-report)
