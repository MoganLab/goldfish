;; raw-string-literal?
;; 判断一个值是否为原始字符串字面量记录。
;;
;; 语法
;; ----
;; (raw-string-literal? value)
;;
;; 参数
;; ----
;; value : any?
;; 待检查的 Scheme 值。
;;
;; 返回值
;; ----
;; boolean?
;; 返回 #t 表示 value 是原始字符串字面量记录；否则返回 #f。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

(check (raw-string-literal? (make-raw-string-literal)) => #t)
(check (raw-string-literal? "string") => #f)
(check (raw-string-literal? '()) => #f)
(check (raw-string-literal? (make-env)) => #f)

(check-report)
