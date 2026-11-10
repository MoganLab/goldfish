;; reader-form-like?
;; 判断 datum 是否为列表形式的表单。
;;
;; 语法
;; ----
;; (reader-form-like? datum)
;;
;; 参数
;; ----
;; datum : any?
;; 待检查的 datum。
;;
;; 返回值
;; ----
;; boolean?
;; 返回 #t 表示 datum 是 pair。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(check (reader-form-like? '(a)) => #t)
(check (reader-form-like? 'a) => #f)

(check-report)
