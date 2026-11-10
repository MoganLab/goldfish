;; reader-rule-head?
;; 判断表单头是否可以作为排版规则的名称。
;;
;; 语法
;; ----
;; (reader-rule-head? head)
;;
;; 参数
;; ----
;; head : any?
;; 表单的第一个元素。
;;
;; 返回值
;; ----
;; boolean?
;; 返回 #t 表示 head 是 symbol 或 syntax 对象。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(check (reader-rule-head? 'car) => #t)
(check (reader-rule-head? 42) => #f)

(check-report)
