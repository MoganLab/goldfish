;; reader-fill-eligible?
;; 判断表单是否可以采用 fill 布局。
;;
;; 语法
;; ----
;; (reader-fill-eligible? datum)
;;
;; 参数
;; ----
;; datum : any?
;; 待检查的表单。
;;
;; 返回值
;; ----
;; boolean?
;; 返回 #t 表示表单可以采用 fill 布局。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(check (reader-fill-eligible? '(a b c d)) => #t)
(check (reader-fill-eligible? '(a b c)) => #f)
(check (reader-fill-eligible? (list 'a 'b :k 'c 'd)) => #f)

(check-report)
