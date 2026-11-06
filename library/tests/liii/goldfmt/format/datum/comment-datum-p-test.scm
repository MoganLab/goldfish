;; comment-datum?
;; 判断 datum 是否为注释标记的内部形式。
;;
;; 语法
;; ----
;; (comment-datum? datum)
;;
;; 参数
;; ----
;; datum : any?
;; 待检查的 datum。
;;
;; 返回值
;; ----
;; boolean?
;; 返回 #t 表示 datum 是 (*comment* 内容) 形式。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(check (comment-datum? (list '*comment* "note")) => #t)
(check (comment-datum? (list '*comment*)) => #f)

(check-report)
