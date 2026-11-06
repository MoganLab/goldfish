;; newline-marker-datum?
;; 判断 datum 是否为换行标记的内部形式。
;;
;; 语法
;; ----
;; (newline-marker-datum? datum)
;;
;; 参数
;; ----
;; datum : any?
;; 待检查的 datum。
;;
;; 返回值
;; ----
;; boolean?
;; 返回 #t 表示 datum 是 (*newline* 数量) 形式。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(check (newline-marker-datum? (list '*newline* 2)) => #t)
(check (newline-marker-datum? (list '*newline* 'x)) => #f)

(check-report)
