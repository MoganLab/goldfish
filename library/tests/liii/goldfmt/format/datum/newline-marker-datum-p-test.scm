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
;; ------
;; boolean?
;; 返回 #t 表示 datum 是 (*goldfmt-newline-<nonce>* 数量) 形式，
;; 标记符号来自 (liii goldfmt marker)，用户源码不可能包含。

(import (liii check) (liii goldfmt format datum) (liii goldfmt marker))

(check-set-mode! 'report-failed)

(check (newline-marker-datum? (list %newline-marker-symbol 2)) => #t)
(check (newline-marker-datum? (list %newline-marker-symbol 'x)) => #f)
;; 用户写的 (*newline* ...) 是普通列表，不是内部标记
(check (newline-marker-datum? (list '*newline* 2)) => #f)

(check-report)
