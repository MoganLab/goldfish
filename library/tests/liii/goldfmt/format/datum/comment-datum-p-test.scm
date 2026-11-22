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
;; ------
;; boolean?
;; 返回 #t 表示 datum 是 (*goldfmt-comment-<nonce>* 内容) 形式，
;; 标记符号来自 (liii goldfmt marker)，用户源码不可能包含。

(import (liii check) (liii goldfmt format datum) (liii goldfmt marker))

(check-set-mode! 'report-failed)

(check (comment-datum? (list %comment-marker-symbol "note")) => #t)
(check (comment-datum? (list %comment-marker-symbol)) => #f)
;; 用户写的 (*comment* ...) 是普通列表，不是内部标记
(check (comment-datum? (list '*comment* "note")) => #f)

(check-report)
