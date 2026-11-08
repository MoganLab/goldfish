;; list%forall
;; 判断是否全部元素满足谓词（对标 Scala List.forall）。
;;
;; 语法
;; ----
;; ($ xs :forall p)
;;
;; 参数
;; ----
;; p : procedure?
;; 谓词；可用 `_` 占位符简写。
;;
;; xs : list?
;; 源链表。
;;
;; 返回值
;; ------
;; boolean
;; 所有元素满足 (p x) 为真则 #t；空表为 #t（空真）。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3 4) :forall (> _ 0)) => #t)
(check ($ '(1 2 3 4) :forall (> _ 1)) => #f)
(check ($ '() :forall (> _ 0)) => #t)
(check ($ '(2 4 6) :forall even?) => #t)

(check-report)
