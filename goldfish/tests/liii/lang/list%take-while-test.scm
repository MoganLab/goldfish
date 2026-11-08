;; list%take-while
;; 从头取连续满足谓词的元素（对标 Scala List.takeWhile）。
;;
;; 语法
;; ----
;; ($ xs :take-while p)
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
;; list?
;; 最长前缀且每个元素满足 (p x)；首元素不满足或空表返回 '()。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3 4) :take-while (< _ 3)) => '(1 2))
(check ($ '(5 1) :take-while (< _ 3)) => '())
(check ($ '() :take-while (> _ 0)) => '())
(check ($ '(1 2 3) :take-while (> _ 0)) => '(1 2 3))

(check-report)
