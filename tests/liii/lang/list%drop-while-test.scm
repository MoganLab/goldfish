;; list%drop-while
;; 从头跳过连续满足谓词的元素（对标 Scala List.dropWhile）。
;;
;; 语法
;; ----
;; ($ xs :drop-while p)
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
;; 去掉最长满足前缀后的剩余；全部满足返回 '()。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3) :drop-while (< _ 3)) => '(3))
(check ($ '(1 2 3) :drop-while (> _ 0)) => '())
(check ($ '() :drop-while (> _ 0)) => '())
(check ($ '(5 1) :drop-while (< _ 3)) => '(5 1))

(check-report)
