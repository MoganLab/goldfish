;; list%exists
;; 判断是否存在满足谓词的元素（对标 Scala List.exists）。
;;
;; 语法
;; ----
;; ($ xs :exists p)
;;
;; 参数
;; ----
;; xs : list?
;; 源链表。
;;
;; p : procedure?
;; 单参谓词函数；可用 `_` 占位符简写。
;;
;; 返回值
;; ------
;; boolean
;; 任一元素满足 (p x) 为真则 #t，否则 #f；空表为 #f。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 3 4) :exists (> _ 3)) => #t)
(check ($ '(1 2 3 4) :exists (> _ 10)) => #f)
(check ($ '() :exists (> _ 0)) => #f)
(check ($ '(1 2 3 4) :exists odd?) => #t)

(check-report)
