;; list%distinct
;; 去重，保留首次出现顺序（对标 Scala List.distinct）。
;;
;; 语法
;; ----
;; ($ xs :distinct)
;;
;; 参数
;; ----
;; xs : list?
;; 源链表。
;;
;; 返回值
;; ------
;; list?
;; 去除重复元素（equal? 比较）后的新链表，保持首次出现顺序。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(1 2 2 3 1) :distinct) => '(1 2 3))
(check ($ '() :distinct) => '())
(check ($ '("a" "b" "a") :distinct) => '("a" "b"))
(check ($ '(1 2 3) :distinct) => '(1 2 3))

(check-report)
