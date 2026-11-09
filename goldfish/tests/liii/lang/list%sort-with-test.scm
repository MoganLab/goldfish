;; list%sort-with
;; 按比较器排序（对标 Scala List.sortWith）。
;;
;; 语法
;; ----
;; ($ xs :sort-with cmp)
;;
;; 参数
;; ----
;; cmp : procedure?
;; 二参比较器 (a b)，返回真表示 a 应排在 b 前。
;;
;; 返回值
;; ------
;; list?
;; 排序后的新链表，不改原表。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ '(3 1 2) :sort-with <) => '(1 2 3))
(check ($ '(3 1 2) :sort-with >) => '(3 2 1))
(check ($ '() :sort-with <) => '())
(check ($ '(5) :sort-with <) => '(5))
(check ($ '(3 1 2) :sort-with < :mk-string ",") => "1,2,3")

(check-report)
