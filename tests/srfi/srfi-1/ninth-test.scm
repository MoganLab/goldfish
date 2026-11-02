;; ninth
;; 获取列表的第九个元素。
;;
;; 语法
;; ----
;; (ninth list)
;;
;; 参数
;; ----
;; list : pair?
;; 至少包含九个元素的列表。
;;
;; 返回值
;; ------
;; any
;; 返回列表的第九个元素。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (ninth '(1 2 3 4 5 6 7 8 9 10)) => 9)

(check-catch 'type-error (ninth '(1 2 3 4 5 6 7 8)))
(check-catch 'type-error (ninth 'not-a-list))

(check-report)
