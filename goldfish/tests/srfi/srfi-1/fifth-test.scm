;; fifth
;; 获取列表的第五个元素。
;;
;; 语法
;; ----
;; (fifth list)
;;
;; 参数
;; ----
;; list : pair?
;; 至少包含五个元素的列表。
;;
;; 返回值
;; ------
;; any
;; 返回列表的第五个元素。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (fifth '(1 2 3 4 5 6 7 8 9 10)) => 5)

(check-catch 'type-error (fifth '(1 2 3 4)))
(check-catch 'type-error (fifth 'not-a-list))

(check-report)
