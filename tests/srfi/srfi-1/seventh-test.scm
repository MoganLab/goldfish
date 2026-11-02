;; seventh
;; 获取列表的第七个元素。
;;
;; 语法
;; ----
;; (seventh list)
;;
;; 参数
;; ----
;; list : pair?
;; 至少包含七个元素的列表。
;;
;; 返回值
;; ------
;; any
;; 返回列表的第七个元素。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (seventh '(1 2 3 4 5 6 7 8 9 10)) => 7)

(check-catch 'type-error (seventh '(1 2 3 4 5 6)))
(check-catch 'type-error (seventh 'not-a-list))

(check-report)
