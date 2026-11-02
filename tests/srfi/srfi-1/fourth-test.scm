;; fourth
;; 获取列表的第四个元素。
;;
;; 语法
;; ----
;; (fourth list)
;;
;; 参数
;; ----
;; list : pair?
;; 至少包含四个元素的列表。
;;
;; 返回值
;; ------
;; any
;; 返回列表的第四个元素。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (fourth '(1 2 3 4 5 6)) => 4)

(check-catch 'type-error (fourth '(1 2 3)))
(check-catch 'type-error (fourth 'not-a-list))

(check-report)
