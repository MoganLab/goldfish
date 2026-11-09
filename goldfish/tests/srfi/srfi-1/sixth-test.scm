;; sixth
;; 获取列表的第六个元素。
;;
;; 语法
;; ----
;; (sixth list)
;;
;; 参数
;; ----
;; list : pair?
;; 至少包含六个元素的列表。
;;
;; 返回值
;; ------
;; any
;; 返回列表的第六个元素。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (sixth '(1 2 3 4 5 6 7 8 9 10)) => 6)

(check-catch 'type-error (sixth '(1 2 3 4 5)))
(check-catch 'type-error (sixth 'not-a-list))

(check-report)
