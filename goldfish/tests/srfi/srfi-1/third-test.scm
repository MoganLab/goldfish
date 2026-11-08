;; third
;; 获取列表的第三个元素。
;;
;; 语法
;; ----
;; (third list)
;;
;; 参数
;; ----
;; list : pair?
;; 至少包含三个元素的列表。
;;
;; 返回值
;; ------
;; any
;; 返回列表的第三个元素。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (third '(1 2 3 4 5 6 7 8 9 10)) => 3)

(check-catch 'type-error (third '(1 2)))

(check-report)
