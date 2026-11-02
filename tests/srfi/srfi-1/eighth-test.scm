;; eighth
;; 获取列表的第八个元素。
;;
;; 语法
;; ----
;; (eighth list)
;;
;; 参数
;; ----
;; list : pair?
;; 至少包含八个元素的列表。
;;
;; 返回值
;; ------
;; any
;; 返回列表的第八个元素。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (eighth '(1 2 3 4 5 6 7 8 9 10)) => 8)

(check-catch 'type-error (eighth '(1 2 3 4 5 6 7)))
(check-catch 'type-error (eighth 'not-a-list))

(check-report)
