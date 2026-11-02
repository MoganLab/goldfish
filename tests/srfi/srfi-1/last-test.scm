;; last
;; 获取列表的最后一个元素。
;;
;; 语法
;; ----
;; (last lst)
;;
;; 参数
;; ----
;; lst : pair?
;; 非空列表或点对结构。
;;
;; 返回值
;; ------
;; any
;; 返回列表的最后一个元素。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (last '(a b c)) => 'c)
(check (last '(c)) => 'c)

(check (last '(a b . c)) => 'b)
(check (last '(b . c)) => 'b)

(check-catch 'type-error (last '()))

(check-report)
