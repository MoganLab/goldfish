(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; fourth 函数测试
;;
;; 语法
;; ----
;; (fourth list)
;;
;; 参数
;; ----
;; list : list?
;; 至少包含四个元素的列表。
;;
;; 返回值
;; ----
;; any
;; 返回列表的第四个元素。
;;
;; 示例
;; ----
;; (fourth '(1 2 3 4 5 6)) => 4


(check (fourth '(1 2 3 4 5 6)) => 4)


(check-report)
