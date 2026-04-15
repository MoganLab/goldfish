(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; sixth 函数测试
;;
;; 语法
;; ----
;; (sixth list)
;;
;; 参数
;; ----
;; list : list?
;; 至少包含六个元素的列表。
;;
;; 返回值
;; ----
;; any
;; 返回列表的第六个元素。
;;
;; 示例
;; ----
;; (sixth '(1 2 3 4 5 6 7 8 9 10)) => 6


(check (sixth '(1 2 3 4 5 6 7 8 9 10))
  =>
  6
) ;check


(check-report)
