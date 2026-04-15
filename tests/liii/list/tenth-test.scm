(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; tenth 函数测试
;;
;; 语法
;; ----
;; (tenth list)
;;
;; 参数
;; ----
;; list : list?
;; 至少包含十个元素的列表。
;;
;; 返回值
;; ----
;; any
;; 返回列表的第十个元素。
;;
;; 示例
;; ----
;; (tenth '(1 2 3 4 5 6 7 8 9 10)) => 10


(check (tenth '(1 2 3 4 5 6 7 8 9 10))
  =>
  10
) ;check


(check-report)
