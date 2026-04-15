(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; seventh 函数测试
;;
;; 语法
;; ----
;; (seventh list)
;;
;; 参数
;; ----
;; list : list?
;; 至少包含七个元素的列表。
;;
;; 返回值
;; ----
;; any
;; 返回列表的第七个元素。
;;
;; 示例
;; ----
;; (seventh '(1 2 3 4 5 6 7 8 9 10)) => 7


(check (seventh '(1 2 3 4 5 6 7 8 9 10))
  =>
  7
) ;check


(check-report)
