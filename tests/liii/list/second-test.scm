(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; second 函数测试
;;
;; 语法
;; ----
;; (second list)
;;
;; 参数
;; ----
;; list : list?
;; 至少包含两个元素的列表。
;;
;; 返回值
;; ----
;; any
;; 返回列表的第二个元素。
;;
;; 示例
;; ----
;; (second '(1 2 3 4 5 6 7 8 9 10)) => 2


(check (second '(1 2 3 4 5 6 7 8 9 10))
  =>
  2
) ;check


(check-catch 'wrong-type-arg
  (second '(left . right))
) ;check-catch
(check-catch 'wrong-type-arg
  (second '(1))
) ;check-catch


(check-report)
