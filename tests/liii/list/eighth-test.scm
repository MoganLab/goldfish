(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; eighth 函数测试
;;
;; 语法
;; ----
;; (eighth list)
;;
;; 参数
;; ----
;; list : list?
;; 至少包含八个元素的列表。
;;
;; 返回值
;; ----
;; any
;; 返回列表的第八个元素。
;;
;; 示例
;; ----
;; (eighth '(1 2 3 4 5 6 7 8 9 10)) => 8


(check (eighth '(1 2 3 4 5 6 7 8 9 10))
  =>
  8
) ;check


(check-report)
