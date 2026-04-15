(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; count 函数测试
;;
;; 语法
;; ----
;; (count pred clist)
;;
;; 参数
;; ----
;; pred : procedure?
;; 谓词函数，接受单个参数并返回布尔值。
;;
;; clist : list?
;; 要计数的列表。
;;
;; 返回值
;; ------
;; integer?
;; 返回列表中满足谓词条件的元素数量。
;;
;; 示例
;; ----
;; (count even? '(3 1 4 1 5 9 2 5 6)) => 3


(check (count even? '(3 1 4 1 5 9 2 5 6))
  =>
  3
) ;check


(check-report)
